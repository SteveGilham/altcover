namespace Tests

open System
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open System.Xml.Linq

open AltCover
open AltCover.Augment
open AltCover.Filter
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open N
open Newtonsoft.Json.Linq
open NUnit.Framework

type ProxyObject() =
   inherit MarshalByRefObject()

   member val Type : Type option = None with get, set
   member val Object = null with get, set
   member this.InstantiateObject(assemblyPath:string, typeName:string, args:obj[]) =
        let assembly = Assembly.LoadFrom(assemblyPath) //LoadFrom loads dependent DLLs (assuming they are in the app domain's base directory
        let t = assembly.ExportedTypes
                |> Seq.filter (fun t -> t.FullName = typeName)
        this.Type <- Seq.tryHead t
        match this.Type with
        | None -> ()
        | Some t -> this.Object <- Activator.CreateInstance(t, args)

   member this.InvokeMethod(methodName:string, args:obj[]) =
        match this.Type with
        | None -> null
        | Some t -> let  methodinfo = t.GetMethod(methodName)
                    methodinfo.Invoke(this.Object, args)

[<TestFixture>]
type AltCoverTests() = class

#if NETCOREAPP2_0
  let sample1 = "Sample1.dll"
  let monoSample1 = "../_Mono/Sample1"
#else
  let sample1 = "Sample1.exe"
  let monoSample1 = "_Mono/Sample1"
  let recorderSnk = typeof<AltCover.Node>.Assembly.GetManifestResourceNames()
                    |> Seq.find (fun n -> n.EndsWith(".Recorder.snk", StringComparison.Ordinal))
#endif

  let infrastructureSnk = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                          |> Seq.find (fun n -> n.EndsWith("Infrastructure.snk", StringComparison.Ordinal))

  // Hack for running while instrumented
  static member private Hack () =
    let where = Assembly.GetExecutingAssembly().Location
    let dir = where |> Path.GetDirectoryName |> Path.GetFileName
    match dir.IndexOf "__" with
    | 0 -> "/.."
    | _ -> String.Empty

  // Augment.fs

  [<Test>]
  member self.AugmentNullableDetectNulls() =
    let input = [ "string"; null; "another string" ]
    let nulls = input |> Seq.map (Option.nullable >> Option.isNone)
    Assert.That(nulls, Is.EquivalentTo([false; true; false]))

  [<Test>]
  member self.AugmentGetOrElseFillsInNone() =
    let input = [ "string"; null; "another string" ]
    let strings = input |> Seq.map (Option.nullable >> (Option.getOrElse "fallback"))
    Assert.That(strings, Is.EquivalentTo([ "string"; "fallback"; "another string" ]))

  // ProgramDatabase.fs

  [<Test>]
  member self.ShouldGetPdbFromImage() =
    let where = Assembly.GetExecutingAssembly().Location
    let pdb = Path.ChangeExtension(where, ".pdb")
    if File.Exists(pdb) then
      // Hack for running while instrumented
      let files = Directory.GetFiles(Path.GetDirectoryName(where) + AltCoverTests.Hack())
                  |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                                          || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.map (fun x -> (x, Mono.Cecil.AssemblyDefinition.ReadAssembly x))
                  |> Seq.filter (fun x -> (fst x) + ".mdb" |> File.Exists |> not)
#if NETCOREAPP2_0
                  |> Seq.filter (fun x -> not <| (snd x).FullName.StartsWith("Mono.", StringComparison.OrdinalIgnoreCase))
                  |> Seq.filter (fun x -> not <| (snd x).FullName.StartsWith("nunit", StringComparison.OrdinalIgnoreCase))
#else
                  |> Seq.filter (fun x -> (snd x).FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.OrdinalIgnoreCase))
#endif
                  |> Seq.toList
      Assert.That(files, Is.Not.Empty)
      files
      |> Seq.iter( fun x ->let pdb = AltCover.ProgramDatabase.GetPdbFromImage (snd x)
                           match pdb with
                           | None -> Assert.Fail("No .pdb for " + (fst x))
                           | Some name ->
                              let probe = Path.ChangeExtension((fst x), ".pdb")
                              let file = FileInfo(probe)
                              let filename = file.Name.Replace("\\","/")
                              Assert.That("/" + name.Replace("\\","/"), Does.EndWith("/" + filename), (fst x) + " -> " + name) )

  [<Test>]
  member self.ShouldGetNoMdbFromMonoImage() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
                |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                                        || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                |> Seq.map (fun x -> (x, Mono.Cecil.AssemblyDefinition.ReadAssembly x))
                |> Seq.toList
    Assert.That(files, Is.Not.Empty)
    files
    |> Seq.iter( fun x -> let probe = (fst x) + ".mdb"
                          let pdb = AltCover.ProgramDatabase.GetPdbFromImage (snd x)
                          match pdb with
                          | None -> Assert.That(File.Exists probe, probe + " not found" )
                          | Some name -> Assert.Fail("Suddenly, an .mdb for " + (fst x)))

  [<Test>]
  member self.ShouldGetPdbWithFallback() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + AltCoverTests.Hack())
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x -> (x + ".mdb") |> File.Exists |> not)
    |> Seq.iter( fun x ->
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly x
      let pdb = AltCover.ProgramDatabase.GetPdbWithFallback(def)
      match pdb with
      | None -> Assert.That(File.Exists(Path.ChangeExtension(x, ".pdb")), Is.Not.True, "No .pdb for " + x)
      | Some name ->
         let probe = Path.ChangeExtension(x, ".pdb")
         let file = FileInfo(probe)
         let filename = file.Name.Replace("\\","/")
         Assert.That("/"+ name.Replace("\\","/"), Does.EndWith("/" + filename), x + " -> " + name)
    )

  [<Test>]
  member self.ShouldGetForeignPdbWithFallback() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "packages")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../packages")
#else
    let path' = path
#endif
    // Looking for the Mono.Options symbols
    let files = Directory.GetFiles(path', "*.pdb", SearchOption.AllDirectories)
    files
    |> Seq.filter(fun p -> Path.ChangeExtension(p, ".dll") |> File.Exists)
    |> Seq.iter( fun p ->
      let dll = Path.ChangeExtension(p, ".dll")
      try
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly dll
        let pdb = AltCover.ProgramDatabase.GetPdbWithFallback(def)
        match pdb with
        | None -> Assert.Fail("Not found " + p)
        | Some name -> Assert.That(name, Is.EqualTo p)
      with
      | :? BadImageFormatException -> ()
    )

  [<Test>]
  member self.ShouldGetMdbWithFallback() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter( fun x ->
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly x
      let mdb = AltCover.ProgramDatabase.GetPdbWithFallback(def)
      match mdb with
      | None -> Assert.That(File.Exists(x + ".mdb"), Is.Not.True, "No .mdb for " + x)
      | Some name ->
         let probe = x + ".mdb"
         let file = FileInfo(probe)
         let filename = file.Name.Replace("\\","/")
         Assert.That(name.Replace("\\","/") + ".mdb", Does.EndWith("/" + filename), x + " -> " + name)
    )

  [<Test>]
  member self.ShouldGetSymbolsFromPdb() =
   let where = Assembly.GetExecutingAssembly().Location
   let pdb = Path.ChangeExtension(where, ".pdb")
   if File.Exists(pdb) then
    // Hack for running while instrumented
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + AltCoverTests.Hack())
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.map Mono.Cecil.AssemblyDefinition.ReadAssembly
    |> Seq.filter (fun x -> x.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter (fun def ->
      let reader = AltCover.ProgramDatabase.ReadSymbols def
      Assert.That (def.MainModule.HasSymbols, def.MainModule.FileName)
      Assert.That (Option.isSome reader, def.MainModule.FileName)
    )

  [<Test>]
  member self.ShouldNotGetSymbolsWhenNoPdb() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + AltCoverTests.Hack())
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x -> Path.GetFileName(x).StartsWith("FSharp", StringComparison.OrdinalIgnoreCase))
    |> Seq.map Mono.Cecil.AssemblyDefinition.ReadAssembly
    |> Seq.filter (fun x -> not <| x.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter (fun def ->
      let reader = AltCover.ProgramDatabase.ReadSymbols def
      Assert.That (not def.MainModule.HasSymbols, def.MainModule.FileName)
      Assert.That (Option.isNone reader, def.MainModule.FileName)
    )

  [<Test>]
  member self.ShouldGetSymbolsFromMdb() =
    let where = Assembly.GetExecutingAssembly().Location
    let pdb = Path.ChangeExtension(where, ".pdb")
    // Hack for running while instrumented
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter( fun x ->
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly x
      AltCover.ProgramDatabase.ReadSymbols def |> ignore
      Assert.That (def.MainModule.HasSymbols, def.MainModule.FileName)
    )

  // Filter.fs

  [<Test>]
  member self.NoneOfTheAboveMatchesNoType() =
     Assert.That (Match () (FilterClass.Type (Regex "23")), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoAttribute() =
     Assert.That (Match () (FilterClass.Attribute (Regex "23")), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoAssembly() =
     Assert.That (Match () (FilterClass.Assembly (Regex "23")), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoModule() =
     Assert.That (Match () (FilterClass.Module (Regex "23")), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoFile() =
     Assert.That (Match () (FilterClass.File (Regex "23")), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoMethod() =
     Assert.That (Match () (FilterClass.Method (Regex "23")), Is.False)

  [<Test>]
  member self.FileDoesNotMatchNonFileClass() =
     Assert.That (Match (Assembly.GetExecutingAssembly().Location) (FilterClass.Type (Regex "23")), Is.False)

  [<Test>]
  member self.FileDoesMatchFileClass() =
     Assert.That (Match (Assembly.GetExecutingAssembly().Location) (FilterClass.File (Regex "Cove")), Is.True)

  [<Test>]
  member self.AssemblyDoesNotMatchNonAssemblyClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That (Match def (FilterClass.Type (Regex "23")), Is.False)

  [<Test>]
  member self.AssemblyDoesMatchAssemblyClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That (Match def (FilterClass.Assembly (Regex "Cove")), Is.True)

  [<Test>]
  member self.ModuleDoesNotMatchNonModuleClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That (Match def.MainModule (FilterClass.Type (Regex "23")), Is.False)

  [<Test>]
  member self.ModuleDoesMatchModuleClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That (Match def.MainModule (FilterClass.Module (Regex "Cove")), Is.True)

  [<Test>]
  member self.TypeDoesNotMatchNonTypeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.iter (fun t -> Assert.That (Match t (FilterClass.File (Regex "23")), Is.False, t.FullName))

  [<Test>]
  member self.TypeDoesMatchTypeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.filter (fun t -> t.IsPublic && t.Name.Contains("AltCover"))  // exclude the many compiler generted chaff classes
     |> Seq.iter (fun t -> Assert.That (Match t (FilterClass.Type (Regex "Cove")), Is.True, t.FullName))

  [<Test>]
  member self.MethodDoesNotMatchNonMethodClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.filter (fun t -> t.IsPublic)
     |> Seq.collect (fun t -> t.Methods)
     |> Seq.iter (fun m -> Assert.That (Match m (FilterClass.Type (Regex "23")), Is.False))

  [<Test>]
  member self.MethodDoesMatchMethodClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That(def.MainModule.Types
                 |> Seq.filter (fun t -> t.IsPublic)  // exclude the many compiler generted chaff classes
                 |> Seq.collect (fun t -> t.Methods)
                 |> Seq.filter (fun m -> m.IsPublic && (not m.IsConstructor))
                 |> Seq.filter (fun m -> Match m (FilterClass.Method (Regex "Augment")))
                 |> Seq.length,
                 Is.EqualTo(2))

  [<Test>]
  member self.AttributeDoesNotMatchNonAttributeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.iter (fun t -> Assert.That (Match t.CustomAttributes (FilterClass.File (Regex "23")), Is.False, t.FullName))

  [<Test>]
  member self.AttributeDoesMatchAttributeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.filter (fun t -> t.IsPublic && t.Name.Contains("AltCover"))  // exclude the many compiler generted chaff classes
     |> Seq.iter (fun t -> Assert.That (Match t (FilterClass.Attribute (Regex "Fix")), Is.True, t.FullName))

  [<Test>]
  member self.Sample3Class1IsCSharpAutoproperty() =
     let sample3 = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Sample3.dll")
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (sample3)
     def.MainModule.Types
     |> Seq.filter(fun t -> t.Name = "Class1")
     |> Seq.collect (fun t ->t.Methods)
     |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
     |> Seq.iter (IsCSharpAutoProperty >> Assert.That)

  [<Test>]
  member self.Sample3Class2IsNotCSharpAutoproperty() =
     let sample3 = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Sample3.dll")
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (sample3)
     def.MainModule.Types
     |> Seq.filter(fun t -> t.Name = "Class2")
     |> Seq.collect (fun t -> t.Methods)
     |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
     |> Seq.iter (fun m -> Assert.That(IsCSharpAutoProperty m, Is.False))

  [<Test>]
  member self.CanIdentifyExcludedFSharpMethods() =
    let tracer = DU.returnFoo 23
    let location = tracer.GetType().Assembly.Location
    let sourceAssembly = AssemblyDefinition.ReadAssembly(location)

    let direct = sourceAssembly.MainModule.Types
                 |> Seq.filter (fun x -> x.Namespace = "N" )
                 |> Seq.toList
    let indirect = direct
                   |> Seq.filter (fun t-> t.HasNestedTypes)
                   |> Seq.collect (fun t -> t.NestedTypes)
                   |> Seq.toList // MyUnion, MyThing
    let indirect2 = indirect
                   |> Seq.filter (fun t-> t.HasNestedTypes)
                   |> Seq.collect (fun t -> t.NestedTypes)
                   |> Seq.toList // Foo, Bar, ...

    let indirect3 = indirect2
                    |> Seq.filter (fun t-> t.HasNestedTypes)
                    |> Seq.collect (fun t -> t.NestedTypes)
                    |> Seq.map (fun t -> t.FullName)
                    |> Seq.toList

    Assert.That (indirect3
                   |> Seq.isEmpty, sprintf "Third order types found %A" indirect3)

    let pass = Seq.concat [direct; indirect; indirect2]
               |> Seq.collect (fun t -> t.Methods)
               |> Seq.filter (not << Filter.IsFSharpInternal)
               |> Seq.map (fun x -> x.Name)
               |> Seq.sort
               |> Seq.toList

    let expected = [  ".ctor" ; "Invoke"; "as_bar"; "bytes"; "get_MyBar" ;
#if NETCOREAPP2_0
                      "main";
#endif
                      "makeThing"; "returnBar"; "returnFoo"; "testMakeThing"; "testMakeUnion" ]

    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass);

  [<Test>]
  member self.CanIdentifyExcludedCSharpAutoProperties() =
    let location = typeof<Sample3.Class1>.Assembly.Location
    let sourceAssembly = AssemblyDefinition.ReadAssembly(location)

    let direct = sourceAssembly.MainModule.Types
                 |> Seq.filter (fun x -> x.Name = "Class1" )
                 |> Seq.head
    let pass = direct.Methods
               |> Seq.filter (not << Filter.IsCSharpAutoProperty)
               |> Seq.map (fun x -> x.Name)
               |> Seq.sort
               |> Seq.toList

    let expected = [".ctor"]
    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass);

  [<Test>]
  member self.CanIdentifyIncludedCSharpProperties() =
    let location = typeof<Sample3.Class1>.Assembly.Location
    let sourceAssembly = AssemblyDefinition.ReadAssembly(location)

    let direct = sourceAssembly.MainModule.Types
                 |> Seq.filter (fun x -> x.Name = "Class2" )
                 |> Seq.head
    let pass = direct.Methods
               |> Seq.filter (not << Filter.IsCSharpAutoProperty)
               |> Seq.map (fun x -> x.Name)
               |> Seq.sort
               |> Seq.toList

    let expected = [".ctor"; "get_Property"; "set_Property"]
    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass);

  // Visitor.fs

  [<Test>]
  member self.EmptyArrayHasExpectedHash() =
    Assert.That ((KeyStore.TokenOfArray [| |]), Is.EquivalentTo [|9uy; 7uy; 216uy; 175uy; 144uy; 24uy; 96uy; 149uy|])

  member private self.ProvideKeyPair () =
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      StrongNameKeyPair(buffer.ToArray())

  [<Test>]
  member self.KeyHasExpectedToken() =
    let token = KeyStore.TokenOfKey <| self.ProvideKeyPair ()
    let token' = String.Join(String.Empty, token |> List.map (fun x -> x.ToString("x2")))
    Assert.That (token', Is.EqualTo(//"c02b1a9f5b7cade8"))
#if NETCOREAPP2_0
                                    "0907d8af90186095"
#else
                                    "c02b1a9f5b7cade8"
#endif
    ), token')

  [<Test>]
  member self.TokenGeneratesExpectedULong() =
    let token = [|1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]
    Assert.That (KeyStore.TokenAsULong token, Is.EqualTo(1UL))

  [<Test>]
  member self.KeyHasExpectedIndex() =
    let token = KeyStore.KeyToIndex <| self.ProvideKeyPair ()
    Assert.That (token, Is.EqualTo(
#if NETCOREAPP2_0
                                    0x95601890afd80709UL
#else
                                    0xe8ad7c5b9f1a2bc0UL
#endif
    ), sprintf "%x" token)

  [<Test>]
  member self.EmptyArrayHasExpectedIndex() =
    Assert.That ((KeyStore.ArrayToIndex [| |]), Is.EqualTo(0x95601890afd80709UL))

  [<Test>]
  member self.KeyHasExpectedRecord() =
    let pair = self.ProvideKeyPair ()
    let token = KeyStore.KeyToRecord <| pair
    Assert.That (token, Is.EqualTo({Pair = pair; Token = BitConverter.GetBytes(
#if NETCOREAPP2_0
                                                                               0x95601890afd80709UL
#else
                                                                               0xe8ad7c5b9f1a2bc0UL
#endif
                                                                               ) |> Array.toList}))

  [<Test>]
  member self.KeyHasExpectedPlaceInIndex() =
    try
      Assert.That (Visitor.keys.Keys.Count, Is.EqualTo(0))
      let pair = self.ProvideKeyPair ()
      Visitor.Add(pair)
      let key =
#if NETCOREAPP2_0
                                    0x95601890afd80709UL
#else
                                    0xe8ad7c5b9f1a2bc0UL
#endif
      Assert.That(Visitor.keys.ContainsKey(key))
      Assert.That(Visitor.keys.[key], Is.EqualTo({Pair = pair; Token = BitConverter.GetBytes(key) |> Array.toList}))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.EmptyFiltersPassAll() =
    Visitor.NameFilters.Clear()
    Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
    Assert.That (Visitor.IsIncluded self)

  [<Test>]
  member self.NonEmptyFiltersCatchAnExpectedValue() =
    try
      Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
      Visitor.NameFilters.AddRange([ FilterClass.File (Regex "Cove")
                                     FilterClass.Method (Regex "Augment")])
      Assert.That (Visitor.IsIncluded (Assembly.GetExecutingAssembly().Location), Is.False)
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.NonEmptyFiltersPassAnExpectedValue() =
    try
      Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
      Visitor.NameFilters.AddRange([ FilterClass.File (Regex "System")
                                     FilterClass.Method (Regex "Augment")])
      Assert.That (Visitor.IsIncluded (Assembly.GetExecutingAssembly().Location))
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.AfterProcessingYieldsAnExpectedValue() =
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
    let inputs = [ Node.Start [] ; Node.Assembly (def, None, true) ; Node.Module (null, None, false) ; Node.Type (null, None, true) ;
                   Node.Method (null, None, false) ; Node.MethodPoint (null, null, 0, true ) ;
                   Node.AfterMethod false ; Node.AfterModule ; Node.AfterAssembly def; Node.Finish ]
    let outputs = inputs |> Seq.map (Visitor.After >> Seq.toList)
    let expected = [ [Finish]; [AfterAssembly def]; [AfterModule]; []; [AfterMethod false]; []; []; []; []; []]
    Assert.That (outputs, Is.EquivalentTo (expected))

  [<Test>]
  member self.Sample3Class1PropertyIsNotSignificant() =
     let sample3 = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Sample3.dll")
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (sample3)
     def.MainModule.Types
     |> Seq.filter(fun t -> t.Name = "Class1")
     |> Seq.collect (fun t -> t.Methods)
     |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
     |> Seq.iter (fun m -> Assert.That(Visitor.significant m, Is.False))

  [<Test>]
  member self.Sample3Class2IPropertyIsSignificant() =
     let sample3 = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Sample3.dll")
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (sample3)
     def.MainModule.Types
     |> Seq.filter(fun t -> t.Name = "Class2")
     |> Seq.collect (fun t -> t.Methods)
     |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
     |> Seq.iter (Visitor.significant >> Assert.That)

  [<Test>]
  member self.TerminalCasesGoNoDeeper() =
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
    let inputs = [ Node.MethodPoint (null, null, 0, true ) ;
                   Node.AfterMethod false ; Node.AfterModule ; Node.AfterAssembly def; Node.Finish ]
    let outputs = inputs |> Seq.map (Visitor.Deeper>> Seq.toList)
    let expected = [[]; []; []; []; []]
    Assert.That (outputs, Is.EquivalentTo (expected))

  [<Test>]
  member self.MethodPointsAreDeeperThanMethods() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let reader = ProgramDatabase.ReadSymbols def
    let method = (def.MainModule.Types |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))|> Seq.head).Methods |> Seq.head
    Visitor.Visit [] [] // cheat reset
    try
        "Program" |> (Regex >> FilterClass.File >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Method (method,
                                                    reader
                                                    |> Option.map (fun r -> r.Read method),
                                                    true)
                     |> Seq.toList
        Assert.That (deeper.Length, Is.EqualTo 10)
        deeper
        |> List.iteri (fun i node -> match node with
                                     | (MethodPoint (_, _, n, b)) ->
                                           Assert.That(n, Is.EqualTo i, "point number")
                                           Assert.That (b, Is.False, "flag")
                                     | _ -> Assert.Fail())
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.MethodsAreDeeperThanTypes() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let rdr = ProgramDatabase.ReadSymbols def
    let type' = (def.MainModule.Types |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))|> Seq.head)
    Visitor.Visit [] [] // cheat reset
    try
        "Main" |> (Regex >> FilterClass.Method >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Type (type', rdr, true)
                     |> Seq.toList
        Visitor.Visit [] [] // cheat reset
        let dbg = Option.get rdr
        let expected = type'.Methods
                    |> Seq.map (fun m -> let flag = m.Name = ".ctor"
                                         let node = Node.Method (m, Option.nullable (dbg.Read m), flag)
                                         List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node;  [Node.AfterMethod flag]])
                    |> List.concat
        Assert.That (deeper.Length, Is.EqualTo 14)
        Assert.That (deeper |> Seq.map string,
                     Is.EquivalentTo (expected |> Seq.map string))
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.TypesAreDeeperThanModules() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let rdr = ProgramDatabase.ReadSymbols def
    let module' = def.MainModule
    Visitor.Visit [] [] // cheat reset
    try
        "Program" |> (Regex >> FilterClass.Type >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Module (module', rdr, true)
                     |> Seq.toList
        Visitor.Visit [] [] // cheat reset
        let expected = module'.Types // we have no nested types in this test
                    |> Seq.map (fun t -> let flag = t.Name <> "Program"
                                         let node = Node.Type (t, rdr, flag)
                                         List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node])
                    |> List.concat
        Assert.That (deeper.Length, Is.EqualTo 16)
        Assert.That (deeper |> Seq.map string,
                     Is.EquivalentTo (expected |> Seq.map string))
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ModulesAreDeeperThanAssemblies() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let rdr = ProgramDatabase.ReadSymbols def
    Visitor.Visit [] [] // cheat reset
    let deeper = Visitor.Deeper <| Node.Assembly (def, rdr, true)
                 |> Seq.toList
    Visitor.Visit [] [] // cheat reset
    let expected = def.Modules // we have no nested types in this test
                |> Seq.map (fun t -> let node = Node.Module (t, rdr, true)
                                     List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node; [AfterModule]])
                |> List.concat
    Assert.That (deeper.Length, Is.EqualTo 18)
    Assert.That (deeper |> Seq.map string,
                 Is.EquivalentTo (expected |> Seq.map string))

  [<Test>]
  member self.AssembliesAreDeeperThanPaths() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    let deeper = Visitor.Deeper <| Node.Start [path]
                    |> Seq.toList
    // assembly definitions care about being separate references in equality tests
    let (def, rdr) = match Seq.head deeper with
                     | Node.Assembly (def', Some rdr', true) -> (def', rdr')
                     | _ -> Assert.Fail(); (null, null)

    let assembly = Node.Assembly (def, Some rdr, true)
    let expected = List.concat [ [assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def]]
    Assert.That (deeper.Length, Is.EqualTo 20)
    Assert.That (deeper |> Seq.map string,
                 Is.EquivalentTo (expected |> Seq.map string))

  [<Test>]
  member self.FilteredAssembliesDoNotHaveSequencePoints() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    try
        "Sample" |> (Regex >> FilterClass.Assembly >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Start [path]
                     |> Seq.toList
        // assembly definitions care about being separate references in equality tests
        let def = match Seq.head deeper with
                  | Node.Assembly (def', None, false) -> def'
                  | _ -> Assert.Fail(); null

        let assembly = Node.Assembly (def, None, false)
        let expected = List.concat [ [assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def]]
        Assert.That (deeper.Length, Is.EqualTo 10)
        Assert.That (deeper, Is.EquivalentTo expected)
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.TestFixPointInvoke() =
    let mutable called = 0
    let rec stateful l = new Fix<Node> (
                           fun node ->
                           called <- called + 1
                           stateful node)

    let input = stateful (Start[])
    let fix = Visitor.invoke (Start[]) input
    Assert.That (called, Is.EqualTo 1)
    Assert.That (fix, Is.Not.Null)
    Assert.That (fix.GetType(), Is.EqualTo(input.GetType()))

  [<Test>]
  member self.TestFixPointApply() =
    let mutable called = 0
    let rec stateful l = new Fix<Node> (
                           fun node ->
                           called <- called + 1
                           stateful node)

    let list = [stateful (Start[]); stateful (Start[])]
    let fix = Visitor.apply list (Start[])
              |> Seq.toList
    Assert.That (called, Is.EqualTo 2)
    Assert.That (Seq.length fix, Is.EqualTo 2)
    Assert.That (fix.[0].GetType(), Is.EqualTo(list.[0].GetType()))
    Assert.That (fix.[1].GetType(), Is.EqualTo(list.[1].GetType()))

  [<Test>]
  member self.PathsAreDeeperThanAVisit() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let accumulator = System.Collections.Generic.List<Node>()
    let fix = Visitor.EncloseState (fun (x:System.Collections.Generic.List<Node>) t -> x.Add t; x) accumulator
    Visitor.Visit [fix] [path]
    // assembly definitions care about being separate references in equality tests
    let (def, rdr) = match accumulator.[1] with
                     | Node.Assembly (def', Some rdr', true) -> (def', rdr')
                     | _ -> Assert.Fail(); (null, null)

    let assembly = Node.Assembly (def, Some rdr, true)
    let expected = List.concat [ [Start[path]; assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def; Finish]]
    Assert.That (accumulator |> Seq.map string,
                 Is.EquivalentTo (expected |> Seq.map string))

  // Naming.fs

  [<Test>]
  member self.NamingDetectNulls() =
    let input = [ "string"; null; "another string" ]
    let nulls = input |> Seq.map Naming.isNotNull
    Assert.That(nulls, Is.EquivalentTo([true; false; true]))

  [<Test>]
  member self.NamingDetectEmpties() =
    let input = [ "string"; null; "another string"; "             " ]
    let nulls = input |> Seq.map Naming.emptyIfIsNullOrWhiteSpace
    Assert.That(nulls, Is.EquivalentTo([ "string"; String.Empty; "another string"; String.Empty ]))

  [<Test>]
  member self.NamingSuffixDetectEmpties() =
    let input = [ "string"; null; "another string"; "             " ]
    let nulls = input |> Seq.map (fun n -> Naming.suffixIfNotIsNullOrWhiteSpace n "*")
    Assert.That(nulls, Is.EquivalentTo([ "string*"; String.Empty; "another string*"; String.Empty ]))

  [<Test>]
  member self.TypeNamesAreExtracted() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.map Naming.TypeName
                |> Seq.toList
    let expected = ["<Module>"; "Class1"; "Class2"; "Class3"; "Class4" ]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.FullTypeNamesAreExtracted() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.map Naming.FullTypeName
                |> Seq.toList
    let expected = ["<Module>"; "Sample3.Class1"; "Sample3.Class2"; "Sample3.Class3"; "Sample3.Class3+Class4"]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.TypeRefNamesAreExtracted() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.map (fun td -> Naming.TypeRefName(TypeReference(td.Namespace, td.Name, def.MainModule, null)))
                |> Seq.toList
    let expected = ["<Module>"; "Class1"; "Class2"; "Class3"; "Class4" ]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.FullTypeRefNamesAreExtracted() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.map (fun td -> let tr = TypeReference(td.Namespace, td.Name, def.MainModule, null)
                                      if Naming.isNotNull td.DeclaringType then
                                         tr.DeclaringType <- TypeReference(td.DeclaringType.Namespace, td.DeclaringType.Name, def.MainModule, null)
                                      Naming.FullTypeRefName(tr))
                |> Seq.toList
    let expected = ["<Module>"; "Sample3.Class1"; "Sample3.Class2"; "Sample3.Class3"; "Sample3.Class3+Class4"]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.MethodNamesAreExtracted() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.map Naming.MethodName
                |> Seq.toList
    let expected = ["get_Property"; "set_Property"; "#ctor"; "get_Property"; "set_Property";
                      "#ctor"; "get_Visits"; "Log"; "#ctor"; ".cctor";
                      "get_Property"; "set_Property"; "get_ReportFile";
                      "set_ReportFile"; "get_Token"; "set_Token"; "ToList"; "#ctor" ]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.FullMethodNamesAreExtracted() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.map Naming.FullMethodName
                |> Seq.toList
    let expected = ["System.Int32 Sample3.Class1.get_Property()"; "System.Void Sample3.Class1.set_Property(System.Int32)";
                    "System.Void Sample3.Class1.#ctor()"; "System.Int32 Sample3.Class2.get_Property()";
                    "System.Void Sample3.Class2.set_Property(System.Int32)"; "System.Void Sample3.Class2.#ctor()";
                    "System.Collections.Generic.List`1 Sample3.Class3.get_Visits()"
                    "System.Void Sample3.Class3.Log(System.String,System.Int32)"
                    "System.Void Sample3.Class3.#ctor()"; "System.Void Sample3.Class3..cctor()"
                    "Sample3.Class1 Sample3.Class3+Class4.get_Property()";
                    "System.Void Sample3.Class3+Class4.set_Property(Sample3.Class1)"
                    "System.String Sample3.Class3+Class4.get_ReportFile()"
                    "System.Void Sample3.Class3+Class4.set_ReportFile(System.String)"
                    "System.String Sample3.Class3+Class4.get_Token()"
                    "System.Void Sample3.Class3+Class4.set_Token(System.String)"
                    "System.Collections.Generic.List`1 Sample3.Class3+Class4.ToList<T>(T)";
                    "System.Void Sample3.Class3+Class4.#ctor()" ]
    Assert.That(names, Is.EquivalentTo expected)

  // Report.fs

  static member TTBaseline = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\">
<method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"true\" instrumented=\"false\" >
<seqpnt visitcount=\"1\" line=\"11\" column=\"9\"  endline=\"11\" endcolumn=\"10\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"12\" column=\"13\" endline=\"12\" endcolumn=\"36\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"13\" column=\"13\" endline=\"13\" endcolumn=\"33\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"14\" column=\"13\" endline=\"14\" endcolumn=\"14\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"15\" column=\"17\" endline=\"15\" endcolumn=\"89\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"16\" column=\"13\" endline=\"16\" endcolumn=\"14\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"18\" column=\"13\" endline=\"18\" endcolumn=\"14\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"19\" column=\"17\" endline=\"19\" endcolumn=\"62\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"20\" column=\"13\" endline=\"20\" endcolumn=\"14\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"21\" column=\"9\"  endline=\"21\" endcolumn=\"10\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
</method>
</module>
</coverage>"

  static member MonoBaseline = "<?xml-stylesheet type='text/xsl' href='coverage.xsl'?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
  <module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null\">
    <method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"false\" instrumented=\"true\" fullname=\"System.Void TouchTest.Program.Main(System.String[])\">
      <seqpnt visitcount=\"0\" line=\"11\" column=\"9\" endline=\"11\" endcolumn=\"10\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"12\" column=\"32\" endline=\"12\" endcolumn=\"33\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"13\" endline=\"13\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"21\" endline=\"13\" endcolumn=\"22\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"14\" column=\"13\" endline=\"14\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"17\" endline=\"15\" endcolumn=\"18\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"72\" endline=\"15\" endcolumn=\"73\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"25\" endline=\"15\" endcolumn=\"26\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"16\" column=\"13\" endline=\"16\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"18\" column=\"13\" endline=\"18\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"17\" endline=\"19\" endcolumn=\"18\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"25\" endline=\"19\" endcolumn=\"26\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"20\" column=\"13\" endline=\"20\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"21\" column=\"9\" endline=\"21\" endcolumn=\"10\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
    </method>
  </module>
</coverage>"

  static member private RecursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length

    Assert.That(rcount, Is.EqualTo(ecount), "Mismatch at depth " + depth.ToString())

    Seq.zip result expected |> Seq.iter (fun ((r:XElement), (e:XElement)) ->
            Assert.That(r.Name, Is.EqualTo(e.Name), "Expected name " + e.Name.ToString())
            let ra = r.Attributes()
            let ea = e.Attributes()
            Seq.zip ra ea |> Seq.iter (fun ((a1:XAttribute), (a2:XAttribute)) ->
                    Assert.That(a1.Name, Is.EqualTo(a2.Name))
                    match a1.Name.ToString() with
                    | "moduleId"
                    | "metadataToken"
                    | "startTime"
                    | "measureTime" -> ()
                    | "document" -> Assert.That(a1.Value.Replace("\\","/"), Does.EndWith(a2.Value.Replace("\\","/")),
                                      a1.Name.ToString() + " : " + r.ToString() + " -> document")
                    | "visitcount" -> let expected = if zero then "0" else a2.Value
                                      Assert.That(a1.Value, Is.EqualTo(expected), r.ToString() + " -> visitcount")
                    | _ -> Assert.That(a1.Value, Is.EqualTo(a2.Value), r.ToString() + " -> " + a1.Name.ToString())
                )

            AltCoverTests.RecursiveValidate (r.Elements()) (e.Elements()) (depth+1) zero)

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromDotNet() =
    let visitor, document = Report.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        "Main" |> (Regex >> FilterClass.Method >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)

        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        let xml = AltCoverTests.TTBaseline
        let xml' = xml.Replace ("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())
        let xml'' = xml'.Replace("name=\"Sample1.exe\"", "name=\"" + sample1 + "\"")

        let baseline = XDocument.Load(new System.IO.StringReader(xml''))
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidate result expected 0 true
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromMono() =
    let visitor, document = Report.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "_Mono/Sample1", "Sample1.exe")
#if NETCOREAPP2_0
    let path' = if File.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + monoSample1, "Sample1.exe")
#else
    let path' = path
#endif

    Visitor.Visit [ visitor ] (Visitor.ToSeq path')

    let baseline = XDocument.Load(new System.IO.StringReader(AltCoverTests.MonoBaseline))
    let result = document.Elements()
    let expected = baseline.Elements()
    AltCoverTests.RecursiveValidate result expected 0 true

  // Instrument.fs

  [<Test>]
  member self.ShouldBeAbleToGetTheDefaultReportFileName () =
    let recorder = AltCover.Instrument.RecorderInstanceType()
    Assert.That(recorder.GetProperty("ReportFile").GetValue(null), Is.EqualTo "Coverage.Default.xml")

  [<Test>]
  member self.ShouldBeAbleToGetTheVisitReportMethod () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "AltCover.Recorder.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let recorder = AltCover.Instrument.RecordingMethod def
    Assert.That(Naming.FullMethodName recorder, Is.EqualTo "System.Void AltCover.Recorder.Instance.Visit(System.String,System.Int32)")

  [<Test>]
  member self.ShouldBeAbleToClearTheStrongNameKey () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
#if NETCOREAPP2_0
#else
    Assert.That (def.Name.HasPublicKey)
    let key0 = def.Name.PublicKey
    Assert.That (key0, Is.Not.Empty)
    let token0 = def.Name.PublicKeyToken
    Assert.That (token0, Is.Not.Empty)
#endif
    AltCover.Instrument.UpdateStrongNaming def.Name None
    Assert.That (def.Name.HasPublicKey, Is.False)
    let key1 = def.Name.PublicKey
    Assert.That (key1, Is.Empty)
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Empty)

  [<Test>]
  member self.ShouldBeAbleToUpdateTheStrongNameKeyWherePossible () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let key0 = def.Name.PublicKey
    let token0 = def.Name.PublicKeyToken
#if NETCOREAPP2_0
    Assert.That (def.Name.HasPublicKey, Is.False)
    Assert.That (key0, Is.Empty)
    Assert.That (token0, Is.Empty)
#else
    Assert.That (def.Name.HasPublicKey)
    Assert.That (key0, Is.Not.Empty)
    Assert.That (token0, Is.Not.Empty)
#endif

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    let key = StrongNameKeyPair(buffer.ToArray())

    AltCover.Instrument.UpdateStrongNaming def.Name (Some key)
#if NETCOREAPP2_0
    Assert.That (def.Name.HasPublicKey, Is.False)
#else
    Assert.That (def.Name.HasPublicKey)
    let key1 = def.Name.PublicKey
    Assert.That (key1, Is.Not.Null)
    Assert.That (key1, Is.Not.EquivalentTo(key0))
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Not.Null)
    Assert.That (token1, Is.Not.EquivalentTo(token0))
    let token' = String.Join(String.Empty, token1|> Seq.map (fun x -> x.ToString("x2")))
    Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))
#endif

  [<Test>]
  member self.NoKnownKeyInEmptyIndex() =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      Assert.That (Option.isNone(Instrument.KnownKey def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.KnownKeyMatchedInIndex() =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      self.ProvideKeyPair() |> Visitor.Add
#if NETCOREAPP2_0
      Assert.That (Option.isNone(Instrument.KnownKey def.Name))
#else
      Assert.That (Option.isSome(Instrument.KnownKey def.Name))
#endif
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.ThirdPartyKeyNotMatchedInIndex() =
    try
      Visitor.keys.Clear()
      let path = typeof<System.IO.FileAccess>.Assembly.Location
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      self.ProvideKeyPair() |> Visitor.Add
      Assert.That (Option.isNone(Instrument.KnownKey def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.FakedUpKeyIsMatchedInIndex() =
    try
      Visitor.keys.Clear()
      let path = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      let key = KeyStore.ArrayToIndex def.Name.PublicKey
      Visitor.keys.Add(key, { Pair=null; Token=[] })
      Assert.That (Option.isSome(Instrument.KnownKey def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.NoKnownKeyIfAssemblyHasNone() =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      AltCover.Instrument.UpdateStrongNaming def.Name None
      self.ProvideKeyPair() |> Visitor.Add
      Assert.That (Option.isNone(Instrument.KnownKey def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.NoKnownTokenInEmptyIndex() =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      Assert.That (Option.isNone(Instrument.KnownToken def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.KnownTokenMatchedInIndex() =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      self.ProvideKeyPair() |> Visitor.Add
#if NETCOREAPP2_0
      Assert.That (Option.isNone(Instrument.KnownToken def.Name))
#else
      Assert.That (Option.isSome(Instrument.KnownToken def.Name))
#endif
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.NoKnownTokenIfAssemblyHasNone() =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      AltCover.Instrument.UpdateStrongNaming def.Name None
      self.ProvideKeyPair() |> Visitor.Add
      Assert.That (Option.isNone(Instrument.KnownToken def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.FakedUpTokenIsMatchedInIndex() =
    try
      Visitor.keys.Clear()
      let path = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      let key = KeyStore.ArrayToIndex def.Name.PublicKey
      Visitor.keys.Add(key, { Pair=null; Token=[] })
      Assert.That (Option.isSome(Instrument.KnownToken def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.ShouldBeAbleToPrepareTheAssembly () =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let prepared = Instrument.PrepareAssembly path
      let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols raw |> ignore
      Assert.That (prepared.Name.Name, Is.EqualTo (raw.Name.Name + ".g"))
#if NETCOREAPP2_0
      Assert.That (prepared.Name.HasPublicKey, Is.False)
#else
      Assert.That (prepared.Name.HasPublicKey)
      Assert.That (prepared.Name.PublicKey, Is.Not.EquivalentTo(raw.Name.PublicKey))
      let token' = String.Join(String.Empty, prepared.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
      Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))
#endif

      let before = raw.MainModule.GetTypes() |> Seq.filter (fun t -> t.Name = "Class4") |> Seq.toList
      Assert.That (before.Length = 1)
      let before' = before.[0].Methods |> Seq.filter (fun t -> t.Name = "get_ReportFile") |> Seq.toList
      Assert.That (before'.Length = 1)

      let after = prepared.MainModule.GetTypes() |> Seq.filter (fun t -> t.Name = "Class4") |> Seq.toList
      Assert.That (after.Length = 1)
      let after' = after.[0].Methods |> Seq.filter (fun t -> t.Name = "get_ReportFile") |> Seq.toList
      Assert.That (after'.Length = 1)

      Assert.That (after'.[0].Body.Instructions.Count, Is.EqualTo(2 + before'.[0].Body.Instructions.Count))
    finally
      Visitor.keys.Clear()

#if NETCOREAPP2_0
// TODO
#else
  [<Test>]
  member self.ShouldGetNewFilePathFromPreparedAssembly () =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let unique = Guid.NewGuid().ToString()
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      let save = Visitor.reportPath
      try
        Visitor.reportPath <- Some unique
        let prepared = Instrument.PrepareAssembly path
        Instrument.WriteAssembly prepared outputdll
        let expectedSymbols = if "Mono.Runtime" |> Type.GetType |> isNull |> not then ".dll.mdb" else ".pdb"
        Assert.That (File.Exists (outputdll.Replace(".dll", expectedSymbols)))
        let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly outputdll
        Assert.That raw.Name.HasPublicKey

        // Assert.That (Option.isSome <| Instrument.KnownKey raw.Name) <- not needed
        let token' = String.Join(String.Empty, raw.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
        Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))

        let setup = AppDomainSetup()
        setup.ApplicationBase <- Path.GetDirectoryName(where)
        let ad = AppDomain.CreateDomain("ShouldGetNewFilePathFromPreparedAssembly", null, setup)
        try
          let proxyObject = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
          proxyObject.InstantiateObject(outputdll,"Sample3.Class3+Class4",[||])
          let report = proxyObject.InvokeMethod("get_ReportFile",[||]).ToString()
          Assert.That (report, Is.EqualTo (Path.GetFullPath unique))
        finally
          AppDomain.Unload(ad)
      finally
        Visitor.reportPath <- save
        Directory.EnumerateFiles(Path.GetDirectoryName output,
                                 (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f -> try File.Delete f
                              with // occasionally the dll file is locked by another process
                              | :? IOException -> ())
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.ShouldWriteMonoAssemblyOK () =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample3/Sample3.dll")
      let unique = Guid.NewGuid().ToString()
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      let save = Visitor.reportPath
      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let key = StrongNameKeyPair(buffer.ToArray())
      Visitor.defaultStrongNameKey <- Some key
      Visitor.Add key
      try
        Visitor.reportPath <- Some unique
        let prepared = Instrument.PrepareAssembly path

        Instrument.WriteAssembly prepared outputdll
// TODO -- see Instrument.WriteAssembly       Assert.That (File.Exists (outputdll + ".mdb"))
        let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly outputdll
        Assert.That raw.Name.HasPublicKey

        // Assert.That (Option.isSome <| Instrument.KnownKey raw.Name) <- not needed
        let token' = String.Join(String.Empty, raw.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
        Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))

        let setup = AppDomainSetup()
        setup.ApplicationBase <- Path.GetDirectoryName(where)
        let ad = AppDomain.CreateDomain("ShouldGetNewFilePathFromPreparedAssembly", null, setup)
        try
          let proxyObject = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
          proxyObject.InstantiateObject(outputdll,"Sample3.Class3+Class4",[||])
          let report = proxyObject.InvokeMethod("get_ReportFile",[||]).ToString()
          Assert.That (report, Is.EqualTo (Path.GetFullPath unique))
        finally
          AppDomain.Unload(ad)
      finally
        Visitor.reportPath <- save
        Directory.EnumerateFiles(Path.GetDirectoryName output,
                                 (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f -> try File.Delete f
                              with // occasionally the mdb file is locked by another process
                              | :? IOException -> ())
    finally
      Visitor.keys.Clear()
      Visitor.defaultStrongNameKey <- None

  [<Test>]
  member self.ShouldGetVisitFromWrittenAssembly () =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let pdb = Path.ChangeExtension(where, ".pdb")
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let unique = Guid.NewGuid().ToString()
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      let save = Visitor.reportPath
      try
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        ProgramDatabase.ReadSymbols def |> ignore

        let clazz = def.MainModule.GetType("Sample3.Class1")
        let func = clazz.GetMethods() |> Seq.find (fun x -> x.Name = "get_Property")

        let clazz' = def.MainModule.GetType("Sample3.Class3")
        let func' = clazz'.GetMethods() |> Seq.find (fun x -> x.Name = "Log")

        let newValue = Instrument.InsertVisit (func.Body.Instructions.[0]) (func.Body.GetILProcessor()) func' unique 42
        Assert.That (newValue.Operand, Is.EqualTo unique)
        Assert.That (newValue.OpCode, Is.EqualTo OpCodes.Ldstr)

        Instrument.WriteAssembly def outputdll
        let expectedSymbols = if "Mono.Runtime" |> Type.GetType |> isNull |> not then ".dll.mdb" else ".pdb"
        Assert.That (File.Exists (outputdll.Replace(".dll", expectedSymbols)))
        let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly outputdll
        Assert.That raw.Name.HasPublicKey

        // Assert.That (Option.isSome <| Instrument.KnownKey raw.Name) <- not needed
        let token' = String.Join(String.Empty, raw.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
        Assert.That (token', Is.EqualTo("c02b1a9f5b7cade8"))

        if File.Exists(pdb) then
            // doesnt' seem to work on Mono
            let setup = AppDomainSetup()
            setup.ApplicationBase <- Path.GetDirectoryName(where)
            let ad = AppDomain.CreateDomain("ShouldGetNewFilePathFromPreparedAssembly", null, setup)
            try
              let proxyObject = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
              proxyObject.InstantiateObject(outputdll,"Sample3.Class1",[||])
              let setting = proxyObject.InvokeMethod("set_Property",[| 17 |])
              Assert.That (setting, Is.Null)
              let getting = proxyObject.InvokeMethod("get_Property",[||]) :?> int
              Assert.That (getting, Is.EqualTo 17)

              let proxyObject' = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
              proxyObject'.InstantiateObject(outputdll,"Sample3.Class3",[||])
              let log = proxyObject'.InvokeMethod("get_Visits",[||]) :?> seq<Tuple<string, int>>
              Assert.That (log, Is.EquivalentTo[(unique, 42)])

            finally
              AppDomain.Unload(ad)
      finally
        Visitor.reportPath <- save
        Directory.EnumerateFiles(Path.GetDirectoryName output,
                                 (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f -> try File.Delete f
                              with // occasionally the dll file is locked by another process
                              | :? IOException -> ())
    finally
      Visitor.keys.Clear()
#endif

  [<Test>]
  member self.ShouldUpdateHandlerOK ([<Range(0,31)>] selection) =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let program = def.MainModule.GetType("TouchTest.Program")
    let main = program.GetMethods() |> Seq.find (fun x -> x.Name = "Main")
    let oldValue = main.Body.Instructions.[0]
    let proc = main.Body.GetILProcessor()
    let newValue = proc.Create(OpCodes.Ldc_I4, 23)
    let other = main.Body.Instructions.[1]

    let subject = Instrument.SubstituteInstruction (oldValue, newValue)

    let handler = ExceptionHandler(ExceptionHandlerType())

    handler.FilterStart <- if selection &&& 1 = 1 then oldValue else other
    handler.HandlerStart <- if selection &&& 2 = 2 then oldValue else other
    handler.HandlerEnd <- if selection &&& 4 = 4 then oldValue else other
    handler.TryStart <- if selection &&& 8 = 8 then oldValue else other
    handler.TryEnd <- if selection &&& 16 = 16 then oldValue else other

    subject.SubstituteExceptionBoundary handler

    Assert.That (handler.FilterStart, Is.EqualTo (if selection &&& 1 = 1 then newValue else other))
    Assert.That (handler.HandlerStart, Is.EqualTo (if selection &&& 2 = 2 then newValue else other))
    Assert.That (handler.HandlerEnd, Is.EqualTo (if selection &&& 4 = 4 then newValue else other))
    Assert.That (handler.TryStart, Is.EqualTo (if selection &&& 8 = 8 then newValue else other))
    Assert.That (handler.TryEnd, Is.EqualTo (if selection &&& 16 = 16 then newValue else other))

  [<Test>]
  member self.ShouldSubstituteInstructionOperand () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let proc = main.Body.GetILProcessor()
    let newValue = proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i -> match i.Operand with
                            | :? Instruction -> true
                            | _ -> false)
    |> Seq.iter (fun i -> let subject = Instrument.SubstituteInstruction (i.Operand :?> Instruction, newValue)
                          subject.SubstituteInstructionOperand i
                          Assert.That (i.Operand, Is.EqualTo newValue))

  [<Test>]
  member self.ShouldNotSubstituteDifferentInstructionOperand () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let proc = main.Body.GetILProcessor()
    let newValue = proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i -> match i.Operand with
                            | :? Instruction -> true
                            | _ -> false)
    |> Seq.iter (fun i -> let subject = Instrument.SubstituteInstruction (i, newValue)
                          let before = i.Operand
                          subject.SubstituteInstructionOperand i
                          Assert.That (i.Operand, Is.SameAs before))

  // work around weird compiler error with array indexing
  member private self.AsIArray (x:obj) (i:int)=
      (x :?> Instruction[])
      |> Seq.mapi (fun index instr -> (index, instr))
      |> Seq.filter (fun (x,y) -> x = i)
      |> Seq.map snd
      |> Seq.head

  [<Test>]
  member self.ShouldSubstituteIntoInstructionOperandArray () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let proc = main.Body.GetILProcessor()
    let newValue = proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i -> match i.Operand with
                            | :? (Instruction[]) -> true
                            | _ -> false)
    |> Seq.collect (fun i -> i.Operand :?> Instruction[]
                             |> Seq.mapi (fun o t -> (i,o,t)))
    |> Seq.iter (fun (i,o,t) -> let subject = Instrument.SubstituteInstruction (t, newValue)
                                Assert.That (self.AsIArray i.Operand o, (Is.SameAs t))
                                Assert.That (t, Is.Not.EqualTo newValue)
                                subject.SubstituteInstructionOperand i
                                let t' = self.AsIArray i.Operand
                                Assert.That (t' o, Is.EqualTo newValue))

  [<Test>]
  member self.ShouldNotSubstituteOutsideInstructionOperandArray () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let proc = main.Body.GetILProcessor()
    let newValue = proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i -> match i.Operand with
                            | :? (Instruction[]) -> true
                            | _ -> false)
    |> Seq.iter (fun i -> let subject = Instrument.SubstituteInstruction (i, newValue)
                          let before = (i.Operand :?> Instruction[])
                                       |> Seq.toList
                          subject.SubstituteInstructionOperand i
                          Seq.zip (i.Operand :?> Instruction[]) before
                          |> Seq.iter (fun (after, before) -> Assert.That (after, Is.SameAs before)))

  [<Test>]
  member self.ShouldNotSubstituteOtherOperand () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let proc = main.Body.GetILProcessor()
    let newValue = proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i -> match i.Operand with
                            | :? Instruction
                            | :? (Instruction[]) -> false
                            | _ -> true)
    |> Seq.collect (fun i -> main.Body.Instructions
                             |> Seq.map (fun other -> (i,other)))
    |> Seq.iter (fun (i, other) -> let subject = Instrument.SubstituteInstruction (other, newValue)
                                   let before = i.Operand
                                   subject.SubstituteInstructionOperand i
                                   Assert.That (i.Operand, Is.SameAs before))

  [<Test>]
  member self.StartShouldLoadRecordingAssembly () =
    let def = Instrument.InstrumentationVisitor (Instrument.Context.Build []) (Start [])
    Assert.That (def.RecordingAssembly.Name.Name, Is.EqualTo "AltCover.Recorder.g")

  [<Test>]
  member self.TypeShouldNotChangeState () =
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input (Node.Type (null, None, false))
    Assert.That (output, Is.SameAs input)

  [<Test>]
  member self.ExcludedMethodShouldNotChangeState () =
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input (Node.Method (null, None, false))
    Assert.That (output, Is.SameAs input)

  [<Test>]
  member self.IncludedMethodShouldChangeState () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let func = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input (Node.Method (func, None, true))
    Assert.That (output.MethodBody, Is.SameAs func.Body)

  [<Test>]
  member self.ExcludedAfterMethodShouldNotChangeState () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let func = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")

    let opcodes = func.Body.Instructions
                   |> Seq.map (fun i -> i.OpCode)
                   |> Seq.toList
    let input = { Instrument.Context.Build [] with MethodBody = func.Body }
    input.MethodBody.SimplifyMacros()

    let paired = Seq.zip opcodes input.MethodBody.Instructions |> Seq.toList
    Assert.That (paired |> Seq.exists (fun (i,j) -> i <> j.OpCode))
    let diff = paired
               |> List.map (fun (i,j) -> (i, i=j.OpCode))

    let output = Instrument.InstrumentationVisitor input (Node.AfterMethod false)
    Assert.That (output, Is.SameAs input)
    let paired' = Seq.zip diff input.MethodBody.Instructions
    Assert.That (paired' |> Seq.forall (fun ((i,x),j) -> x = (i = j.OpCode)))

  [<Test>]
  member self.IncludedAfterMethodShouldRewriteMethod () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let func = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")

    let opcodes = func.Body.Instructions
                   |> Seq.map (fun i -> i.OpCode)
                   |> Seq.toList
    let input = { Instrument.Context.Build [] with MethodBody = func.Body }
    input.MethodBody.SimplifyMacros()

    let paired = Seq.zip opcodes input.MethodBody.Instructions
    Assert.That (paired |> Seq.exists (fun (i,j) -> i <> j.OpCode))

    let output = Instrument.InstrumentationVisitor input (Node.AfterMethod true)
    Assert.That (output, Is.SameAs input)
    let paired' = Seq.zip opcodes input.MethodBody.Instructions
    Assert.That (paired' |> Seq.forall (fun (i,j) -> i = j.OpCode))

  [<Test>]
  member self.UpdateStrongReferencesShouldChangeSigningKeyWherePossible () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let token0 = def.Name.PublicKeyToken

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    Visitor.defaultStrongNameKey <- Some (StrongNameKeyPair(buffer.ToArray()))

    let result = Instrument.UpdateStrongReferences def []
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Not.Null)
#if NETCOREAPP2_0
    Assert.That (token1, Is.EquivalentTo(token0))
#else
    Assert.That (token1, Is.Not.EquivalentTo(token0))
#endif
    let token' = String.Join(String.Empty, token1|> Seq.map (fun x -> x.ToString("x2")))
#if NETCOREAPP2_0
    Assert.That (token', Is.EqualTo String.Empty)
#else
    Assert.That (token', Is.EqualTo "4ebffcaabf10ce6a" )
#endif
    Assert.That (result, Is.Empty)

  [<Test>]
  member self.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let token0 = def.Name.PublicKeyToken

    Visitor.defaultStrongNameKey <- None

    let result = Instrument.UpdateStrongReferences def ["nunit.framework"]
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Empty)
#if NETCOREAPP2_0
    Assert.That (token1, Is.EquivalentTo(token0))
#else
    Assert.That (token1, Is.Not.EquivalentTo(token0))
#endif

    let token' = String.Join(String.Empty, token1|> Seq.map (fun x -> x.ToString("x2")))
    Assert.That (token', Is.EqualTo String.Empty)
#if NETCOREAPP2_0
    Assert.That (result.Count, Is.EqualTo 0)
#else
    Assert.That (result.Count, Is.EqualTo 1)
    let key = result.Keys |> Seq.head
    let value = result.Values |> Seq.head
    let ptr = key.LastIndexOf("=")
    Assert.That (key.Substring(0, ptr), Is.EqualTo(value.Substring(0, ptr)))
    Assert.That (value.Substring(ptr), Is.EqualTo "=null")
#endif

  [<Test>]
  member self.UpdateStrongReferencesShouldNotAddASigningKey () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "_Mono/Sample1", "Sample1.exe")
#if NETCOREAPP2_0
    let path' = if File.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + monoSample1, "Sample1.exe")
#else
    let path' = path
#endif
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path'
    ProgramDatabase.ReadSymbols def |> ignore

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    Visitor.defaultStrongNameKey <- Some (StrongNameKeyPair(buffer.ToArray()))

    let result = Instrument.UpdateStrongReferences def []
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Empty)
    Assert.That (result, Is.Empty)

  [<Test>]
  member self.UpdateStrongReferencesShouldTrackReferences () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    Visitor.defaultStrongNameKey <- Some (StrongNameKeyPair(buffer.ToArray()))

    let result = Instrument.UpdateStrongReferences def ["nunit.framework"; "nonesuch"]
#if NETCOREAPP2_0
    Assert.That (result.Count, Is.EqualTo 0)
#else
    Assert.That (result.Count, Is.EqualTo 1)
    Assert.That (result.Values |> Seq.head, Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a")
    let key = result.Keys |> Seq.head
    Assert.That (result.Values |> Seq.head,
                 Is.EqualTo (key.Substring(0, key.Length - 16) + "4ebffcaabf10ce6a"))
#endif

  [<Test>]
  member self.UpdateStrongReferencesShouldTrackReferencesEvenFakes () =
    try
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def |> ignore
      let npath = typeof<TestAttribute>.Assembly.Location
      let ndef = Mono.Cecil.AssemblyDefinition.ReadAssembly npath
      let key = KeyStore.ArrayToIndex ndef.Name.PublicKey

#if NETCOREAPP2_0
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let ourKeyPair = StrongNameKeyPair(buffer.ToArray())
      Visitor.defaultStrongNameKey <- Some ourKeyPair
      Visitor.keys.Add(key, { Pair=ourKeyPair; Token=[] })

      let result = Instrument.UpdateStrongReferences def ["nunit.framework"; "nonesuch"]
#if NETCOREAPP2_0
      Assert.That (result.Count, Is.EqualTo 0)
#else
      Assert.That (result.Count, Is.EqualTo 1)
      Assert.That (result.Values |> Seq.head, Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a")
      let key = result.Keys |> Seq.head
      Assert.That (result.Values |> Seq.head,
                   Is.EqualTo (key.Substring(0, key.Length - 16) + "4ebffcaabf10ce6a"))
#endif
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.ExcludedAssemblyRefsAreNotUpdated () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let refs = def.MainModule.AssemblyReferences |> Seq.toList

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    Visitor.defaultStrongNameKey <- Some (StrongNameKeyPair(buffer.ToArray()))
    let fake = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]
    let visited = Node.Assembly (def, None, false)

    let result = Instrument.InstrumentationVisitor {state with RecordingAssembly = fake } visited
#if NETCOREAPP2_0
    Assert.That (result.RenameTable.Count, Is.EqualTo 0)
#else
    Assert.That (result.RenameTable.Count, Is.EqualTo 1)
    Assert.That (result.RenameTable.Values |> Seq.head, Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a")
    let key = result.RenameTable.Keys |> Seq.head
    Assert.That (result.RenameTable.Values |> Seq.head,
                 Is.EqualTo (key.Substring(0, key.Length - 16) + "4ebffcaabf10ce6a"))
#endif
    Assert.That (def.MainModule.AssemblyReferences, Is.EquivalentTo refs)


  [<Test>]
  member self.IncludedAssemblyRefsAreUpdated () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let refs = def.MainModule.AssemblyReferences |> Seq.toList

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    Visitor.defaultStrongNameKey <- Some (StrongNameKeyPair(buffer.ToArray()))
    let fake = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]
    let visited = Node.Assembly (def, None, true)

    let result = Instrument.InstrumentationVisitor {state with RecordingAssembly = fake } visited
#if NETCOREAPP2_0
    Assert.That (result.RenameTable.Count, Is.EqualTo 0)
#else
    Assert.That (result.RenameTable.Count, Is.EqualTo 1)
    Assert.That (result.RenameTable.Values |> Seq.head, Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a")
    let key = result.RenameTable.Keys |> Seq.head
    Assert.That (result.RenameTable.Values |> Seq.head,
                 Is.EqualTo (key.Substring(0, key.Length - 16) + "4ebffcaabf10ce6a"))
#endif
    Assert.That (def.MainModule.AssemblyReferences, Is.EquivalentTo (refs @ [fake.Name]))

  [<Test>]
  member self.ExcludedModuleJustRecordsMVid () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let visited = Node.Module (def.MainModule, None, false)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]
    let result = Instrument.InstrumentationVisitor  state visited
    Assert.That (result, Is.EqualTo  { state with ModuleId = def.MainModule.Mvid })

  [<Test>]
  member self.IncludedModuleEnsuresRecorder () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let visited = Node.Module (def.MainModule, None, true)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]

    let path' = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(),
                             "AltCover.Recorder.dll")
    let def' = Mono.Cecil.AssemblyDefinition.ReadAssembly path'
    let visit = def'.MainModule.GetAllTypes()
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.filter (fun m -> m.Name = "Visit")
                |> Seq.head

    let state' = { state with RecordingAssembly = def' }
    let result = Instrument.InstrumentationVisitor state' visited
    Assert.That (result.RecordingMethodRef.Module,
                Is.EqualTo ( def.MainModule))
    Assert.That (string result.RecordingMethodRef,
                Is.EqualTo (string visit))
    Assert.That ({ result with RecordingMethodRef = null},
                 Is.EqualTo  { state' with ModuleId = def.MainModule.Mvid
                                                      RecordingMethod = visit
                                                      RecordingMethodRef = null })

  [<Test>]
  member self.ExcludedMethodPointIsPassThrough () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let visited = Node.MethodPoint (null, null, 0, false)
    let state = Instrument.Context.Build []
    let result = Instrument.InstrumentationVisitor state visited
    Assert.That (result, Is.SameAs state)

  [<Test>]
  member self.IncludedMethodPointInsertsVisit () =
   let where = Assembly.GetExecutingAssembly().Location
   let pdb = Path.ChangeExtension(where, ".pdb")
   if File.Exists(pdb) then // skip when we don't have symbols on travis
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let reader = ProgramDatabase.ReadSymbols def
    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let proc = main.Body.GetILProcessor()
    let dbg = (Option.get reader).Read main
    let target = main.Body.Instructions
                 |> Seq.filter (dbg.GetSequencePoint >> isNull >> not)
                 |> Seq.head
    let visited = Node.MethodPoint (target, null, 32767, true)
    Assert.That (target.Previous, Is.Null)
    let state = { (Instrument.Context.Build []) with MethodWorker = proc
                                                     MethodBody = main.Body
                                                     RecordingMethodRef = def.MainModule.ImportReference main}
    let result = Instrument.InstrumentationVisitor state visited
    Assert.That (result, Is.SameAs state)
    Assert.That (target.Previous.OpCode, Is.EqualTo OpCodes.Call)

  [<Test>]
  member self.IncludedModuleDoesNotChangeRecorderJustTheReference () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore
    let visited = Node.Module (def.MainModule, None, true)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]

    let path' = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(),
                             "AltCover.Recorder.dll")
    let def' = Mono.Cecil.AssemblyDefinition.ReadAssembly path'
    let visit = def'.MainModule.GetAllTypes()
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.filter (fun m -> m.Name = "Visit")
                |> Seq.head
    let def'' = Mono.Cecil.AssemblyDefinition.ReadAssembly where

    let state' = { state with RecordingAssembly = def'
                              RecordingMethod = visit
                              RecordingMethodRef = def''.MainModule.ImportReference visit}
    let result = Instrument.InstrumentationVisitor state' visited
    let ref'' = def.MainModule.ImportReference visit

    Assert.That (result.RecordingMethodRef.Module,
                Is.EqualTo ( def.MainModule))
    Assert.That (string result.RecordingMethodRef,
                Is.EqualTo (string visit))
    Assert.That ({ result with RecordingMethodRef = null},
                 Is.EqualTo  { state' with ModuleId = def.MainModule.Mvid
                                                      RecordingMethod = visit
                                                      RecordingMethodRef = null })

  [<Test>]
  member self.AfterModuleShouldNotChangeState () =
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input AfterModule
    Assert.That (output, Is.SameAs input)

  [<Test>]
  member self.AfterAssemblyCommitsThatAssembly () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = Visitor.outputDirectory
    try
      Visitor.outputDirectory <- Some output
      let visited = Node.AfterAssembly def
      let input = Instrument.Context.Build []
      let result = Instrument.InstrumentationVisitor input visited
      Assert.That (result, Is.SameAs input, "result differs")
      let created = Path.Combine (output, "Sample2.dll")
      Assert.That (File.Exists created, created + " not found")
#if NETCOREAPP2_0
#else
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      if File.Exists pdb then
        Assert.That (File.Exists (Path.ChangeExtension(created, ".pdb")), created + " pdb not found")
#endif
    finally
      Visitor.outputDirectory <- saved

  [<Test>]
  member self.FinishCommitsTheRecordingAssembly () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def |> ignore

    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = Visitor.outputDirectory
    try
      Visitor.outputDirectory <- Some output
      let input = { Instrument.Context.Build [] with RecordingAssembly = def }
      let result = Instrument.InstrumentationVisitor input Finish
      Assert.That (result, Is.SameAs input, "results differ")
      let created = Path.Combine (output, "Sample2.dll")
      Assert.That (File.Exists created, created + " not found")
#if NETCOREAPP2_0
      Assert.That (File.Exists (Path.Combine(output, "FSharp.Core.dll")), "Core not found")
#else
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      if File.Exists pdb then
        Assert.That (File.Exists (Path.ChangeExtension(created, ".pdb")), created + " pdb not found")
#endif
    finally
      Visitor.outputDirectory <- saved

  [<Test>]
  member self.JSONInjectionTransformsStandaloneFileAsExpected () =
    let inputName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.json")
#if NETCOREAPP2_0
    let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.ncafter.json")
#else
    let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.after.json")
#endif
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(inputName)
    use reader = new StreamReader(stream)
    let result = Instrument.injectJSON <| reader.ReadToEnd()
    use stream' = Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)
    use reader' = new StreamReader(stream')
    let expected = reader'.ReadToEnd()
    Assert.That (result.Replace("\r\n","\n"),
                 Is.EqualTo(expected.Replace("\r\n","\n")))

  [<Test>]
  member self.JSONInjectionTransformsDependencyFileAsExpected () =
    let inputName = infrastructureSnk.Replace("Infrastructure.snk", "Sample2.deps.json")
#if NETCOREAPP2_0
    let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample2.deps.ncafter.json")
#else
    let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample2.deps.after.json")
#endif
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(inputName)
    use reader = new StreamReader(stream)
    let result = Instrument.injectJSON <| reader.ReadToEnd()
    use stream' = Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)
    use reader' = new StreamReader(stream')
    let expected = reader'.ReadToEnd()
    Assert.That (result.Replace("\r\n","\n"),
                 Is.EqualTo(expected.Replace("\r\n","\n")))

  [<Test>]
  member self.JSONInjectionIsIdempotent () =
#if NETCOREAPP2_0
    let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.ncafter.json")
#else
    let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.after.json")
#endif
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)
    use reader = new StreamReader(stream)
    let expected = reader.ReadToEnd()
    let result = Instrument.injectJSON <| expected
    Assert.That (result.Replace("\r\n","\n"),
                 Is.EqualTo(expected.Replace("\r\n","\n")))

  // CommandLine.fs

  [<Test>]
  member self.NoThrowNoErrorLeavesAllOK () =
    try
      CommandLine.error <- false
      CommandLine.doPathOperation ignore
      Assert.That(CommandLine.error, Is.False)
    finally
      CommandLine.error <- false

  [<Test>]
  member self.NoThrowWithErrorIsSignalled () =
    try
      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> CommandLine.error <- true)
      Assert.That(CommandLine.error, Is.True)
    finally
      CommandLine.error <- false

  [<Test>]
  member self.ArgumentExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "ArgumentException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> ArgumentException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + "\r\n"))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.IOExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "IOException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> IOException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + "\r\n"))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.NotSupportedExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "NotSupportedException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> NotSupportedException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + "\r\n"))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.SecurityExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "SecurityException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> System.Security.SecurityException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + "\r\n"))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  // AltCover.fs and CommandLine.fs

  [<Test>]
  member self.ShouldLaunchWithExpectedOutput() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    let program = files
                  |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.head

    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      CommandLine.Launch program (String.Empty) (Path.GetDirectoryName (Assembly.GetExecutingAssembly().Location))

      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()
      // hack for Mono
      let computed = if result.Length = 14 then
                       result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
                     else result

      if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
        Assert.That(computed.Trim(), Is.EqualTo("Where is my rocket pack?"))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ShouldHaveExpectedOptions() =
    let options = Main.DeclareOptions ()
    Assert.That (options.Count, Is.EqualTo
#if NETCOREAPP2_0
                                            11
#else
                                            13
#endif
                 )
    Assert.That(options |> Seq.filter (fun x -> x.Prototype <> "<>")
                        |> Seq.forall (fun x -> (String.IsNullOrWhiteSpace >> not) x.Description))
    Assert.That (options |> Seq.filter (fun x -> x.Prototype = "<>") |> Seq.length, Is.EqualTo 1)

  [<Test>]
  member self.ParsingJunkIsAnError() =
    let options = Main.DeclareOptions ()
    let parse = CommandLine.ParseCommandLine [| "/@thisIsNotAnOption" |] options
    match parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

  [<Test>]
  member self.ParsingJunkBeforeSeparatorIsAnError() =
    let options = Main.DeclareOptions ()
    let parse = CommandLine.ParseCommandLine [| "/@thisIsNotAnOption"; "--";  "this should be OK" |] options
    match parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

  [<Test>]
  member self.ParsingJunkAfterSeparatorIsExpected() =
    let options = Main.DeclareOptions ()
    let input = [| "--";  "/@thisIsNotAnOption"; "this should be OK" |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (x, Is.EquivalentTo (input |> Seq.skip 1))
                      Assert.That (y, Is.SameAs options)

  [<Test>]
  member self.ParsingHelpGivesHelp() =
    let options = Main.DeclareOptions ()
    let input = [| "--?" |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (y, Is.SameAs options)

    match CommandLine.ProcessHelpOption parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "HelpText")
                     Assert.That (y, Is.SameAs options)

    // a "not sticky" test
    match CommandLine.ParseCommandLine [| "/t"; "x" |] options
          |> CommandLine.ProcessHelpOption with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (y, Is.SameAs options)
                      Assert.That (x, Is.Empty)

  [<Test>]
  member self.ParsingErrorHelpGivesHelp() =
    let options = Main.DeclareOptions ()
    let input = [| "--o"; Path.GetInvalidPathChars() |> String |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

    match CommandLine.ProcessHelpOption parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

    // a "not sticky" test
    match CommandLine.ParseCommandLine [| "/t"; "x" |] options
          |> CommandLine.ProcessHelpOption with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (y, Is.SameAs options)
                      Assert.That (x, Is.Empty)

  [<Test>]
  member self.ParsingAttributesGivesAttributes() =
    try
      Visitor.NameFilters.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-a"; "1;a"; "--a"; "2"; "/a"; "3"; "-a=4"; "--a=5"; "/a=6" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.NameFilters.Count, Is.EqualTo 7)
      Assert.That (Visitor.NameFilters |> Seq.forall (fun x -> match x with
                                                               | FilterClass.Attribute _ -> true
                                                               | _ -> false))
      Assert.That (Visitor.NameFilters |> Seq.map (fun x -> match x with
                                                            | FilterClass.Attribute i -> i.ToString()
                                                            | _ -> "*"),
                   Is.EquivalentTo [| "1"; "a"; "2"; "3"; "4"; "5"; "6" |])
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ParsingMethodsGivesMethods() =
    try
      Visitor.NameFilters.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-m"; "1"; "--m"; "2;b;c"; "/m"; "3"; "-m=4"; "--m=5"; "/m=6" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.NameFilters.Count, Is.EqualTo 8)
      Assert.That (Visitor.NameFilters |> Seq.forall (fun x -> match x with
                                                               | FilterClass.Method _ -> true
                                                               | _ -> false))
      Assert.That (Visitor.NameFilters |> Seq.map (fun x -> match x with
                                                            | FilterClass.Method i -> i.ToString()
                                                            | _ -> "*"),
                   Is.EquivalentTo [| "1"; "2"; "b"; "c"; "3"; "4"; "5"; "6" |])
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ParsingTypesGivesTypes() =
    try
      Visitor.NameFilters.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-t"; "1"; "--t"; "2"; "/t"; "3;x;y;z"; "-t=4"; "--t=5"; "/t=6" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.NameFilters.Count, Is.EqualTo 9)
      Assert.That (Visitor.NameFilters |> Seq.forall (fun x -> match x with
                                                               | FilterClass.Type _ -> true
                                                               | _ -> false))
      Assert.That (Visitor.NameFilters |> Seq.map (fun x -> match x with
                                                            | FilterClass.Type i -> i.ToString()
                                                            | _ -> "*"),
                   Is.EquivalentTo [| "1"; "2"; "3"; "x"; "y"; "z"; "4"; "5"; "6" |])
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ParsingAssembliesGivesAssemblies() =
    try
      Visitor.NameFilters.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-s"; "1"; "--s"; "2"; "/s"; "3"; "-s=4;p;q"; "--s=5"; "/s=6" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.NameFilters.Count, Is.EqualTo 8)
      Assert.That (Visitor.NameFilters |> Seq.forall (fun x -> match x with
                                                               | FilterClass.Assembly _ -> true
                                                               | _ -> false))
      Assert.That (Visitor.NameFilters |> Seq.map (fun x -> match x with
                                                            | FilterClass.Assembly i -> i.ToString()
                                                            | _ -> "*"),
                   Is.EquivalentTo [| "1"; "2"; "3"; "4"; "p"; "q"; "5"; "6" |])
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ParsingModulesGivesModules() =
    try
      Visitor.NameFilters.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-e"; "1"; "--e"; "2"; "/e"; "3"; "-e=4;p;q"; "--e=5"; "/e=6" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.NameFilters.Count, Is.EqualTo 8)
      Assert.That (Visitor.NameFilters |> Seq.forall (fun x -> match x with
                                                               | FilterClass.Module _ -> true
                                                               | _ -> false))
      Assert.That (Visitor.NameFilters |> Seq.map (fun x -> match x with
                                                            | FilterClass.Module i -> i.ToString()
                                                            | _ -> "*"),
                   Is.EquivalentTo [| "1"; "2"; "3"; "4"; "p"; "q"; "5"; "6" |])
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ParsingFilesGivesFiles() =
    try
      Visitor.NameFilters.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-f"; "1"; "--f"; "2"; "/f"; "3"; "-f=4"; "--f=5;m;n"; "/f=6" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.NameFilters.Count, Is.EqualTo 8)
      Assert.That (Visitor.NameFilters |> Seq.forall (fun x -> match x with
                                                               | FilterClass.File _ -> true
                                                               | _ -> false))
      Assert.That (Visitor.NameFilters |> Seq.map (fun x -> match x with
                                                            | FilterClass.File i -> i.ToString()
                                                            | _ -> "*"),
                   Is.EquivalentTo [| "1"; "2"; "3"; "4"; "5"; "m"; "n"; "6" |])
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ParsingXmlGivesXml() =
    try
      Visitor.reportPath <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where), unique)

      let input = [| "-x"; path |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Visitor.reportPath with
      | None -> Assert.Fail()
      | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
    finally
      Visitor.reportPath <- None

  [<Test>]
  member self.ParsingMultipleXmlGivesFailure() =
    try
      Visitor.reportPath <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-x"; unique; "/x"; unique.Replace("-", "+") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.reportPath <- None

  [<Test>]
  member self.ParsingBadXmlGivesFailure() =
    try
      Visitor.reportPath <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-x"; unique.Replace("-", Path.GetInvalidPathChars() |> String) |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.inputDirectory <- None

  [<Test>]
  member self.ParsingNoXmlGivesFailure() =
    try
      Visitor.reportPath <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-x" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.inputDirectory <- None

  [<Test>]
  member self.ParsingEmptyXmlGivesFailure() =
    try
      Visitor.reportPath <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-x"; " " |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.inputDirectory <- None

  [<Test>]
  member self.ParsingInputGivesInput() =
    try
      Visitor.inputDirectory <- None
      let options = Main.DeclareOptions ()
      let unique = Path.GetFullPath(".")
      let input = [| "-i"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Visitor.inputDirectory with
      | None -> Assert.Fail()
      | Some x -> Assert.That(x, Is.EqualTo unique)
    finally
      Visitor.inputDirectory <- None

  [<Test>]
  member self.ParsingMultipleInputGivesFailure() =
    try
      Visitor.inputDirectory <- None
      let options = Main.DeclareOptions ()
      let input = [| "-i"; Path.GetFullPath("."); "/i"; Path.GetFullPath("..") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.inputDirectory <- None

  [<Test>]
  member self.ParsingBadInputGivesFailure() =
    try
      Visitor.inputDirectory <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-i"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.inputDirectory <- None
  [<Test>]

  member self.ParsingNoInputGivesFailure() =
    try
      Visitor.inputDirectory <- None
      let options = Main.DeclareOptions ()
      let input = [| "-i" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.inputDirectory <- None

  [<Test>]
  member self.ParsingOutputGivesOutput() =
    try
      Visitor.outputDirectory <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-o"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Visitor.outputDirectory with
      | None -> Assert.Fail()
      | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
    finally
      Visitor.outputDirectory <- None

  [<Test>]
  member self.ParsingMultipleOutputGivesFailure() =
    try
      Visitor.outputDirectory <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-o"; unique; "/o"; unique.Replace("-", "+") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.outputDirectory <- None

  [<Test>]
  member self.ParsingBadOutputGivesFailure() =
    try
      Visitor.outputDirectory <- None
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-o"; unique.Replace("-", Path.GetInvalidPathChars() |> String) |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.outputDirectory <- None

  [<Test>]
  member self.ParsingNoOutputGivesFailure() =
    try
      Visitor.outputDirectory <- None
      let options = Main.DeclareOptions ()
      let input = [| "-o" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.outputDirectory <- None

  [<Test>]
  member self.ParsingEmptyOutputGivesFailure() =
    try
      Visitor.outputDirectory <- None
      let options = Main.DeclareOptions ()
      let input = [| "-o"; " " |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.outputDirectory <- None

  member private self.IsolateRootPath () =
    let where = Assembly.GetExecutingAssembly().Location
    where.Substring(0, where.IndexOf("_Binaries"))

#if NETCOREAPP2_0
#else
  [<Test>]
  member self.ParsingStrongNameGivesStrongName() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-sn"; Path.Combine(self.IsolateRootPath(), "Build/Infrastructure.snk") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Visitor.defaultStrongNameKey with
      | None -> Assert.Fail()
      | Some x -> let token = x
                              |> KeyStore.TokenOfKey
                              |> List.map (fun x -> x.ToString("x2"))
                  Assert.That (String.Join (String.Empty, token), Is.EqualTo("c02b1a9f5b7cade8"))
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  [<Test>]
  member self.ParsingMultipleStrongNameGivesFailure() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let path = self.IsolateRootPath()

      let input = [| "-sn"; Path.Combine(path, "Build/Infrastructure.snk") ; "/sn"; Path.GetFullPath("Build/Recorder.snk") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  [<Test>]
  member self.ParsingBadStrongNameGivesFailure() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-sn"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  [<Test>]
  member self.ParsingNonStrongNameGivesFailure() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let unique = Assembly.GetExecutingAssembly().Location
      let input = [| "-sn"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  [<Test>]
  member self.ParsingNoStrongNameGivesFailure() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-sn" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  [<Test>]
  member self.ParsingMultipleAltStrongNameIsOk() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let path = self.IsolateRootPath()

      let input = [| "-k"; Path.Combine(path, "Build/Infrastructure.snk");
                     "/k"; Path.Combine(path, "Build/Recorder.snk") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      let expected = Visitor.keys.Keys
                     |> Seq.map (fun x -> String.Join(String.Empty,
                                                      BitConverter.GetBytes(x)
                                                      |> Seq.map (fun x -> x.ToString("x2"))))
                     |> Seq.sort
      Assert.That (String.Join(" ", expected), Is.EqualTo ("4ebffcaabf10ce6a c02b1a9f5b7cade8"))
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  member self.ParsingNoAltStrongNameGivesFailure() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-k" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  [<Test>]
  member self.ParsingBadAltStrongNameGivesFailure() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-k"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()

  [<Test>]
  member self.ParsingNonAltsStrongNameGivesFailure() =
    try
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
      let options = Main.DeclareOptions ()
      let unique = Assembly.GetExecutingAssembly().Location
      let input = [| "-k"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.defaultStrongNameKey <- None
      Visitor.keys.Clear()
#endif

  [<Test>]
  member self.OutputLeftPassesThrough() =
    let arg = (Guid.NewGuid().ToString(),Main.DeclareOptions())
    let fail = Left arg
    match Main.ProcessOutputLocation fail with
    | Right _ -> Assert.Fail()
    | Left x -> Assert.That (x, Is.SameAs arg)

  [<Test>]
  member self.OutputInPlaceFails() =
    let options = Main.DeclareOptions ()
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      Visitor.inputDirectory <- Some (Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location))
      Visitor.outputDirectory <- Visitor.inputDirectory

      let arg = ([], options)
      let fail = Right arg
      match Main.ProcessOutputLocation fail with
      | Right _ -> Assert.Fail()
      | Left (x,y) -> Assert.That (y, Is.SameAs options)
                      Assert.That (x, Is.EqualTo "UsageError")
                      Assert.That (stderr.ToString().Replace("\r",String.Empty),
                                   Is.EqualTo "From and to directories are identical\n")
                      Assert.That (stdout.ToString(), Is.Empty)
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.OutputToNewPlaceIsOK() =
    let options = Main.DeclareOptions ()
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      Visitor.inputDirectory <- Some here
      Visitor.outputDirectory <- Some (Path.GetDirectoryName here)

      let rest = [Guid.NewGuid().ToString()]
      let arg = (rest, options)
      let ok = Right arg
      match Main.ProcessOutputLocation ok with
      | Left _ -> Assert.Fail()
      | Right (x,y,z) -> Assert.That (x, Is.SameAs rest)
                         Assert.That (y.FullName, Is.EqualTo here)
                         Assert.That (z.FullName, Is.EqualTo (Path.GetDirectoryName here))
                         Assert.That (stdout.ToString().Replace("\r",String.Empty),
                                      Is.EqualTo ("Instrumenting files from " +
                                                  here + "\nWriting files to " +
                                                  (Path.GetDirectoryName here) + "\n"))
                         Assert.That (stderr.ToString(), Is.Empty)
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.OutputToReallyNewPlaceIsOK() =
    let options = Main.DeclareOptions ()
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let there = Path.Combine(here, Guid.NewGuid().ToString())
      Visitor.inputDirectory <- Some here
      Visitor.outputDirectory <- Some there

      let rest = [Guid.NewGuid().ToString()]
      let arg = (rest, options)
      let ok = Right arg
      Assert.That (Directory.Exists there, Is.False)
      match Main.ProcessOutputLocation ok with
      | Left _ -> Assert.Fail()
      | Right (x,y,z) -> Assert.That (x, Is.SameAs rest)
                         Assert.That (y.FullName, Is.EqualTo here)
                         Assert.That (z.FullName, Is.EqualTo there)
                         Assert.That (stdout.ToString().Replace("\r",String.Empty),
                                      Is.EqualTo ("Creating folder " + there +
                                                  "\nInstrumenting files from " +
                                                  here + "\nWriting files to " +
                                                  there + "\n"))
                         Assert.That (stderr.ToString(), Is.Empty)
                         Assert.That (Directory.Exists there)
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.PreparingNewPlaceShouldCopyEverything() =
    let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let there = Path.Combine(here, Guid.NewGuid().ToString())
    let toInfo = Directory.CreateDirectory there
    let fromInfo = DirectoryInfo(here)
    let (x,y) = Main.PrepareTargetFiles fromInfo toInfo
    Assert.That (toInfo.EnumerateFiles()
                 |> Seq.map (fun x -> x.Name),
                 Is.EquivalentTo (fromInfo.EnumerateFiles()
                 |>Seq.map (fun x -> x.Name)),
                 "Simple to-from comparison failed")
    Assert.That (x,
                 Is.EquivalentTo (fromInfo.EnumerateFiles()
                 |> Seq.map (fun x -> x.FullName)
                 |> Seq.filter (fun f -> f.EndsWith(".exe", StringComparison.OrdinalIgnoreCase) ||
                                         f.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
                 |> Seq.filter (fun f -> File.Exists(Path.ChangeExtension(f, ".pdb")) ||
                                         File.Exists (f + ".mdb"))),
                 "First list mismatch with from files")
    Assert.That (y,
                 Is.EquivalentTo (x
                 |> Seq.map Path.GetFileNameWithoutExtension),
                 "Second list mismatch")

  [<Test>]
  member self.ShouldProcessTrailingArguments() =
    // Hack for running while instrumented
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    let program = files
                  |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.head

    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let u1 = Guid.NewGuid().ToString()
      let u2 = Guid.NewGuid().ToString()

      CommandLine.ProcessTrailingArguments [program; u1; u2]
                                     (DirectoryInfo(where))

      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()

      // hack for Mono
      let computed = if result.Length = 50 then
                       result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
                     else result
      if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
        Assert.That(computed.Trim(), Is.EqualTo("Where is my rocket pack? " +
                                                  u1 + "*" + u2))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ADryRunLooksAsExpected() =
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
    let key0 = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "Build/SelfTest.snk")
#if NETCOREAPP2_0
    let input = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
    let key = if File.Exists key0 then key0
              else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../Build/SelfTest.snk")
#else
    let input = path
    let key = key0
#endif
    let unique = Guid.NewGuid().ToString()
    let unique' = Path.Combine (where, Guid.NewGuid().ToString())
    Directory.CreateDirectory unique' |> ignore
    let report = Path.Combine(unique', "ADryRunLooksAsExpected.xml")
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    let outputSaved = Visitor.outputDirectory
    let inputSaved = Visitor.inputDirectory
    let reportSaved = Visitor.reportPath
    let keySaved = Visitor.defaultStrongNameKey
    let saved = (Console.Out, Console.Error)
    Visitor.keys.Clear()
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let args = [| "-i"; input
                    "-o"; output
                    "-x"; report
#if NETCOREAPP2_0
#else
                    "-sn"; key
#endif
                 |]
      Main.DoInstrumentation args
      Assert.That (stderr.ToString(), Is.Empty)

      let expected = "Creating folder " + output +
                     "\nInstrumenting files from " + (Path.GetFullPath input) +
                     "\nWriting files to " + output +
                     "\nCoverage Report: " + report + "\n"

      Assert.That (stdout.ToString().Replace("\r\n", "\n").Replace("\\", "/"),
                   Is.EqualTo (expected.Replace("\\", "/")))

      Assert.That (Visitor.OutputDirectory(), Is.EqualTo output)
      Assert.That (Visitor.InputDirectory().Replace("\\", "/"),
                   Is.EqualTo ((Path.GetFullPath input).Replace("\\", "/")))
      Assert.That (Visitor.ReportPath (), Is.EqualTo report)

      use stream = new FileStream(key, FileMode.Open)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let snk = StrongNameKeyPair(buffer.ToArray())

#if NETCOREAPP2_0
      Assert.That (Visitor.keys.Count, Is.EqualTo 0)
#else
      Assert.That (Visitor.keys.ContainsKey(KeyStore.KeyToIndex snk))
      Assert.That (Visitor.keys.Count, Is.EqualTo 1)
#endif

      Assert.That (File.Exists report)
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")

      let expected = if File.Exists(pdb) then
                        ["AltCover.Recorder.g.dll"
#if NETCOREAPP2_0
                         "AltCover.Recorder.g.dll.mdb"
                         "FSharp.Core.dll"
#else
                         "AltCover.Recorder.g.pdb"
#endif
                         "Sample1.exe"
                         "Sample1.exe.mdb"
#if NETCOREAPP2_0
#else
                         "Sample1.pdb"
#endif
                         ] // See Instrument.WriteAssembly
                     else
                        ["AltCover.Recorder.g.dll"
                         "AltCover.Recorder.g.dll.mdb"
                         "Sample1.exe"
                         "Sample1.exe.mdb"]

      Assert.That (Directory.GetFiles(output)
                   |> Seq.map Path.GetFileName,
                   Is.EquivalentTo expected)

      let expectedXml = XDocument.Load(new System.IO.StringReader(AltCoverTests.MonoBaseline))
      let recordedXml = XDocument.Load(report)
      AltCoverTests.RecursiveValidate (recordedXml.Elements()) (expectedXml.Elements()) 0 true

    finally
      Visitor.outputDirectory <- outputSaved
      Visitor.inputDirectory <- inputSaved
      Visitor.reportPath <- reportSaved
      Visitor.defaultStrongNameKey <- keySaved
      Console.SetOut (fst saved)
      Console.SetError (snd saved)
      Visitor.keys.Clear()

  [<Test>]
  member self.ADotNetDryRunLooksAsExpected() =
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0")
    let key0 = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "Build/SelfTest.snk")
#if NETCOREAPP2_0
    let input = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0")
    let key = if File.Exists key0 then key0
              else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../Build/SelfTest.snk")
#else
    let input = path
    let key = key0
#endif
    let unique = Guid.NewGuid().ToString()
    let unique' = Path.Combine (where, Guid.NewGuid().ToString())
    Directory.CreateDirectory unique' |> ignore
    let report = Path.Combine(unique', "ADotNetDryRunLooksAsExpected.xml")
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    let outputSaved = Visitor.outputDirectory
    let inputSaved = Visitor.inputDirectory
    let reportSaved = Visitor.reportPath
    let keySaved = Visitor.defaultStrongNameKey
    let saved = (Console.Out, Console.Error)
    Visitor.keys.Clear()
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let args = [| "-i"; input
                    "-o"; output
                    "-x"; report
#if NETCOREAPP2_0
#else
                    "-sn"; key
#endif
                 |]
      Main.DoInstrumentation args
      Assert.That (stderr.ToString(), Is.Empty)

      let expected = "Creating folder " + output +
                     "\nInstrumenting files from " + (Path.GetFullPath input) +
                     "\nWriting files to " + output +
                     "\nCoverage Report: " + report + "\n"

      Assert.That (stdout.ToString().Replace("\r\n", "\n").Replace("\\", "/"),
                   Is.EqualTo (expected.Replace("\\", "/")))
      Assert.That (Visitor.OutputDirectory(), Is.EqualTo output)
      Assert.That (Visitor.InputDirectory().Replace("\\", "/"),
                   Is.EqualTo ((Path.GetFullPath input).Replace("\\", "/")))
      Assert.That (Visitor.ReportPath (), Is.EqualTo report)

      use stream = new FileStream(key, FileMode.Open)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let snk = StrongNameKeyPair(buffer.ToArray())

#if NETCOREAPP2_0
      Assert.That (Visitor.keys.Count, Is.EqualTo 0)
#else
      Assert.That (Visitor.keys.ContainsKey(KeyStore.KeyToIndex snk))
      Assert.That (Visitor.keys.Count, Is.EqualTo 1)
#endif

      Assert.That (File.Exists report)
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")

      let expected =
                        ["AltCover.Recorder.g.dll"
#if NETCOREAPP2_0
                         "AltCover.Recorder.g.dll.mdb"
                         "FSharp.Core.dll"
#else
                         "AltCover.Recorder.g.pdb"
#endif
                         "Mono.Cecil.dll"
                         "nunit.engine.netstandard.dll"
                         "NUnit3.TestAdapter.dll"
                         "Sample2.deps.json"
                         "Sample2.dll"
#if NETCOREAPP2_0
                         "Sample2.dll.mdb"
#endif
                         "Sample2.runtimeconfig.dev.json"
                         "Sample2.runtimeconfig.json"
                         "Sample2.pdb"]

      let expected' = if pdb |> File.Exists |> not then
                        List.concat [expected; ["AltCover.Recorder.g.dll.mdb"; "Sample2.dll.mdb" ]]
                        |> List.filter (fun f -> f.EndsWith(".g.pdb", StringComparison.Ordinal) |> not)
                        |> List.sortBy (fun f -> f.ToUpperInvariant())
                      else
                        expected

      Assert.That (Directory.GetFiles(output)
                   |> Seq.map Path.GetFileName
                   |> Seq.filter (fun f -> f.EndsWith(".tmp", StringComparison.Ordinal) |> not)
                   |> Seq.sortBy (fun f -> f.ToUpperInvariant()),
                   Is.EquivalentTo expected')

    finally
      Visitor.outputDirectory <- outputSaved
      Visitor.inputDirectory <- inputSaved
      Visitor.reportPath <- reportSaved
      Visitor.defaultStrongNameKey <- keySaved
      Console.SetOut (fst saved)
      Console.SetError (snd saved)
      Visitor.keys.Clear()

    let before = File.ReadAllText(Path.Combine(input, "Sample2.deps.json"))
    Assert.That(before.IndexOf("AltCover.Recorder.g"), Is.EqualTo -1)

    let o = JObject.Parse (File.ReadAllText(Path.Combine(output, "Sample2.deps.json")))
    let target = ((o.Property "runtimeTarget").Value :?> JObject).Property("name").Value.ToString()
    let targets = (o.Properties()
                    |> Seq.find (fun p -> p.Name = "targets")).Value :?> JObject
    let targeted = (targets.Properties()
                    |> Seq.find (fun p -> p.Name= target)).Value :?> JObject
    let app = (targeted.PropertyValues() |> Seq.head)  :?> JObject
    let existingDependencies = app.Properties() |> Seq.tryFind (fun p -> p.Name = "dependencies")
    let reset = match existingDependencies with
                | None -> Set.empty<string>
                | Some p -> (p.Value :?> JObject).Properties()
                            |> Seq.map (fun p -> p.Name)
                            |> Set.ofSeq
    Assert.That(reset |> Set.contains "AltCover.Recorder.g")
    let aux = targeted.Properties()
              |> Seq.map (fun p -> p.Name)
              |> Set.ofSeq
    Assert.That(aux |> Set.contains "AltCover.Recorder.g/1.4.0.0")
    let libraries = (o.Properties()
                    |> Seq.find (fun p -> p.Name = "libraries")).Value :?> JObject
    let lib = libraries.Properties()
              |> Seq.map (fun p -> p.Name)
              |> Set.ofSeq
    Assert.That(lib |> Set.contains "AltCover.Recorder.g/1.4.0.0")

  [<Test>]
  member self.UsageIsAsExpected() =
    let options = Main.DeclareOptions ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      CommandLine.Usage "UsageError" options
      let result = stderr.ToString().Replace("\r\n", "\n")
      let expected = """Error - usage is:
  -i, --inputDirectory=VALUE Optional: The folder containing assemblies to
                               instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional: The folder to receive the instrumented
                               assemblies and their companions (default: sub-
                               folder '__Instrumented' of the current directory)
"""
#if NETCOREAPP2_0
#else
                     + """  -k, --key=VALUE            Optional, multiple: any other strong-name key to
                               use
      --sn, --strongNameKey=VALUE
                             Optional: The default strong naming key to apply
                               to instrumented assemblies (default: None)
"""
#endif
                     + """  -x, --xmlReport=VALUE      Optional: The output report template file (default:
                                coverage.xml in the current directory)
  -f, --fileFilter=VALUE     Optional: source file name to exclude from
                               instrumentation (may repeat)
  -s, --assemblyFilter=VALUE Optional: assembly name to exclude from
                               instrumentation (may repeat)
  -e, --assemblyExcludeFilter=VALUE
                             Optional: assembly which links other instrumented
                               assemblies but for which internal details may be
                               excluded (may repeat)
  -t, --typeFilter=VALUE     Optional: type name to exclude from
                               instrumentation (may repeat)
  -m, --methodFilter=VALUE   Optional: method name to exclude from
                               instrumentation (may repeat)
  -a, --attributeFilter=VALUE
                             Optional: attribute name to exclude from
                               instrumentation (may repeat)
  -?, --help, -h             Prints out the options.
"""

      Assert.That (result, Is.EqualTo (expected.Replace("\r\n", "\n")))

    finally Console.SetError saved

  [<Test>]
  member self.ErrorResponseIsAsExpected() =
    let saved = Console.Error
    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let unique = Guid.NewGuid().ToString()
      Main.DoInstrumentation [| "-i"; unique |]
      let result = stderr.ToString().Replace("\r\n", "\n")
      let expected = "\"-i\" \"" + unique + "\"\n" +
                       """Error - usage is:
  -i, --inputDirectory=VALUE Optional: The folder containing assemblies to
                               instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional: The folder to receive the instrumented
                               assemblies and their companions (default: sub-
                               folder '__Instrumented' of the current directory)
"""
#if NETCOREAPP2_0
#else
                     + """  -k, --key=VALUE            Optional, multiple: any other strong-name key to
                               use
      --sn, --strongNameKey=VALUE
                             Optional: The default strong naming key to apply
                               to instrumented assemblies (default: None)
"""
#endif
                     + """  -x, --xmlReport=VALUE      Optional: The output report template file (default:
                                coverage.xml in the current directory)
  -f, --fileFilter=VALUE     Optional: source file name to exclude from
                               instrumentation (may repeat)
  -s, --assemblyFilter=VALUE Optional: assembly name to exclude from
                               instrumentation (may repeat)
  -e, --assemblyExcludeFilter=VALUE
                             Optional: assembly which links other instrumented
                               assemblies but for which internal details may be
                               excluded (may repeat)
  -t, --typeFilter=VALUE     Optional: type name to exclude from
                               instrumentation (may repeat)
  -m, --methodFilter=VALUE   Optional: method name to exclude from
                               instrumentation (may repeat)
  -a, --attributeFilter=VALUE
                             Optional: attribute name to exclude from
                               instrumentation (may repeat)
  -?, --help, -h             Prints out the options.
"""

      Assert.That (result, Is.EqualTo (expected.Replace("\r\n", "\n")))

    finally Console.SetError saved

  // Recorder.fs => Shadow.Tests

end