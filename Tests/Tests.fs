namespace Tests

open System
open System.IO
open System.Linq
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq

open AltCover
open AltCover.Augment
open AltCover.Filter
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open N
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
  static member Hack () =
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
                  |> Seq.filter (fun x -> not <| (snd x).FullName.StartsWith("FSharp.", StringComparison.OrdinalIgnoreCase))
// for coverlet
                  |> Seq.filter (fun x -> not <| (snd x).FullName.StartsWith("coverlet", StringComparison.OrdinalIgnoreCase))
                  |> Seq.filter (fun x -> not <| (snd x).FullName.StartsWith("AltCover,", StringComparison.OrdinalIgnoreCase))
                  |> Seq.filter (fun x -> not <| (snd x).FullName.StartsWith("AltCover.Recorder", StringComparison.OrdinalIgnoreCase))
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
        let normalized = Path.Combine(Path.GetDirectoryName p, Path.GetFileName p)
        match pdb with
        | None -> Assert.Fail("Not found " + p)
        | Some name -> Assert.That(name, Is.EqualTo normalized)
      with
      | :? BadImageFormatException -> ()
    )

  [<Test>]
  member self.ShouldGetForeignPdbWithFallbackWhenNotColocated() =
    try
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
        let dll0 = Path.ChangeExtension(p, ".dll")
        let unique = Guid.NewGuid().ToString()
        let output = Path.Combine(Path.GetDirectoryName(where), unique)
        Directory.CreateDirectory(output) |> ignore
        let dll = Path.Combine(output, Path.GetFileName dll0)
        System.IO.File.Copy (dll0, dll)
        ProgramDatabase.SymbolFolders.Clear()
        p |> Path.GetDirectoryName |> ProgramDatabase.SymbolFolders.Add
        try
          let def = Mono.Cecil.AssemblyDefinition.ReadAssembly dll
          let pdb = AltCover.ProgramDatabase.GetPdbWithFallback(def)
          let normalized = Path.Combine(Path.GetDirectoryName p, Path.GetFileName p)
          match pdb with
          | None -> Assert.Fail("Not found " + p)
          | Some name -> Assert.That(name, Is.EqualTo normalized)
                         AltCover.ProgramDatabase.ReadSymbols def
                         Assert.That (def.MainModule.HasSymbols, def.MainModule.FileName)
        with
        | :? BadImageFormatException -> ()
      )
    finally
      ProgramDatabase.SymbolFolders.Clear()

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
#if COVERLET
    |> Seq.filter (fun x -> not <| x.FullName.StartsWith("AltCover,", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x -> not <| x.FullName.StartsWith("AltCover.Recorder", StringComparison.OrdinalIgnoreCase))
#endif
    |> Seq.filter (fun x -> x.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter (fun def ->
      AltCover.ProgramDatabase.ReadSymbols def
      Assert.That (def.MainModule.HasSymbols, def.MainModule.FileName)
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
      AltCover.ProgramDatabase.ReadSymbols def
      Assert.That (not def.MainModule.HasSymbols, def.MainModule.FileName)
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
      AltCover.ProgramDatabase.ReadSymbols def
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
  member self.NoneOfTheAboveMatchesNoPath() =
     Assert.That (Match () (FilterClass.Path (Regex "23")), Is.False)

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
  member self.PathDoesNotMatchNonPathClass() =
     Assert.That (Match (Assembly.GetExecutingAssembly().Location) (FilterClass.Type (Regex "23")), Is.False)

  [<Test>]
  member self.PathDoesMatchPathClass() =
     let x = String [| '\\'; Path.DirectorySeparatorChar |]
     Assert.That (Match (Assembly.GetExecutingAssembly().Location) (FilterClass.Path (Regex (x + "_Binaries" + x))), Is.True)

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

    let expected = [  ".ctor" ; ".ctor" ; "Invoke"; "as_bar"; "bytes"; "get_MyBar" ;
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
  member self.CSharpNestedMethods() =
     let sample3 = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Sample5.dll")
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (sample3)
     let methods = def.MainModule.GetAllTypes()
                    |> Seq.collect(fun t -> t.Methods)
                    |> Seq.toList
     let result = methods
                    |> Seq.map Visitor.ContainingMethod
                    |> Seq.map (fun (mo : MethodDefinition option) ->
                                    mo |> Option.map (fun m -> m.Name))

     let expected = [
                     None // System.Int32 Sample5.Class1::F1(System.String)
                     None // System.Collections.Generic.IEnumerable`1<System.Int32> Sample5.Class1::F2(System.String)
                     None // System.Threading.Tasks.Task`1<System.String> Sample5.Class1::F3(System.String)
                     None // System.Void Sample5.Class1::.ctor()
                     Some "F1" // "System.Int32 Sample5.Class1::<F1>g__Interior|0_1(System.Int32,System.Int32)"
                     Some "F1" // "System.Int32 Sample5.Class1::<F1>g__Recursive|0_3(System.Int32)"
                     None // System.Int32 Sample5.Class1/Inner::G1(System.String)
                     None // System.Void Sample5.Class1/Inner::G1(System.Int32)
                     None // System.Collections.Generic.IEnumerable`1<System.Int32> Sample5.Class1/Inner::G2(System.String)
                     None // System.Void Sample5.Class1/Inner::G2(System.Int32)
                     None // System.Threading.Tasks.Task`1<System.String> Sample5.Class1/Inner::G3(System.String)
                     None // System.Void Sample5.Class1/Inner::G3(System.Int32)
                     None // System.Void Sample5.Class1/Inner::.ctor()
                     Some "<G1>g__Recursive|0_4" // T[] Sample5.Class1/Inner::<G1>g__InteriorToArray|0_1(T)
                     Some "<G1>b__3" // "System.Int32 Sample5.Class1/Inner::<G1>g__Interior|0_1(System.Int32,System.Int32)"
                     Some "<G1>g__Interior|0_2" // "System.Int32 Sample5.Class1/Inner::<G1>g__Recursive|0_3(System.Int32)"
                     None // System.Void Sample5.Class1/Inner/<>c__DisplayClass0_0::.ctor()
                     Some "G1" // System.Int32 Sample5.Class1/Inner/<>c__DisplayClass0_0::<G1>b__2(System.Char)
                     None // System.Void Sample5.Class1/Inner/<>c::.cctor()
                     None // System.Void Sample5.Class1/Inner/<>c::.ctor()
                     Some "G1" // System.Int32 Sample5.Class1/Inner/<>c::<G1>b__0_0(System.Char)
                     Some "G2" // System.Void Sample5.Class1/Inner/<G2>d__1::.ctor(System.Int32)
                     Some "G2" // System.Void Sample5.Class1/Inner/<G2>d__1::System.IDisposable.Dispose()
                     Some "G2" // System.Boolean Sample5.Class1/Inner/<G2>d__1::MoveNext()
                     Some "G2" // System.Int32 Sample5.Class1/Inner/<G2>d__1::System.Collections.Generic.IEnumerator<System.Int32>.get_Current()
                     Some "G2" // System.Void Sample5.Class1/Inner/<G2>d__1::System.Collections.IEnumerator.Reset()
                     Some "G2" // System.Object Sample5.Class1/Inner/<G2>d__1::System.Collections.IEnumerator.get_Current()
                     Some "G2" // System.Collections.Generic.IEnumerator`1<System.Int32> Sample5.Class1/Inner/<G2>d__1::System.Collections.Generic.IEnumerable<System.Int32>.GetEnumerator()
                     Some "G2" // System.Collections.IEnumerator Sample5.Class1/Inner/<G2>d__1::System.Collections.IEnumerable.GetEnumerator()
                     Some "G3" // System.Void Sample5.Class1/Inner/<G3>d__2::.ctor()
                     Some "G3" // System.Void Sample5.Class1/Inner/<G3>d__2::MoveNext()
                     Some "G3" // System.Void Sample5.Class1/Inner/<G3>d__2::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
                     None // System.Void Sample5.Class1/<>c__DisplayClass0_0::.ctor()
                     Some "F1" // System.Int32 Sample5.Class1/<>c__DisplayClass0_0::<F1>b__2(System.Char)
                     None // System.Void Sample5.Class1/<>c::.cctor()
                     None // System.Void Sample5.Class1/<>c::.ctor()
                     Some "F1" // System.Int32 Sample5.Class1/<>c::<F1>b__0_0(System.Char)
                     Some "F2" // System.Void Sample5.Class1/<F2>d__1::.ctor(System.Int32)
                     Some "F2" // System.Void Sample5.Class1/<F2>d__1::System.IDisposable.Dispose()
                     Some "F2" // System.Boolean Sample5.Class1/<F2>d__1::MoveNext()
                     Some "F2" // System.Int32 Sample5.Class1/<F2>d__1::System.Collections.Generic.IEnumerator<System.Int32>.get_Current()
                     Some "F2" // System.Void Sample5.Class1/<F2>d__1::System.Collections.IEnumerator.Reset()
                     Some "F2" // System.Object Sample5.Class1/<F2>d__1::System.Collections.IEnumerator.get_Current()
                     Some "F2" // System.Collections.Generic.IEnumerator`1<System.Int32> Sample5.Class1/<F2>d__1::System.Collections.Generic.IEnumerable<System.Int32>.GetEnumerator()
                     Some "F2" // System.Collections.IEnumerator Sample5.Class1/<F2>d__1::System.Collections.IEnumerable.GetEnumerator()
                     Some "F3" // System.Void Sample5.Class1/<F3>d__2::.ctor()
                     Some "F3" // System.Void Sample5.Class1/<F3>d__2::MoveNext()
                     Some "F3" // System.Void Sample5.Class1/<F3>d__2::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
                     ]
     // methods |> Seq.iter (fun x -> printfn "%A" x.FullName)
     // Assert.That (result, Is.EquivalentTo expected)

     result |> Seq.toList
      |> List.zip expected
      |> List.iteri (fun i (x,y) -> Assert.That(y, Is.EqualTo x, sprintf "%A %A %d" x y i))

     // Disambiguation checks
     let g3 = methods.[10]
     Assert.That (methods
                  |> Seq.map Visitor.ContainingMethod
                  |> Seq.choose id
                  |> Seq.filter (fun m -> m.Name = "G3"),
                  Is.EquivalentTo [g3;g3;g3])

     let g1 = methods.[6]
     Assert.That (methods
                  |> Seq.map Visitor.ContainingMethod
                  |> Seq.choose id
                  |> Seq.filter (fun m -> m.Name = "G1"),
                  Is.EquivalentTo [g1;g1])

  [<Test>]
  member self.FSharpNestedMethods() =
     let sample3 = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Sample6.dll")
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (sample3)
     let methods = def.MainModule.GetAllTypes()
                    |> Seq.collect(fun t -> t.Methods)
                    |> Seq.toList

     let result = methods
                    |> Seq.map Visitor.ContainingMethod
                    |> Seq.map (fun (mo : MethodDefinition option) ->
                                    mo |> Option.map (fun m -> m.DeclaringType.Name + "::" + m.Name))
                    |> Seq.toList

     let expected = [
                     None //Microsoft.FSharp.Collections.FSharpList`1<System.Int32> Sample6.Module::F1(Microsoft.FSharp.Collections.FSharpList`1<System.Object>)
                     None //Microsoft.FSharp.Core.Unit[] Sample6.Module::F2(Microsoft.FSharp.Collections.FSharpList`1<System.String>)
                     Some "Module::F1" //System.Void Sample6.Module/aux@9::.ctor()
                     Some "Module::F1" //System.Int32 Sample6.Module/aux@9::Invoke(System.Int32)
                     Some "FI@10T::Invoke" //System.Void Sample6.Module/FII@11::.ctor()
                     Some "FI@10T::Invoke" //System.Object Sample6.Module/FII@11::Specialize()
                     Some "FII@11::Specialize" //System.Void Sample6.Module/FII@11T::.ctor(Sample6.Module/FII@11)
                     Some "FII@11::Specialize"  //System.Int32 Sample6.Module/FII@11T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<b>,System.Int32)
                     Some "Module::F1" //System.Void Sample6.Module/FI@10::.ctor()
                     Some "Module::F1" //System.Object Sample6.Module/FI@10::Specialize()
                     Some "FI@10::Specialize" //System.Void Sample6.Module/FI@10T::.ctor(Sample6.Module/FI@10)
                     Some "FI@10::Specialize" //System.Int32 Sample6.Module/FI@10T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<a>)
                     Some "Module::F1" //System.Void Sample6.Module/F1@18::.ctor()
                     Some "Module::F1" //System.Int32 Sample6.Module/F1@18::Invoke(System.Object)
#if NETCOREAPP2_0
                     Some "fetchUrlAsync@26-4::Invoke" //System.Void Sample6.Module/fetchUrlAsync@26-5::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@26-4::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@26-5::Invoke(System.IO.StreamReader)
                     Some "fetchUrlAsync@24-3::Invoke" //System.Void Sample6.Module/fetchUrlAsync@25-4::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@24-3::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@25-4::Invoke(System.IO.Stream)
                     Some "fetchUrlAsync@24-2::Invoke" //System.Void Sample6.Module/fetchUrlAsync@23-3::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@24-2::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-3::Invoke(System.Net.WebResponse)
                     Some "fetchUrlAsync@23-1::Invoke" //System.Void Sample6.Module/fetchUrlAsync@23-2::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@23-1::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-2::Invoke(System.Net.WebResponse)
                     Some "fetchUrlAsync@22::Invoke" //System.Void Sample6.Module/fetchUrlAsync@22-1::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@22::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@22-1::Invoke(Microsoft.FSharp.Core.Unit)
                     Some "Module::F2" //System.Void Sample6.Module/fetchUrlAsync@21::.ctor()
                     Some "Module::F2" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@21::Invoke(System.String)
#else
// F# 4.5.1
                     Some "fetchUrlAsync@26-4::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@27-5::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
                     Some "fetchUrlAsync@26-4::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@27-5::Invoke(System.IO.StreamReader)"
                     Some "fetchUrlAsync@24-3::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@26-4::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
                     Some "fetchUrlAsync@24-3::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@26-4::Invoke(System.IO.Stream)"
                     Some "fetchUrlAsync@24-2::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@24-3::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
                     Some "fetchUrlAsync@24-2::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@24-3::Invoke(System.Net.WebResponse)"
                     Some "fetchUrlAsync@23-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@24-2::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
                     Some "fetchUrlAsync@23-1::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@24-2::Invoke(System.Net.WebResponse)"
                     Some "fetchUrlAsync@23-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@24-6::.ctor(Microsoft.FSharp.Control.FSharpAsync`1<System.Net.WebResponse>,Microsoft.FSharp.Core.FSharpFunc`2<System.Net.WebResponse,Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit>>)"
                     Some "fetchUrlAsync@23-1::Invoke" //"Microsoft.FSharp.Control.AsyncReturn Sample6.Module/fetchUrlAsync@24-6::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<Microsoft.FSharp.Core.Unit>)"
                     Some "fetchUrlAsync@22::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-1::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
                     Some "fetchUrlAsync@22::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-1::Invoke(Microsoft.FSharp.Core.Unit)"
                     Some "Module::F2" //"System.Void Sample6.Module/fetchUrlAsync@22::.ctor()"
                     Some "Module::F2" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@22::Invoke(System.String)"
#endif
                         ]
     //methods |> Seq.iter (fun x -> printfn "%A" x.FullName)
     //Assert.That (result, Is.EquivalentTo expected)
     result
      |> List.zip expected
      |> List.iteri (fun i (x,y) -> Assert.That(y, Is.EqualTo x, sprintf "%A %A %d %s" x y i methods.[i].FullName))

  [<Test>]
  member self.ValidateSeqPntFixUp() = // HACK HACK HACK
    let location = typeof<Sample3.Class1>.Assembly.Location
    let sourceAssembly = AssemblyDefinition.ReadAssembly(location)
    let i = sourceAssembly.MainModule.GetAllTypes()
            |> Seq.collect(fun t -> t.Methods)
            |> Seq.filter(fun m -> m.HasBody && m.Body.Instructions.Any())
            |> Seq.map (fun m -> m.Body.Instructions |> Seq.head)
            |> Seq.head

    let dummy = Cil.Document("dummy")
    let before = Cil.SequencePoint(i, dummy)
    before.GetType().GetProperty("StartLine").SetValue(before, 23)
    before.GetType().GetProperty("StartColumn").SetValue(before, 42)
    before.GetType().GetProperty("EndLine").SetValue(before, -1)
    before.GetType().GetProperty("EndColumn").SetValue(before, -1)
    let after = SeqPnt.Build(before)
    Assert.That (after.EndLine, Is.EqualTo before.StartLine)
    Assert.That (after.EndColumn, Is.EqualTo (before.StartColumn + 1))

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

  member self.IsIncluded x =
    x |> Visitor.IsIncluded |> Visitor.IsInstrumented

  [<Test>]
  member self.EmptyFiltersPassAll() =
    Visitor.NameFilters.Clear()
    Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
    Assert.That (self.IsIncluded self)

  [<Test>]
  member self.NonEmptyFiltersCatchAnExpectedValue() =
    try
      Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
      Visitor.NameFilters.AddRange([ FilterClass.File (Regex "Cove")
                                     FilterClass.Method (Regex "Augment")])
      Assert.That (self.IsIncluded (Assembly.GetExecutingAssembly().Location), Is.False)
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.NonEmptyFiltersPassAnExpectedValue() =
    try
      Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
      Visitor.NameFilters.AddRange([ FilterClass.File (Regex "System")
                                     FilterClass.Method (Regex "Augment")])
      Assert.That (self.IsIncluded (Assembly.GetExecutingAssembly().Location))
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.AfterProcessingYieldsAnExpectedValue() =
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
    let inputs = [ Node.Start [] ; Node.Assembly (def, Inspect.Instrument) ;
                   Node.Module (null, Inspect.Ignore) ; Node.Type (null, Inspect.Instrument) ;
                   Node.Method (null, Inspect.Ignore, None) ;
                   Node.MethodPoint (null, None, 0, true ) ;
                   Node.AfterMethod (null, Inspect.Ignore, None) ; Node.AfterModule ;
                   Node.AfterAssembly def; Node.Finish ]
    let outputs = inputs |> Seq.map (fun n -> n.After() |> Seq.toList)
    let expected = [ [Finish]; [AfterAssembly def]; [AfterModule]; [AfterType];
                     [AfterMethod (null, Inspect.Ignore, None)];
                     []; []; []; []; []]
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
    let inputs = [ Node.MethodPoint (null, None, 0, true ) ;
                   Node.AfterMethod (null, Inspect.Ignore, None) ; Node.AfterModule ; Node.AfterAssembly def; Node.Finish ]
    let outputs = inputs |> Seq.map (Visitor.Deeper>> Seq.toList)
    let expected = [[]; []; []; []; []]
    Assert.That (outputs, Is.EquivalentTo (expected))

  [<Test>]
  member self.MethodPointsAreDeeperThanMethods() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let method = (def.MainModule.Types |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))|> Seq.head).Methods |> Seq.head
    Visitor.Visit [] [] // cheat reset
    try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        "Program" |> (Regex >> FilterClass.File >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Method (method,
                                                    Inspect.Instrument,
                                                    None)
                     |> Seq.toList
        Assert.That (deeper.Length, Is.EqualTo 12)
        deeper
        |> List.skip 10
        |> List.iteri (fun i node -> match node with
                                     | (BranchPoint b) ->
                                           Assert.That(b.Uid, Is.EqualTo i, "branch point number")
                                     | _ -> Assert.Fail())

        deeper
        |> List.take 10
        |> List.iteri (fun i node -> match node with
                                     | (MethodPoint (_, _, n, b)) ->
                                           Assert.That(n, Is.EqualTo i, "point number")
                                           Assert.That (b, Is.False, "flag")
                                     | _ -> Assert.Fail())
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.MethodsAreDeeperThanTypes() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let type' = (def.MainModule.Types |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))|> Seq.head)
    Visitor.Visit [] [] // cheat reset
    try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        "Main" |> (Regex >> FilterClass.Method >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Type (type', Inspect.Instrument)
                     |> Seq.toList
        Visitor.Visit [] [] // cheat reset
        let expected = type'.Methods
                    |> Seq.map (fun m -> let flag = if m.Name = ".ctor" then Inspect.Instrument
                                                    else Inspect.Ignore
                                         let node = Node.Method (m, flag, None)
                                         List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node;  [Node.AfterMethod (m,flag, None)]])
                    |> List.concat
        Assert.That (deeper.Length, Is.EqualTo 17)
        Assert.That (deeper |> Seq.map string,
                     Is.EquivalentTo (expected |> Seq.map string))
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.TypesAreDeeperThanModules() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let module' = def.MainModule
    Visitor.Visit [] [] // cheat reset
    try
        "Program" |> (Regex >> FilterClass.Type >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Module (module', Inspect.Instrument)
                     |> Seq.toList
        Visitor.Visit [] [] // cheat reset
        let expected = module'.Types // we have no nested types in this test
                    |> Seq.map (fun t -> let flag = if t.Name <> "Program"
                                                    then Inspect.Instrument
                                                    else Inspect.Ignore
                                         let node = Node.Type (t, flag)
                                         List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node; [Node.AfterType]])
                    |> List.concat
        Assert.That (deeper.Length, Is.EqualTo 18)
        Assert.That (deeper |> Seq.map string,
                     Is.EquivalentTo (expected |> Seq.map string))
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ModulesAreDeeperThanAssemblies() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    Visitor.Visit [] [] // cheat reset
    let deeper = Visitor.Deeper <| Node.Assembly (def, Inspect.Instrument)
                 |> Seq.toList
    Visitor.Visit [] [] // cheat reset
    let expected = def.Modules // we have no nested types in this test
                |> Seq.map (fun t -> let node = Node.Module (t, Inspect.Instrument)
                                     List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node; [AfterModule]])
                |> List.concat
    Assert.That (deeper.Length, Is.EqualTo 21)
    Assert.That (deeper |> Seq.map string,
                 Is.EquivalentTo (expected |> Seq.map string))

  [<Test>]
  member self.AssembliesAreDeeperThanPaths() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    let deeper = Visitor.Deeper <| Node.Start [path]
                    |> Seq.toList
    // assembly definitions care about being separate references in equality tests
    let def = match Seq.head deeper with
              | Node.Assembly (def', Inspect.Instrument) -> def'
              | _ -> Assert.Fail(); null

    let assembly = Node.Assembly (def, Inspect.Instrument)
    let expected = List.concat [ [assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def]]
    Assert.That (deeper.Length, Is.EqualTo 23)
    Assert.That (deeper |> Seq.map string,
                 Is.EquivalentTo (expected |> Seq.map string))

  [<Test>]
  member self.FilteredAssembliesDoNotHaveSequencePoints() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    try
        Assert.That(Visitor.ReportFormat(),
                    Is.EqualTo Base.ReportFormat.NCover)
        "Sample" |> (Regex >> FilterClass.Assembly >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Start [path]
                     |> Seq.toList
        // assembly definitions care about being separate references in equality tests
        let def = match Seq.head deeper with
                  | Node.Assembly (def', Inspect.Ignore) -> def'
                  | _ -> Assert.Fail(); null

        let assembly = Node.Assembly (def, Inspect.Ignore)
        let expected = List.concat [ [assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def]]
        Assert.That (deeper.Length, Is.EqualTo 4)
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
    let def = match accumulator.[1] with
              | Node.Assembly (def', Inspect.Instrument) -> def'
              | _ -> Assert.Fail(); null

    let assembly = Node.Assembly (def, Inspect.Instrument)
    let expected = List.concat [ [Start[path]; assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def; Finish]]
    Assert.That (accumulator |> Seq.map string,
                 Is.EquivalentTo (expected |> Seq.map string))

  [<Test>]
  member self.TrackingDetectsTests() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    try
      Visitor.TrackingNames.Clear()
      Visitor.TrackingNames.AddRange(["Junk"; "[MoreJunk"; "[Test"])
      Visitor.Visit [] [] // cheat reset
      let tracks = def.MainModule.GetAllTypes()
                   |> Seq.collect (fun t -> t.Methods)
                   |> Seq.map (Visitor.Track)
                   |> Seq.choose id
                   |> Seq.toList
      Assert.That (tracks, Is.EquivalentTo [ (1, "[Test"); (2, "[Test")])

    finally
      Visitor.TrackingNames.Clear()
      Visitor.Visit [] [] // cheat reset

  [<Test>]
  member self.TrackingDetectsExpectedTests() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    try
      Visitor.TrackingNames.Clear()
      Visitor.TrackingNames.AddRange(["Junk"; "[MoreJunk"; "[Test"])
      Visitor.Visit [] [] // cheat reset
      let tracks = def.MainModule.GetAllTypes()
                   |> Seq.collect (fun t -> t.Methods)
                   |> Seq.filter (fun m -> m |> Visitor.Track |> Option.isSome)
                   |> Seq.map (fun m-> m.Name)
                   |> Seq.toList
      Assert.That (tracks, Is.EquivalentTo [ "testMakeUnion"; "testMakeThing" ])

    finally
      Visitor.TrackingNames.Clear()
      Visitor.Visit [] [] // cheat reset

  [<Test>]
  member self.TrackingDetectsTestsByFullType() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    try
      Visitor.TrackingNames.Clear()
      Visitor.TrackingNames.AddRange(["Junk"; "[MoreJunk"; "[NUnit.Framework.TestAttribute]"])
      Visitor.Visit [] [] // cheat reset
      let tracks = def.MainModule.GetAllTypes()
                   |> Seq.collect (fun t -> t.Methods)
                   |> Seq.map (Visitor.Track)
                   |> Seq.choose id
                   |> Seq.toList
      Assert.That (tracks, Is.EquivalentTo [ (1, "[NUnit.Framework.TestAttribute]")
                                             (2, "[NUnit.Framework.TestAttribute]")])

    finally
      Visitor.TrackingNames.Clear()
      Visitor.Visit [] [] // cheat reset

  [<Test>]
  member self.TrackingDetectsMethods() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    try
      Visitor.TrackingNames.Clear()
      Visitor.TrackingNames.AddRange(["Junk"; "[MoreJunk"; "returnFoo"; "N.DU.MyUnion.as_bar"])
      Visitor.Visit [] [] // cheat reset
      let tracks = def.MainModule.GetAllTypes()
                   |> Seq.collect (fun t -> t.Methods)
                   |> Seq.map (Visitor.Track)
                   |> Seq.choose id
                   |> Seq.toList
      Assert.That (tracks, Is.EquivalentTo [ (1, "returnFoo"); (2, "N.DU.MyUnion.as_bar")])

    finally
      Visitor.TrackingNames.Clear()
      Visitor.Visit [] [] // cheat reset

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
                      "#ctor"; "get_Visits"; "Log"; "GetOperandType"; "#ctor"; ".cctor";
                      "get_Property"; "set_Property"; "get_ReportFile";
                      "set_ReportFile"; "get_Timer"; "set_Timer"; "get_Token"; "set_Token";
                      "get_CoverageFormat"; "set_CoverageFormat";
                      "get_Sample"; "set_Sample";
                      "ToList"; "#ctor" ]
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
                    "System.Int32 Sample3.Class3.GetOperandType(Mono.Cecil.Cil.Instruction)"
                    "System.Void Sample3.Class3.#ctor()"; "System.Void Sample3.Class3..cctor()"
                    "Sample3.Class1 Sample3.Class3+Class4.get_Property()";
                    "System.Void Sample3.Class3+Class4.set_Property(Sample3.Class1)"
                    "System.String Sample3.Class3+Class4.get_ReportFile()"
                    "System.Void Sample3.Class3+Class4.set_ReportFile(System.String)"
                    "System.Int64 Sample3.Class3+Class4.get_Timer()"
                    "System.Void Sample3.Class3+Class4.set_Timer(System.Int64)"
                    "System.String Sample3.Class3+Class4.get_Token()"
                    "System.Void Sample3.Class3+Class4.set_Token(System.String)"
                    "System.Int32 Sample3.Class3+Class4.get_CoverageFormat()"
                    "System.Void Sample3.Class3+Class4.set_CoverageFormat(System.Int32)"
                    "System.Int32 Sample3.Class3+Class4.get_Sample()"
                    "System.Void Sample3.Class3+Class4.set_Sample(System.Int32)"
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
                    | "profilerVersion"
                    | "driverVersion"
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
  member self.ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded() =
    let visitor, document = Report.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        "Sample" |> (Regex >> FilterClass.Module >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)

        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        let xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\" excluded=\"true\" />
</coverage>"
        let xml' = xml.Replace ("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())
        let xml'' = xml'.Replace("name=\"Sample1.exe\"", "name=\"" + sample1 + "\"")

        let baseline = XDocument.Load(new System.IO.StringReader(xml''))
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidate result expected 0 true
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked() =
    let visitor, document = Report.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        "Sample" |> (Regex >> FilterClass.Module >> Visitor.NameFilters.Add)
        Visitor.TrackingNames.Add("Main")
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)

        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        let xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\" excluded=\"true\" />
</coverage>"
        let xml' = xml.Replace ("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())
        let xml'' = xml'.Replace("name=\"Sample1.exe\"", "name=\"" + sample1 + "\"")

        let baseline = XDocument.Load(new System.IO.StringReader(xml''))
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidate result expected 0 true
    finally
      Visitor.NameFilters.Clear()
      Visitor.TrackingNames.Clear()

  // Gendarme.fs (except where I need to compare with the original, which are the weakname tests)

  [<Test>]
  member self.ShouldDetectTernary() =
    let where = Assembly.GetExecutingAssembly().Location
    let path0 = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "_Binaries/Sample3/Debug+AnyCPU/netstandard2.0", "Sample3.dll")
    let path = if File.Exists path0 then path0
               else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "../_Binaries/Sample3/Debug+AnyCPU/netstandard2.0", "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let target = def.MainModule.GetType("Sample3.Class2").GetMethods()
                 |> Seq.filter(fun m -> m.Name = "set_Property")
                 |> Seq.head
    let ternary = target.Body.Instructions
                    |> Seq.cast<Cil.Instruction>
                    |> Seq.filter (fun i -> i.OpCode.FlowControl = FlowControl.Branch)
                    |> Seq.fold(fun state i -> state + (Gendarme.``detect ternary pattern`` <| Some i.Previous.OpCode.Code)) 0
    Assert.That(ternary, Is.EqualTo 1)
    Assert.That(Gendarme.``detect ternary pattern`` None, Is.EqualTo 0)
    Assert.That(Gendarme.CyclomaticComplexity target, Is.EqualTo 3)
    Assert.That(Gendarme.SwitchCyclomaticComplexity target.Body.Instructions, Is.EqualTo 3)

  [<Test>]
  member self.ShouldDetectSwitchNesting() =
    let where = Assembly.GetExecutingAssembly().Location
    let path0 = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "_Binaries/Sample3/Debug+AnyCPU/netstandard2.0", "Sample3.dll")
    let path = if File.Exists path0 then path0
               else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "../_Binaries/Sample3/Debug+AnyCPU/netstandard2.0", "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let target = def.MainModule.GetType("Sample3.Class3").GetMethods()
                 |> Seq.filter(fun m -> m.Name = "GetOperandType")
                 |> Seq.head
    Assert.That(Gendarme.SwitchCyclomaticComplexity target.Body.Instructions, Is.EqualTo 24)

  // OpenCover.fs

  [<Test>]
  member self.SafeMultiplyIsSafe() =
    Assert.That (OpenCover.SafeMultiply 1 0, Is.EqualTo 1)
    Assert.That (OpenCover.SafeMultiply 2 3, Is.EqualTo 6)
    Assert.That (OpenCover.SafeMultiply 65536 65536, Is.EqualTo Int32.MaxValue)

  [<Test>]
  member self.EmptyMethodHasComplexity1() =
    let m = MethodDefinition("dummy", MethodAttributes.Abstract, TypeDefinition("System", "Void", TypeAttributes.Public))
    Assert.That (Gendarme.CyclomaticComplexity m, Is.EqualTo 1)

  [<Test>]
  member self.BranchChainsSerialize() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")
    use def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let method = def.MainModule.GetAllTypes()
                 |> Seq.collect (fun t -> t.Methods)
                 |> Seq.find (fun m -> m.Name = "as_bar")
    Visitor.Visit [] [] // cheat reset
    try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        "Program" |> (Regex >> FilterClass.File >> Visitor.NameFilters.Add)
        let branches = Visitor.Deeper <| Node.Method (method,
                                                    Inspect.Instrument,
                                                    None)
                       |> Seq.map (fun n -> match n with
                                            | BranchPoint b -> Some b
                                            | _ -> None)
                       |> Seq.choose id |> Seq.toList

        // The only overt branching in this function are the 4 match cases
        // Internal IL conditional branching is a compiler thing from inlining "string"
        Assert.That (branches |> Seq.length, Is.EqualTo 4)
        let branch = branches |> Seq.head
        Assert.That (branch.Target.Length, Is.EqualTo 2)
        let xbranch = XElement(XName.Get "test")
        OpenCover.setChain xbranch branch.Target.Tail
        Assert.That (xbranch.ToString(),
                     Is.EqualTo """<test offsetchain="29" />""")
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.BranchChainsTerminate() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")
    use def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let method = def.MainModule.GetAllTypes()
                 |> Seq.collect (fun t -> t.Methods)
                 |> Seq.find (fun m -> m.Name = "as_bar")
    let fin = method.Body.Instructions |> Seq.last
    let list = Visitor.getJumpChain fin fin
    Assert.That (list, Is.EquivalentTo [fin])

  static member private RecursiveValidateOpenCover result expected' depth zero expectSkipped =
    let X name =
      XName.Get(name)

    let rcount = result |> Seq.length

    let expected = expected'
                   |> Seq.filter (fun (el:XElement) -> el.Name.LocalName <> "Module" ||
                                                       expectSkipped ||
                                                       "skippedDueTo" |> X |> el.Attributes |> Seq.isEmpty)
                   |> Seq.toList
    let ecount = expected |> Seq.length

    Assert.That(rcount, Is.EqualTo(ecount), "Mismatch at depth " + depth.ToString() + " : " +
                                            expected.ToString() + " but got" + (result |> Seq.toList).ToString())

    Seq.zip result expected |> Seq.iter (fun ((r:XElement), (e:XElement)) ->
            Assert.That(r.Name, Is.EqualTo(e.Name), "Expected name " + e.Name.ToString())
            let ra = r.Attributes()
            let ea = e.Attributes()

            Seq.zip ra ea |> Seq.iter (fun ((a1:XAttribute), (a2:XAttribute)) ->
                    Assert.That(a1.Name, Is.EqualTo(a2.Name))
                    match a1.Name.ToString() with
                    | "bev"
                    | "visited"
                    | "visitedSequencePoints"
                    | "visitedBranchPoints"
                    | "visitedClasses"
                    | "visitedMethods"
                    | "sequenceCoverage"
                    | "branchCoverage"
                    | "uspid"
                    | "minCrapScore"
                    | "maxCrapScore"
                    | "crapScore"
                    | "hash" -> ()
                    | "fullPath" -> Assert.That(a1.Value.Replace("\\","/"), Does.EndWith(a2.Value.Replace("\\","/")),
                                                a1.Name.ToString() + " : " + r.ToString() + " -> document")
                    | "vc" -> let expected = if zero then "0" else a2.Value
                              Assert.That(a1.Value, Is.EqualTo(expected), r.ToString() + " -> visitcount")
                    | _ -> Assert.That(a1.Value, Is.EqualTo(a2.Value), r.ToString() + " -> " + a1.Name.ToString())
                )

            AltCoverTests.RecursiveValidateOpenCover (r.Elements()) (e.Elements()) (depth+1) zero expectSkipped)

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        Visitor.NameFilters.Clear()
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)
        let resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

        let baseline = XDocument.Load(stream)
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let X name =
      XName.Get(name)

    try
        Visitor.NameFilters.Clear()
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        Visitor.coverstyle <- CoverStyle.LineOnly
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)
        let resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

        let baseline = XDocument.Load(stream)

        // strip out branch information
        baseline.Descendants(X "Summary")
        |> Seq.filter(fun x -> x.Attribute(X "numBranchPoints").Value = "3")
        |> Seq.iter(fun x -> x.Attribute(X "numBranchPoints").Value <- "1")

        baseline.Descendants(X "Method")
        |> Seq.iter(fun x -> x.Attribute(X "nPathComplexity").Value <- "0")
        baseline.Descendants(X "SequencePoint")
        |> Seq.iter(fun x -> x.Attribute(X "bec").Value <- "0")
        baseline.Descendants(X "BranchPoint")
        |> Seq.toList
        |> Seq.iter(fun x -> x.Remove())

        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None
      Visitor.coverstyle <- CoverStyle.All

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let X name =
      XName.Get(name)

    try
        Visitor.NameFilters.Clear()
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        Visitor.coverstyle <- CoverStyle.BranchOnly
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)
        let resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

        let baseline = XDocument.Load(stream)

        // strip out line information
        baseline.Descendants(X "Summary")
        |> Seq.filter(fun x -> x.Attribute(X "numSequencePoints").Value = "10")
        |> Seq.iter(fun x -> x.Attribute(X "numSequencePoints").Value <- "0")
        baseline.Descendants(X "SequencePoint")
        |> Seq.toList
        |> Seq.iter(fun x -> x.Remove())

        let result = document.Elements()
        let expected = baseline.Elements()

        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None
      Visitor.coverstyle <- CoverStyle.All

  member self.AddTrackingForMain xml =
    let resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                        |> Seq.find (fun n -> n.EndsWith(xml, StringComparison.Ordinal))

    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
    let baseline = XDocument.Load(stream)
    let tail = baseline.Descendants(XName.Get "Module") |> Seq.last
    let tracked = XElement(XName.Get "TrackedMethods")
    tail.Add(tracked)
    tracked.Add(XElement(XName.Get "TrackedMethod",
                            XAttribute(XName.Get "uid", "1"),
                            XAttribute(XName.Get "token", "100663297"),
                            XAttribute(XName.Get "name", "System.Void TouchTest.Program::Main(System.String[])"),
                            XAttribute(XName.Get "strategy", "Main")))
    baseline

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        Visitor.NameFilters.Clear()
        Visitor.TrackingNames.Clear()
        Visitor.TrackingNames.Add("Main")
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)

        let baseline = self.AddTrackingForMain "Sample1WithOpenCover.xml"
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.reportFormat <- None
      Visitor.NameFilters.Clear()
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        "Sample" |> (Regex >> FilterClass.Module >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)
        let raw = "<CoverageSession xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <Summary numSequencePoints=\"0\" visitedSequencePoints=\"0\" numBranchPoints=\"0\" visitedBranchPoints=\"0\" sequenceCoverage=\"0\" branchCoverage=\"0\" maxCyclomaticComplexity=\"0\" minCyclomaticComplexity=\"1\" visitedClasses=\"0\" numClasses=\"0\" visitedMethods=\"0\" numMethods=\"0\" />
        <Modules>
        <Module skippedDueTo=\"Filter\" hash=\"C2-87-B9-AA-6B-1D-03-60-30-9A-15-4A-D5-28-87-C2-9E-B9-8E-8D\">
        <ModulePath>_Binaries\\AltCover.Tests\\Debug+AnyCPU\\Sample1.exe</ModulePath>
        <ModuleTime>2018-03-15T14:00:17.3385938Z</ModuleTime>
        <ModuleName>Sample1</ModuleName>
        <Classes />
        </Module>
        </Modules>
        </CoverageSession>"
        let baseline = XDocument.Load(new System.IO.StringReader(raw))
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true true
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        Visitor.TrackingNames.Clear()
        Visitor.TrackingNames.Add("Main")
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        "Sample" |> (Regex >> FilterClass.Module >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)
        let raw = "<CoverageSession xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <Summary numSequencePoints=\"0\" visitedSequencePoints=\"0\" numBranchPoints=\"0\" visitedBranchPoints=\"0\" sequenceCoverage=\"0\" branchCoverage=\"0\" maxCyclomaticComplexity=\"0\" minCyclomaticComplexity=\"1\" visitedClasses=\"0\" numClasses=\"0\" visitedMethods=\"0\" numMethods=\"0\" />
        <Modules>
        <Module skippedDueTo=\"Filter\" hash=\"C2-87-B9-AA-6B-1D-03-60-30-9A-15-4A-D5-28-87-C2-9E-B9-8E-8D\">
        <ModulePath>_Binaries\\AltCover.Tests\\Debug+AnyCPU\\Sample1.exe</ModulePath>
        <ModuleTime>2018-03-15T14:00:17.3385938Z</ModuleTime>
        <ModuleName>Sample1</ModuleName>
        <Classes />
        <TrackedMethods>
        <TrackedMethod uid=\"1\" token=\"100663297\" name=\"System.Void TouchTest.Program::Main(System.String[])\" strategy=\"Main\" />
        </TrackedMethods>
        </Module>
        </Modules>
        </CoverageSession>"
        let baseline = XDocument.Load(new System.IO.StringReader(raw))
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true true
    finally
      Visitor.NameFilters.Clear()
      Visitor.TrackingNames.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        Assert.That (Visitor.ReportFormat(),
                     Is.EqualTo Base.ReportFormat.OpenCover)
        "Program" |> (Regex >> FilterClass.Type >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)
        let resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("Sample1ClassExclusion.xml", StringComparison.Ordinal))

        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        let baseline = XDocument.Load(stream)
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        Visitor.TrackingNames.Clear()
        Visitor.TrackingNames.Add("Main")
        Assert.That (Visitor.ReportFormat(),
                     Is.EqualTo Base.ReportFormat.OpenCoverWithTracking)
        "Program" |> (Regex >> FilterClass.Type >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)

        let baseline = self.AddTrackingForMain "Sample1ClassExclusion.xml"
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()
      Visitor.TrackingNames.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        "Main" |> (Regex >> FilterClass.Method >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)
        let resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("Sample1MethodExclusion.xml", StringComparison.Ordinal))

        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        let baseline = XDocument.Load(stream)
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)

    try
        Visitor.TrackingNames.Clear()
        Visitor.TrackingNames.Add("Main")
        Assert.That (Visitor.ReportFormat(),
                     Is.EqualTo Base.ReportFormat.OpenCoverWithTracking)
        "Main" |> (Regex >> FilterClass.Method >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)

        let baseline = self.AddTrackingForMain "Sample1MethodExclusion.xml"
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()
      Visitor.TrackingNames.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.ShouldSortFileIds() =
    let visitor, document = OpenCover.ReportGenerator()
    let X name =
      XName.Get(name)
    // Hack for running while instrumented
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0", "AltCover.dll")
#if NETCOREAPP2_0
    let path' = if File.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "../_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0", "AltCover.dll")
#else
    let path' = path
#endif

    Visitor.Visit [ visitor ] (Visitor.ToSeq path')
    Assert.That (document.Descendants(X "Module") |> Seq.length, Is.EqualTo 1)
    Assert.That (document.Descendants(X "File") |> Seq.length, Is.GreaterThan 1)
    document.Descendants(X "File")
    |> Seq.iteri (fun i x -> Assert.That(x.Attribute(X "uid").Value, Is.EqualTo (string (1 + i))))

end