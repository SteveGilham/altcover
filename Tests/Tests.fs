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
open Mono.Options
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
                  |> Seq.filter (fun x -> not <| (snd x).FullName.StartsWith("FSharp.", StringComparison.OrdinalIgnoreCase))
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
                    |> Seq.map Visitor.DeclaringMethod
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
                     None // System.Collections.Generic.IEnumerable`1<System.Int32> Sample5.Class1/Inner::G2(System.String)
                     None // System.Threading.Tasks.Task`1<System.String> Sample5.Class1/Inner::G3(System.String)
                     None // System.Void Sample5.Class1/Inner::G3(System.Int32)
                     None // System.Void Sample5.Class1/Inner::.ctor()
                     Some "G1" // "System.Int32 Sample5.Class1/Inner::<G1>g__Interior|0_1(System.Int32,System.Int32)"
                     Some "G1" // "System.Int32 Sample5.Class1/Inner::<G1>g__Recursive|0_3(System.Int32)"
                     None // System.Void Sample5.Class1/Inner/<>c__DisplayClass0_0::.ctor()
                     Some "G1" // System.Int32 Sample5.Class1/Inner/<>c__DisplayClass0_0::<G1>b__1(System.Char)
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
                     Some "F1" // System.Int32 Sample5.Class1/<>c__DisplayClass0_0::<F1>b__1(System.Char)
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
     result |> Seq.toList
      |> List.zip expected
      |> List.iteri (fun i (x,y) -> Assert.That(y, Is.EqualTo x, sprintf "%A %A %d" x y i))

     let g3 = methods.[8]
     Assert.That (methods
                  |> Seq.map Visitor.DeclaringMethod
                  |> Seq.choose id
                  |> Seq.filter (fun m -> m.Name = "G3"),
                  Is.EquivalentTo [g3;g3;g3])

  [<Test>]
  member self.FSharpNestedMethods() =
     let sample3 = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Sample6.dll")
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (sample3)
     let methods = def.MainModule.GetAllTypes()
                    |> Seq.collect(fun t -> t.Methods)
                    |> Seq.toList

     let result = methods
                    |> Seq.map Visitor.DeclaringMethod
                    |> Seq.map (fun (mo : MethodDefinition option) ->
                                    mo |> Option.map (fun m -> m.DeclaringType.Name + "::" + m.Name))

     let expected = [
                     None //Microsoft.FSharp.Collections.FSharpList`1<System.Int32> Sample6.Module::F1(Microsoft.FSharp.Collections.FSharpList`1<System.Object>)
                     None //Microsoft.FSharp.Core.Unit[] Sample6.Module::F2(Microsoft.FSharp.Collections.FSharpList`1<System.String>)
                     Some "FI@9T::Invoke" //System.Void Sample6.Module/FII@10::.ctor()
                     Some "FI@9T::Invoke" //System.Object Sample6.Module/FII@10::Specialize()
                     Some "FI@9T::Invoke" //System.Void Sample6.Module/FII@10T::.ctor(Sample6.Module/FII@10)
                     Some "FI@9T::Invoke"  //System.Int32 Sample6.Module/FII@10T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<b>,System.Int32)
                     Some "Module::F1" //System.Void Sample6.Module/FI@9::.ctor()
                     Some "Module::F1" //System.Object Sample6.Module/FI@9::Specialize()
                     Some "Module::F1" //System.Void Sample6.Module/FI@9T::.ctor(Sample6.Module/FI@9)
                     Some "Module::F1" //System.Int32 Sample6.Module/FI@9T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<a>)
                     Some "Module::F1" //System.Void Sample6.Module/F1@17::.ctor()
                     Some "Module::F1" //System.Int32 Sample6.Module/F1@17::Invoke(System.Object)
                     Some "fetchUrlAsync@25-4::Invoke" //System.Void Sample6.Module/fetchUrlAsync@26-5::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@25-4::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@26-5::Invoke(System.IO.StreamReader)
                     Some "fetchUrlAsync@23-3::Invoke" //System.Void Sample6.Module/fetchUrlAsync@25-4::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@23-3::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@25-4::Invoke(System.IO.Stream)
                     Some "fetchUrlAsync@23-2::Invoke" //System.Void Sample6.Module/fetchUrlAsync@23-3::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@23-2::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-3::Invoke(System.Net.WebResponse)
                     Some "fetchUrlAsync@22-1::Invoke" //System.Void Sample6.Module/fetchUrlAsync@23-2::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@22-1::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-2::Invoke(System.Net.WebResponse)
                     Some "fetchUrlAsync@21::Invoke" //System.Void Sample6.Module/fetchUrlAsync@22-1::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)
                     Some "fetchUrlAsync@21::Invoke" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@22-1::Invoke(Microsoft.FSharp.Core.Unit)
                     Some "Module::F2" //System.Void Sample6.Module/fetchUrlAsync@21::.ctor()
                     Some "Module::F2" //Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@21::Invoke(System.String)
                         ]
     Assert.That (result, Is.EquivalentTo expected)

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
                      "get_CoverageFormat"; "set_CoverageFormat"; "ToList"; "#ctor" ]
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
    recorder
    |> List.zip [
                  "System.Void AltCover.Recorder.Instance.Visit(System.String,System.Int32)"
                  "System.Void AltCover.Recorder.Instance.Push(System.Int32)"
                  "System.Void AltCover.Recorder.Instance.Pop()"
                ]
    |> List.iter(fun (n,m) -> Assert.That(Naming.FullMethodName m, Is.EqualTo n))

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
  member self.ForeignTokenIsNotMatchedInIndex() =
    try
      Visitor.keys.Clear()
      self.ProvideKeyPair() |> Visitor.Add
      let path = typeof<System.IO.FileAccess>.Assembly.Location
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      let key = KeyStore.ArrayToIndex def.Name.PublicKey
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
  member self.GuardShouldDisposeRecordingAssemblyOnException () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let prepared = AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols prepared

    let bang = fun () -> InvalidOperationException("Bang") |> raise

    Assert.Throws<InvalidOperationException>(fun() -> Instrument.Guard prepared bang |> ignore) |> ignore
    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"
    try
      Assert.Throws<ArgumentException>(fun () -> Instrument.WriteAssembly prepared outputdll) |> ignore
    finally
        Directory.EnumerateFiles(Path.GetDirectoryName output,
                                 (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f -> try File.Delete f
                              with // occasionally the dll file is locked by another process
                              | :? System.UnauthorizedAccessException
                              | :? IOException -> ())

  [<Test>]
  member self.ShouldBeAbleToPrepareTheAssembly () =
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let prepared = Instrument.PrepareAssembly path
      let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols raw
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

      Assert.That (after'.[0].Body.Instructions.Count, Is.EqualTo(2))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.ShouldGetTrackingStyleIfSet () =
      let save2 = Visitor.reportFormat
      let save3 = Visitor.interval
      Visitor.TrackingNames.Clear()
      try
        Visitor.reportFormat <- Some AltCover.Base.ReportFormat.OpenCover
        Visitor.interval <- Some 1234567890
        Assert.That (Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.OpenCoverWithTracking)
        Visitor.interval <- None
        Visitor.TrackingNames.Add("dummy")
        Assert.That (Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.OpenCoverWithTracking)
        Visitor.TrackingNames.Clear()
        Assert.That (Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
        Visitor.reportFormat <- Some AltCover.Base.ReportFormat.NCover
        Visitor.interval <- Some 1234567890
        Assert.That (Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.NCover)
        Visitor.interval <- None
        Visitor.TrackingNames.Add("dummy")
        Assert.That (Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.NCover)
        Visitor.TrackingNames.Clear()
        Assert.That (Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.NCover)
      finally
        Visitor.reportFormat <- save2
        Visitor.interval <- save3
      Visitor.TrackingNames.Clear()

#if NETCOREAPP2_0
// TODO
#else
  [<Test>]
  member self.ShouldSymbolWriterOnWindowsOnly () =
    match Instrument.CreateSymbolWriter ".pdb" true true with
    | :? Mono.Cecil.Mdb.MdbWriterProvider -> ()
    | x -> Assert.Fail("Mono.Cecil.Mdb.MdbWriterProvider expected but got " + x.GetType().FullName)

    match Instrument.CreateSymbolWriter ".pdb" true false with
    | :? Mono.Cecil.Pdb.PdbWriterProvider -> ()
    | x -> Assert.Fail("Mono.Cecil.Pdb.PdbWriterProvider expected but got " + x.GetType().FullName)

    match Instrument.CreateSymbolWriter ".pdb" false false with
    | null -> ()
    | x -> Assert.Fail("null expected but got " + x.GetType().FullName)

    match Instrument.CreateSymbolWriter ".exe" true false with
    | :? Mono.Cecil.Mdb.MdbWriterProvider -> ()
    | x -> Assert.Fail("Mono.Cecil.Mdb.MdbWriterProvider expected but got " + x.GetType().FullName)

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
      let save2 = Visitor.reportFormat
      let save3 = Visitor.interval
      try
        Visitor.reportPath <- Some unique
        Visitor.reportFormat <- Some AltCover.Base.ReportFormat.OpenCover
        Visitor.interval <- Some 1234567890
        let prepared = Instrument.PrepareAssembly path
        Instrument.WriteAssembly prepared outputdll
        let expectedSymbols = if "Mono.Runtime" |> Type.GetType |> isNull |> not then ".dll.mdb" else ".pdb"
        let isWindows =
#if NETCOREAPP2_0
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif

        if isWindows then Assert.That (File.Exists (outputdll.Replace(".dll", expectedSymbols)))
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
          let report2 = proxyObject.InvokeMethod("get_CoverageFormat",[||]) :?> System.Int32
          Assert.That (report2, AltCover.Base.ReportFormat.OpenCoverWithTracking |> int |> Is.EqualTo)
          let report3 = proxyObject.InvokeMethod("get_Timer",[||]) :?> System.Int64
          Assert.That (report3, 1234567890L |> Is.EqualTo)
        finally
          AppDomain.Unload(ad)
      finally
        Visitor.reportPath <- save
        Visitor.reportFormat <- save2
        Visitor.interval <- save3
        Directory.EnumerateFiles(Path.GetDirectoryName output,
                                 (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f -> try File.Delete f
                              with // occasionally the dll file is locked by another process
                              | :? System.UnauthorizedAccessException
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
        ProgramDatabase.ReadSymbols def

        let clazz = def.MainModule.GetType("Sample3.Class1")
        let func = clazz.GetMethods() |> Seq.find (fun x -> x.Name = "get_Property")

        let clazz' = def.MainModule.GetType("Sample3.Class3")
        let func' = clazz'.GetMethods() |> Seq.find (fun x -> x.Name = "Log")

        let newValue = Instrument.InsertVisit (func.Body.Instructions.[0]) (func.Body.GetILProcessor()) func' unique 42
        Assert.That (newValue.Operand, Is.EqualTo unique)
        Assert.That (newValue.OpCode, Is.EqualTo OpCodes.Ldstr)

        Instrument.WriteAssembly def outputdll
        let expectedSymbols = if "Mono.Runtime" |> Type.GetType |> isNull |> not then ".dll.mdb" else ".pdb"
        let isWindows =
#if NETCOREAPP2_0
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
        if isWindows then Assert.That (File.Exists (outputdll.Replace(".dll", expectedSymbols)))
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
                              | :? System.UnauthorizedAccessException
                              | :? IOException -> ())
    finally
      Visitor.keys.Clear()
#endif

  [<Test>]
  member self.ShouldUpdateHandlerOK ([<Range(0,31)>] selection) =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def

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
    ProgramDatabase.ReadSymbols def

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
    ProgramDatabase.ReadSymbols def

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
    ProgramDatabase.ReadSymbols def

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
    ProgramDatabase.ReadSymbols def

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
    ProgramDatabase.ReadSymbols def

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
  member self.ShouldBeAbleToTrackAMethod () =
    let where = Assembly.GetExecutingAssembly().Location
#if NETCOREAPP2_0
    let shift = String.Empty
#else
    let shift = "/netcoreapp2.0"
#endif
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack() +
                            shift, "AltCover.Recorder.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let recorder = AltCover.Instrument.RecordingMethod def
    let raw = AltCover.Instrument.Context.Build([])
    let state = {  raw with
                    RecordingMethodRef = { raw.RecordingMethodRef with
                                             Visit = null
                                             Push = recorder.[1]
                                             Pop = recorder.[2] }}
    let countBefore = recorder.Head.Body.Instructions.Count
    let tailsBefore = recorder.Head.Body.Instructions
                      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
                      |> Seq.length
    let handlersBefore = recorder.Head.Body.ExceptionHandlers.Count

    AltCover.Instrument.Track state recorder.Head Inspect.Track <| Some(42, "hello")
    Assert.That (recorder.Head.Body.Instructions.Count, Is.EqualTo (countBefore + 5 - tailsBefore))
    Assert.That (recorder.Head.Body.ExceptionHandlers.Count, Is.EqualTo (handlersBefore + 1))

  [<Test>]
  member self.ShouldBeAbleToTrackAMethodWithTailCalls () =
    let where = Assembly.GetExecutingAssembly().Location
#if NETCOREAPP2_0
    let shift = String.Empty
#else
    let shift = "/netcoreapp2.0"
#endif
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack() +
                            shift, "AltCover.Recorder.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let recorder = AltCover.Instrument.RecordingMethod def
    let target = def.MainModule.GetType("AltCover.Recorder.Instance").Methods
                 |> Seq.find (fun m -> m.Name = "loop")
    let raw = AltCover.Instrument.Context.Build([])
    let state = {  raw with
                    RecordingMethodRef = { raw.RecordingMethodRef with
                                             Visit = null
                                             Push = recorder.[1]
                                             Pop = recorder.[2] }}
    let countBefore = target.Body.Instructions.Count
    let tailsBefore = target.Body.Instructions
                      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
                      |> Seq.length
    let handlersBefore = target.Body.ExceptionHandlers.Count

    AltCover.Instrument.Track state target Inspect.Track <| Some(42, "hello")
    Assert.That (target.Body.Instructions.Count, Is.EqualTo (countBefore + 5 - tailsBefore))
    Assert.That (target.Body.ExceptionHandlers.Count, Is.EqualTo (handlersBefore + 1))

  [<Test>]
  member self.ShouldNotChangeAnUntrackedMethod () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "AltCover.Recorder.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let recorder = AltCover.Instrument.RecordingMethod def
    let state = AltCover.Instrument.Context.Build([])
    let countBefore = recorder.Head.Body.Instructions.Count
    let handlersBefore = recorder.Head.Body.ExceptionHandlers.Count

    AltCover.Instrument.Track state recorder.Head Inspect.Track None
    Assert.That (recorder.Head.Body.Instructions.Count, Is.EqualTo countBefore)
    Assert.That (recorder.Head.Body.ExceptionHandlers.Count, Is.EqualTo handlersBefore)

  [<Test>]
  member self.SwitchBranchesShouldInstrumentByPushingDown() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let method = def.MainModule.GetAllTypes()
                 |> Seq.collect (fun t -> t.Methods)
                 |> Seq.find (fun m -> m.Name = "as_bar")
    Visitor.Visit [] [] // cheat reset
    try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        let branches = Visitor.Deeper <| Node.Method (method,
                                                    Inspect.Instrument,
                                                    None)
                     |> Seq.map (fun n -> match n with
                                          | BranchPoint b -> Some b
                                          | _ -> None)
                     |> Seq.choose id
                     |> Seq.take 2 // start of a switch
                     |> Seq.toList
        match branches with
        | [b1 ; b2] ->
            Assert.That (b1.Start.OpCode, Is.EqualTo OpCodes.Switch)
            Assert.That (b2.Start.OpCode, Is.EqualTo OpCodes.Switch)
            Assert.That (b1.Start.Offset, Is.EqualTo b2.Start.Offset)
        | _ -> Assert.Fail("wrong number of items")

        let raw = AltCover.Instrument.Context.Build([])
        let state = {  raw with
                           RecordingMethodRef = { raw.RecordingMethodRef with
                                                                          Visit = method
                                                                          Push = null
                                                                          Pop = null }
                           MethodWorker = method.Body.GetILProcessor()}
        let next = branches.Head.Start.Next
        branches
        |> Seq.iter(fun b -> Instrument.VisitBranchPoint state b
                             |> ignore)

        let inject = Seq.unfold (fun (state:Cil.Instruction) -> if isNull state || state = next then None else Some (state, state.Next)) branches.Head.Start
                     |> Seq.skip 1 |> Seq.toList

        Assert.That (inject.Length, Is.EqualTo 8)
        let switches = branches.Head.Start.Operand :?> Instruction[]
                       |> Seq.toList
        Assert.That (switches.[0], Is.EqualTo inject.[1])
        Assert.That (switches.[1], Is.EqualTo inject.[0])
        Assert.That (inject.[0].Operand, Is.EqualTo inject.[5])
        Assert.That ((inject.[2].Operand :?> int) &&& Base.Counter.BranchMask , Is.EqualTo 1)
        Assert.That ((inject.[6].Operand :?> int) &&& Base.Counter.BranchMask , Is.EqualTo 0)

    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.SimpleBranchShouldInstrumentByPushingDown() =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where),
#if NETCOREAPP2_0
                    "Sample1.dll")
#else
                    "Sample1.exe")
#endif

    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let method = def.MainModule.GetAllTypes()
                 |> Seq.collect (fun t -> t.Methods)
                 |> Seq.find (fun m -> m.Name = "Main")
    Visitor.Visit [] [] // cheat reset
    try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        let branches = Visitor.Deeper <| Node.Method (method,
                                                    Inspect.Instrument,
                                                    None)
                     |> Seq.map (fun n -> match n with
                                          | BranchPoint b -> Some b
                                          | _ -> None)
                     |> Seq.choose id
                     |> Seq.take 2 // start of a switch
                     |> Seq.toList
        match branches with
        | [b1 ; b2] -> ()
        | _ -> Assert.Fail("wrong number of items")

        let raw = AltCover.Instrument.Context.Build([])
        let state = {  raw with
                           RecordingMethodRef = { raw.RecordingMethodRef with
                                                                          Visit = method
                                                                          Push = null
                                                                          Pop = null }
                           MethodWorker = method.Body.GetILProcessor()}
        let next = branches.Head.Start.Next

        branches
        |> Seq.iter(fun b -> Instrument.VisitBranchPoint state b
                             |> ignore)

        let inject = Seq.unfold (fun (state:Cil.Instruction) -> if isNull state || state = next then None else Some (state, state.Next)) branches.Head.Start
                     |> Seq.skip 1 |> Seq.toList

        Assert.That (inject.Length, Is.EqualTo 8)
        Assert.That (inject.[0].Operand, Is.EqualTo inject.[5])
        Assert.That ((inject.[2].Operand :?> int) &&& Base.Counter.BranchMask , Is.EqualTo 1)
        Assert.That ((inject.[6].Operand :?> int) &&& Base.Counter.BranchMask , Is.EqualTo 0)

    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None

  [<Test>]
  member self.StartShouldLoadRecordingAssembly () =
    let def = Instrument.InstrumentationVisitor (Instrument.Context.Build []) (Start [])
    Assert.That (def.RecordingAssembly.Name.Name, Is.EqualTo "AltCover.Recorder.g")

  [<Test>]
  member self.TypeShouldNotChangeState () =
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input (Node.Type (null, Inspect.Ignore))
    Assert.That (output, Is.SameAs input)

  [<Test>]
  member self.ExcludedMethodShouldNotChangeState () =
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input (Node.Method (null, Inspect.Ignore, None))
    Assert.That (output, Is.SameAs input)

  [<Test>]
  member self.IncludedMethodShouldChangeState () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def

    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let func = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input (Node.Method (func, Inspect.Instrument, None))
    Assert.That (output.MethodBody, Is.SameAs func.Body)

  [<Test>]
  member self.ExcludedAfterMethodShouldNotChangeState () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def

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

    let output = Instrument.InstrumentationVisitor input (Node.AfterMethod (func, Inspect.Ignore, None))
    Assert.That (output, Is.SameAs input)
    let paired' = Seq.zip diff input.MethodBody.Instructions
    Assert.That (paired' |> Seq.forall (fun ((i,x),j) -> x = (i = j.OpCode)))

  [<Test>]
  member self.IncludedAfterMethodShouldRewriteMethod () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def

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

    let output = Instrument.InstrumentationVisitor input (Node.AfterMethod (func, Inspect.Instrument, None))
    Assert.That (output, Is.SameAs input)
    let paired' = Seq.zip opcodes input.MethodBody.Instructions
    Assert.That (paired' |> Seq.forall (fun (i,j) -> i = j.OpCode))

  [<Test>]
  member self.UpdateStrongReferencesShouldChangeSigningKeyWherePossible () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let token0 = def.Name.PublicKeyToken

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    Visitor.defaultStrongNameKey <- Some (StrongNameKeyPair(buffer.ToArray()))

    Instrument.UpdateStrongReferences def
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

  [<Test>]
  member self.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let token0 = def.Name.PublicKeyToken

    Visitor.defaultStrongNameKey <- None

    Instrument.UpdateStrongReferences def
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Empty)
#if NETCOREAPP2_0
    Assert.That (token1, Is.EquivalentTo(token0))
#else
    Assert.That (token1, Is.Not.EquivalentTo(token0))
#endif

    let token' = String.Join(String.Empty, token1|> Seq.map (fun x -> x.ToString("x2")))
    Assert.That (token', Is.EqualTo String.Empty)

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
    ProgramDatabase.ReadSymbols def

#if NETCOREAPP2_0
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    Visitor.defaultStrongNameKey <- Some (StrongNameKeyPair(buffer.ToArray()))

    Instrument.UpdateStrongReferences def
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Empty)

  [<Test>]
  member self.ExcludedAssemblyRefsAreNotUpdated () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
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
    let visited = Node.Assembly (def, Inspect.Ignore)

    let result = Instrument.InstrumentationVisitor {state with RecordingAssembly = fake } visited
    Assert.That (def.MainModule.AssemblyReferences, Is.EquivalentTo refs)

  [<Test>]
  member self.IncludedAssemblyRefsAreUpdated () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
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
    let visited = Node.Assembly (def, Inspect.Instrument)

    let result = Instrument.InstrumentationVisitor {state with RecordingAssembly = fake } visited
    Assert.That (def.MainModule.AssemblyReferences, Is.EquivalentTo (refs @ [fake.Name]))

  [<Test>]
  member self.ExcludedModuleJustRecordsMVid () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let visited = Node.Module (def.MainModule, Inspect.Ignore)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]
    let result = Instrument.InstrumentationVisitor  state visited
    Assert.That (result, Is.EqualTo  { state with ModuleId = def.MainModule.Mvid.ToString() })

  [<Test>]
  member self.IncludedModuleEnsuresRecorder () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let visited = Node.Module (def.MainModule, Inspect.Instrument)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]

    let path' = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(),
                             "AltCover.Recorder.dll")
    let def' = Mono.Cecil.AssemblyDefinition.ReadAssembly path'
    let visit = def'.MainModule.GetAllTypes()
                |> Seq.filter (fun t -> t.Name = "Instance")
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.filter (fun m -> m.Name = "Visit" || m.Name = "Push" || m.Name = "Pop")
                |> Seq.sortBy(fun m -> m.Name)
                |> Seq.toList
                |> List.rev

    let state' = { state with RecordingAssembly = def' }
    let result = Instrument.InstrumentationVisitor state' visited

    Assert.That(result.RecordingMethodRef.Visit.Module,
                                        Is.EqualTo (def.MainModule))
    Assert.That (string result.RecordingMethodRef.Visit,
                 visit |> Seq.head |> string |> Is.EqualTo)
    Assert.That (string result.RecordingMethodRef.Push,
                 visit |> Seq.skip 1 |> Seq.head |> string |> Is.EqualTo)
    Assert.That (string result.RecordingMethodRef.Pop,
                 visit |> Seq.skip 2 |> Seq.head |> string |> Is.EqualTo)

    Assert.That ({ result with RecordingMethodRef = {Visit = null; Push = null; Pop = null}},
                 Is.EqualTo  { state' with ModuleId = def.MainModule.Mvid.ToString()
                                                      RecordingMethod = visit
                                                      RecordingMethodRef = {Visit = null; Push = null; Pop = null} })

  [<Test>]
  member self.ExcludedMethodPointIsPassThrough () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let visited = Node.MethodPoint (null, None, 0, false)
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
    ProgramDatabase.ReadSymbols def
    let module' = def.MainModule.GetType("N.DU")
    let du = module'.NestedTypes |> Seq.filter (fun t -> t.Name = "MyUnion") |> Seq.head
    let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
    let proc = main.Body.GetILProcessor()
    let dbg = main.DebugInformation
    let target = main.Body.Instructions
                 |> Seq.filter (dbg.GetSequencePoint >> isNull >> not)
                 |> Seq.head
    let visited = Node.MethodPoint (target, None, 32767, true)
    Assert.That (target.Previous, Is.Null)
    let state = { (Instrument.Context.Build []) with MethodWorker = proc
                                                     MethodBody = main.Body
                                                     RecordingMethodRef = {Visit = def.MainModule.ImportReference main; Push = null; Pop = null}}
    let result = Instrument.InstrumentationVisitor state visited
    Assert.That (result, Is.SameAs state)
    Assert.That (target.Previous.OpCode, Is.EqualTo OpCodes.Call)

  [<Test>]
  member self.IncludedModuleDoesNotChangeRecorderJustTheReference () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let visited = Node.Module (def.MainModule, Inspect.Instrument)
    let state = Instrument.Context.Build ["nunit.framework"; "nonesuch"]

    let path' = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(),
                             "AltCover.Recorder.dll")
    let def' = Mono.Cecil.AssemblyDefinition.ReadAssembly path'
    let visit = def'.MainModule.GetAllTypes()
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.filter (fun m -> m.Name = "Visit")
                |> Seq.head
    let def'' = Mono.Cecil.AssemblyDefinition.ReadAssembly where

    let v = def''.MainModule.ImportReference visit
    let r = {Instrument.RecorderRefs.Build() with Visit = v; Push = v; Pop = v}
    let state' = { state with RecordingAssembly = def'
                              RecordingMethod = [visit;visit;visit]
                              RecordingMethodRef = r}
    let result = Instrument.InstrumentationVisitor state' visited
    let ref'' = def.MainModule.ImportReference visit

    Assert.That (result.RecordingMethodRef.Visit.Module,
                Is.EqualTo ( def.MainModule))
    Assert.That (string result.RecordingMethodRef,
                Is.EqualTo (string r))
    Assert.That ({ result with RecordingMethodRef = Instrument.RecorderRefs.Build()},
                 Is.EqualTo  { state' with ModuleId = def.MainModule.Mvid.ToString()
                                                      RecordingMethod = [visit;visit;visit]
                                                      RecordingMethodRef = Instrument.RecorderRefs.Build()})

  [<Test>]
  member self.AfterModuleShouldNotChangeState () =
    let input = Instrument.Context.Build []
    let output = Instrument.InstrumentationVisitor input AfterModule
    Assert.That (output, Is.SameAs input)

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
    let version = Assembly.GetExecutingAssembly().GetName().Version.ToString()
    let transform (s:string) =
        s.Replace("\r\n","\n"
        ).Replace("AltCover.Recorder.g/1.4.0.0", "AltCover.Recorder.g/" + version
        ).Replace("AltCover.Recorder.g\": \"1.4.0.0", "AltCover.Recorder.g\": \"" + version)
    Assert.That (transform result,
                 Is.EqualTo(transform expected))

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

    let version = Assembly.GetExecutingAssembly().GetName().Version.ToString()
    let transform (s:string) =
        s.Replace("\r\n","\n"
        ).Replace("AltCover.Recorder.g/2.0.0.0", "AltCover.Recorder.g/" + version
        ).Replace("AltCover.Recorder.g\": \"2.0.0.0", "AltCover.Recorder.g\": \"" + version)
    Assert.That (transform result,
                 Is.EqualTo(transform expected))

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

  [<Test>]
  member self.NonFinishShouldDisposeRecordingAssembly () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let prepared = AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols prepared
    let state = { Instrument.Context.Build [] with RecordingAssembly = prepared }

    Assert.Throws<InvalidOperationException>(fun () ->
                                               Instrument.InstrumentationVisitorWrapper
                                                (fun _ _ -> InvalidOperationException("Bang") |> raise)
                                                state AfterType
                                                |> ignore) |> ignore
    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"
    try
      Assert.Throws<ArgumentException>(fun () -> Instrument.WriteAssembly prepared outputdll  ) |> ignore
    finally
        Directory.EnumerateFiles(Path.GetDirectoryName output,
                                 (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f -> try File.Delete f
                              with // occasionally the dll file is locked by another process
                              | :? System.UnauthorizedAccessException
                              | :? IOException -> ())

  [<Test>]
  member self.NonFinishShouldNotDisposeNullRecordingAssembly () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let state = { Instrument.Context.Build [] with RecordingAssembly = null }

    // Would be NullreferenceException if we tried it
    Assert.Throws<InvalidOperationException>(fun () ->
                                               Instrument.InstrumentationVisitorWrapper
                                                (fun _ _ -> InvalidOperationException("Bang") |> raise)
                                                state AfterType
                                                |> ignore) |> ignore

  [<Test>]
  member self.FinishShouldLeaveRecordingAssembly () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let state = { Instrument.Context.Build [] with RecordingAssembly = null }
    let prepared = AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols prepared

    Assert.Throws<InvalidOperationException>(fun () ->
                                               Instrument.InstrumentationVisitorWrapper
                                                (fun _ _ -> InvalidOperationException("Bang") |> raise)
                                                state Finish
                                                |> ignore) |> ignore
    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"
    try
      Instrument.WriteAssembly prepared outputdll
    finally
        Directory.EnumerateFiles(Path.GetDirectoryName output,
                                 (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f -> try File.Delete f
                              with // occasionally the dll file is locked by another process
                              | :? System.UnauthorizedAccessException
                              | :? IOException -> ())

  // CommandLine.fs
  [<Test>]
  member self.OutputCanBeExercised () =
    Output.Info <- ignore
    Output.Error <- ignore
    Output.Echo <- ignore
    Output.Usage <- ignore
    Assert.That(Output.Usage, Is.Not.Null)
    typeof<Tracer>.Assembly.GetExportedTypes()
    |> Seq.filter (fun t -> (string t = "AltCover.Output") || (string t = "AltCover.AltCover"))
    |> Seq.collect (fun t -> t.GetNestedTypes(BindingFlags.NonPublic))
    |> Seq.filter (fun t -> let tokens = [
                                            "Info"
                                            "Echo"
                                            "Error"
                                            "Usage"
                                            "ToConsole"
                                          ]
                            let name = t.Name
                            tokens |> List.exists (fun n -> name.StartsWith n))
    |> Seq.iter (fun t ->
                          let p = t.GetType().GetProperty("DeclaredConstructors")
                          let c = p.GetValue(t, null) :?> ConstructorInfo[]
                          let o = (c |> Seq.head).Invoke(null)
                          let invoke = t.GetMethod("Invoke")
                          let param = invoke.GetParameters() |> Seq.head

                          let arg : obj = if param.ParameterType = typeof<String> then
                                            String.Empty :> obj
                                          else (String.Empty, OptionSet() :> obj, OptionSet() :> obj) :> obj

                          invoke.Invoke(o, [| arg |]) |> ignore)

  [<Test>]
  member self.NoThrowNoErrorLeavesAllOK () =
    try
      CommandLine.error <- []
      CommandLine.exceptions <- []
      CommandLine.doPathOperation ignore () true
      Assert.That(CommandLine.error, Is.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []

  [<Test>]
  member self.NoThrowWithErrorIsSignalled () =
    try
      CommandLine.error <- []
      CommandLine.exceptions <- []
      CommandLine.doPathOperation (fun () -> CommandLine.error <- ["NoThrowWithErrorIsSignalled"]) () true
      Assert.That(CommandLine.error, Is.Not.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []

  [<Test>]
  member self.ArgumentExceptionWrites () =
    let saved = (Output.Info, Output.Error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()
    try
      Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
      Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
      let unique = "ArgumentException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []
      CommandLine.doPathOperation (fun () -> let inner = InvalidOperationException()
                                             ArgumentException(unique, inner) |> raise) () true
      Assert.That(CommandLine.error, Is.EquivalentTo [unique])
      Assert.That(CommandLine.exceptions |> List.map (fun e -> e.Message), Is.EquivalentTo [unique])
      let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let there = Path.Combine(here, Guid.NewGuid().ToString())
      let toInfo = Directory.CreateDirectory there
      Visitor.outputDirectory <- Some toInfo.FullName

      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)

      CommandLine.logExceptionsToFile "ArgumentExceptionWrites"
      let target = Path.Combine(toInfo.FullName, "ArgumentExceptionWrites")
      Assert.That (File.Exists target, target)
      let lines = target |> File.ReadAllLines |> Seq.toList
      Assert.That (lines.[0], Is.EqualTo ("System.ArgumentException: " + unique +
                                            " ---> System.InvalidOperationException: Operation is not valid due to the current state of the object." ))
      Assert.That (lines.[1], Does.StartWith("   --- End of inner exception stack trace ---"))
      Assert.That (lines.[2].Replace("+",".").Trim(), Does.StartWith("at <StartupCode$AltCover-Tests>.$Tests.ArgumentExceptionWrites"))
      Assert.That (lines.[3].Trim(), Does.StartWith("at AltCover.CommandLine.doPathOperation"))
      Assert.That (lines |> List.skip 4, Is.Not.Empty)
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString().Trim(), Is.EqualTo ("Details written to " + target + "|"))
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.Info <- (fst saved)
      Output.Error <- (snd saved)
      Visitor.outputDirectory <- None

  [<Test>]
  member self.IOExceptionWrites () =
    let saved = (Output.Info, Output.Error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()
    try
      Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
      Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
      let unique = "IOException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []
      CommandLine.doPathOperation (fun () -> IOException(unique) |> raise) () false
      Assert.That(CommandLine.error, Is.EquivalentTo [unique])

      CommandLine.ReportErrors "Instrumentation"
      Assert.That(info.ToString(), Is.Empty)

      let logged = err.ToString().Replace("\r", String.Empty).Replace("\n","|")
      Assert.That(logged, Is.EqualTo ("|ERROR *** Instrumentation phase failed|||" + unique + "|"))
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.Info <- (fst saved)
      Output.Error <- (snd saved)

  [<Test>]
  member self.NotSupportedExceptionWrites () =
    let saved = (Output.Info, Output.Error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()
    try
      Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
      Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
      let unique = "NotSupportedException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.doPathOperation (fun () -> NotSupportedException(unique) |> raise) () false
      Assert.That(CommandLine.error, Is.EquivalentTo [unique])
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      Output.Info <- (fst saved)
      Output.Error <- (snd saved)

  [<Test>]
  member self.SecurityExceptionWrites () =
    let saved = (Output.Info, Output.Error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()
    try
      Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
      Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
      let unique = "SecurityException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []
      CommandLine.doPathOperation (fun () -> System.Security.SecurityException(unique) |> raise) () false
      Assert.That(CommandLine.error, Is.EquivalentTo [unique])
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.Info <- (fst saved)
      Output.Error <- (snd saved)

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
    let e0 = Console.Out.Encoding
    let e1 = Console.Error.Encoding
    AltCover.ToConsole()
    try
      use stdout = { new StringWriter() with override self.Encoding with get() = e0 }
      use stderr = { new StringWriter() with override self.Encoding with get() = e1 }
      Console.SetOut stdout
      Console.SetError stderr

      let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"
      let exe, args = if nonWindows then ("mono", "\"" + program + "\"") else (program, String.Empty)

      let r = CommandLine.Launch exe args (Path.GetDirectoryName (Assembly.GetExecutingAssembly().Location))

      Assert.That(r, Is.EqualTo 0)
      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()
      let quote = if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" then "\"" else String.Empty
      let expected = "Command line : '" + quote + exe + quote + " " + args + "\'" + Environment.NewLine +
                     "Where is my rocket pack? " + Environment.NewLine

      // hack for Mono
      //let computed = (if result.Length = 14 then
      //                 result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
      //               else result).Split('\n') |> Seq.last

      //if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
      Assert.That(result, Is.EqualTo(expected))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ShouldHaveExpectedOptions() =
    let options = Main.DeclareOptions ()
    Assert.That (options.Count, Is.EqualTo
#if NETCOREAPP2_0
                                            16
#else
                                            18
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
      Visitor.reportPath <- None

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
      Visitor.reportPath <- None

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
      Visitor.reportPath <- None

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

  [<Test>]
  member self.ParsingSymbolGivesSymbol() =
    try
      ProgramDatabase.SymbolFolders.Clear()
      let options = Main.DeclareOptions ()
      let unique = Path.GetFullPath(".")
      let Symbol = [| "-y"; unique |]
      let parse = CommandLine.ParseCommandLine Symbol options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match ProgramDatabase.SymbolFolders.Count with
      | 1 -> Assert.That(ProgramDatabase.SymbolFolders, Is.EquivalentTo [unique])
      | _ -> Assert.Fail()
    finally
      ProgramDatabase.SymbolFolders.Clear()

  [<Test>]
  member self.ParsingMultipleSymbolGivesOK() =
    try
      ProgramDatabase.SymbolFolders.Clear()
      let options = Main.DeclareOptions ()
      let Symbol = [| "-y"; Path.GetFullPath("."); "/y"; Path.GetFullPath("..") |]
      let parse = CommandLine.ParseCommandLine Symbol options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match ProgramDatabase.SymbolFolders.Count with
      | 2 -> Assert.That(ProgramDatabase.SymbolFolders,
                            Is.EquivalentTo (Symbol |> Seq.filter (fun x -> x.Length > 2)))
      | _ -> Assert.Fail()
    finally
      ProgramDatabase.SymbolFolders.Clear()

  [<Test>]
  member self.ParsingBadSymbolGivesFailure() =
    try
      ProgramDatabase.SymbolFolders.Clear()
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let Symbol = [| "-y"; unique |]
      let parse = CommandLine.ParseCommandLine Symbol options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      ProgramDatabase.SymbolFolders.Clear()

  [<Test>]
  member self.ParsingNoSymbolGivesFailure() =
    try
      ProgramDatabase.SymbolFolders.Clear()
      let options = Main.DeclareOptions ()
      let Symbol = [| "-y" |]
      let parse = CommandLine.ParseCommandLine Symbol options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      ProgramDatabase.SymbolFolders.Clear()

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
  member self.ParsingTimeGivesTime() =
    try
      Visitor.TrackingNames.Clear()
      Visitor.interval <- None
      let options = Main.DeclareOptions ()
      let input = [| "-c"; "5" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.Interval(), Is.EqualTo 100)
    finally
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime() =
    try
      Visitor.TrackingNames.Clear()
      Visitor.interval <- None
      let options = Main.DeclareOptions ()
      let input = [| "-c"; "٣" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
      Assert.That (Visitor.Interval(), Is.EqualTo 0)
    finally
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ParsingMultipleTimesGivesFailure() =
    try
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()
      let options = Main.DeclareOptions ()
      let path = self.IsolateRootPath()

      let input = [| "-c"; "3" ; "/c"; "5" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
      Assert.That (Visitor.Interval(), Is.EqualTo 10000)
    finally
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ParsingTimeAndNamesGivesOK() =
    try
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()
      let options = Main.DeclareOptions ()
      let path = self.IsolateRootPath()

      let input = [| "-c"; "3" ; "/c"; "x"; "--callContext"; "Hello, World!" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)
      Assert.That (Visitor.Interval(), Is.EqualTo 10000)
      Assert.That (Visitor.TrackingNames, Is.EquivalentTo ["x"; "Hello, World!"])
    finally
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ParsingBadTimeGivesNoOp() =
    try
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()
      let options = Main.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-c"; "9" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)
      Assert.That (Visitor.Interval(), Is.EqualTo 0)
    finally
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ParsingNonTimeGivesFailure() = //TODO
    try
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()
      let options = Main.DeclareOptions ()
      let unique = Assembly.GetExecutingAssembly().Location
      let input = [| "-c"; "99" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ParsingNoTimeGivesFailure() =
    try
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()
      let options = Main.DeclareOptions ()
      let input = [| "-c"; " " |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.interval <- None
      Visitor.TrackingNames.Clear()

  [<Test>]
  member self.ParsingOpenCoverGivesOpenCover() =
    try
      Visitor.reportFormat <- None
      let options = Main.DeclareOptions ()
      let input = [| "--opencover" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Visitor.reportFormat with
      | None -> Assert.Fail()
      | Some x -> Assert.That(x, Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
    finally
      Visitor.reportFormat <- None

  [<Test>]
  member self.ParsingMultipleOpenCoverGivesFailure() =
    try
      Visitor.reportFormat <- None
      let options = Main.DeclareOptions ()
      let input = [| "--opencover"; "--opencover" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.reportFormat <- None

  [<Test>]
  member self.ParsingInPlaceGivesInPlace() =
    try
      Visitor.inplace <- false
      let options = Main.DeclareOptions ()
      let input = [| "--inplace" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That (Visitor.inplace, Is.True)
    finally
      Visitor.inplace <- false

  [<Test>]
  member self.ParsingMultipleInPlaceGivesFailure() =
    try
      Visitor.inplace <- false
      let options = Main.DeclareOptions ()
      let input = [| "--inplace"; "--inplace" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.inplace <- false

  [<Test>]
  member self.ParsingSaveGivesSave() =
    try
      Visitor.collect <- false
      let options = Main.DeclareOptions ()
      let input = [| "--save" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      Assert.That(Visitor.collect, Is.True)
    finally
      Visitor.collect <- false

  [<Test>]
  member self.ParsingMultipleSaveGivesFailure() =
    try
      Visitor.collect <- false
      let options = Main.DeclareOptions ()
      let input = [| "--save"; "--save" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Visitor.collect <- false

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
                      Assert.That (stderr.ToString(), Is.Empty)
                      Assert.That (CommandLine.error,
                                   Is.EquivalentTo ["From and to directories are identical"])
                      Assert.That (stdout.ToString(), Is.Empty)
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.OutputToNewPlaceIsOK() =
    let options = Main.DeclareOptions ()
    let saved = (Console.Out, Console.Error)
    AltCover.ToConsole()
    CommandLine.error <- []
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
      | Right (x,y,z,t) -> Assert.That (x, Is.SameAs rest)
                           Assert.That (y.FullName, Is.EqualTo here)
                           Assert.That (z.FullName, Is.EqualTo (Path.GetDirectoryName here))
                           Assert.That (t.FullName, Is.EqualTo y.FullName)
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
    AltCover.ToConsole()
    let saved = (Console.Out, Console.Error)
    CommandLine.error <- []
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
      | Right (x,y,z,t) -> Assert.That (x, Is.SameAs rest)
                           Assert.That (y.FullName, Is.EqualTo here)
                           Assert.That (z.FullName, Is.EqualTo there)
                           Assert.That (t.FullName, Is.EqualTo here)
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
  member self.InPlaceToExistingPlaceFails() =
    let options = Main.DeclareOptions ()
    let saved = (Console.Out, Console.Error)
    CommandLine.error <- []
    Visitor.inplace <- true
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
      | Right _ -> Assert.Fail()
      | Left _ -> Assert.That (stdout.ToString(), Is.Empty)
                  Assert.That (stderr.ToString(), Is.Empty)
                  Assert.That (CommandLine.error,
                               Is.EquivalentTo ["Output directory for saved files " +
                                                Visitor.OutputDirectory() +
                                                " already exists"])
    finally
      Visitor.inplace <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.InPlaceOperationIsAsExpected() =
    let options = Main.DeclareOptions ()
    let saved = (Console.Out, Console.Error)
    CommandLine.error <- []
    Visitor.inplace <- true
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
      | Right (x,y,z,t) -> Assert.That (x, Is.SameAs rest)
                           Assert.That (y.FullName, Is.EqualTo here)
                           Assert.That (z.FullName, Is.EqualTo there)
                           Assert.That (t.FullName, Is.EqualTo there)
                           Assert.That (stdout.ToString().Replace("\r",String.Empty),
                                        Is.EqualTo ("Creating folder " + there +
                                                    "\nSaving files to " + there +
                                                    "\nInstrumenting files in " +
                                                    here + "\n"))
                           Assert.That (stderr.ToString(), Is.Empty)
                           Assert.That (Directory.Exists there)
                           Assert.That (Visitor.SourceDirectory(), Is.EqualTo there)
                           Assert.That (Visitor.InstrumentDirectory(), Is.EqualTo here)
    finally
      Visitor.inplace <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ImageLoadResilientPassesThrough() =
    let one = ref false
    let two = ref false
    Main.ImageLoadResilient (fun () -> one := true) (fun () -> two := true)
    Assert.That(!one)
    Assert.That(!two, Is.False)

  [<Test>]
  member self.ResilientHandlesIOException () =
    let one = ref false
    let two = ref false
    Main.ImageLoadResilient (fun () ->
        IOException("fail") |> raise
        one := true) (fun () -> two := true)
    Assert.That(!one, Is.False)
    Assert.That(!two)

  [<Test>]
  member self.ResilientHandlesBadImageFormatException () =
    let one = ref false
    let two = ref false
    Main.ImageLoadResilient (fun () ->
        BadImageFormatException("fail") |> raise
        one := true) (fun () -> two := true)
    Assert.That(!one, Is.False)
    Assert.That(!two)

  [<Test>]
  member self.ResilientHandlesArgumentException () =
    let one = ref false
    let two = ref false
    Main.ImageLoadResilient (fun () ->
        ArgumentException("fail") |> raise
        one := true) (fun () -> two := true)
    Assert.That(!one, Is.False)
    Assert.That(!two)

  [<Test>]
  member self.PreparingNewPlaceShouldCopyEverything() =
    let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let there = Path.Combine(here, Guid.NewGuid().ToString())
    let toInfo = Directory.CreateDirectory there
    let fromInfo = DirectoryInfo(here)
    let (x,y) = Main.PrepareTargetFiles fromInfo toInfo fromInfo
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
    let e0 = Console.Out.Encoding
    let e1 = Console.Error.Encoding
    AltCover.ToConsole()
    try
      use stdout = { new StringWriter() with override self.Encoding with get() = e0 }
      use stderr = { new StringWriter() with override self.Encoding with get() = e1 }
      Console.SetOut stdout
      Console.SetError stderr

      let u1 = Guid.NewGuid().ToString()
      let u2 = Guid.NewGuid().ToString()

      let baseArgs= [program; u1; u2]
      let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"
      let args = if nonWindows then "mono" :: baseArgs else baseArgs

      let r = CommandLine.ProcessTrailingArguments args
                                     (DirectoryInfo(where))
      Assert.That(r, Is.EqualTo 0)

      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()
      let quote = if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" then "\"" else String.Empty
      let expected = "Command line : '" + quote + args.Head + quote + " " + String.Join(" ", args.Tail) +
                     "'" + Environment.NewLine + "Where is my rocket pack? " +
                     u1 + "*" + u2 + Environment.NewLine
      // hack for Mono
      //let computed = if result.Length = 50 then
      //                 result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
      //               else result
      //if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
      Assert.That(result, Is.EqualTo expected)
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.UsageIsAsExpected() =
    let options = Main.DeclareOptions ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let empty = OptionSet()
      CommandLine.Usage ("UsageError", options, empty)
      let result = stderr.ToString().Replace("\r\n", "\n")
      let expected = """Error - usage is:
  -i, --inputDirectory=VALUE Optional: The folder containing assemblies to
                               instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional: The folder to receive the instrumented
                               assemblies and their companions (default: sub-
                               folder '__Instrumented' of the current directory;
                                or '__Saved' if 'inplace' is set)
  -y, --symbolDirectory=VALUE
                             Optional, multiple: Additional directory to search
                               for matching symbols for the assemblies in the
                               input directory
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
  -f, --fileFilter=VALUE     Optional, multiple: source file name to exclude
                               from instrumentation
  -s, --assemblyFilter=VALUE Optional, multiple: assembly name to exclude from
                               instrumentation
  -e, --assemblyExcludeFilter=VALUE
                             Optional, multiple: assembly which links other
                               instrumented assemblies but for which internal
                               details may be excluded
  -t, --typeFilter=VALUE     Optional, multiple: type name to exclude from
                               instrumentation
  -m, --methodFilter=VALUE   Optional, multiple: method name to exclude from
                               instrumentation
  -a, --attributeFilter=VALUE
                             Optional, multiple: attribute name to exclude from
                               instrumentation
  -c, --callContext=VALUE    Optional, multiple: Tracking either times of
                               visits in ticks or designated method calls
                               leading to the visits.
                                   A single digit 0-7 gives the number of
                               decimal places of seconds to report; everything
                               else is at the mercy of the system clock
                               information available through DateTime.UtcNow
                                   A string in brackets "[]" is interpreted as
                               an attribute type name (the trailing "Attribute"
                               is optional), so [Test] or [TestAttribute] will
                               match; if the name contains one or more ".",
                               then it will be matched against the full name of
                               the attribute type.
                                   Other strings are interpreted as method
                               names (fully qualified if the string contains
                               any "." characters).
      --opencover            Optional: Generate the report in OpenCover format
      --inplace              Optional: Instrument the inputDirectory, rather
                               than the outputDirectory (e.g. for dotnet test)
      --save                 Optional: Write raw coverage data to file for
                               later processing
  -?, --help, -h             Prints out the options.
"""

      Assert.That (result.Replace("\r\n", "\n"), Is.EqualTo (expected.Replace("\r\n", "\n")))

    finally Console.SetError saved

  [<Test>]
  member self.ErrorResponseIsAsExpected() =
    let saved = Console.Error
    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let unique = Guid.NewGuid().ToString()
      let main = typeof<Node>.Assembly.GetType("AltCover.AltCover").GetMethod("Main", BindingFlags.NonPublic ||| BindingFlags.Static)
      let returnCode = main.Invoke(null, [| [| "-i"; unique |] |])
      Assert.That(returnCode, Is.EqualTo 255)
      let result = stderr.ToString().Replace("\r\n", "\n")
      let expected = "\"-i\" \"" + unique + "\"\n" +
                     "--inputDirectory : Directory " + unique + " not found\n" +
                       """Error - usage is:
  -i, --inputDirectory=VALUE Optional: The folder containing assemblies to
                               instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional: The folder to receive the instrumented
                               assemblies and their companions (default: sub-
                               folder '__Instrumented' of the current directory;
                                or '__Saved' if 'inplace' is set)
  -y, --symbolDirectory=VALUE
                             Optional, multiple: Additional directory to search
                               for matching symbols for the assemblies in the
                               input directory
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
  -f, --fileFilter=VALUE     Optional, multiple: source file name to exclude
                               from instrumentation
  -s, --assemblyFilter=VALUE Optional, multiple: assembly name to exclude from
                               instrumentation
  -e, --assemblyExcludeFilter=VALUE
                             Optional, multiple: assembly which links other
                               instrumented assemblies but for which internal
                               details may be excluded
  -t, --typeFilter=VALUE     Optional, multiple: type name to exclude from
                               instrumentation
  -m, --methodFilter=VALUE   Optional, multiple: method name to exclude from
                               instrumentation
  -a, --attributeFilter=VALUE
                             Optional, multiple: attribute name to exclude from
                               instrumentation
  -c, --callContext=VALUE    Optional, multiple: Tracking either times of
                               visits in ticks or designated method calls
                               leading to the visits.
                                   A single digit 0-7 gives the number of
                               decimal places of seconds to report; everything
                               else is at the mercy of the system clock
                               information available through DateTime.UtcNow
                                   A string in brackets "[]" is interpreted as
                               an attribute type name (the trailing "Attribute"
                               is optional), so [Test] or [TestAttribute] will
                               match; if the name contains one or more ".",
                               then it will be matched against the full name of
                               the attribute type.
                                   Other strings are interpreted as method
                               names (fully qualified if the string contains
                               any "." characters).
      --opencover            Optional: Generate the report in OpenCover format
      --inplace              Optional: Instrument the inputDirectory, rather
                               than the outputDirectory (e.g. for dotnet test)
      --save                 Optional: Write raw coverage data to file for
                               later processing
  -?, --help, -h             Prints out the options.
or
  Runner
  -r, --recorderDirectory=VALUE
                             The folder containing the instrumented code to
                               monitor (including the AltCover.Recorder.g.dll
                               generated by previous a use of the .net core
                               AltCover).
  -w, --workingDirectory=VALUE
                             Optional: The working directory for the
                               application launch
  -x, --executable=VALUE     The executable to run e.g. dotnet
      --collect              Optional: Process previously saved raw coverage
                               data, rather than launching a process.
  -l, --lcovReport=VALUE     Optional: File for lcov format version of the
                               collected data
  -t, --threshold=VALUE      Optional: minimum acceptable coverage percentage (
                               integer, 0 to 100).  If the coverage result is
                               below threshold, the return code of the process
                               is (threshold - actual) rounded up to the
                               nearest integer.
  -c, --cobertura=VALUE      Optional: File for Cobertura format version of the
                               collected data
  -o, --outputFile=VALUE     Optional: write the recorded coverage to this file
                               rather than overwriting the original report file.
  -?, --help, -h             Prints out the options.
"""

      Assert.That (result.Replace("\r\n", "\n"), Is.EqualTo (expected.Replace("\r\n", "\n")))

    finally Console.SetError saved

  // Tasks.fs
  [<Test>]
  member self.EmptyInstrumentIsJustTheDefaults() =
    let subject = Prepare()
    let save = Main.EffectiveMain
    let mutable args = [| "some junk "|]
    let saved = (Output.Info, Output.Error)
    try
        Main.EffectiveMain <- (fun a -> args <- a
                                        255)
        let result = subject.Execute()
        Assert.That(result, Is.False)
        Assert.That(args, Is.EquivalentTo ["--opencover"
                                           "--inplace"
                                           "--save"])
    finally
      Main.EffectiveMain <- save
      Output.Info <- fst saved
      Output.Error <- snd saved

  [<Test>]
  member self.NonDefaultInstrumentIsOK() =
    let subject = Prepare()
    let save = Main.EffectiveMain
    let mutable args = [| "some junk "|]
    let saved = (Output.Info, Output.Error)
    try
        Main.EffectiveMain <- (fun a -> args <- a
                                        0)
        subject.OpenCover <- false
        subject.CommandLine <- "testing 1 2 3"
        subject.SymbolDirectories <- [| "a"; "b" |]
        let result = subject.Execute()
        Assert.That(result, Is.True)
        Assert.That(args, Is.EquivalentTo ["-y"; "a"
                                           "-y"; "b"
                                           "--inplace"
                                           "--save"
                                           "--"
                                           "testing 1 2 3"])

        Assert.Throws<InvalidOperationException>(fun () -> subject.Message "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.Info "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.Error "x") |> ignore

    finally
      Main.EffectiveMain <- save
      Output.Info <- fst saved
      Output.Error <- snd saved

  [<Test>]
  member self.EmptyCollectIsJustTheDefaults() =
    let subject = Collect()
    let save = Main.EffectiveMain
    let mutable args = [| "some junk "|]
    let saved = (Output.Info, Output.Error)
    try
        Main.EffectiveMain <- (fun a -> args <- a
                                        255)
        let result = subject.Execute()
        Assert.That(result, Is.False)
        Assert.That(args, Is.EquivalentTo ["Runner"
                                           "--collect"])
    finally
      Main.EffectiveMain <- save
      Output.Info <- fst saved
      Output.Error <- snd saved

  [<Test>]
  member self.CollectWithExeIsNotCollecting() =
    let subject = Collect()
    let save = Main.EffectiveMain
    let mutable args = [| "some junk "|]
    let saved = (Output.Info, Output.Error)
    try
        Main.EffectiveMain <- (fun a -> args <- a
                                        0)
        subject.Executable <- "dotnet"
        let result = subject.Execute()
        Assert.That(result, Is.True)
        Assert.That(args, Is.EquivalentTo ["Runner"
                                           "-x"
                                           "dotnet"])
        Assert.Throws<InvalidOperationException>(fun () -> subject.Message "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.Info "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.Error "x") |> ignore
    finally
      Main.EffectiveMain <- save
      Output.Info <- fst saved
      Output.Error <- snd saved

  // Recorder.fs => Shadow.Tests
end