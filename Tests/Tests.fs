namespace Tests

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover
open AltCover.Augment
open AltCover.Filter
open Mono.Cecil
open Mono.Cecil.Rocks
open N
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() = class
  // Hack for running while instrumented
  static member private Hack () =
    let where = Assembly.GetExecutingAssembly().Location;
    let dir = where |> Path.GetDirectoryName |> Path.GetFileName
    match dir.IndexOf "__" with
    | 0 -> "\\.."
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
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + AltCoverTests.Hack())
                |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                                        || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                |> Seq.map (fun x -> (x, Mono.Cecil.AssemblyDefinition.ReadAssembly x))
                |> Seq.filter (fun x -> (snd x).FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.OrdinalIgnoreCase))
                |> Seq.toList
    Assert.That(files, Is.Not.Empty)
    files
    |> Seq.iter( fun x ->let pdb = AltCover.ProgramDatabase.GetPdbFromImage (snd x)
                         match pdb with
                         | None -> Assert.Fail("No .pdb for " + (fst x))
                         | Some name ->
                            let probe = Path.ChangeExtension((fst x), ".pdb")
                            let file = FileInfo(probe)
                            let filename = file.Name
                            Assert.That(name, Does.EndWith("\\" + filename), (fst x) + " -> " + name) )

  [<Test>]
  member self.ShouldGetPdbWithFallback() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + AltCoverTests.Hack())
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter( fun x ->
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly x
      let pdb = AltCover.ProgramDatabase.GetPdbWithFallback(def)
      match pdb with
      | None -> Assert.That(File.Exists(Path.ChangeExtension(x, ".pdb")), Is.Not.True, "No .pdb for " + x)
      | Some name ->
         let probe = Path.ChangeExtension(x, ".pdb")
         let file = FileInfo(probe)
         let filename = file.Name
         Assert.That(name, Does.EndWith("\\" + filename), x + " -> " + name)
    )

  [<Test>]
  member self.ShouldGetMdbWithFallback() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let files = Directory.GetFiles(where.Substring(0, where.IndexOf("_Binaries")) + "_Mono\\Sample1")
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
         let filename = file.Name
         Assert.That(name + ".mdb", Does.EndWith("\\" + filename), x + " -> " + name)
    )

  [<Test>]
  member self.ShouldGetSymbolsFromPdb() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + AltCoverTests.Hack())
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.map Mono.Cecil.AssemblyDefinition.ReadAssembly
    |> Seq.filter (fun x -> x.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter (fun def ->
      AltCover.ProgramDatabase.ReadSymbols def
      Assert.That (def.MainModule.HasSymbols, def.MainModule.FullyQualifiedName)
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
      printfn "%A" def.MainModule.FullyQualifiedName
      Assert.That (not def.MainModule.HasSymbols, def.MainModule.FullyQualifiedName)
    )

  [<Test>]
  member self.ShouldGetSymbolsFromMdb() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let files = Directory.GetFiles(where.Substring(0, where.IndexOf("_Binaries")) + "_Mono\\Sample1")
    files
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter( fun x ->
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly x
      AltCover.ProgramDatabase.ReadSymbols def
      Assert.That (def.MainModule.HasSymbols, def.MainModule.FullyQualifiedName)
    )

  // Filter.fs

  [<Test>]
  member self.NoneOfTheAboveMatchesNoType() =
     Assert.That (Match () (FilterClass.Type "23"), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoAttribute() =
     Assert.That (Match () (FilterClass.Attribute "23"), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoAssembly() =
     Assert.That (Match () (FilterClass.Assembly "23"), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoFile() =
     Assert.That (Match () (FilterClass.File "23"), Is.False)

  [<Test>]
  member self.NoneOfTheAboveMatchesNoMethod() =
     Assert.That (Match () (FilterClass.Method "23"), Is.False)

  [<Test>]
  member self.FileDoesNotMatchNonFileClass() =
     Assert.That (Match (Assembly.GetExecutingAssembly().Location) (FilterClass.Type "23"), Is.False)

  [<Test>]
  member self.FileDoesMatchFileClass() =
     Assert.That (Match (Assembly.GetExecutingAssembly().Location) (FilterClass.File "Cove"), Is.True)

  [<Test>]
  member self.AssemblyDoesNotMatchNonAssemblyClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That (Match def (FilterClass.Type "23"), Is.False)

  [<Test>]
  member self.AssemblyDoesMatchAssemblyClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That (Match def (FilterClass.Assembly "Cove"), Is.True)

  [<Test>]
  member self.TypeDoesNotMatchNonTypeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.iter (fun t -> Assert.That (Match t (FilterClass.File "23"), Is.False))

  [<Test>]
  member self.TypeDoesMatchTypeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.filter (fun t -> t.IsPublic)  // exclude the many compiler generted chaff classes
     |> Seq.iter (fun t -> Assert.That (Match t (FilterClass.Type "Cove"), Is.True))

  [<Test>]
  member self.MethodDoesNotMatchNonMethodClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.filter (fun t -> t.IsPublic) 
     |> Seq.collect (fun t -> t.Methods)
     |> Seq.iter (fun m -> Assert.That (Match m (FilterClass.Type "23"), Is.False))

  [<Test>]
  member self.MethodDoesMatchMethodClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     Assert.That(def.MainModule.Types
                 |> Seq.filter (fun t -> t.IsPublic)  // exclude the many compiler generted chaff classes
                 |> Seq.collect (fun t -> t.Methods)
                 |> Seq.filter (fun m -> m.IsPublic && (not m.IsConstructor))
                 |> Seq.filter (fun m -> Match m (FilterClass.Method "Augment"))
                 |> Seq.length,
                 Is.EqualTo(2))

  [<Test>]
  member self.AttributeDoesNotMatchNonAttributeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.iter (fun t -> Assert.That (Match t.CustomAttributes (FilterClass.File "23"), Is.False))

  [<Test>]
  member self.AttributeDoesMatchAttributeClass() =
     let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
     def.MainModule.Types
     |> Seq.filter (fun t -> t.IsPublic)  // exclude the many compiler generted chaff classes
     |> Seq.iter (fun t -> Assert.That (Match t (FilterClass.Attribute "Fix"), Is.True))

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

    let expected = [  ".ctor" ; "Invoke"; "as_bar"; "bytes"; "get_MyBar" ; "makeThing"; "returnBar"; "returnFoo"; "testMakeThing"; "testMakeUnion" ]

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

  static member private ProvideKeyPair () =
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Infrastructure.snk")
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      StrongNameKeyPair(buffer.ToArray())

  [<Test>]
  member self.KeyHasExpectedToken() = 
    let token = KeyStore.TokenOfKey <| AltCoverTests.ProvideKeyPair ()
    let token' = String.Join(String.Empty, token |> List.map (fun x -> x.ToString("x2")))
    Assert.That (token', Is.EqualTo("c02b1a9f5b7cade8"))

  [<Test>]
  member self.TokenGeneratesExpectedULong() =
    let token = [|1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]
    Assert.That (KeyStore.TokenAsULong token, Is.EqualTo(1UL))

  [<Test>]
  member self.KeyHasExpectedIndex() = 
    let token = KeyStore.KeyToIndex <| AltCoverTests.ProvideKeyPair ()
    Assert.That (token, Is.EqualTo(0xe8ad7c5b9f1a2bc0UL))

  [<Test>]
  member self.EmptyArrayHasExpectedIndex() = 
    Assert.That ((KeyStore.ArrayToIndex [| |]), Is.EqualTo(0x95601890afd80709UL))

  [<Test>]
  member self.KeyHasExpectedRecord() = 
    let pair = AltCoverTests.ProvideKeyPair ()
    let token = KeyStore.KeyToRecord <| pair
    Assert.That (token, Is.EqualTo({Pair = pair; Token = BitConverter.GetBytes(0xe8ad7c5b9f1a2bc0UL) |> Array.toList}))

  [<Test>]
  member self.KeyHasExpectedPlaceInIndex() = 
    try
      Assert.That (Visitor.keys.Keys.Count, Is.EqualTo(0))
      let pair = AltCoverTests.ProvideKeyPair ()
      Visitor.Add(pair)
      Assert.That(Visitor.keys.ContainsKey(0xe8ad7c5b9f1a2bc0UL))
      Assert.That(Visitor.keys.[0xe8ad7c5b9f1a2bc0UL], Is.EqualTo({Pair = pair; Token = BitConverter.GetBytes(0xe8ad7c5b9f1a2bc0UL) |> Array.toList}))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.EmptyFiltersPassAll() = 
    Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
    Assert.That (Visitor.IsIncluded self)

  [<Test>]
  member self.NonEmptyFiltersCatchAnExpectedValue() = 
    try
      Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
      Visitor.NameFilters.AddRange([ FilterClass.File "Cove"; FilterClass.Method "Augment"])
      Assert.That (Visitor.IsIncluded (Assembly.GetExecutingAssembly().Location), Is.False)
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.NonEmptyFiltersPassAnExpectedValue() = 
    try
      Assert.That (Visitor.NameFilters.Count, Is.EqualTo(0))
      Visitor.NameFilters.AddRange([ FilterClass.File "System"; FilterClass.Method "Augment"])
      Assert.That (Visitor.IsIncluded (Assembly.GetExecutingAssembly().Location))
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.AfterProcessingYieldsAnExpectedValue() = 
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly (Assembly.GetExecutingAssembly().Location)
    let inputs = [ Node.Start [] ; Node.Assembly (def, true) ; Node.Module (null, false) ; Node.Type (null, true) ;
                   Node.Method (null, false) ; Node.MethodPoint ( null, null, 0, true ) ;
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
    let inputs = [ Node.MethodPoint ( null, null, 0, true ) ;
                   Node.AfterMethod false ; Node.AfterModule ; Node.AfterAssembly def; Node.Finish ]
    let outputs = inputs |> Seq.map (Visitor.Deeper>> Seq.toList)
    let expected = [[]; []; []; []; []]
    Assert.That (outputs, Is.EquivalentTo (expected))

  [<Test>]
  member self.MethodPointsAreDeeperThanMethods() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let method = (def.MainModule.Types |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))|> Seq.head).Methods |> Seq.head
    Visitor.Visit [] [] // cheat reset
    try
        "Program" |> (FilterClass.File >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Method (method, true)
                     |> Seq.toList
        Assert.That (deeper.Length, Is.EqualTo 10)
        deeper 
        |> List.iteri (fun i node -> match node with 
                                     | (MethodPoint (_, _, n, b)) ->Assert.That(n, Is.EqualTo i); Assert.That (b, Is.False)
                                     | _ -> Assert.Fail())
    finally
      Visitor.NameFilters.Clear()
      
  [<Test>]
  member self.MethodsAreDeeperThanTypes() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let type' = (def.MainModule.Types |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))|> Seq.head)
    Visitor.Visit [] [] // cheat reset
    try
        "Main" |> (FilterClass.Method >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Type (type', true)
                     |> Seq.toList
        Visitor.Visit [] [] // cheat reset
        let expected = type'.Methods
                    |> Seq.map (fun m -> let flag = m.Name = ".ctor"
                                         let node = Node.Method (m, flag)
                                         List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node;  [Node.AfterMethod flag]])
                    |> List.concat
        Assert.That (deeper.Length, Is.EqualTo 14)
        Assert.That (deeper, Is.EquivalentTo expected)
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.TypesAreDeeperThanModules() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let module' = def.MainModule
    Visitor.Visit [] [] // cheat reset
    try
        "Program" |> (FilterClass.Type >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Module (module', true)
                     |> Seq.toList
        Visitor.Visit [] [] // cheat reset
        let expected = module'.Types // we have no nested types in this test
                    |> Seq.map (fun t -> let flag = t.Name <> "Program"
                                         let node = Node.Type (t,flag)
                                         List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node])
                    |> List.concat
        Assert.That (deeper.Length, Is.EqualTo 16)
        Assert.That (deeper, Is.EquivalentTo expected)
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ModulesAreDeeperThanAssemblies() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    Visitor.Visit [] [] // cheat reset
    let deeper = Visitor.Deeper <| Node.Assembly (def, true)
                 |> Seq.toList
    Visitor.Visit [] [] // cheat reset
    let expected = def.Modules // we have no nested types in this test
                |> Seq.map (fun t -> let node = Node.Module (t, true)     
                                     List.concat [ [node]; (Visitor.Deeper >> Seq.toList) node; [AfterModule]])
                |> List.concat
    Assert.That (deeper.Length, Is.EqualTo 18)
    Assert.That (deeper, Is.EquivalentTo expected)

  [<Test>]
  member self.AssembliesAreDeeperThanPaths() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")

    let deeper = Visitor.Deeper <| Node.Start [path]
                    |> Seq.toList
    // assembly definitions care about being separate references in equality tests
    let def = match Seq.head deeper with
                | Node.Assembly (def', true) -> def'
                | _ -> Assert.Fail(); null

    let assembly = Node.Assembly (def, true)
    let expected = List.concat [ [assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def]]
    Assert.That (deeper.Length, Is.EqualTo 20)
    Assert.That (deeper, Is.EquivalentTo expected)

  [<Test>]
  member self.FilteredAssembliesDoNotHaveSequencePoints() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")
    try
        "Sample" |> (FilterClass.Assembly >> Visitor.NameFilters.Add)
        let deeper = Visitor.Deeper <| Node.Start [path]
                     |> Seq.toList
        // assembly definitions care about being separate references in equality tests
        let def = match Seq.head deeper with
                  | Node.Assembly (def', false) -> def'
                  | _ -> Assert.Fail(); null

        let assembly = Node.Assembly (def, false)
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
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")
    let accumulator = System.Collections.Generic.List<Node>()
    let fix = Visitor.EncloseState (fun (x:System.Collections.Generic.List<Node>) t -> x.Add t; x) accumulator
    Visitor.Visit [fix] [path]
    // assembly definitions care about being separate references in equality tests
    let def = match accumulator.[1] with
              | Node.Assembly (def', true) -> def'
              | _ -> Assert.Fail(); null

    let assembly = Node.Assembly (def, true)
    let expected = List.concat [ [Start[path]; assembly]; (Visitor.Deeper >> Seq.toList) assembly; [AfterAssembly def; Finish]]
    Assert.That (accumulator, Is.EquivalentTo expected)

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
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.map Naming.TypeName
                |> Seq.toList
    let expected = ["<Module>"; "Class1"; "Class2"; "Class3"; "Class4" ]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.FullTypeNamesAreExtracted() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.map Naming.FullTypeName
                |> Seq.toList
    let expected = ["<Module>"; "Sample3.Class1"; "Sample3.Class2"; "Sample3.Class3"; "Sample3.Class3+Class4"]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.TypeRefNamesAreExtracted() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.map (fun td -> Naming.TypeRefName(TypeReference(td.Namespace, td.Name, def.MainModule, null)))
                |> Seq.toList
    let expected = ["<Module>"; "Class1"; "Class2"; "Class3"; "Class4" ]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.FullTypeRefNamesAreExtracted() = 
    let where = Assembly.GetExecutingAssembly().Location;
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
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.map Naming.MethodName
                |> Seq.toList
    let expected = ["get_Property"; "set_Property"; ".ctor"; "get_Property"; "set_Property";
                      ".ctor"; ".ctor"; "get_Property"; "set_Property"; "get_ReportFile";
                      "set_ReportFile"; "ToList"; ".ctor" ]
    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  member self.FullMethodNamesAreExtracted() = 
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let names = def.MainModule.GetAllTypes()
                |> Seq.collect (fun t -> t.Methods)
                |> Seq.map Naming.FullMethodName
                |> Seq.toList
    let expected = ["System.Int32 Sample3.Class1.get_Property()"; "System.Void Sample3.Class1.set_Property(System.Int32)";
                    "System.Void Sample3.Class1.#ctor()"; "System.Int32 Sample3.Class2.get_Property()";
                    "System.Void Sample3.Class2.set_Property(System.Int32)"; "System.Void Sample3.Class2.#ctor()";
                    "System.Void Sample3.Class3.#ctor()"; "Sample3.Class1 Sample3.Class3+Class4.get_Property()";
                    "System.Void Sample3.Class3+Class4.set_Property(Sample3.Class1)";
                    "System.String Sample3.Class3+Class4.get_ReportFile()";
                    "System.Void Sample3.Class3+Class4.set_ReportFile(System.String)";
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
<seqpnt visitcount=\"1\" line=\"15\" column=\"17\" endline=\"15\" endcolumn=\"63\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
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
                    | "document" -> Assert.That(a1.Value, Does.EndWith(a2.Value),
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
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample1.exe")

    try
        "Main" |> (FilterClass.Method >> Visitor.NameFilters.Add)
        Visitor.Visit [ visitor ] (Visitor.ToSeq path)

        let baseline = XDocument.Load(new System.IO.StringReader(AltCoverTests.TTBaseline))
        let result = document.Elements()
        let expected = baseline.Elements()
        AltCoverTests.RecursiveValidate result expected 0 true
    finally
      Visitor.NameFilters.Clear()

  [<Test>]
  member self.ShouldGenerateExpectedXmlReportFromMono() =
    let visitor, document = Report.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "_Mono\\Sample1", "Sample1.exe")

    Visitor.Visit [ visitor ] (Visitor.ToSeq path)

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
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "AltCover.Recorder.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let recorder = AltCover.Instrument.RecordingMethod def
    Assert.That(Naming.FullMethodName recorder, Is.EqualTo "System.Void AltCover.Recorder.Instance.Visit(System.String,System.Int32)")

  [<Test>]
  member self.ShouldBeAbleToClearTheStrongNameKey () =
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    Assert.That (def.Name.HasPublicKey)
    let key0 = def.Name.PublicKey
    Assert.That (key0, Is.Not.Null)
    let token0 = def.Name.PublicKeyToken
    Assert.That (token0, Is.Not.Null)
    AltCover.Instrument.UpdateStrongNaming def.Name None
    Assert.That (def.Name.HasPublicKey, Is.False)
    let key1 = def.Name.PublicKey
    Assert.That (key1, Is.Empty)
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Empty)

  [<Test>]
  member self.ShouldBeAbleToUpdateTheStrongNameKey () =
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    Assert.That (def.Name.HasPublicKey)
    let key0 = def.Name.PublicKey
    Assert.That (key0, Is.Not.Null)
    let token0 = def.Name.PublicKeyToken
    Assert.That (token0, Is.Not.Null)

    use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream("AltCover.Recorder.snk")
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    let key = StrongNameKeyPair(buffer.ToArray())

    AltCover.Instrument.UpdateStrongNaming def.Name (Some key)
    Assert.That (def.Name.HasPublicKey)
    let key1 = def.Name.PublicKey
    Assert.That (key1, Is.Not.Null)
    Assert.That (key1, Is.Not.EquivalentTo(key0))
    let token1 = def.Name.PublicKeyToken
    Assert.That (token1, Is.Not.Null)
    Assert.That (token1, Is.Not.EquivalentTo(token0))
    let token' = String.Join(String.Empty, token1|> Seq.map (fun x -> x.ToString("x2")))
    Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))

  [<Test>]
  member self.NoKnownKeyInEmptyIndex() = 
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location;
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      Assert.That (Option.isNone(Instrument.KnownKey def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.KnownKeyMatchedInIndex() = 
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location;
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      AltCoverTests.ProvideKeyPair() |> Visitor.Add
      Assert.That (Option.isSome(Instrument.KnownKey def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.NoKnownKeyIfAssemblyHasNone() = 
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location;
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      AltCover.Instrument.UpdateStrongNaming def.Name None
      AltCoverTests.ProvideKeyPair() |> Visitor.Add
      Assert.That (Option.isNone(Instrument.KnownKey def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.NoKnownTokenInEmptyIndex() = 
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location;
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      Assert.That (Option.isNone(Instrument.KnownToken def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.KnownTokenMatchedInIndex() = 
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location;
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      AltCoverTests.ProvideKeyPair() |> Visitor.Add
      Assert.That (Option.isSome(Instrument.KnownToken def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.NoKnownTokenIfAssemblyHasNone() = 
    try
      Visitor.keys.Clear()
      let where = Assembly.GetExecutingAssembly().Location;
      let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      AltCover.Instrument.UpdateStrongNaming def.Name None
      AltCoverTests.ProvideKeyPair() |> Visitor.Add
      Assert.That (Option.isNone(Instrument.KnownToken def.Name))
    finally
      Visitor.keys.Clear()

  [<Test>]
  member self.ShouldBeAbleToPrepareTheAssembly () =
    let where = Assembly.GetExecutingAssembly().Location;
    let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
    let prepared = Instrument.PrepareAssembly path
    let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols raw
    Assert.That (prepared.Name.Name, Is.EqualTo (raw.Name.Name + ".g"))
    Assert.That (prepared.Name.HasPublicKey)
    Assert.That (prepared.Name.PublicKey, Is.Not.EquivalentTo(raw.Name.PublicKey))
    let token' = String.Join(String.Empty, prepared.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
    Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))

    let before = raw.MainModule.GetTypes() |> Seq.filter (fun t -> t.Name = "Class4") |> Seq.toList
    Assert.That (before.Length = 1)
    let before' = before.[0].Methods |> Seq.filter (fun t -> t.Name = "get_ReportFile") |> Seq.toList
    Assert.That (before'.Length = 1)

    let after = prepared.MainModule.GetTypes() |> Seq.filter (fun t -> t.Name = "Class4") |> Seq.toList
    Assert.That (after.Length = 1)
    let after' = after.[0].Methods |> Seq.filter (fun t -> t.Name = "get_ReportFile") |> Seq.toList
    Assert.That (after'.Length = 1)

    Assert.That (after'.[0].Body.Instructions.Count, Is.EqualTo(2 + before'.[0].Body.Instructions.Count))
  // AltCover.fs


end