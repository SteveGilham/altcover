namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open AltCover

#nowarn "25"

module ACFilter = // Avoid expecto name clash
  let private ff (a, b, c) = { Scope = a; Regex = b; Sense = c }

  // Filter.fs
  [<Test>]
  let NoneOfTheAboveMatchesNoType () =
    Assert.That(
      Filter.``match`` () (ff (FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff (FilterScope.Type, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoAttribute () =
    Assert.That(
      Filter.``match`` () (ff (FilterScope.Attribute, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff (FilterScope.Attribute, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoAssembly () =
    Assert.That(
      Filter.``match`` () (ff (FilterScope.Assembly, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff (FilterScope.Assembly, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoModule () =
    Assert.That(
      Filter.``match`` () (ff (FilterScope.Module, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff (FilterScope.Module, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoFile () =
    Assert.That(
      Filter.``match`` () (ff (FilterScope.File, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff (FilterScope.File, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoPath () =
    Assert.That(
      Filter.``match`` () (ff (FilterScope.Path, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff (FilterScope.Path, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoMethod () =
    Assert.That(
      Filter.``match`` () (ff (FilterScope.Method, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff (FilterScope.Method, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let FileDoesNotMatchNonFileClass () =
    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff (FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

  [<Test>]
  let FileDoesMatchFileClass () =
    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff (FilterScope.File, Regex "Cove", Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff (FilterScope.File, Regex "Cove", Include)),
      Is.False
    )

  [<Test>]
  let PathDoesNotMatchNonPathClass () =
    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff (FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

  [<Test>]
  let PathDoesMatchPathClass () =
    let x =
      String [| '\\'; Path.DirectorySeparatorChar |]

    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff (FilterScope.Path, Regex(x + "_Binaries" + x), Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff (FilterScope.Path, Regex(x + "_Binaries" + x), Include)),
      Is.False
    )

  [<Test>]
  let AssemblyDoesNotMatchNonAssemblyClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def (ff (FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` def (ff (FilterScope.Type, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let AssemblyDoesMatchAssemblyClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def (ff (FilterScope.Assembly, Regex "Cove", Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match`` def (ff (FilterScope.Assembly, Regex "Cove", Include)),
      Is.False
    )

  [<Test>]
  let ModuleDoesNotMatchNonModuleClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def.MainModule (ff (FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

  [<Test>]
  let ModuleDoesMatchModuleClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def.MainModule (ff (FilterScope.Module, Regex "Cove", Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match`` def.MainModule (ff (FilterScope.Module, Regex "Cove", Include)),
      Is.False
    )

  [<Test>]
  let TypeDoesNotMatchNonTypeClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.iter (fun t ->
      Assert.That(
        Filter.``match`` t (ff (FilterScope.File, Regex "23", Exclude)),
        Is.False,
        t.FullName
      ))

  [<Test>]
  let TypeDoesMatchTypeClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.IsPublic && t.Name.Contains("ACFilter")) // exclude the many compiler generted chaff classes
    |> Seq.iter (fun t ->
      Assert.That(
        Filter.``match`` t (ff (FilterScope.Type, Regex "Filt", Exclude)),
        Is.True,
        t.FullName
      )

      Assert.That(
        Filter.``match`` t (ff (FilterScope.Type, Regex "Filt", Include)),
        Is.False,
        t.FullName
      ))

  [<Test>]
  let MethodDoesNotMatchNonMethodClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.filter _.IsPublic
    |> Seq.collect _.Methods
    |> Seq.iter (fun m ->
      Assert.That(
        Filter.``match`` m (ff (FilterScope.Type, Regex "23", Exclude)),
        Is.False
      ))

  [<Test>]
  let MethodDoesMatchMethodClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      def.MainModule.Types
      |> Seq.filter _.IsPublic // exclude the many compiler generated chaff classes
      |> Seq.collect _.Methods
      |> Seq.filter (fun m -> m.IsPublic && (not m.IsConstructor))
      |> Seq.filter (fun m ->
        Filter.``match``
          m
          (ff (FilterScope.Method, Regex "MethodDoesMatchMethodClass", Exclude)))
      |> Seq.length,
      Is.EqualTo(1)
    )

    Assert.That(
      def.MainModule.Types
      |> Seq.filter _.IsPublic // exclude the many compiler generated chaff classes
      |> Seq.collect _.Methods
      |> Seq.filter (fun m -> m.IsPublic && (not m.IsConstructor))
      |> Seq.filter (fun m ->
        Filter.``match``
          m
          (ff (FilterScope.Method, Regex "MethodDoesMatchMethodClass", Include))
        |> not)
      |> Seq.length,
      Is.EqualTo(1)
    )

  [<Test>]
  let AttributeDoesNotMatchNonAttributeClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.iter (fun t ->
      Assert.That(
        Filter.``match`` t.CustomAttributes (ff (FilterScope.File, Regex "23", Exclude)),
        Is.False,
        t.FullName
      ))

  [<Test>]
  let AttributeDoesMatchAttributeClass () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.filter (fun t ->
      t.IsPublic
      && t.Name.Contains("ProxyObject")
      && (not (t.FullName.Contains("Coverlet.Core.Instrumentation")))) // exclude the many compiler generted chaff classes
    |> Seq.iter (fun t ->
      Assert.That(
        Filter.``match`` t (ff (FilterScope.Attribute, Regex "Exclu", Exclude)),
        Is.True,
        t.FullName
      )

      Assert.That(
        Filter.``match`` t (ff (FilterScope.Attribute, Regex "Exclu", Include)),
        Is.False,
        t.FullName
      ))

  [<Test>]
  let CanExcludeCSharpPropertiesByAttribute () =
    let location =
      typeof<Sample11.Class1>.Assembly.Location

    let sourceAssembly =
      AssemblyResolver.ReadAssembly(location)

    let direct =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x -> x.Name = "Class1")
      |> Seq.head

    let filter =
      "ExcludeFromCodeCoverage"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Attribute)

    let pass =
      direct.Methods
      |> Seq.filter (fun x -> Filter.``match`` x filter |> not)
      |> Seq.map _.Name
      |> Seq.sort
      |> Seq.toList

    let expected = [ ".ctor" ]
    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)

  [<Test>]
  let RefStructsAreNotObsolete () =
    let location =
      typeof<Sample11.Class1>.Assembly.Location

    let sourceAssembly =
      AssemblyResolver.ReadAssembly(location)

    let filter =
      "Obsolete"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Attribute)

    let pass =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x ->
        x.Namespace = "Sample11"
        && Filter.``match`` x filter |> not)
      |> Seq.map _.Name
      |> Seq.sort
      |> Seq.toList

    let expected = [ "Class1"; "Token" ]
    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)

  [<Test>]
  let Sample3Class1IsCSharpAutoproperty () =
    let sample3 =
      Path.Combine(dir, "Sample3.dll")

    use def =
      AssemblyResolver.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class1")
    |> Seq.collect _.Methods
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (Filter.isCSharpAutoProperty >> Assert.That)

  [<Test>]
  let Sample3Class2IsNotCSharpAutoproperty () =
    let sample3 =
      Path.Combine(dir, "Sample3.dll")

    use def =
      AssemblyResolver.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class2")
    |> Seq.collect _.Methods
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (fun m -> Assert.That(Filter.isCSharpAutoProperty m, Is.False))

  [<Test>]
  let CanIdentifyExcludedFSharpMethods () =
    let sourceAssembly =
      AssemblyResolver.ReadAssembly(sample2path)

    let direct =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x -> x.Namespace = "N")
      |> Seq.toList

    let indirect =
      direct
      |> Seq.filter _.HasNestedTypes
      |> Seq.collect _.NestedTypes
      |> Seq.toList // MyUnion, MyThing

    let indirect2 =
      indirect
      |> Seq.filter _.HasNestedTypes
      |> Seq.collect _.NestedTypes
      |> Seq.toList // Foo, Bar, ...

    let indirect3 =
      indirect2
      |> Seq.filter _.HasNestedTypes
      // |> Seq.collect _.NestedTypes
      // |> Seq.map _.FullName
      |> Seq.toList

    Assert.That(indirect3 |> Seq.isEmpty, sprintf "Third order types found %A" indirect3)

    let pass =
      Seq.concat [ direct; indirect; indirect2 ]
      |> Seq.collect _.Methods
      |> Seq.filter (not << Filter.isFSharpInternal)
      |> Seq.map _.Name
      |> Seq.sort
      |> Seq.toList

    let flaky =
      Seq.concat [ direct; indirect; indirect2 ]
      |> Seq.collect _.Methods
      |> Seq.filter _.Name.Equals(".ctor")
      |> Seq.filter _.DeclaringType.Name.Equals("get_MyBar@44")
      |> Seq.head

    let skips =
      [ ("System.Void N.DU/get_MyBar@44::.ctor(N.DU/MyUnion)", 0)
        ("System.Void N.DU/MyUnion/get_MyBar@44::.ctor(N.DU/MyUnion)", 1) ] // filtered as algebraic
      |> Map.ofSeq

    let skip = skips |> Map.find flaky.FullName

    let expected =
      [ ".ctor" // "System.Void N.DU/MyClass::.ctor()";
        ".ctor" // "System.Void N.DU/get_MyBar@44::.ctor(N.DU/MyUnion)"; <= flaky
        "Invoke" //  "N.DU/MyUnion N.DU/get_MyBar@44::Invoke(Microsoft.FSharp.Core.Unit)";
        "as_bar"
        "bytes"
        "get_MyBar"
#if !NET472
        "main"
#endif
        "makeThing"
        "returnBar"
        "returnFoo"
        "testMakeThing"
        "testMakeUnion" ]

    Assert.That(
      pass,
      Is.EquivalentTo(expected |> List.skip skip),
      sprintf "Got sequence %A" pass
    )

  [<Test>]
  let CanIdentifyExcludedCSharpAutoProperties () =
    let location =
      typeof<Sample3.Class1>.Assembly.Location

    let sourceAssembly =
      AssemblyResolver.ReadAssembly(location)

    let direct =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x -> x.Name = "Class1")
      |> Seq.head

    let pass =
      direct.Methods
      |> Seq.filter (not << Filter.isCSharpAutoProperty)
      |> Seq.map _.Name
      |> Seq.sort
      |> Seq.toList

    let expected = [ ".ctor" ]
    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)

  [<Test>]
  let CanIdentifyIncludedCSharpProperties () =
    let location =
      typeof<Sample3.Class1>.Assembly.Location

    let sourceAssembly =
      AssemblyResolver.ReadAssembly(location)

    let direct =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x -> x.Name = "Class2")
      |> Seq.head

    let pass =
      direct.Methods
      |> Seq.filter (not << Filter.isCSharpAutoProperty)
      |> Seq.map _.Name
      |> Seq.sort
      |> Seq.toList

    let expected =
      [ ".ctor"
        "get_Property"
        "set_Property" ]

    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)