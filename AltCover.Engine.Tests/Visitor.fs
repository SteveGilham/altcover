﻿namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq

open AltCover
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

#nowarn "25"

module Visitor =

  [<AutoOpen>]
  module Extra =
    type Hallmark with
      static member internal Build() =
        { Assembly = String.Empty
          Configuration = String.Empty }

  let recorderSnk =
    typeof<AltCover.Node>.Assembly.GetManifestResourceNames()
    |> Seq.find _.EndsWith(".Recorder.snk", StringComparison.Ordinal)

  let infrastructureSnk =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find _.EndsWith("Infrastructure.snk", StringComparison.Ordinal)

  [<Test>]
  let ReportFileShouldBeCorrectlyExtended () =
    try
      Main.init ()

      let path1 =
        Path.Combine(SolutionRoot.location, "test.xml")

      let from = Path.Combine(path1, "from")
      let toward = Path.Combine(path1, "to")

      CoverageParameters.theReportFormat <- Some ReportFormat.NCover
      CoverageParameters.theReportPath <- Some path1
      test <@ CoverageParameters.reportPath () = path1 @>

      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Add from
      CoverageParameters.theOutputDirectories.Add toward

      CoverageParameters.portable.Value <- true
      test <@ CoverageParameters.reportPath () = "./test.xml" @>

      test
        <@
          Main.I.canonicalReportPath () = (Path.Combine(toward, "test.xml")
                                           |> Canonical.canonicalPath)
        @>

      CoverageParameters.inplace.Value <- true

      test
        <@
          Main.I.canonicalReportPath () = (Path.Combine(from, "test.xml")
                                           |> Canonical.canonicalPath)
        @>

    finally
      Main.init ()

  [<Test>]
  let ReportFileShouldBeCorrectlySuffixed () =
    try
      let path1 =
        Path.Combine(SolutionRoot.location, "test.xml")

      let path2 =
        Path.Combine(SolutionRoot.location, "test")

      let path3 =
        Path.Combine(SolutionRoot.location, "test.json")

      CoverageParameters.theReportFormat <- Some ReportFormat.NCover
      CoverageParameters.theReportPath <- Some path1
      test <@ CoverageParameters.reportPath () = path1 @>
      test <@ Main.I.canonicalReportPath () = Canonical.canonicalPath (path1) @>

      CoverageParameters.theReportPath <- Some path2
      test <@ CoverageParameters.reportPath () = path2 @>
      test <@ Main.I.canonicalReportPath () = Canonical.canonicalPath (path2) @>

      CoverageParameters.theReportPath <- Some path3
      test <@ CoverageParameters.reportPath () = path1 @>
      test <@ Main.I.canonicalReportPath () = Canonical.canonicalPath (path1) @>

      CoverageParameters.theReportFormat <- Some ReportFormat.NativeJson
      CoverageParameters.theReportPath <- Some path1
      test <@ CoverageParameters.reportPath () = path3 @>
      test <@ Main.I.canonicalReportPath () = Canonical.canonicalPath (path3) @>

      CoverageParameters.theReportPath <- Some path2
      test <@ CoverageParameters.reportPath () = path2 @>
      test <@ Main.I.canonicalReportPath () = Canonical.canonicalPath (path2) @>

      CoverageParameters.theReportPath <- Some path3
      test <@ CoverageParameters.reportPath () = path3 @>
      test <@ Main.I.canonicalReportPath () = Canonical.canonicalPath (path3) @>

    finally
      CoverageParameters.theReportPath <- None
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let CanSwitchSampling () =
    let save = CoverageParameters.all

    try
      CoverageParameters.all <- false
      test <@ CoverageParameters.sampling () = 1 @>
      CoverageParameters.all <- true
      test <@ CoverageParameters.sampling () = 0 @>
    finally
      CoverageParameters.all <- save

  [<Test>]
  let ValidateStaticExemption () =
    let result =
      [ StaticFilter.AsCovered
        StaticFilter.Hidden
        StaticFilter.NoFilter ]
      |> List.map (fun k -> Visitor.I.selectExemption k List.empty<obj> Exemption.None)

    test
      <@
        result = [ Exemption.StaticAnalysis
                   Exemption.None
                   Exemption.None ]
      @>

  [<Test>]
  let ValidateStaticClass () =
    let where =
      typeof<AltCover.CommandLine.Format>.Assembly.Location

    use def =
      AssemblyResolver.ReadAssembly where

    let cl =
      def.MainModule.GetType("AltCover.CommandLine")

    test <@ cl |> Visitor.I.isFSharpStaticClass |> not @>

    let format =
      cl.NestedTypes
      |> Seq.find (fun t -> t.Name = "Format")

    test <@ format |> Visitor.I.isFSharpStaticClass @>

  [<Test>]
  let ValidateAutomaticExemption () =
    try
      CoverageParameters.showGenerated.Value <- true
      let path = sample4path

      use def = AssemblyResolver.ReadAssembly path

      let items =
        def.MainModule.GetAllTypes()
        |> Seq.collect _.Methods
        |> Seq.filter (fun m -> m.Name = "testMakeUnion")
        |> Seq.toList

      let result =
        [ StaticFilter.AsCovered
          StaticFilter.Hidden
          StaticFilter.NoFilter ]
        |> List.map (fun k -> Visitor.I.selectExemption k items Exemption.None)

      test
        <@
          result = [ Exemption.StaticAnalysis
                     Exemption.Automatic
                     Exemption.Automatic ]
        @>
    finally
      CoverageParameters.showGenerated.Value <- false

  [<Test>]
  let DetectLocalSource () =

    let toolPackages =
      let xml =
        Path.Combine(SolutionDir(), "./Directory.Packages.props")
        |> Path.GetFullPath
        |> XDocument.Load

      // beware conditionals like on NUnit.ConsoleRunner
      xml.Descendants()
      |> Seq.filter _.Attribute("Include".X).IsNotNull
      |> Seq.map (fun x ->
        (x.Attribute(XName.Get("Include")).Value.ToLowerInvariant(),
         x.Attribute(XName.Get("Version")).Value))
      |> Map.ofSeq

    CoverageParameters.local.Value <- false
    CoverageParameters.nameFilters.Clear()

    let fscore =
      Path.Combine(
        SolutionDir(),
        "packages/fsharp.core/"
        + "4.0.0.1" // (libPackages.Item "fsharp.core")
        + "/lib/net40"
      )

    let mono =
      Path.Combine(
        SolutionDir(),
        "packages/mono.options/"
        + (toolPackages.Item "mono.options")
        + "/lib/net40"
      )

    let nuget =
      Path.Combine(
        SolutionDir(),
        "packages/nuget.commandline/"
        + (toolPackages.Item "nuget.commandline")
        + "/tools"
      )

    let exe = Path.Combine(nuget, "NuGet.exe")
    Assert.That(File.Exists exe, Is.True, "NuGet.exe not found")
    let pdb = Path.Combine(nuget, "NuGet.pdb")
    Assert.That(File.Exists pdb, Is.True, "NuGet.pdb not found")

    let fdll =
      Path.Combine(fscore, "FSharp.Core.dll")

    Assert.That(File.Exists fdll, Is.True, "FSharp.Core.dll not found")
    //let pdb2 = Path.Combine(fscore, "FSharp.Core.pdb")
    //Assert.That(File.Exists pdb2, Is.True, "FSharp.Core.pdb not found")

    let dll =
      Path.Combine(mono, "Mono.Options.dll")

    Assert.That(File.Exists dll, Is.True, "Mono.Options.dll not found")
    //let pdb3 = Path.Combine(mono, "Mono.Options.pdb")
    //Assert.That(File.Exists pdb3, Is.True, "Mono.Options.pdb not found")

    let a = AssemblyResolver.ReadAssembly exe
    ProgramDatabase.readSymbols a

    let m = AssemblyResolver.ReadAssembly dll
    ProgramDatabase.readSymbols m

    let f = AssemblyResolver.ReadAssembly fdll
    ProgramDatabase.readSymbols f

    // work round the instrumented assemblies having unreliable symbols
    let dir =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/AltCover.Engine/Debug+AnyCPU/netstandard2.0"
      )

    let localAssembly =
      Path.Combine(dir, "AltCover.Engine.dll")
      |> AssemblyResolver.ReadAssembly

    ProgramDatabase.readSymbols localAssembly

    Assert.That(localAssembly.LocalFilter, Is.False, "local engine Assembly non-local")

    Assert.That(
      localAssembly.MainModule.LocalFilter,
      Is.False,
      "local engine  MainModule non-local"
    )

    Assert.That(a.LocalFilter, Is.False, "Assembly non-local")
    Assert.That(a.MainModule.LocalFilter, Is.False, "MainModule non-local")
    Assert.That(m.LocalFilter, Is.False, "dll Assembly non-local")
    Assert.That(m.MainModule.LocalFilter, Is.False, "dll MainModule non-local")
    Assert.That(f.LocalFilter, Is.False, "f# Assembly non-local")
    Assert.That(f.MainModule.LocalFilter, Is.False, "f# MainModule non-local")

    try
      CoverageParameters.local.Value <- true
      Assert.That(localAssembly.LocalFilter, Is.False, "local engine  Assembly local")

      Assert.That(
        localAssembly.MainModule.LocalFilter,
        Is.False,
        "local engine  MainModule local"
      )

      Assert.That(a.LocalFilter, Is.True, "Assembly local")
      Assert.That(a.MainModule.LocalFilter, Is.False, "MainModule local")
      Assert.That(m.LocalFilter, Is.True, "dll Assembly local")
      Assert.That(m.MainModule.LocalFilter, Is.False, "dll MainModule local")
      Assert.That(f.LocalFilter, Is.True, "f# Assembly local")
      Assert.That(f.MainModule.LocalFilter, Is.False, "f# MainModule local")

    finally
      CoverageParameters.local.Value <- false

  [<Test>]
  let LocateMatchShouldChooseLongerWildCardPath () =
    let dict =
      System.Collections.Generic.Dictionary<string, string>()

    let file =
      Assembly.GetExecutingAssembly().Location

    let p1 = Path.GetDirectoryName file
    let p2 = Path.GetDirectoryName p1
    let pp1 = Path.Combine(p1, "*")
    let pp2 = Path.Combine(p2, "*")
    dict.Add(pp1, pp1)
    dict.Add(pp2, pp2)

    let find =
      Visitor.I.findClosestMatch file dict

    Assert.That(find, Is.EqualTo(Some(pp1, String.Empty)))

  [<Test>]
  let AsyncTestInContext () =
    let sample23 =
      sample24path.Replace("24", "23")

    use def =
      AssemblyResolver.ReadAssembly(sample23)

    let symbols23 =
      Path.ChangeExtension(sample23, ".pdb")

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, symbols23)

    def.MainModule.ReadSymbols(rr)

    let types = def.MainModule.GetAllTypes()

    let synch =
      types
      |> Seq.filter (fun t -> t.Name = "Async97")
      |> Seq.collect _.Methods
      |> Seq.filter (fun m -> m.Name = "DoSomethingSynch")
      |> Seq.head

    //synch.Body.Instructions
    //|> Seq.iter (fun i ->
    //  let sp = synch.DebugInformation.GetSequencePoint(i)
    //           |> Option.ofObj
    //           |> Option.map _.StartLine
    //  printfn "%A %A" i sp)

    let method =
      (def.MainModule.GetAllTypes()
       |> Seq.filter (fun t -> t.Name = "<DoSomethingAsync>d__0")
       |> Seq.head)
        .Methods
      |> Seq.filter (fun m -> m.Name = "MoveNext")
      |> Seq.head

    //method.Body.Instructions
    //|> Seq.iter (fun i ->
    //  let sp = method.DebugInformation.GetSequencePoint(i)
    //           |> Option.ofObj
    //           |> Option.map _.StartLine
    //  printfn "%A %A" i sp)

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.nameFilters.Clear()

      let synchStructure =
        Visitor.I.deeper
        <| Node.Method
          { Method = synch
            VisibleMethod = synch
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.toList

      synchStructure
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = n
                        Interesting = b
                        DefaultVisitCount = Exemption.None } ->
          Assert.That(n, Is.EqualTo i, "synch point number")
          Assert.That(b, Is.True, "synch flag " + i.ToString()))

      Visitor.visit [] []

      let deeper =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.toList

      deeper
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = n
                        Interesting = b
                        DefaultVisitCount = Exemption.None } ->
          Assert.That(n, Is.EqualTo i, "point number")
          Assert.That(b, Is.True, "flag " + i.ToString()))

      Assert.That(deeper.Length, Is.EqualTo synchStructure.Length)
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let AnotherAsyncTestInContext () =
    let sample24 = sample24path

    use def =
      AssemblyResolver.ReadAssembly(sample24)

    let symbols24 =
      Path.ChangeExtension(sample24, ".pdb")

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, symbols24)

    def.MainModule.ReadSymbols(rr)

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.filter (fun m -> m.Name = "MoveNext")
      |> Seq.toList

    methods
    |> Seq.iter (fun method ->
      Visitor.visit [] [] // cheat reset

      try
        CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
        CoverageParameters.nameFilters.Clear()

        let deeper =
          Visitor.I.deeper
          <| Node.Method
            { Method = method
              VisibleMethod = method
              Inspection = Inspections.Instrument
              Track = None
              DefaultVisitCount = Exemption.None }
          |> Seq.toList
        // Expect no branch points here from the async (or yield)
        test
          <@
            deeper
            |> List.forall (fun n ->
              match n with
              | MethodPoint _ -> true
              | _ -> false)
          @>
      finally
        CoverageParameters.nameFilters.Clear()
        CoverageParameters.theReportFormat <- None)

  [<Test>]
  let DebugBuildTernaryTestInContext () =
    let sample23 =
      sample24path.Replace("24", "23")

    use def =
      AssemblyResolver.ReadAssembly(sample23)

    let symbols23 =
      Path.ChangeExtension(sample23, ".pdb")

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, symbols23)

    def.MainModule.ReadSymbols(rr)

    let method =
      (def.MainModule.GetAllTypes()
       |> Seq.filter (fun t -> t.Name = "Strings")
       |> Seq.head)
        .Methods
      |> Seq.filter (fun m -> m.Name = "BRB")
      |> Seq.head

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.nameFilters.Clear()

      let deeper =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.toList

      Assert.That(deeper.Length, Is.EqualTo 3)

      deeper
      |> List.skip 1
      |> List.iteri (fun i node ->
        match node with
        | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 1
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = uid
                        DefaultVisitCount = Exemption.None
                        Interesting = true } ->
          Assert.That(uid, Is.EqualTo i, "point number"))
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ReleaseBuildTernaryTest () =
    let nop = Instruction.Create(OpCodes.Nop)
    let ret = Instruction.Create(OpCodes.Ret)
    let seq = SequencePoint(nop, Document(null))

    // transparent
    Assert.That(Visitor.I.fakeSequencePoint Genuine seq nop, Is.SameAs seq)
    Assert.That(Visitor.I.fakeSequencePoint FakeAtReturn seq nop, Is.SameAs seq)

    Assert.That(Visitor.I.fakeSequencePoint Genuine null null, Is.Null)
    Assert.That(Visitor.I.fakeSequencePoint FakeAtReturn null null, Is.Null)

    Assert.That(Visitor.I.fakeSequencePoint Genuine null nop, Is.Null)
    Assert.That(Visitor.I.fakeSequencePoint FakeAtReturn null nop, Is.Null)

    Assert.That(Visitor.I.fakeSequencePoint Genuine null ret, Is.Null)

    // One fake-out
    Assert.That(Visitor.I.fakeSequencePoint FakeAtReturn null ret, Is.Not.Null)

  [<Test>]
  let ReleaseBuildTernaryTestInContext () =
    let res =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("issue37.dl_", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(res)

    let res2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("issue37.pd_", StringComparison.Ordinal)

    use stream2 =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(res2)

    use def =
      AssemblyResolver.ReadAssembly stream

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, stream2)

    def.MainModule.ReadSymbols(rr)

    let method =
      (def.MainModule.GetAllTypes()
       |> Seq.filter (fun t -> t.Name = "Tests")
       |> Seq.head)
        .Methods
      |> Seq.filter (fun m -> m.Name = "Ternary")
      |> Seq.head

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.nameFilters.Clear()

      let deeper =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.toList

      Assert.That(deeper.Length, Is.EqualTo 3)

      deeper
      |> List.skip 1
      |> List.iteri (fun i node ->
        match node with
        | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 1
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = uid
                        DefaultVisitCount = Exemption.None
                        Interesting = true } ->
          Assert.That(uid, Is.EqualTo i, "point number"))
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ReleaseBuildTernaryTestInContextWithCoalescence () =
    let res =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("issue37.dl_", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(res)

    let res2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("issue37.pd_", StringComparison.Ordinal)

    use stream2 =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(res2)

    use def =
      AssemblyResolver.ReadAssembly stream

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, stream2)

    def.MainModule.ReadSymbols(rr)

    let method =
      (def.MainModule.GetAllTypes()
       |> Seq.filter (fun t -> t.Name = "Tests")
       |> Seq.head)
        .Methods
      |> Seq.filter (fun m -> m.Name = "Ternary")
      |> Seq.head

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.coalesceBranches.Value <- true
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.nameFilters.Clear()

      let deeper =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.Automatic }
        |> Seq.toList

      Assert.That(deeper.Length, Is.EqualTo 3)

      deeper
      |> List.skip 1
      |> List.iteri (fun i node ->
        match node with
        | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 1
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = uid
                        DefaultVisitCount = Exemption.Automatic
                        Interesting = true } ->
          Assert.That(uid, Is.EqualTo i, "point number"))
    finally
      CoverageParameters.coalesceBranches.Value <- false
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let CSharpNestedMethods () =
    let sample5 =
      sample24path.Replace("24", "5").Replace("net9.0", "netstandard2.0")

    use def =
      AssemblyResolver.ReadAssembly(sample5)

    // ProgramDatabase.readSymbols def

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.sortBy (fun m -> m.DeclaringType.FullName + "::" + m.Name)
      |> Seq.toList

    //methods
    //|> Seq.iter (fun m -> printf "%A::%A   " m.DeclaringType.FullName m.Name
    //                      m.DebugInformation.SequencePoints
    //                      |> Seq.iter (fun s -> printf "%A " s.StartLine)
    //                      printfn ".")

    let containing =
      methods |> Seq.map Visitor.I.containingMethod

    let result =
      containing
      |> Seq.map (fun (mo: MethodDefinition option) -> mo |> Option.map id)
      |> Seq.toList

    //let problem = Visitor.I.containingMethod methods.[68]

    let expected =
      [ None // System.Void Sample5.Class1/<>c::.cctor()
        None // System.Void Sample5.Class1/<>c::.ctor()
        Some "F1" // System.Int32 Sample5.Class1/<>c::<F1>b__0_0(System.Char)
        None // System.Void Sample5.Class1/<>c__DisplayClass0_0::.ctor()
        Some "F1" // System.Int32 Sample5.Class1/<>c__DisplayClass0_0::<F1>b__2(System.Char)
        Some "F2" // System.Void Sample5.Class1/<F2>d__1::.ctor(System.Int32)
        Some "F2" // System.Boolean Sample5.Class1/<F2>d__1::MoveNext()
        Some "F2" // System.Collections.Generic.IEnumerator`1<System.Int32> Sample5.Class1/<F2>d__1::System.Collections.Generic.IEnumerable<System.Int32>.GetEnumerator()
        Some "F2" // System.Int32 Sample5.Class1/<F2>d__1::System.Collections.Generic.IEnumerator<System.Int32>.get_Current()
        Some "F2" // System.Collections.IEnumerator Sample5.Class1/<F2>d__1::System.Collections.IEnumerable.GetEnumerator()
        Some "F2" // System.Void Sample5.Class1/<F2>d__1::System.Collections.IEnumerator.Reset()
        Some "F2" // System.Object Sample5.Class1/<F2>d__1::System.Collections.IEnumerator.get_Current()
        Some "F2" // System.Void Sample5.Class1/<F2>d__1::System.IDisposable.Dispose()
        Some "F3" // System.Void Sample5.Class1/<F3>d__2::.ctor()
        Some "F3" // System.Void Sample5.Class1/<F3>d__2::MoveNext()
        Some "F3" // System.Void Sample5.Class1/<F3>d__2::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Class1/Inner/<>c::.cctor()
        None // System.Void Sample5.Class1/Inner/<>c::.ctor()
        Some "G1" // System.Int32 Sample5.Class1/Inner/<>c::<G1>b__0_0(System.Char)
        None // System.Void Sample5.Class1/Inner/<>c__DisplayClass0_0::.ctor()
        Some "G1" // System.Int32 Sample5.Class1/Inner/<>c__DisplayClass0_0::<G1>b__3(System.Char)
        Some "G2" // System.Void Sample5.Class1/Inner/<G2>d__2`1::.ctor(System.Int32)
        Some "G2" // System.Boolean Sample5.Class1/Inner/<G2>d__2`1::MoveNext()
        Some "G2" // System.Collections.Generic.IEnumerator`1<T> Sample5.Class1/Inner/<G2>d__2`1::System.Collections.Generic.IEnumerable<T>.GetEnumerator()
        Some "G2" // T Sample5.Class1/Inner/<G2>d__2`1::System.Collections.Generic.IEnumerator<T>.get_Current()
        Some "G2" // System.Collections.IEnumerator Sample5.Class1/Inner/<G2>d__2`1::System.Collections.IEnumerable.GetEnumerator()
        Some "G2" // System.Void Sample5.Class1/Inner/<G2>d__2`1::System.Collections.IEnumerator.Reset()
        Some "G2" // System.Object Sample5.Class1/Inner/<G2>d__2`1::System.Collections.IEnumerator.get_Current()
        Some "G2" // System.Void Sample5.Class1/Inner/<G2>d__2`1::System.IDisposable.Dispose()
        Some "G3" // System.Void Sample5.Class1/Inner/<G3>d__4::.ctor()
        Some "G3" // System.Void Sample5.Class1/Inner/<G3>d__4::MoveNext()
        Some "G3" // System.Void Sample5.Class1/Inner/<G3>d__4::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Class1/Inner::.ctor()
        Some "<G1>g__Recursive|0_4" // T[] Sample5.Class1/Inner::<G1>g__InteriorToArray|0_1(T)
        Some "<G1>b__3" // System.Int32 Sample5.Class1/Inner::<G1>g__Interior|0_2(System.Int32,System.Int32)
        Some "<G1>g__Interior|0_2" // System.Int32 Sample5.Class1/Inner::<G1>g__Recursive|0_4(System.Int32)
        None // System.Int32 Sample5.Class1/Inner::G1(System.String)
        None // System.Void Sample5.Class1/Inner::G1(System.Int32)
        None // System.Collections.Generic.IEnumerable`1<T> Sample5.Class1/Inner::G2(T)
        None // System.Void Sample5.Class1/Inner::G2(System.Int32)
        None // System.Threading.Tasks.Task`1<System.String> Sample5.Class1/Inner::G3(System.String)
        None // System.Void Sample5.Class1/Inner::G3(System.Int32)
        None // System.Void Sample5.Class1::.ctor()
        Some "F1" // System.Int32 Sample5.Class1::<F1>g__Interior|0_1(System.Int32,System.Int32)
        Some "F1" // System.Int32 Sample5.Class1::<F1>g__Recursive|0_3(System.Int32)
        None // System.Int32 Sample5.Class1::F1(System.String)
        None // System.Collections.Generic.IEnumerable`1<System.Int32> Sample5.Class1::F2(System.String)
        None // System.Threading.Tasks.Task`1<System.String> Sample5.Class1::F3(System.String)
        Some "<OuterAsync2>g__InnerAsync3|0" // System.Void Sample5.Issue135/<>c__DisplayClass2_0/<<OuterAsync2>g__InnerAsync3|0>d::.ctor()
        Some "<OuterAsync2>g__InnerAsync3|0" // System.Void Sample5.Issue135/<>c__DisplayClass2_0/<<OuterAsync2>g__InnerAsync3|0>d::MoveNext()
        Some "<OuterAsync2>g__InnerAsync3|0" // System.Void Sample5.Issue135/<>c__DisplayClass2_0/<<OuterAsync2>g__InnerAsync3|0>d::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Issue135/<>c__DisplayClass2_0::.ctor()
        Some "OuterAsync2" // System.Threading.Tasks.Task Sample5.Issue135/<>c__DisplayClass2_0::<OuterAsync2>g__InnerAsync3|0(System.Object) <== "is" InnerAsync
        Some "<OuterSynch4>g__InnerAsync5|0" // System.Void Sample5.Issue135/<>c__DisplayClass3_0/<<OuterSynch4>g__InnerAsync5|0>d::.ctor()
        Some "<OuterSynch4>g__InnerAsync5|0" // System.Void Sample5.Issue135/<>c__DisplayClass3_0/<<OuterSynch4>g__InnerAsync5|0>d::MoveNext()
        Some "<OuterSynch4>g__InnerAsync5|0" // System.Void Sample5.Issue135/<>c__DisplayClass3_0/<<OuterSynch4>g__InnerAsync5|0>d::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Issue135/<>c__DisplayClass3_0::.ctor()
        Some "OuterSynch4" // System.Threading.Tasks.Task Sample5.Issue135/<>c__DisplayClass3_0::<OuterSynch4>g__InnerAsync5|0(System.Object)
        Some "OuterAsync2" // System.Void Sample5.Issue135/<OuterAsync2>d__2::.ctor()
        Some "OuterAsync2" // System.Void Sample5.Issue135/<OuterAsync2>d__2::MoveNext()
        Some "OuterAsync2" // System.Void Sample5.Issue135/<OuterAsync2>d__2::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Issue135::.ctor(System.Object)
        None // System.Threading.Tasks.Task Sample5.Issue135::OuterAsync2(System.Object)
        None // System.Void Sample5.Issue135::OuterSynch4(System.Object)
        Some "<OuterAsync2>g__InnerAsync3|0" // System.Void Sample5.Issue135Complex/<>c__DisplayClass5_0`1/<<OuterAsync2>g__InnerAsync3|0>d::.ctor()
        Some "<OuterAsync2>g__InnerAsync3|0" // System.Void Sample5.Issue135Complex/<>c__DisplayClass5_0`1/<<OuterAsync2>g__InnerAsync3|0>d::MoveNext()
        Some "<OuterAsync2>g__InnerAsync3|0" // System.Void Sample5.Issue135Complex/<>c__DisplayClass5_0`1/<<OuterAsync2>g__InnerAsync3|0>d::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Issue135Complex/<>c__DisplayClass5_0`1::.ctor()
        Some "MoveNext" // System.Threading.Tasks.Task Sample5.Issue135Complex/<>c__DisplayClass5_0`1::<OuterAsync2>g__InnerAsync3|0(T)
        Some "<OuterSynch4>g__InnerAsync5|0" // System.Void Sample5.Issue135Complex/<>c__DisplayClass6_0`1/<<OuterSynch4>g__InnerAsync5|0>d::.ctor()
        Some "<OuterSynch4>g__InnerAsync5|0" // System.Void Sample5.Issue135Complex/<>c__DisplayClass6_0`1/<<OuterSynch4>g__InnerAsync5|0>d::MoveNext()
        Some "<OuterSynch4>g__InnerAsync5|0" // System.Void Sample5.Issue135Complex/<>c__DisplayClass6_0`1/<<OuterSynch4>g__InnerAsync5|0>d::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Issue135Complex/<>c__DisplayClass6_0`1::.ctor()
        Some "OuterSynch4" // System.Threading.Tasks.Task Sample5.Issue135Complex/<>c__DisplayClass6_0`1::<OuterSynch4>g__InnerAsync5|0(T)
        Some "OuterAsync2" // System.Void Sample5.Issue135Complex/<OuterAsync2>d__5`1::.ctor()
        Some "OuterAsync2" // System.Void Sample5.Issue135Complex/<OuterAsync2>d__5`1::MoveNext()
        Some "OuterAsync2" // System.Void Sample5.Issue135Complex/<OuterAsync2>d__5`1::SetStateMachine(System.Runtime.CompilerServices.IAsyncStateMachine)
        None // System.Void Sample5.Issue135Complex::.ctor(System.Object)
        None // System.Void Sample5.Issue135Complex::OuterAsync2()
        None // System.String Sample5.Issue135Complex::OuterAsync2(System.Object)
        None // System.String Sample5.Issue135Complex::OuterAsync2(T,U)
        None // System.Threading.Tasks.Task Sample5.Issue135Complex::OuterAsync2(T)
        None // System.Void Sample5.Issue135Complex::OuterSynch4(T)
        None // System.Void Sample5.Issue135Complex::OuterSynch4()
        None // System.String Sample5.Issue135Complex::OuterSynch4(System.Object)
        None // System.String Sample5.Issue135Complex::OuterSynch4(T,U)
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // System.Void Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::.ctor(System.Int32)
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // System.Boolean Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::MoveNext()
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // System.Collections.Generic.IEnumerator`1<K> Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.Generic.IEnumerable<K>.GetEnumerator()
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // K Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.Generic.IEnumerator<K>.get_Current()
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // System.Collections.IEnumerator Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.IEnumerable.GetEnumerator()
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // System.Void Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.IEnumerator.Reset()
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // System.Object Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.IEnumerator.get_Current()
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // System.Void Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.IDisposable.Dispose()
        Some "get_ValuesWorks" // System.Void Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::.ctor(System.Int32)
        Some "get_ValuesWorks" // System.Boolean Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::MoveNext()
        Some "get_ValuesWorks" // System.Collections.Generic.IEnumerator`1<K> Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.Generic.IEnumerable<K>.GetEnumerator()
        Some "get_ValuesWorks" // K Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.Generic.IEnumerator<K>.get_Current()
        Some "get_ValuesWorks" // System.Collections.IEnumerator Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.IEnumerable.GetEnumerator()
        Some "get_ValuesWorks" // System.Void Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.IEnumerator.Reset()
        Some "get_ValuesWorks" // System.Object Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.IEnumerator.get_Current()
        Some "get_ValuesWorks" // System.Void Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.IDisposable.Dispose()
        None // System.Void Sample5.RecursiveSyntheticInvocation`2::.ctor()
        None // System.Collections.Generic.IEnumerator`1<System.Collections.Generic.KeyValuePair`2<T,K>> Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<T,K>>.GetEnumerator()
        None // System.Int32 Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyCollection<System.Collections.Generic.KeyValuePair<T,K>>.get_Count()
        None // System.Boolean Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.ContainsKey(T)
        None // System.Boolean Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.TryGetValue(T,K&)
        None // K Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Item(T)
        None // System.Collections.Generic.IEnumerable`1<T> Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Keys()
        None // System.Collections.Generic.IEnumerable`1<K> Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values()
        None // System.Collections.IEnumerator Sample5.RecursiveSyntheticInvocation`2::System.Collections.IEnumerable.GetEnumerator()
        None ] // System.Collections.Generic.IEnumerable`1<K> Sample5.RecursiveSyntheticInvocation`2::get_ValuesWorks()

    // methods |> Seq.iter (fun x -> printfn "%A" x.FullName)
    // Assert.That (result, Is.EquivalentTo expected)

    //result
    //|> Seq.zip methods
    //|> Seq.iter (fun (a,b) -> printfn "          %A // %A" (b |> Option.map (fun bb -> bb.Name)) a)

    let toName (m: MethodDefinition) = m.Name
    let toFullName (m: MethodDefinition) = m.FullName

    methods
    |> List.map Some
    |> List.zip (containing |> Seq.toList)
    |> List.iteri // Issue #43
      (fun i (x, y) ->
        Assert.That(
          y,
          x |> Option.map toName |> Is.Not.EqualTo,
          sprintf "%A %A %d" (x |> Option.map toFullName) y i
        ))

    result
    |> List.zip expected
    |> List.iteri (fun i (x, y) ->
      Assert.That(
        y |> Option.map toName,
        x |> Is.EqualTo,
        sprintf "%A %A %d" x (y |> Option.map toFullName) i
      ))

    // Disambiguation checks
    //  System.Threading.Tasks.Task Sample5.Issue135/<>c__DisplayClass2_0::<OuterAsync2>g__InnerAsync3|0(System.Object)
    // => System.Void Sample5.Issue135/<OuterAsync2>d__2::MoveNext()
    Assert.That(
      result.[68].Value,
      Is.EqualTo methods.[75],
      "The outer move-next was wanted"
    )

    let g3 = methods.[40] // "System.Threading.Tasks.Task`1<System.String> Sample5.Class1/Inner::G3(System.String)"

    Assert.That(
      methods
      |> Seq.choose Visitor.I.containingMethod
      |> Seq.filter (fun m -> m.Name = "G3"),
      Is.EquivalentTo [ g3; g3; g3 ]
    )

    let g1 = methods.[36] // "System.Int32 Sample5.Class1/Inner::G1(System.String)"

    Assert.That(
      methods
      |> Seq.choose Visitor.I.containingMethod
      |> Seq.filter (fun m -> m.Name = "G1"),
      Is.EquivalentTo [ g1; g1 ]
    )

  [<Test>]
  let FSharpNestedMethodsClassic () =
    let sample3 =
      Path.Combine(SolutionRoot.location, "Samples/Sample6/Sample6Classic.dll")

    use def =
      AssemblyResolver.ReadAssembly(sample3)

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.toList

    let result =
      methods
      |> Seq.map (
        Visitor.I.containingMethod
        >> (fun (mo: MethodDefinition option) ->
          mo
          |> Option.map (fun m -> m.DeclaringType.Name + "::" + m.Name))
      )
      |> Seq.toList

    let expected =
      [ None //Microsoft.FSharp.Collections.FSharpList`1<System.Int32> Sample6.Module::F1(Microsoft.FSharp.Collections.FSharpList`1<System.Object>)
        None //Microsoft.FSharp.Core.Unit[] Sample6.Module::F2(Microsoft.FSharp.Collections.FSharpList`1<System.String>)
        Some "Module::F1" //System.Void Sample6.Module/aux@9::.ctor()
        Some "Module::F1" //System.Int32 Sample6.Module/aux@9::Invoke(System.Int32)
        Some "FI@11T::Invoke" //System.Void Sample6.Module/FII@11::.ctor()
        Some "FI@11T::Invoke" //System.Object Sample6.Module/FII@11::Specialize()
        Some "FII@12::Specialize" //System.Void Sample6.Module/FII@11T::.ctor(Sample6.Module/FII@11)
        Some "FII@12::Specialize" //System.Int32 Sample6.Module/FII@11T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<b>,System.Int32)
        Some "Module::F1" //System.Void Sample6.Module/FI@10::.ctor()
        Some "Module::F1" //System.Object Sample6.Module/FI@10::Specialize()
        Some "FI@11::Specialize" //System.Void Sample6.Module/FI@10T::.ctor(Sample6.Module/FI@10)
        Some "FI@11::Specialize" //System.Int32 Sample6.Module/FI@10T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<a>)
        Some "Module::F1" //System.Void Sample6.Module/F1@18::.ctor()
        Some "Module::F1"
        Some "fetchUrlAsync@25-4::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@27-5::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@25-4::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@27-5::Invoke(System.IO.StreamReader)"
        Some "fetchUrlAsync@23-3::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@26-4::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@23-3::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@26-4::Invoke(System.IO.Stream)"
        Some "fetchUrlAsync@23-2::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@24-3::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@23-2::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@24-3::Invoke(System.Net.WebResponse)"
        Some "fetchUrlAsync@22-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@24-2::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@22-1::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@24-2::Invoke(System.Net.WebResponse)"
        Some "fetchUrlAsync@22-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@24-6::.ctor(Microsoft.FSharp.Control.FSharpAsync`1<System.Net.WebResponse>,Microsoft.FSharp.Core.FSharpFunc`2<System.Net.WebResponse,Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit>>)"
        Some "fetchUrlAsync@22-1::Invoke" //"Microsoft.FSharp.Control.AsyncReturn Sample6.Module/fetchUrlAsync@24-6::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<Microsoft.FSharp.Core.Unit>)"
        Some "fetchUrlAsync@21::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-1::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@21::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-1::Invoke(Microsoft.FSharp.Core.Unit)"
        Some "Module::F2" //"System.Void Sample6.Module/fetchUrlAsync@22::.ctor()"
        Some "Module::F2" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@22::Invoke(System.String)"

        ]

    //methods |> Seq.iter (fun x -> printfn "%A" x.FullName)
    //Assert.That (result, Is.EquivalentTo expected)
    //result
    //|> List.zip methods
    //|> List.iteri
    //     (fun i (x, y) ->
    //       printfn "%A %A %d" y x i)

    Assert.Multiple(fun () ->
      result
      |> List.zip expected
      |> List.iteri (fun i (x, y) ->
        Assert.That(y, Is.EqualTo x, sprintf "%A %A %d %s" x y i methods.[i].FullName)))

  [<Test>]
  let FSharpNestedMethods5x0x201 () =
    // let sample3 = Path.Combine(SolutionRoot.location, "Samples/Sample6/Sample6_5_0_201.dll")
    let sample6 =
      sample24path.Replace("24", "6")

    use def =
      AssemblyResolver.ReadAssembly(sample6)

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.toList

    let result =
      methods
      |> Seq.map (
        Visitor.I.containingMethod
        >> (fun (mo: MethodDefinition option) ->
          mo
          |> Option.map (fun m -> m.DeclaringType.Name + "::" + m.Name))
      )
      |> Seq.toList

    let expected =
      [ None //Microsoft.FSharp.Collections.FSharpList`1<System.Int32> Sample6.Module::F1(Microsoft.FSharp.Collections.FSharpList`1<System.Object>)
        None //Microsoft.FSharp.Core.Unit[] Sample6.Module::F2(Microsoft.FSharp.Collections.FSharpList`1<System.String>)
        Some "Module::F1" //*//System.Void Sample6.Module/aux@9::.ctor()
        Some "Module::F1" //*//System.Int32 Sample6.Module/aux@9::Invoke(System.Int32)
        Some "Module::F1" //*//System.Void Sample6.Module/aux@9::.cctor()
        Some "FI@11T::Invoke" //System.Void Sample6.Module/FII@12::.ctor()
        Some "FI@11T::Invoke" //System.Object Sample6.Module/FII@12::Specialize()
        Some "FII@12::Specialize" //System.Void Sample6.Module/FII@12T::.ctor(Sample6.Module/FII@12)
        Some "FII@12::Specialize" //System.Int32 Sample6.Module/FII@12T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<b>,System.Int32)
        Some "Module::F1" //System.Void Sample6.Module/FI@11::.ctor()
        Some "Module::F1" //System.Object Sample6.Module/FI@11::Specialize()
        Some "FI@11::Specialize" //System.Void Sample6.Module/FI@11T::.ctor(Sample6.Module/FI@11)
        Some "FI@11::Specialize" //System.Int32 Sample6.Module/FI@11T::Invoke(Microsoft.FSharp.Collections.FSharpList`1<a>)
        Some "Module::F1" //*//System.Void Sample6.Module/F1@17::.ctor()
        Some "Module::F1" //*//System.Void Sample6.Module/F1@17::Invoke()
        Some "Module::F1" //*//System.Void Sample6.Module/F1@17::.cctor()
        Some "fetchUrlAsync@27-4::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@26-5::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@27-4::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@26-5::Invoke(System.IO.StreamReader)"
        Some "fetchUrlAsync@25-3::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@25-4::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@25-3::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@25-4::Invoke(System.IO.Stream)"
        Some "fetchUrlAsync@25-2::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-3::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@25-2::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-3::Invoke(System.Net.WebResponse)"
        Some "fetchUrlAsync@24-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-2::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@24-1::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-2::Invoke(System.Net.WebResponse)"
        Some "fetchUrlAsync@24-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-6::.ctor(Microsoft.FSharp.Control.FSharpAsync`1<System.Net.WebResponse>,Microsoft.FSharp.Core.FSharpFunc`2<System.Net.WebResponse,Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit>>)"
        Some "fetchUrlAsync@24-1::Invoke" //"Microsoft.FSharp.Control.AsyncReturn Sample6.Module/fetchUrlAsync@23-6::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<Microsoft.FSharp.Core.Unit>)"
        Some "fetchUrlAsync@23::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@22-1::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@23::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@22-1::Invoke(Microsoft.FSharp.Core.Unit)"
        Some "Module::F2" //*//"System.Void Sample6.Module/fetchUrlAsync@21::.ctor()"
        Some "Module::F2" //*//"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@21::Invoke(System.String)"
        Some "Module::F2" ] //*//"System.Void Sample6.Module/fetchUrlAsync@21::.cctor()"

    //methods |> Seq.iter (fun x -> printfn "%A" x.FullName)
    //Assert.That (result, Is.EquivalentTo expected)
    //result
    //|> List.zip methods
    //|> List.iteri
    //     (fun i (x, y) ->
    //       printfn "%A %A %d" y x i)

    //result
    //|> List.zip expected
    //|> List.mapi (fun i x -> (i, x))
    //|> List.filter (fun (_, (x,y)) -> x <> y)
    //|> List.iter
    //     (fun (i, (x, y)) ->
    //       printfn "%A %A %d" y x i)

    Assert.Multiple(fun () ->
      result
      |> List.zip expected
      |> List.iteri (fun i (x, y) ->
        Assert.That(y, Is.EqualTo x, sprintf "%A %A %d %s" x y i methods.[i].FullName)))

  [<Test>]
  let ValidateSeqPntFixUp () = // HACK HACK HACK
    let location =
      typeof<Sample3.Class1>.Assembly.Location

    use sourceAssembly =
      AssemblyResolver.ReadAssembly(location)

    let i =
      sourceAssembly.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.filter (fun m -> m.HasBody && m.Body.Instructions.Any())
      |> Seq.map (_.Body.Instructions >> Seq.head)
      |> Seq.head

    let dummy = Cil.Document("dummy")
    let before = Cil.SequencePoint(i, dummy)

    before.GetType().GetProperty("StartLine").SetValue(before, 23)

    before.GetType().GetProperty("StartColumn").SetValue(before, 42)

    before.GetType().GetProperty("EndLine").SetValue(before, -1)

    before.GetType().GetProperty("EndColumn").SetValue(before, -1)

    let after = SeqPnt.Build(before)
    Assert.That(after.EndLine, Is.EqualTo before.StartLine)
    Assert.That(after.EndColumn, Is.EqualTo(before.StartColumn + 1))

  [<Test>]
  let EmptyArrayHasExpectedHash () =
    Assert.That(
      (KeyStore.I.tokenOfArray [||]),
      Is.EquivalentTo
        [| 9uy
           7uy
           216uy
           175uy
           144uy
           24uy
           96uy
           149uy |]
    )

  let private provideKeyPair () =
    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    StrongNameKeyData.Make(buffer.ToArray())

  [<Test>]
  let KeyHasExpectedToken () =
    let token =
      KeyStore.tokenOfKey <| provideKeyPair ()

    let token' =
      String.Join(String.Empty, token |> List.map _.ToString("x2"))

    Assert.That(token', Is.EqualTo("c02b1a9f5b7cade8"))

  [<Test>]
  let TokenGeneratesExpectedULong () =
    let token =
      [| 1uy
         0uy
         0uy
         0uy
         0uy
         0uy
         0uy
         0uy |]

    Assert.That(KeyStore.tokenAsULong token, Is.EqualTo(1UL))

  [<Test>]
  let KeyHasExpectedIndex () =
    let token =
      KeyStore.keyToIndex <| provideKeyPair ()

    Assert.That(token, Is.EqualTo(0xe8ad7c5b9f1a2bc0UL), sprintf "%x" token)

  [<Test>]
  let EmptyArrayHasExpectedIndex () =
    Assert.That((KeyStore.arrayToIndex [||]), Is.EqualTo(0x95601890afd80709UL))

  [<Test>]
  let KeyHasExpectedRecord () =
    let pair = provideKeyPair ()
#if NET472  // Strong-name signing is not supported on this platform.
    let computed = pair.PublicKey

    let definitive =
      StrongNameKeyPair(pair.Blob |> List.toArray).PublicKey

    Assert.That(computed, Is.EquivalentTo definitive)
#endif

    let token = KeyStore.keyToRecord <| pair

    Assert.That(
      token,
      Is.EqualTo(
        { Pair = pair
          Token =
            BitConverter.GetBytes(0xe8ad7c5b9f1a2bc0UL)
            |> Array.toList }
      )
    )

  [<Test>]
  let KeyHasExpectedPlaceInIndex () =
    try
      CoverageParameters.keys.Clear()
      Assert.That(CoverageParameters.keys.Count, Is.EqualTo(0))

      let pair = provideKeyPair ()
      CoverageParameters.add (pair)
      let key = 0xe8ad7c5b9f1a2bc0UL

      Assert.That(CoverageParameters.keys.ContainsKey(key))

      Assert.That(
        CoverageParameters.keys.[key],
        Is.EqualTo(
          { Pair = pair
            Token = BitConverter.GetBytes(key) |> Array.toList }
        )
      )
    finally
      CoverageParameters.keys.Clear()

  let IsItIncluded x = x.IsIncluded.IsInstrumented

  [<Test>]
  let EmptyFiltersPassAll () =
    CoverageParameters.nameFilters.Clear()
    Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo(0))
    Assert.That(typeof<ProxyObject> |> IsItIncluded)

  let private ff (a, b, c) = { Scope = a; Regex = b; Sense = c }

  [<Test>]
  let NonEmptyFiltersCatchAnExpectedValue () =
    try
      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo(0))

      CoverageParameters.nameFilters.AddRange(
        [ ff (FilterScope.File, Regex "Cove", Exclude)
          ff (FilterScope.Method, Regex "Augment", Exclude) ]
      )

      Assert.That(
        Assembly.GetExecutingAssembly().Location
        |> IsItIncluded,
        Is.False
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let NonEmptyFiltersPassAnExpectedValue () =
    try
      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo(0))

      CoverageParameters.nameFilters.AddRange(
        [ ff (FilterScope.File, Regex "System", Exclude)
          ff (FilterScope.Method, Regex "Augment", Exclude) ]
      )

      Assert.That(
        Assembly.GetExecutingAssembly().Location
        |> IsItIncluded,
        Is.True
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let AfterProcessingYieldsAnExpectedValue () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    let inputs =
      [ Node.Start []
        Node.Assembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = [] }
        Node.Module
          { Module = null
            Inspection = Inspections.Ignore }
        Node.Type
          { VisibleType = null
            Type = null
            Inspection = Inspections.Instrument
            DefaultVisitCount = Exemption.None }
        Node.Method
          { Method = null
            VisibleMethod = null
            Inspection = Inspections.Ignore
            Track = None
            DefaultVisitCount = Exemption.None }
        Node.MethodPoint
          { Instruction = null
            SeqPnt = None
            Uid = 0
            Interesting = true
            DefaultVisitCount = Exemption.None }
        Node.AfterMethod
          { Method = null
            VisibleMethod = null
            Inspection = Inspections.Ignore
            Track = None
            DefaultVisitCount = Exemption.None }
        Node.AfterModule
        Node.AfterAssembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = [] }
        Node.Finish ]

    let outputs =
      inputs
      |> Seq.map (_.After() >> Seq.toList)
      |> Seq.toList

    let expected =
      [ [ Finish ]
        [ AfterAssembly
            { Assembly = def
              Inspection = Inspections.Instrument
              Identity = Hallmark.Build()
              Destinations = [] } ]
        [ AfterModule ]
        [ AfterType ]
        [ AfterMethod
            { Method = null
              VisibleMethod = null
              Inspection = Inspections.Ignore
              Track = None
              DefaultVisitCount = Exemption.None } ]
        []
        []
        []
        []
        [] ]

    test <@ outputs = expected @>

  [<Test>]
  let Sample3Class1PropertyIsNotSignificant () =
    let sample3 =
      Path.Combine(dir, "Sample3.dll")

    use def =
      AssemblyResolver.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class1")
    |> Seq.collect _.Methods
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (fun m -> Assert.That(Visitor.I.significant m, Is.False))

  [<Test>]
  let Sample3Class2IPropertyIsSignificant () =
    let sample3 =
      Path.Combine(dir, "Sample3.dll")

    use def =
      AssemblyResolver.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class2")
    |> Seq.collect _.Methods
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (Visitor.I.significant >> Assert.That)

  [<Test>]
  let TerminalCasesGoNoDeeper () =
    use def =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    let inputs =
      [ Node.MethodPoint
          { Instruction = null
            SeqPnt = None
            Uid = 0
            Interesting = true
            DefaultVisitCount = Exemption.None }
        Node.AfterMethod
          { Method = null
            VisibleMethod = null
            Inspection = Inspections.Ignore
            Track = None
            DefaultVisitCount = Exemption.None }
        Node.AfterModule
        Node.AfterAssembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = [] }
        Node.Finish ]

    let outputs =
      inputs
      |> Seq.map (Visitor.I.deeper >> Seq.toList)
      |> Seq.toList

    let expected: Node list list =
      [ []; []; []; []; [] ]
    //Assert.That(outputs, Is.EquivalentTo(expected))
    test <@ outputs = expected @>

  [<Test>]
  let MethodPointsAreDeeperThanMethods () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      (def.MainModule.Types
       |> Seq.skipWhile _.Name.StartsWith("<")
       |> Seq.head)
        .Methods
      |> Seq.head

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.File
          >> CoverageParameters.nameFilters.Add)

      let deeper =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.toList

      Assert.That(deeper.Length, Is.EqualTo 12)

      deeper
      |> List.skip 10
      |> List.iteri (fun i node ->
        match node with
        | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 10
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = uid
                        DefaultVisitCount = Exemption.None
                        Interesting = false } ->
          Assert.That(uid, Is.EqualTo i, "point number"))

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let BranchPointsAreComputedForSwitch () =
    let path = Path.Combine(dir, "Sample16.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      (def.MainModule.GetAllTypes()
       |> Seq.filter (fun t -> t.Name = "Foo")
       |> Seq.head)
        .Methods
      |> Seq.filter (fun m -> m.Name = "Bar")
      |> Seq.head

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.coalesceBranches.Value <- true
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.nameFilters.Clear()

      let deeper =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.Declared }
        |> Seq.toList

      //deeper |> List.skip 21 |> Seq.iter (fun n -> match n with
      //                                             | BranchPoint x -> printfn "deeper = %A sl = %A offset = %A" x x.SequencePoint.StartLine x.SequencePoint.Offset
      //                                             | _ ->())
      let reported =
        deeper
        |> List.filter (fun n ->
          match n with
          | BranchPoint b -> b.Representative = Reporting.Representative
          | _ -> true)
      //reported |> List.skip 21 |> Seq.iter (printfn "reported = %A")
      Assert.That(reported.Length, Is.EqualTo 29)

      let branches =
        reported
        |> List.skip 21
        |> List.mapi (fun i node ->
          match node with
          | (BranchPoint b) ->
            Assert.That(b.Uid, Is.EqualTo i, "branch point number")
            Some b)
        |> List.choose id

      deeper
      |> List.take 21
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = uid
                        DefaultVisitCount = Exemption.Declared
                        Interesting = true } ->
          Assert.That(uid, Is.EqualTo i, "point number"))

      Assert.That(branches |> List.map _.Path, Is.EquivalentTo [ 0; 1; 0; 1; 2; 3; 0; 1 ])
    finally
      CoverageParameters.coalesceBranches.Value <- false
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let BranchPointsAreComputedForMatch () =
    let path = sample24path.Replace("24", "17")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      (def.MainModule.GetAllTypes()
       |> Seq.filter (fun t -> t.Name = "Carrier")
       |> Seq.head)
        .Methods
      |> Seq.filter (fun m -> m.Name = "Function")
      |> Seq.head

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.coalesceBranches.Value <- true

      let deeper =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.StaticAnalysis }
        |> Seq.toList

      let reported =
        deeper
        |> List.filter (fun n ->
          match n with
          | BranchPoint b -> b.Representative = Reporting.Representative
          | _ -> true)

      Assert.That(reported.Length, Is.EqualTo 11)

      reported
      |> List.skip 6
      |> List.iteri (fun i node ->
        match node with
        | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 6
      |> List.iteri (fun i node ->
        match node with
        | MethodPoint { Instruction = _
                        SeqPnt = _
                        Uid = uid
                        DefaultVisitCount = Exemption.StaticAnalysis
                        Interesting = true } ->
          Assert.That(uid, Is.EqualTo i, "point number"))

    finally
      CoverageParameters.coalesceBranches.Value <- false
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let MethodsAreDeeperThanTypes () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let type' =
      (def.MainModule.Types
       |> Seq.skipWhile _.Name.StartsWith("<")
       |> Seq.head)

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      "Main"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Method
          >> CoverageParameters.nameFilters.Add)

      let deeper =
        Visitor.I.deeper
        <| Node.Type
          { Type = type'
            VisibleType = type'
            Inspection = Inspections.Instrument
            DefaultVisitCount = Exemption.None }
        |> Seq.toList

      Visitor.visit [] [] // cheat reset

      let expected =
        type'.Methods
        |> Seq.map (fun m ->
          let flag =
            if m.Name = ".ctor" then
              Inspections.Instrument
            else
              Inspections.Ignore

          let node =
            Node.Method
              { Method = m
                VisibleMethod = m
                Inspection = flag
                Track = None
                DefaultVisitCount = Exemption.None }

          List.concat
            [ [ node ]
              (Visitor.I.deeper >> Seq.toList) node
              [ Node.AfterMethod
                  { Method = m
                    VisibleMethod = m
                    Inspection = flag
                    Track = None
                    DefaultVisitCount = Exemption.None } ] ])
        |> List.concat

      Assert.That(deeper.Length, Is.EqualTo 17)
      Assert.That(deeper |> Seq.map string, Is.EquivalentTo(expected |> Seq.map string))
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let TypesAreDeeperThanModules () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule
    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Type
          >> CoverageParameters.nameFilters.Add)

      let deeper =
        Visitor.I.deeper
        <| Node.Module
          { Module = module'
            Inspection = Inspections.Instrument }
        |> Seq.toList

      Visitor.visit [] [] // cheat reset

      let expected =
        module'.Types // we have no nested types in this test
        |> Seq.filter Visitor.I.stripInterfaces
        |> Seq.map (fun t ->
          let flag =
            Maybe (t.Name <> "Program") Inspections.Instrument Inspections.Ignore

          let node =
            Node.Type
              { VisibleType = t
                Type = t
                Inspection = flag
                DefaultVisitCount = Exemption.None }

          List.concat
            [ [ node ]
              (Visitor.I.deeper >> Seq.toList) node
              [ Node.AfterType ] ])
        |> List.concat

      Assert.That(deeper.Length, Is.EqualTo 16)
      Assert.That(deeper |> Seq.map string, Is.EquivalentTo(expected |> Seq.map string))
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ModulesAreDeeperThanAssemblies () =
    try
      let where =
        Assembly.GetExecutingAssembly().Location

      let path = sample1path

      use def = AssemblyResolver.ReadAssembly path

      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      ProgramDatabase.readSymbols def
      Visitor.visit [] [] // cheat reset

      let deeper =
        Visitor.I.deeper
        <| Node.Assembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = [] }
        |> Seq.toList

      Visitor.visit [] [] // cheat reset

      let expected =
        def.Modules // we have no nested types in this test
        |> Seq.map (fun t ->
          let node =
            Node.Module
              { Module = t
                Inspection = Inspections.Instrument }

          List.concat
            [ [ node ]
              (Visitor.I.deeper >> Seq.toList) node
              [ AfterModule ] ])
        |> List.concat

      Assert.That(deeper.Length, Is.EqualTo 19)
      Assert.That(deeper |> Seq.map string, Is.EquivalentTo(expected |> Seq.map string))
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let AssembliesAreDeeperThanPaths () =
    try
      CoverageParameters.staticFilter <- Some StaticFilter.AsCovered
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      let where =
        Assembly.GetExecutingAssembly().Location

      let path = sample1path

      let deeper =
        Visitor.I.deeper
        <| Node.Start
          [ { AssemblyPath = path
              Identity = Hallmark.Build()
              Destinations = [] } ]
        |> Seq.toList

      // assembly definitions care about being separate references in equality tests
      use def =
        match Seq.head deeper with
        | Node.Assembly { Assembly = assembly
                          Inspection = Inspections.Instrument
                          Destinations = [] } -> assembly

      let assembly =
        Node.Assembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = [] }

      let expected =
        List.concat
          [ [ assembly ]
            (Visitor.I.deeper >> Seq.toList) assembly
            [ AfterAssembly
                { Assembly = def
                  Inspection = Inspections.Instrument
                  Identity = Hallmark.Build()
                  Destinations = [] } ] ]

      //deeper |> Seq.map (fun x -> x.GetType().Name) |> Seq.iter (printfn "%A")
      //printfn "-----------"
      //expected |> Seq.map (fun x -> x.GetType().Name) |> Seq.iter (printfn "%A")

      Assert.That(deeper.Length, Is.EqualTo 21)
      Assert.That(deeper |> Seq.map string, Is.EquivalentTo(expected |> Seq.map string))
    finally
      CoverageParameters.staticFilter <- None
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let FilteredAssembliesDoNotHaveSequencePoints () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path
    CoverageParameters.theReportFormat <- Some ReportFormat.NCover

    try
      Assert.That(CoverageParameters.reportFormat (), Is.EqualTo ReportFormat.NCover)

      "Sample"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Assembly
          >> CoverageParameters.nameFilters.Add)

      let deeper =
        Visitor.I.deeper
        <| Node.Start
          [ { AssemblyPath = path
              Identity = Hallmark.Build()
              Destinations = [] } ]
        |> Seq.toList

      // assembly definitions care about being separate references in equality tests
      use def =
        match Seq.head deeper with
        | Node.Assembly { Assembly = assembly
                          Inspection = Inspections.Ignore
                          Destinations = [] } -> assembly

      let assembly =
        Node.Assembly
          { Assembly = def
            Inspection = Inspections.Ignore
            Identity = Hallmark.Build()
            Destinations = [] }

      let expected =
        List.concat
          [ [ assembly ]
            (Visitor.I.deeper >> Seq.toList) assembly
            [ AfterAssembly
                { Assembly = def
                  Inspection = Inspections.Ignore
                  Identity = Hallmark.Build()
                  Destinations = [] } ] ]

      Assert.That(deeper.Length, Is.EqualTo 4)
      Assert.That(deeper, Is.EquivalentTo expected)
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let TestExceptionWrapping () =
    let deeper =
      Visitor.I.deeper
      <| Node.Start
        [ { AssemblyPath = sample1path
            Identity = Hallmark.Build()
            Destinations = [] } ]
      |> Seq.toList

    deeper
    |> List.iter (fun n ->
      let text =
        (sprintf "%A" n)
          .Replace(
            "Mono.Cecil.Cil.Document",
            Path
              .Combine(SolutionRoot.location, "Samples/Sample1/Program.cs")
              .Replace('/', Path.DirectorySeparatorChar)
          )

      let unique = Guid.NewGuid().ToString()
      let op _ : Object = raise <| IOException(unique)

      let x =
        Assert.Throws<InvalidOperationException>(fun () ->
          (Visitor.I.wrap op n) |> ignore)

      test <@ x.Message = "'" + unique + "' while visiting '" + text + "'" @>)

  [<Test>]
  let TestFixPointInvoke () =
    let mutable called = 0

    // [<TailCall>]
    let rec stateful l =
      new Fix<Node>(fun node ->
        called <- called + 1
        stateful node)

    let input = stateful (Start [])
    let fix = Visitor.I.invoke (Start []) input
    Assert.That(called, Is.EqualTo 1)
    Assert.That(fix, Is.Not.Null)
    Assert.That(fix.GetType(), Is.EqualTo(input.GetType()))

  [<Test>]
  let TestFixPointApply () =
    let mutable called = 0

    // [<TailCall>]
    let rec stateful l =
      new Fix<Node>(fun node ->
        called <- called + 1
        stateful node)

    let list =
      [ stateful (Start [])
        stateful (Start []) ]

    let fix =
      Visitor.I.apply list (Start []) |> Seq.toList

    Assert.That(called, Is.EqualTo 2)
    Assert.That(Seq.length fix, Is.EqualTo 2)
    Assert.That(fix.[0].GetType(), Is.EqualTo(list.[0].GetType()))
    Assert.That(fix.[1].GetType(), Is.EqualTo(list.[1].GetType()))

  [<Test>]
  let PathsAreDeeperThanAVisit () =
    try
      CoverageParameters.showGenerated.Value <- true

      let where =
        Assembly.GetExecutingAssembly().Location

      let path = sample1path

      let accumulator =
        System.Collections.Generic.List<Node>()

      let fix =
        Visitor.encloseState
          (fun (x: System.Collections.Generic.List<Node>) t ->
            x.Add t
            x)
          accumulator

      let u = Guid.NewGuid().ToString()
      let u2 = Guid.NewGuid().ToString()
      let ux = [ u; u2 ]

      Visitor.visit
        [ fix ]
        [ { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = ux } ]
      // assembly definitions care about being separate references in equality tests
      use def =
        match accumulator.[1] with
        | Node.Assembly { Assembly = assembly
                          Inspection = Inspections.Instrument
                          Destinations = ux } -> assembly

      let assembly =
        Node.Assembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = ux }

      let expected =
        List.concat
          [ [ Start
                [ { AssemblyPath = path
                    Identity = Hallmark.Build()
                    Destinations = ux } ]
              assembly ]
            (Visitor.I.deeper >> Seq.toList) assembly
            [ AfterAssembly
                { Assembly = def
                  Inspection = Inspections.Instrument
                  Identity = Hallmark.Build()
                  Destinations = ux }
              Finish ] ]

      Assert.That(
        accumulator |> Seq.map string,
        Is.EquivalentTo(expected |> Seq.map string)
      )
    finally
      CoverageParameters.showGenerated.Value <- false

  [<Test>]
  let TrackingDetectsTests () =
    let path = Path.Combine(dir, "Sample2.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.AddRange([ "Junk"; "[MoreJunk"; "[Test" ])
      Visitor.visit [] [] // cheat reset

      let tracks =
        def.MainModule.GetAllTypes()
        |> Seq.collect _.Methods
        |> Seq.choose (Visitor.I.track)
        |> Seq.toList

      Assert.That(tracks, Is.EquivalentTo [ (1, "[Test"); (2, "[Test") ])
    finally
      CoverageParameters.trackingNames.Clear()
      Visitor.visit [] [] // cheat reset

  [<Test>]
  let TrackingDetectsExpectedTests () =
    let path = Path.Combine(dir, "Sample2.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.AddRange([ "Junk"; "[MoreJunk"; "[Test" ])
      Visitor.visit [] [] // cheat reset

      let tracks =
        def.MainModule.GetAllTypes()
        |> Seq.collect _.Methods
        |> Seq.filter (Visitor.I.track >> Option.isSome)
        |> Seq.map _.Name
        |> Seq.toList

      Assert.That(tracks, Is.EquivalentTo [ "testMakeUnion"; "testMakeThing" ])
    finally
      CoverageParameters.trackingNames.Clear()
      Visitor.visit [] [] // cheat reset

  [<Test>]
  let TrackingDetectsTestsByFullType () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample2.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    try
      CoverageParameters.trackingNames.Clear()

      CoverageParameters.trackingNames.AddRange(
        [ "Junk"
          "[MoreJunk"
          "[NUnit.Framework.TestAttribute]" ]
      )

      Visitor.visit [] [] // cheat reset

      let tracks =
        def.MainModule.GetAllTypes()
        |> Seq.collect _.Methods
        |> Seq.choose (Visitor.I.track)
        |> Seq.toList

      Assert.That(
        tracks,
        Is.EquivalentTo
          [ (1, "[NUnit.Framework.TestAttribute]")
            (2, "[NUnit.Framework.TestAttribute]") ]
      )
    finally
      CoverageParameters.trackingNames.Clear()
      Visitor.visit [] [] // cheat reset

  [<Test>]
  let TrackingDetectsMethods () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample2.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    try
      CoverageParameters.trackingNames.Clear()

      CoverageParameters.trackingNames.AddRange(
        [ "Junk"
          "[MoreJunk"
          "returnFoo"
          "N.DU.MyUnion.as_bar" ]
      )

      Visitor.visit [] [] // cheat reset

      let tracks =
        def.MainModule.GetAllTypes()
        |> Seq.collect _.Methods
        |> Seq.choose (Visitor.I.track)
        |> Seq.toList

      Assert.That(
        tracks,
        Is.EquivalentTo
          [ (1, "returnFoo")
            (2, "N.DU.MyUnion.as_bar") ]
      )
    finally
      CoverageParameters.trackingNames.Clear()
      Visitor.visit [] [] // cheat reset

  // Naming.fs
  [<Test>]
  let NamingDetectEmpties () =
    let input =
      [ "string"
        null
        "another string"
        "             " ]

    let nulls =
      input
      |> Seq.map Naming.I.emptyIfIsNullOrWhiteSpace

    Assert.That(
      nulls,
      Is.EquivalentTo(
        [ "string"
          String.Empty
          "another string"
          String.Empty ]
      )
    )

  [<Test>]
  let NamingSuffixDetectEmpties () =
    let input =
      [ "string"
        null
        "another string"
        "             " ]

    let nulls =
      input
      |> Seq.map (fun n -> Naming.I.suffixIfNotIsNullOrWhiteSpace n "*")

    Assert.That(
      nulls,
      Is.EquivalentTo(
        [ "string*"
          String.Empty
          "another string*"
          String.Empty ]
      )
    )

  [<Test>]
  let TypeNamesAreExtracted () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map Naming.I.typeName
      |> Seq.toList

    let expected =
      [ "<Module>"
        "Class1"
        "Class2"
        "Class3"
        "Class4"
        "IsExternalInit"
        "InstrumentationAttribute" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let FullTypeNamesAreExtracted () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map Naming.fullTypeName
      |> Seq.toList

    let expected =
      [ "<Module>"
        "AltCover.Sample3.Class1"
        "AltCover.Sample3.Class2"
        "AltCover.Sample3.Class3"
        "AltCover.Sample3.Class3+Class4"
        "System.Runtime.CompilerServices.IsExternalInit"
        "AltCover.Recorder.InstrumentationAttribute" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let TypeRefNamesAreExtracted () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map (fun td ->
        Naming.I.typeRefName (TypeReference(td.Namespace, td.Name, def.MainModule, null)))
      |> Seq.toList

    let expected =
      [ "<Module>"
        "Class1"
        "Class2"
        "Class3"
        "Class4"
        "IsExternalInit"
        "InstrumentationAttribute" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let FullTypeRefNamesAreExtracted () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map (fun td ->
        let tr =
          TypeReference(td.Namespace, td.Name, def.MainModule, null)

        if td.DeclaringType.IsNotNull then
          tr.DeclaringType <-
            TypeReference(
              td.DeclaringType.Namespace,
              td.DeclaringType.Name,
              def.MainModule,
              null
            )

        Naming.I.fullTypeRefName (tr))
      |> Seq.toList

    let expected =
      [ "<Module>"
        "AltCover.Sample3.Class1"
        "AltCover.Sample3.Class2"
        "AltCover.Sample3.Class3"
        "AltCover.Sample3.Class3+Class4"
        "System.Runtime.CompilerServices.IsExternalInit"
        "AltCover.Recorder.InstrumentationAttribute" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let MethodNamesAreExtracted () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.map Naming.I.methodName
      |> Seq.toList

    let expected =
      [ "get_Property"
        "set_Property"
        "get_Property2"
        "set_Property2"
        "#ctor"
        "get_Property"
        "set_Property"
        "#ctor"
        "get_Visits"
        "Log"
        "GetOperandType"
        "#ctor"
        ".cctor"
        "get_Eager"
        "set_Eager"
        "get_Property"
        "set_Property"
        "get_ReportFile"
        "set_ReportFile"
        "get_Timer"
        "set_Timer"
        "get_Token"
        "set_Token"
        "get_CoverageFormat"
        "set_CoverageFormat"
        "get_Sample"
        "set_Sample"
        "get_Modules"
        "set_Modules"
        "ToList"
        "#ctor"
        "#ctor"
        "get_Assembly"
        "get_Configuration"
        "set_Assembly"
        "set_Configuration" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let FullMethodNamesAreExtracted () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.map Naming.fullMethodName
      |> Seq.toList

    let expected =
      [ "System.Int32 AltCover.Sample3.Class1.get_Property()"
        "System.Void AltCover.Sample3.Class1.set_Property(System.Int32)"
        "System.Int32 AltCover.Sample3.Class1.get_Property2()"
        "System.Void modreq(System.Runtime.CompilerServices.IsExternalInit) AltCover.Sample3.Class1.set_Property2(System.Int32)"
        "System.Void AltCover.Sample3.Class1.#ctor()"
        "System.Int32 AltCover.Sample3.Class2.get_Property()"
        "System.Void AltCover.Sample3.Class2.set_Property(System.Int32)"
        "System.Void AltCover.Sample3.Class2.#ctor()"
        "System.Collections.Generic.List`1 AltCover.Sample3.Class3.get_Visits()"
        "System.Void AltCover.Sample3.Class3.Log(System.String,System.Int32)"
        "System.Int32 AltCover.Sample3.Class3.GetOperandType(Mono.Cecil.Cil.Instruction)"
        "System.Void AltCover.Sample3.Class3.#ctor()"
        "System.Void AltCover.Sample3.Class3..cctor()"
        "System.Boolean AltCover.Sample3.Class3+Class4.get_Eager()"
        "System.Void AltCover.Sample3.Class3+Class4.set_Eager(System.Boolean)"
        "AltCover.Sample3.Class1 AltCover.Sample3.Class3+Class4.get_Property()"
        "System.Void AltCover.Sample3.Class3+Class4.set_Property(AltCover.Sample3.Class1)"
        "System.String AltCover.Sample3.Class3+Class4.get_ReportFile()"
        "System.Void AltCover.Sample3.Class3+Class4.set_ReportFile(System.String)"
        "System.Int64 AltCover.Sample3.Class3+Class4.get_Timer()"
        "System.Void AltCover.Sample3.Class3+Class4.set_Timer(System.Int64)"
        "System.String AltCover.Sample3.Class3+Class4.get_Token()"
        "System.Void AltCover.Sample3.Class3+Class4.set_Token(System.String)"
        "System.Int32 AltCover.Sample3.Class3+Class4.get_CoverageFormat()"
        "System.Void AltCover.Sample3.Class3+Class4.set_CoverageFormat(System.Int32)"
        "System.Int32 AltCover.Sample3.Class3+Class4.get_Sample()"
        "System.Void AltCover.Sample3.Class3+Class4.set_Sample(System.Int32)"
        "System.String[] AltCover.Sample3.Class3+Class4.get_Modules()"
        "System.Void AltCover.Sample3.Class3+Class4.set_Modules(System.String[])"
        "System.Collections.Generic.List`1<T> AltCover.Sample3.Class3+Class4.ToList<T>(T)"
        "System.Void AltCover.Sample3.Class3+Class4.#ctor()"
        "System.String AltCover.Recorder.InstrumentationAttribute.get_Assembly()"
        "System.String AltCover.Recorder.InstrumentationAttribute.get_Configuration()"
        "System.Void AltCover.Recorder.InstrumentationAttribute.#ctor()"
        "System.Void AltCover.Recorder.InstrumentationAttribute.set_Assembly(System.String)"
        "System.Void AltCover.Recorder.InstrumentationAttribute.set_Configuration(System.String)" ]

    Assert.That(names, Is.EquivalentTo expected)

  // Report.fs
  let TTBaseline =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\">
<method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"true\" instrumented=\"false\" >
<seqpnt visitcount=\"1\" line=\"11\" column=\"3\"  endline=\"11\" endcolumn=\"4\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"12\" column=\"4\" endline=\"12\" endcolumn=\"27\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"13\" column=\"4\" endline=\"13\" endcolumn=\"24\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"14\" column=\"4\" endline=\"14\" endcolumn=\"5\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"15\" column=\"5\" endline=\"15\" endcolumn=\"77\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"16\" column=\"4\" endline=\"16\" endcolumn=\"5\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"18\" column=\"4\" endline=\"18\" endcolumn=\"5\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"19\" column=\"5\" endline=\"19\" endcolumn=\"50\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"20\" column=\"4\" endline=\"20\" endcolumn=\"5\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"21\" column=\"3\"  endline=\"21\" endcolumn=\"4\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
</method>
</module>
</coverage>"

  let TriviaBaseline =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\">
<method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"true\" instrumented=\"false\" >
<seqpnt visitcount=\"1\" line=\"12\" column=\"4\" endline=\"12\" endcolumn=\"27\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"13\" column=\"4\" endline=\"13\" endcolumn=\"24\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"15\" column=\"5\" endline=\"15\" endcolumn=\"77\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"19\" column=\"5\" endline=\"19\" endcolumn=\"50\" excluded=\"true\" document=\"Sample1\\Program.cs\" />
</method>
</module>
</coverage>"

  [<TailCall>]
  let rec private recursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length
    Assert.That(rcount, Is.EqualTo(ecount), "Mismatch at depth " + depth.ToString())

    Seq.zip result expected
    |> Seq.iter (fun (r: XElement, e: XElement) ->
      Assert.That(r.Name, Is.EqualTo(e.Name), "Expected name " + e.Name.ToString())
      let ra = r.Attributes()
      let ea = e.Attributes()

      Seq.zip ra ea
      |> Seq.iter (fun (a1: XAttribute, a2: XAttribute) ->
        Assert.That(a1.Name, Is.EqualTo(a2.Name))

        match a1.Name.ToString() with
        | "profilerVersion"
        | "driverVersion"
        | "moduleId"
        | "metadataToken"
        | "startTime"
        | "measureTime" -> ()
        | "document" ->
          Assert.That(
            a1.Value.Replace("\\", "/"),
            Does.EndWith(a2.Value.Replace("\\", "/")),
            a1.Name.ToString()
            + " : "
            + r.ToString()
            + " -> document"
          )
        | "visitcount" ->
          let expected = Maybe zero "0" a2.Value

          Assert.That(a1.Value, Is.EqualTo(expected), r.ToString() + " -> visitcount")
        | _ ->
          Assert.That(
            a1.Value.Replace("\\", "/"),
            Is.EqualTo(a2.Value.Replace("\\", "/")),
            r.ToString() + " -> " + a1.Name.ToString()
          ))

      recursiveValidate (r.Elements()) (e.Elements()) (depth + 1) zero)

  let makeDocument (f: Stream -> unit) =
    use stash = new MemoryStream()
    stash |> f
    stash.Position <- 0L
    XDocument.Load stash

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNet () =
    let visitor, document =
      Report.reportGenerator ()

    let path = sample1path

    try
      "Main"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Method
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      use def = AssemblyResolver.ReadAssembly path

      let xml = TTBaseline

      let xml' =
        xml.Replace("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())

      let xml'' =
        xml'.Replace("name=\"Sample1.exe\"", "name=\"" + path + "\"")

      let baseline =
        XDocument.Load(new System.IO.StringReader(xml''))

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidate result expected 0 true
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithoutTriviaFromDotNet () =
    let visitor, document =
      Report.reportGenerator ()

    let path = sample1path
    Main.init ()

    try
      "Main"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Method
          >> CoverageParameters.nameFilters.Add)

      CoverageParameters.trivia.Value <- true

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      use def = AssemblyResolver.ReadAssembly path

      let xml = TriviaBaseline

      let xml' =
        xml.Replace("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())

      let xml'' =
        xml'.Replace("name=\"Sample1.exe\"", "name=\"" + path + "\"")

      let baseline =
        XDocument.Load(new System.IO.StringReader(xml''))

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidate result expected 0 true

      CoverageParameters.makeConfiguration ()

      test
        <@
          CoverageParameters.configurationHash = Some
            "UYZ+0a5G0Qzl3yx3ivPJjoKwu/Wb8OnzVaBLMTTKNzA="
        @>
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trivia.Value <- false
      CoverageParameters.configurationHash <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithEmbeds () =
    let visitor, document =
      Report.reportGenerator ()

    let path =
      Path.Combine(
        SolutionDir(),
        "_Binaries/CSharpGeneratedDemo/Debug+AnyCPU/netcoreapp3.1/CSharpGeneratedDemo.dll"
      )

    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      //printfn "%A" (makeDocument document)
      let results =
        (makeDocument document).Descendants("altcover.file".X)
        |> Seq.toList

      Assert.That(results |> List.length, Is.EqualTo 9)

      results
      |> Seq.iter (fun x ->
        let doc = x.Attribute("document".X)
        Assert.That(doc, Is.Not.Null, x.ToString())
        let path = doc.Value
        Assert.That(path |> File.Exists, Is.False, path))

      let lead = results |> Seq.head
      let prev = lead.PreviousNode :?> XElement
      Assert.That(prev, Is.Not.Null)
      Assert.That(prev.Name, Is.EqualTo("method".X))

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedNCoverReportWithOverloads () =
    let visitor, document =
      Report.reportGenerator ()

    let path = sample32path

    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      // printfn "%A" (makeDocument document)

      let doc = makeDocument document

      let results =
        doc.Descendants("method".X)
        |> Seq.map _.Attribute("name".X).Value
        |> Seq.toList

      //printfn "%A" results

      Assert.That(results |> List.length, Is.EqualTo 7)
      Assert.That(results |> List.distinct |> List.length, Is.EqualTo 7)

      let expected =
        [ "Main"
          "GetService"
          "AddSingleton`3"
          "AddSingleton`4"
          "<AddSingleton>b__1_0"
          "<AddSingleton>b__2_0"
          "<AddSingleton>b__2_1" ]

      Assert.That(results, Is.EquivalentTo expected)

      let results =
        doc.Descendants("method".X)
        |> Seq.map _.Attribute("fullname".X).Value
        |> Seq.toList

      let expected =
        [ "T issue222.Class1+<>c__1`3.<AddSingleton>b__1_0(System.IServiceProvider)"
          "T issue222.Class1+<>c__2`4.<AddSingleton>b__2_0(System.IServiceProvider)"
          "T issue222.Class1+<>c__2`4.<AddSingleton>b__2_1(System.IServiceProvider)"
          "System.Void issue222.Class1.AddSingleton<T1,T2,T>(issue222.IServiceCollection,System.Func`2)"
          "System.Void issue222.Class1.AddSingleton<T1,T2,T3,T>(issue222.IServiceCollection,System.Func`2)"
          "System.Void Sample32.Program.Main(System.String[])"
          "T issue222.Class1.GetService<T>(System.IServiceProvider)" ]

      Assert.That(results, Is.EquivalentTo expected)

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithOverloads () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let path = sample32path

    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      // printfn "%A" (makeDocument document)

      let results =
        (makeDocument document).Descendants("Name".X)
        |> Seq.map _.Value
        |> Seq.toList

      //printfn "%A" results

      Assert.That(results |> List.length, Is.EqualTo 8)
      Assert.That(results |> List.distinct |> List.length, Is.EqualTo 8)

      let expected =
        [ "System.Void Sample32.Program::Main(System.String[])"
          "System.Void Sample32.Program::.ctor()"
          "T issue222.Class1::GetService(System.IServiceProvider)"
          "System.Void issue222.Class1::AddSingleton`3(issue222.IServiceCollection,System.Func`2<System.IServiceProvider,T>)"
          "System.Void issue222.Class1::AddSingleton`4(issue222.IServiceCollection,System.Func`2<System.IServiceProvider,T>)"
          "T issue222.Class1/<>c__1`3::<AddSingleton>b__1_0(System.IServiceProvider)"
          "T issue222.Class1/<>c__2`4::<AddSingleton>b__2_0(System.IServiceProvider)"
          "T issue222.Class1/<>c__2`4::<AddSingleton>b__2_1(System.IServiceProvider)" ]

      Assert.That(results, Is.EquivalentTo expected)

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithPartials () =
    let visitor, document =
      Report.reportGenerator ()

    let path =
      Path.Combine(SolutionDir(), "AltCover.TestData/SimpleMix.exe")

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      //printfn "%A" (makeDocument document)
      let results = (makeDocument document)

      let methods =
        results.Descendants("method".X) |> Seq.toList

      let classes =
        methods
        |> List.groupBy _.Attribute("class".X).Value

      let documents =
        classes
        |> List.map (fun (n, ml) ->
          n,
          ml
          |> Seq.map (fun m ->
            m.Descendants("seqpnt".X)
            |> Seq.map _.Attribute("document".X).Value
            |> Seq.distinct
            |> Seq.toList))

      let classdocs =
        documents
        |> List.map (fun (n, dl) -> (n, dl |> Seq.concat |> Seq.distinct |> Seq.toList))

      let classcount =
        classdocs
        |> List.map (fun (n, dl) -> (n, dl |> List.length))
        |> List.sortBy fst

      // printfn "%A" classcount

      // snd > 1 => partial class at least
      test
        <@
          classcount = [ ("<CrtImplementationDetails>.ModuleLoadException", 1)
                         ("<CrtImplementationDetails>.ModuleLoadExceptionHandlerException",
                          1)
                         ("<CrtImplementationDetails>.ModuleUninitializer", 1)
                         ("<Module>", 10)
                         ("Example", 2) ]
        @>

      let mcount =
        documents
        |> List.map (fun (n, ml) -> (n, ml |> Seq.maxBy List.length |> List.length))
        |> List.sortBy fst

      // snd > 1 => inlined method at least
      test
        <@
          mcount = [ ("<CrtImplementationDetails>.ModuleLoadException", 1)
                     ("<CrtImplementationDetails>.ModuleLoadExceptionHandlerException", 1)
                     ("<CrtImplementationDetails>.ModuleUninitializer", 1)
                     ("<Module>", 2)
                     ("Example", 2) ]
        @>

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  let internal makeJson (f: Stream -> unit) =
    use stash = new MemoryStream()
    stash |> f
    stash.Position <- 0L
    use reader = new StreamReader(stash)
    reader.ReadToEnd()

  [<Test>]
  let ShouldGenerateExpectedJsonReportFromDotNet () =
    CoverageParameters.theReportFormat <- Some ReportFormat.NativeJson

    try
      let visitor, document =
        Main.I.selectReportGenerator ()

      let path =
        Path.Combine(SolutionDir(), "_Binaries/Sample4/Debug+AnyCPU/net9.0/Sample4.dll")

      "Main"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Method
          >> CoverageParameters.nameFilters.Add)

      CoverageParameters.trackingNames.Add("testMakeUnion")

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo(
          ReportFormat.NativeJson
          ||| ReportFormat.WithTracking
        )
      )

      let result = makeJson document

      let nativeJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample4.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(nativeJson)

      use reader = new StreamReader(stream)

      let expected =
        reader
          .ReadToEnd()
          .Replace(
            "/_//Samples/Sample4/Tests.fs", // Not compiled deterministic
            (Path.Combine(SolutionDir(), "Samples/Sample4/Tests.fs")
             |> canonicalPath)
              .Replace("\\", "\\\\")
          )
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])
      //printfn "expected %A" expected
      //printfn "result %s" result
      Assert.That(
        result
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |]),
        Is.EqualTo expected
      )
    finally
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedJsonReportWithOverloads () =
    CoverageParameters.theReportFormat <- Some ReportFormat.NativeJson

    try
      let visitor, document =
        Main.I.selectReportGenerator ()

      let path = sample32path

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      Assert.That(CoverageParameters.reportFormat (), Is.EqualTo(ReportFormat.NativeJson))

      let result = makeJson document

      let json =
        result |> Manatee.Json.JsonValue.Parse

      let s1 =
        json.Object |> Seq.map _.Value |> Seq.toList

      let collapse =
        List.collect (fun (v: Manatee.Json.JsonValue) -> v.Object |> Seq.toList)
        >> List.map _.Value

      let key =
        List.collect (fun (v: Manatee.Json.JsonValue) -> v.Object |> Seq.toList)
        >> List.map _.Key

      let s4 = s1 |> collapse |> collapse |> key

      let expected =
        [ "T issue222.Class1::GetService(System.IServiceProvider)"
          "System.Void issue222.Class1::AddSingleton`3(issue222.IServiceCollection,System.Func`2<System.IServiceProvider,T>)"
          "System.Void issue222.Class1::AddSingleton`4(issue222.IServiceCollection,System.Func`2<System.IServiceProvider,T>)"
          "System.Void Sample32.Program::Main(System.String[])" ]

      Assert.That(s4, Is.EquivalentTo expected)

    finally
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedJsonReportWithPartials () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let path =
      Path.Combine(SolutionDir(), "AltCover.TestData/SimpleMix.exe")

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.NativeJson

      let visitor, document =
        Main.I.selectReportGenerator ()

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let result = makeJson document
      //printfn "%A" result
      let json = NativeJson.fromJsonText result
      Assert.That(json.Count, Is.EqualTo 1)
      let documents = json.Values |> Seq.head

      Assert.That(documents.Count, Is.EqualTo 10)

      let classdocs =
        documents
        |> Seq.collect (fun kvp -> kvp.Value.Keys |> Seq.map (fun k -> (k, kvp.Key)))
        |> Seq.groupBy fst
        |> Seq.toList

      let classcount =
        classdocs
        |> List.map (fun (n, dl) -> (n, dl |> Seq.length))
        |> List.sortBy fst

      // printfn "%A" classcount

      // snd > 1 => partial class at least
      test
        <@
          classcount = [ ("<CrtImplementationDetails>.ModuleLoadException", 1)
                         ("<CrtImplementationDetails>.ModuleLoadExceptionHandlerException",
                          1)
                         ("<CrtImplementationDetails>.ModuleUninitializer", 1)
                         ("<Module>", 10)
                         ("Example", 2) ]
        @>

      let mcount =
        documents
        |> Seq.collect (fun kvp ->
          kvp.Value
          |> Seq.collect (fun kv ->
            kv.Value.Keys
            |> Seq.map (fun k -> kv.Key, (k, kvp.Key))))

        |> Seq.groupBy fst
        |> Seq.map (fun (n, l) ->
          n,
          (l
           |> Seq.map snd
           |> Seq.groupBy fst
           |> Seq.map (fun (_, dl) -> dl |> Seq.map snd |> Seq.distinct |> Seq.length))
          |> Seq.max)
        |> Seq.toList
        |> List.sortBy fst

      //mcount
      //|> Seq.iter (printfn "%A")

      // snd > 1 => inlined method at least
      test
        <@
          mcount = [ ("<CrtImplementationDetails>.ModuleLoadException", 1)
                     ("<CrtImplementationDetails>.ModuleLoadExceptionHandlerException", 1)
                     ("<CrtImplementationDetails>.ModuleUninitializer", 1)
                     ("<Module>", 2)
                     ("Example", 2) ]
        @>
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedJsonReportWithEmbeds () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let path =
      Path.Combine(
        SolutionDir(),
        "_Binaries/CSharpGeneratedDemo/Debug+AnyCPU/netcoreapp3.1/CSharpGeneratedDemo.dll"
      )

    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.NativeJson

      let visitor, document =
        Main.I.selectReportGenerator ()

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let result = makeJson document
      //printfn "%A" result
      let json = NativeJson.fromJsonText result
      Assert.That(json.Count, Is.EqualTo 1)
      Assert.That(json.["CSharpGeneratedDemo.dll"].Count, Is.EqualTo 16)
      let dict = json.["CSharpGeneratedDemo.dll"]

      let embeds =
        dict.Keys
        |> Seq.map (fun k ->
          let file = k |> File.Exists |> not

          let embed =
            dict.[k].ContainsKey "\u00ABAltCover.embed\u00BB"

          Assert.That(file, Is.EqualTo embed, k)
          if file then 1 else 0)
        |> Seq.toList

      test
        <@
          embeds = [ 1
                     1
                     1
                     1
                     1
                     1
                     1
                     1
                     1
                     0
                     0
                     0
                     0
                     0
                     0
                     0 ]
        @>

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly () =
    let visitor, document =
      Report.reportGenerator ()

    let path = sample4path

    try
      CoverageParameters.methodPoint.Value <- true

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      (makeDocument document).Descendants(XName.Get "method")
      |> Seq.iter (fun mx ->
        let sx = mx.Descendants(XName.Get "seqpnt")
        test <@ sx |> Seq.length = 1 @>)
    finally
      CoverageParameters.methodPoint.Value <- false

  [<Test>]
  let ShouldGenerateExpectedXmlReportForNCoverWithTopLevel () =
    let path = sample4path

    let path5 =
      sample4path
        .Replace("4", "5")
        .Replace("572", "472")
        .Replace("net9.0", "netstandard2.0")

    let path6 =
      sample4path.Replace("4", "6").Replace("672", "472").Replace("2.1", "2.0")

    try
      AltCover.Main.init ()

      let visitor1, document1 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor1 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let names1 =
        (makeDocument document1).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        //  |> Seq.map (fun mx -> mx.Attribute(XName.Get "name").Value)
        //  |> Seq.filter (fun n -> n <> "Main")
        //  |> Seq.sortBy (fun n -> BitConverter.ToInt32(
        //                           n.ToCharArray()
        //                           |> Seq.take 4
        //                           |> Seq.rev
        //                           |> Seq.map byte
        //                           |> Seq.toArray,
        //                           0))
        |> Seq.toList

      test <@ List.isEmpty names1 @>

      { Scope = Attribute
        Regex = Regex "NoComparison"
        Sense = Exclude }
      |> CoverageParameters.nameFilters.Add

      let visitor2, document2 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor2 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let names2 =
        (makeDocument document2).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map _.Attribute(XName.Get "name").Value
        |> Seq.filter (fun n -> n <> "Main")
        |> Seq.sortBy (fun n ->
          BitConverter.ToInt32(
            n.ToCharArray()
            |> Seq.take 4
            |> Seq.rev
            |> Seq.map byte
            |> Seq.toArray,
            0
          ))
        |> Seq.toList

      test
        <@
          names2 = [ "bytes"
                     "makeThing"
                     "testMakeThing" ]
        @>

      { Scope = Attribute
        Regex = Regex "AutoSerializable"
        Sense = Exclude }
      |> CoverageParameters.topLevel.Add

      let visitor3, document3 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor3 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let names3 =
        (makeDocument document3).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map _.Attribute(XName.Get "name").Value
        |> Seq.filter (fun n -> n <> "Main")
        |> Seq.sortBy (fun n ->
          BitConverter.ToInt32(
            n.ToCharArray()
            |> Seq.take 4
            |> Seq.rev
            |> Seq.map byte
            |> Seq.toArray,
            0
          ))
        |> Seq.toList

      test <@ names3 = [ "makeThing"; "testMakeThing" ] @>

      CoverageParameters.topLevel.Clear()

      { Scope = Type
        Regex = Regex "Thing"
        Sense = Exclude }
      |> CoverageParameters.topLevel.Add

      let visitor5, document5 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor5 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let names5 =
        (makeDocument document5).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map _.Attribute(XName.Get "name").Value
        |> Seq.filter (fun n -> n <> "Main")
        |> Seq.sortBy (fun n ->
          BitConverter.ToInt32(
            n.ToCharArray()
            |> Seq.take 4
            |> Seq.rev
            |> Seq.map byte
            |> Seq.toArray,
            0
          ))
        |> Seq.toList

      test <@ names5 = [ "makeThing"; "testMakeThing" ] @>

      CoverageParameters.topLevel.Clear()
      CoverageParameters.nameFilters.Clear()

      { Scope = Method
        Regex = Regex "F1"
        Sense = Exclude }
      |> CoverageParameters.nameFilters.Add

      { Scope = Method
        Regex = Regex "F2"
        Sense = Exclude }
      |> CoverageParameters.nameFilters.Add

      let visitor6, document6 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor6 ]
        (Visitor.I.toSeq
          { AssemblyPath = path6
            Identity = Hallmark.Build()
            Destinations = [] })

      let names6 =
        (makeDocument document6).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "false")
        //  |> Seq.map (fun mx -> (mx.Attribute(XName.Get "name").Value + "    ",
        //                         mx.Attribute(XName.Get "class").Value))
        //  |> Seq.sortBy (fun (n, _) -> BitConverter.ToInt32(
        //                                 n.ToCharArray()
        //                                 |> Seq.take 4
        //                                 |> Seq.rev
        //                                 |> Seq.map byte
        //                                 |> Seq.toArray,
        //                                 0))
        //  |> Seq.map (fun (n,c) -> c + "." + n.Trim())
        |> Seq.toList

      test <@ names6 |> List.isEmpty @>

      { Scope = Method
        Regex = Regex "aux"
        Sense = Exclude }
      |> CoverageParameters.topLevel.Add

      { Scope = Method
        Regex = Regex "fetchUrlAsync"
        Sense = Exclude }
      |> CoverageParameters.topLevel.Add

      let visitor7, document7 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor7 ]
        (Visitor.I.toSeq
          { AssemblyPath = path6
            Identity = Hallmark.Build()
            Destinations = [] })

      let names7 =
        (makeDocument document7).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map (fun mx ->
          (mx.Attribute(XName.Get "name").Value + "    ",
           mx.Attribute(XName.Get "class").Value))
        |> Seq.sortBy (fun (n, _) ->
          BitConverter.ToInt32(
            n.ToCharArray()
            |> Seq.take 4
            |> Seq.rev
            |> Seq.map byte
            |> Seq.toArray,
            0
          ))
        |> Seq.map (fun (n, c) -> c + "." + n.Trim())
        |> Seq.toList

      test
        <@
          names7 = [ "Sample6.Module.F1"
                     "Sample6.Module.F2"
                     "Sample6.Module+FII@12T.Invoke"
                     "Sample6.Module+FI@11T.Invoke"
                     "Sample6.Module+F1@19.Invoke" ]
        @>

      CoverageParameters.topLevel.Clear()
      CoverageParameters.nameFilters.Clear()

      { Scope = Attribute
        Regex = Regex "Exclude"
        Sense = Exclude }
      |> CoverageParameters.nameFilters.Add

      let visitor8, document8 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor8 ]
        (Visitor.I.toSeq
          { AssemblyPath = path5
            Identity = Hallmark.Build()
            Destinations = [] })

      let names8 =
        (makeDocument document8).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "false")
        //  |> Seq.map (fun mx -> (mx.Attribute(XName.Get "name").Value + "    ",
        //                         mx.Attribute(XName.Get "class").Value))
        //  |> Seq.sortBy (fun (n, _) -> BitConverter.ToInt32(
        //                                 n.ToCharArray()
        //                                 |> Seq.take 4
        //                                 |> Seq.rev
        //                                 |> Seq.map byte
        //                                 |> Seq.toArray,
        //                                 0))
        //  |> Seq.map (fun (n,c) -> c + "." + n.Trim())
        |> Seq.toList

      test <@ names8 |> List.isEmpty @>

      CoverageParameters.topLevel.Clear()

      { Scope = Method
        Regex = Regex "Interior"
        Sense = Exclude }
      |> CoverageParameters.topLevel.Add

      let seqTrim (s: String seq) = s |> Seq.map _.Trim()

      let visitor9, document9 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor9 ]
        (Visitor.I.toSeq
          { AssemblyPath = path5
            Identity = Hallmark.Build()
            Destinations = [] })

      let names9 =
        (makeDocument document9).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "false")
        |> Seq.map (fun mx -> mx.Attribute(XName.Get "name").Value + "    ")
        //  |> Seq.sortBy (fun n -> BitConverter.ToInt32(
        //                           n.ToCharArray()
        //                           |> Seq.take 4
        //                           |> Seq.rev
        //                           |> Seq.map byte
        //                           |> Seq.toArray,
        //                           0))
        |> seqTrim
        |> Seq.toList

      test <@ names9 = [ "<F1>g__Interior|0_1" ] @>

      CoverageParameters.nameFilters.Clear()

      let visitor4, document4 =
        Report.reportGenerator ()

      Visitor.visit
        [ visitor4 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let names4 =
        (makeDocument document4).Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        //  |> Seq.map (fun mx -> mx.Attribute(XName.Get "name").Value)
        //  |> Seq.filter (fun n -> n <> "Main")
        //  |> Seq.sortBy (fun n -> BitConverter.ToInt32(
        //                           n.ToCharArray()
        //                           |> Seq.take 4
        //                           |> Seq.rev
        //                           |> Seq.map byte
        //                           |> Seq.toArray,
        //                           0))
        |> Seq.toList

      test <@ List.isEmpty names4 @>

    finally
      Main.init ()

  [<Test>]
  let ShouldGenerateExpectedXmlReportForOpenCoverWithMethodPointOnly () =
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample4path

    try
      CoverageParameters.methodPoint.Value <- true
      CoverageParameters.theReportFormat <- None

      let visitor, document =
        Main.I.selectReportGenerator () //OpenCover.reportGenerator()

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      (makeDocument document).Descendants(XName.Get "Method")
      |> Seq.iter (fun mx ->
        let sx =
          mx.Descendants(XName.Get "SequencePoint")

        test <@ sx |> Seq.length = 1 @>)
    finally
      CoverageParameters.methodPoint.Value <- false

  [<Test>]
  let LocateMatchFallsBackOK () =
    let file =
      Assembly.GetExecutingAssembly().Location

    let empty = Dictionary<string, string>()
    test <@ Visitor.I.locateMatch file empty = file @>

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithSourceLink () =
    let visitor, document =
      Report.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let here = SolutionDir()

    let path =
      Path.Combine(here, "_SourceLink/Sample14.dll")

    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.sourcelink.Value <- true
      CoverageParameters.staticFilter <- Some StaticFilter.NoFilter

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      Assert.That(
        Visitor.sourceLinkDocuments |> Option.isSome,
        "Documents should be present"
      )

      let map =
        Visitor.sourceLinkDocuments |> Option.get

      let url =
        map.Values
        |> Seq.find _.EndsWith("*", StringComparison.Ordinal)

      let files =
        (makeDocument document).Descendants(XName.Get "seqpnt")
        |> Seq.map _.Attribute(XName.Get "document").Value
        |> Seq.distinct
        |> Seq.filter _.StartsWith("https://", StringComparison.Ordinal)
        |> Seq.sort
        |> Seq.toList

      let expected =
        [ url.Replace("*", "Samples/Sample14/Sample14/Program.cs")
          url.Replace("*", "Samples/Sample5/Class1.cs") ]

      Assert.That(files, Is.EquivalentTo expected)

      let untracked =
        (makeDocument document).Descendants(XName.Get "seqpnt")
        |> Seq.map _.Attribute(XName.Get "document").Value
        |> Seq.distinct
        |> Seq.filter (
          _.StartsWith("https://", StringComparison.Ordinal)
          >> not
        )
        |> Seq.map Path.GetFileName
        |> Seq.sort
        |> Seq.toList

      let expected2 =
        [ "Class2.cs"
          "Sample14.SourceLink.Class3.cs" ]

      Assert.That(untracked, Is.EquivalentTo expected2)

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.sourcelink.Value <- false
      CoverageParameters.staticFilter <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter () =
    let visitor, document =
      Report.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Path
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      use def = AssemblyResolver.ReadAssembly path

      let xml = TTBaseline

      let xml' =
        xml
          .Replace("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())
          .Replace(
            "excluded=\"true\" instrumented=\"false\"",
            "excluded=\"false\" instrumented=\"true\""
          )

      let xml'' =
        xml'.Replace("name=\"Sample1.exe\"", "name=\"" + path + "\"")

      let baseline =
        XDocument.Load(new System.IO.StringReader(xml''))

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidate result expected 0 true
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded () =
    let visitor, document =
      Report.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      "Sample"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Module
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      use def = AssemblyResolver.ReadAssembly path

      let xml =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\" excluded=\"true\" />
</coverage>"

      let xml' =
        xml.Replace("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())

      let xml'' =
        xml'.Replace("name=\"Sample1.exe\"", "name=\"" + path + "\"")

      let baseline =
        XDocument.Load(new System.IO.StringReader(xml''))

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidate result expected 0 true
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked () =
    let visitor, document =
      Report.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path
    CoverageParameters.theReportFormat <- Some ReportFormat.NCover

    try
      "Sample"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Module
          >> CoverageParameters.nameFilters.Add)

      CoverageParameters.trackingNames.Add("Main")

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      use def = AssemblyResolver.ReadAssembly path

      let xml =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\" excluded=\"true\" />
</coverage>"

      let xml' =
        xml.Replace("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())

      let xml'' =
        xml'.Replace("name=\"Sample1.exe\"", "name=\"" + path + "\"")

      let baseline =
        XDocument.Load(new System.IO.StringReader(xml''))

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidate result expected 0 true
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None

  // Gendarme.fs (except where I need to compare with the original, which are the ValidateGendarmeEmulation tests)
  [<Test>]
  let ShouldDetectTernary () =
    let path =
      Path.Combine(
        SolutionDir(),
        "_Binaries/Sample3/Debug+AnyCPU/netstandard2.0/Sample3.dll"
      )

    use def = AssemblyResolver.ReadAssembly path

    let target =
      def.MainModule.GetType("AltCover.Sample3.Class2").GetMethods()
      |> Seq.filter (fun m -> m.Name = "set_Property")
      |> Seq.head

    let ternary =
      target.Body.Instructions
      |> Seq.cast<Cil.Instruction>
      |> Seq.filter (fun i -> i.OpCode.FlowControl = FlowControl.Branch)
      |> Seq.fold
        (fun state i ->
          state
          + (Gendarme.I.``detect ternary pattern``
             <| Some i.Previous.OpCode.Code))
        0

    Assert.That(ternary, Is.EqualTo 1)
    Assert.That(Gendarme.I.``detect ternary pattern`` None, Is.EqualTo 0)
    Assert.That(Gendarme.cyclomaticComplexity target, Is.EqualTo 3)

    Assert.That(
      Gendarme.I.switchCyclomaticComplexity target.Body.Instructions,
      Is.EqualTo 3
    )

  [<Test>]
  let ShouldDetectSwitchNesting () =
    let path =
      Path.Combine(
        SolutionDir(),
        "_Binaries/Sample3/Debug+AnyCPU/netstandard2.0/Sample3.dll"
      )

    use def = AssemblyResolver.ReadAssembly path

    let target =
      def.MainModule.GetType("AltCover.Sample3.Class3").GetMethods()
      |> Seq.filter (fun m -> m.Name = "GetOperandType")
      |> Seq.head

    Assert.That(
      Gendarme.I.switchCyclomaticComplexity target.Body.Instructions,
      Is.EqualTo 24
    )

  // OpenCover.fs
  [<Test>]
  let SafeMultiplyIsSafe () =
    Assert.That(OpenCover.safeMultiply 1 0, Is.EqualTo 1)
    Assert.That(OpenCover.safeMultiply 2 3, Is.EqualTo 6)
    Assert.That(OpenCover.safeMultiply 65536 65536, Is.EqualTo Int32.MaxValue)

  [<Test>]
  let EmptyMethodHasComplexity1 () =
    let m =
      MethodDefinition(
        "dummy",
        MethodAttributes.Abstract,
        TypeDefinition("System", "Void", TypeAttributes.Public)
      )

    Assert.That(Gendarme.cyclomaticComplexity m, Is.EqualTo 1)

  [<Test>]
  let BranchChainsSerialize () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path =
      Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.find (fun m -> m.Name = "as_bar")

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.File
          >> CoverageParameters.nameFilters.Add)

      let branches =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.choose (fun n ->
          match n with
          | BranchPoint b -> Some b
          | _ -> None)
        |> Seq.toList
      // The only overt branching in this function are the 4 match cases
      // Internal IL conditional branching is a compiler thing from inlining "string"
      Assert.That(branches |> Seq.length, Is.EqualTo 4)
      let branch = branches |> Seq.head
      Assert.That(branch.Target.Length, Is.EqualTo 2)
      let xbranch = XElement(XName.Get "test")
      OpenCover.I.setChain xbranch branch
      Assert.That(xbranch.ToString(), Is.EqualTo """<test offsetchain="30" />""")
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let BranchChainsTerminate () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path =
      Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.find (fun m -> m.Name = "as_bar")

    let fin =
      method.Body.Instructions |> Seq.last

    let list = Visitor.I.getJumpChain fin fin
    Assert.That(list, Is.EquivalentTo [ fin ])

  [<TailCall>]
  let rec private recursiveValidateOpenCover result expected' depth zero expectSkipped =
    let xn name = XName.Get(name)
    let rcount = result |> Seq.length

    let expected =
      expected'
      |> Seq.filter (fun (el: XElement) ->
        el.Name.LocalName <> "Module"
        || expectSkipped
        || "skippedDueTo"
           |> xn
           |> el.Attributes
           |> Seq.isEmpty)
      |> Seq.toList

    let ecount = expected |> Seq.length

    Assert.That(
      rcount,
      Is.EqualTo(ecount),
      "Mismatch at depth "
      + depth.ToString()
      + " : "
      + expected.ToString()
      + " but got"
      + (result |> Seq.toList).ToString()
    )

    Seq.zip result expected
    |> Seq.iter (fun (r: XElement, e: XElement) ->
      Assert.That(r.Name, Is.EqualTo(e.Name), "Expected name " + e.Name.ToString())
      let ra = r.Attributes()
      let ea = e.Attributes()

      Seq.zip ra ea
      |> Seq.iter (fun (a1: XAttribute, a2: XAttribute) ->
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
        | "fullPath" ->
          Assert.That(
            a1.Value
              .Replace("\\", "/")
              .Replace("Samples/", String.Empty)
              .Replace("altcover", "AltCover"),
            Does.EndWith(a2.Value.Replace("\\", "/").Replace("altcover", "AltCover")),
            a1.Name.ToString()
            + " : "
            + r.ToString()
            + " -> document"
          )
        | "vc" ->
          let expected = Maybe zero "0" a2.Value

          Assert.That(a1.Value, Is.EqualTo(expected), r.ToString() + " -> visitcount")
        | _ ->
          Assert.That(
            a1.Value,
            Is.EqualTo(a2.Value),
            r.ToString() + " -> " + a1.Name.ToString()
          ))

      recursiveValidateOpenCover
        (r.Elements())
        (e.Elements())
        (depth + 1)
        zero
        expectSkipped)

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithSourceLinkOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let here = SolutionDir()

    let path =
      Path.Combine(here, "_SourceLink/Sample14.dll")

    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover
      CoverageParameters.sourcelink.Value <- true

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      Assert.That(
        Visitor.sourceLinkDocuments |> Option.isSome,
        "Documents should be present"
      )

      let map =
        Visitor.sourceLinkDocuments |> Option.get

      let url =
        map.Values
        |> Seq.find _.EndsWith("*", StringComparison.Ordinal)

      let files =
        (makeDocument document).Descendants(XName.Get "File")
        |> Seq.map _.Attribute(XName.Get "fullPath").Value
        |> Seq.filter _.StartsWith("https://", StringComparison.Ordinal)
        |> Seq.sort
        |> Seq.toList

      let expected =
        [ url.Replace("*", "Samples/Sample14/Sample14/Program.cs")
          url.Replace("*", "Samples/Sample5/Class1.cs") ]

      Assert.That(files, Is.EquivalentTo expected)

      let untracked =
        (makeDocument document).Descendants(XName.Get "File")
        |> Seq.map _.Attribute(XName.Get "fullPath").Value
        |> Seq.filter (
          _.StartsWith("https://", StringComparison.Ordinal)
          >> not
        )
        |> Seq.map Path.GetFileName
        |> Seq.sort
        |> Seq.toList

      let expected2 =
        [ "Class2.cs"
          "Sample14.SourceLink.Class3.cs" ]

      Assert.That(untracked, Is.EquivalentTo expected2)
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.sourcelink.Value <- false
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let path = sample1path
    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithEmbedsOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let path =
      Path.Combine(
        SolutionDir(),
        "_Binaries/CSharpGeneratedDemo/Debug+AnyCPU/netcoreapp3.1/CSharpGeneratedDemo.dll"
      )

    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let expected =
        [ 1
          1
          1
          1
          1
          1
          1
          1
          0
          1
          0
          0
          0
          0
          0
          0 ]
      //printfn "%A" (makeDocument document)
      let result =
        (makeDocument document).Descendants("File".X)
        |> Seq.map (fun f ->
          if
            f.Attribute("fullPath".X).Value
            |> File.Exists
            |> not
          then
            1
          else
            0)
        |> Seq.toList
      // Generated source does not exist at the specified path -- a check on the MSFT inputs
      test <@ result = expected @>

      // but we should have picked up the embedded source
      let embeds =
        (makeDocument document).Descendants("File".X)
        |> Seq.map (fun f ->
          if f.Attribute("altcover.embed".X).IsNotNull then
            1
          else
            0)
        |> Seq.toList

      test <@ embeds = expected @>

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithPartialsOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let path =
      Path.Combine(SolutionDir(), "AltCover.TestData/SimpleMix.exe")

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      // printfn "%A" (makeDocument document)
      let results = (makeDocument document)

      let classes =
        results.Descendants("Class".X)
        |> Seq.map (fun c ->
          (c.Element("FullName".X).Value, c.Descendants("Method".X) |> Seq.toList))
        |> Seq.toList

      let documents =
        classes
        |> List.map (fun (n, ml) ->
          (n,
           ml
           |> Seq.map (fun m ->
             [ m.Descendants("SequencePoint".X)
               m.Descendants("Branch".X) ]
             |> Seq.concat
             |> Seq.map _.Attribute("fileid".X).Value
             |> Seq.distinct
             |> Seq.toList)))

      let classdocs =
        documents
        |> List.map (fun (n, dl) -> (n, dl |> Seq.concat |> Seq.distinct |> Seq.toList))

      let classcount =
        classdocs
        |> List.map (fun (n, dl) -> (n, dl |> List.length))
        |> List.sortBy fst

      // printfn "%A" classcount

      // snd > 1 => partial class at least
      test
        <@
          classcount = [ ("<CrtImplementationDetails>.ModuleLoadException", 1)
                         ("<CrtImplementationDetails>.ModuleLoadExceptionHandlerException",
                          1)
                         ("<CrtImplementationDetails>.ModuleUninitializer", 1)
                         ("<Module>", 10)
                         ("Example", 2) ]
        @>

      let mcount =
        documents
        |> List.map (fun (n, ml) -> (n, ml |> Seq.maxBy List.length |> List.length))
        |> List.sortBy fst

      // snd > 1 => inlined method at least
      test
        <@
          mcount = [ ("<CrtImplementationDetails>.ModuleLoadException", 1)
                     ("<CrtImplementationDetails>.ModuleLoadExceptionHandlerException", 1)
                     ("<CrtImplementationDetails>.ModuleUninitializer", 1)
                     ("<Module>", 2)
                     ("Example", 2) ]
        @>

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path
    let xn name = XName.Get(name)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.coverstyle <- CoverStyle.LineOnly

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)
      // strip out branch information
      baseline.Descendants(xn "Summary")
      |> Seq.filter (fun x -> x.Attribute(xn "numBranchPoints").Value = "3")
      |> Seq.iter (fun x -> x.Attribute(xn "numBranchPoints").Value <- "1")

      baseline.Descendants(xn "Method")
      |> Seq.iter (fun x -> x.Attribute(xn "nPathComplexity").Value <- "0")

      baseline.Descendants(xn "SequencePoint")
      |> Seq.iter (fun x -> x.Attribute(xn "bec").Value <- "0")

      baseline.Descendants(xn "BranchPoint")
      |> Seq.toList
      |> Seq.iter _.Remove()

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path
    let xn name = XName.Get(name)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      CoverageParameters.coverstyle <- CoverStyle.BranchOnly

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)
      // strip out line information
      baseline.Descendants(xn "Summary")
      |> Seq.filter (fun x -> x.Attribute(xn "numSequencePoints").Value = "10")
      |> Seq.iter (fun x -> x.Attribute(xn "numSequencePoints").Value <- "0")

      baseline.Descendants(xn "SequencePoint")
      |> Seq.toList
      |> Seq.iter _.Remove()

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

  let AddTrackingForMain xml =
    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith(xml, StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let tail =
      baseline.Descendants(XName.Get "Module")
      |> Seq.last

    let tracked =
      XElement(XName.Get "TrackedMethods")

    tail.Add(tracked)

    tracked.Add(
      XElement(
        XName.Get "TrackedMethod",
        XAttribute(XName.Get "uid", "1"),
        XAttribute(XName.Get "token", "100663297"),
        XAttribute(
          XName.Get "name",
          "System.Void TouchTest.Program::Main(System.String[])"
        ),
        XAttribute(XName.Get "strategy", "Main")
      )
    )

    baseline

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.Add("Main")
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let baseline =
        AddTrackingForMain "Sample1WithOpenCover.xml"

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.theReportFormat <- None
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      "Sample"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Module
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let raw =
        "<CoverageSession xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <Summary numSequencePoints=\"0\" visitedSequencePoints=\"0\" numBranchPoints=\"0\" visitedBranchPoints=\"0\" sequenceCoverage=\"0\" branchCoverage=\"0\" maxCyclomaticComplexity=\"0\" minCyclomaticComplexity=\"1\" visitedClasses=\"0\" numClasses=\"0\" visitedMethods=\"0\" numMethods=\"0\" />
        <Modules>
        <Module skippedDueTo=\"Filter\" hash=\"C2-87-B9-AA-6B-1D-03-60-30-9A-15-4A-D5-28-87-C2-9E-B9-8E-8D\">
        <ModulePath>_Binaries\\AltCover.TestData\\Debug+AnyCPU\\Sample1.exe</ModulePath>
        <ModuleTime>2018-03-15T14:00:17.3385938Z</ModuleTime>
        <ModuleName>Sample1</ModuleName>
        <Classes />
        </Module>
        </Modules>
        </CoverageSession>"

      let baseline =
        XDocument.Load(new System.IO.StringReader(raw))

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true true
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.Add("Main")
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      "Sample"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Module
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let raw =
        "<CoverageSession xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <Summary numSequencePoints=\"0\" visitedSequencePoints=\"0\" numBranchPoints=\"0\" visitedBranchPoints=\"0\" sequenceCoverage=\"0\" branchCoverage=\"0\" maxCyclomaticComplexity=\"0\" minCyclomaticComplexity=\"1\" visitedClasses=\"0\" numClasses=\"0\" visitedMethods=\"0\" numMethods=\"0\" />
        <Modules>
        <Module skippedDueTo=\"Filter\" hash=\"C2-87-B9-AA-6B-1D-03-60-30-9A-15-4A-D5-28-87-C2-9E-B9-8E-8D\">
        <ModulePath>_Binaries\\AltCover.TestData\\Debug+AnyCPU\\Sample1.exe</ModulePath>
        <ModuleTime>2018-03-15T14:00:17.3385938Z</ModuleTime>
        <ModuleName>Sample1</ModuleName>
        <Classes />
        <TrackedMethods>
        <TrackedMethod uid=\"1\" token=\"100663297\" name=\"System.Void TouchTest.Program::Main(System.String[])\" strategy=\"Main\" />
        </TrackedMethods>
        </Module>
        </Modules>
        </CoverageSession>"

      let baseline =
        XDocument.Load(new System.IO.StringReader(raw))

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true true
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      Assert.That(CoverageParameters.reportFormat (), Is.EqualTo ReportFormat.OpenCover)

      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Type
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample1ClassExclusion.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.Add("Main")

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo(
          ReportFormat.OpenCover
          ||| ReportFormat.WithTracking
        )
      )

      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Type
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let baseline =
        AddTrackingForMain "Sample1ClassExclusion.xml"

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      "Main"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Method
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample1MethodExclusion.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Path
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample1MethodExclusion.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)

      baseline.Descendants(XName.Get "Method")
      |> Seq.filter _.Attributes(XName.Get "skippedDueTo").Any()
      |> Seq.iter (fun x ->
        x.SetAttributeValue(XName.Get "skippedDueTo", "File")

        x.Descendants(XName.Get "Summary")
        |> Seq.toList
        |> Seq.iter _.Remove())
      //|> Seq.iter (fun s -> s.SetAttributeValue(XName.Get "maxCyclomaticComplexity", "2")
      //                      s.SetAttributeValue(XName.Get "minCyclomaticComplexity", "2")))
      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.Add("Main")

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo(
          ReportFormat.OpenCover
          ||| ReportFormat.WithTracking
        )
      )

      "Main"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Method
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let baseline =
        AddTrackingForMain "Sample1MethodExclusion.xml"

      let result =
        (makeDocument document).Elements()

      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let sample21trad =
      Path.Combine(SolutionDir(), "./_Binaries/Sample21/Debug+AnyCPU/net472/Sample21.dll")

    Assert.That(File.Exists sample21trad, "Test file Sample21 for net47 not built")

    try
      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Type
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = sample21trad
            Identity = Hallmark.Build()
            Destinations = [] })

      let doc = makeDocument document

      let classes =
        doc.Descendants(XName.Get "FullName")
        |> Seq.map _.Value
        |> Seq.filter (Seq.head >> Char.IsLetterOrDigit)
        |> Seq.sort
        |> Seq.toList

      let methods =
        doc.Descendants(XName.Get "Name")
        |> Seq.map _.Value
        |> Seq.sort
        |> Seq.toList

      test
        <@
          classes = [ "Sample21.Product"
                      "Sample21.Tests"
                      "Sample21.Traditional" ]
        @>

      let expectedMethods =
        [ "System.String Sample21.Product::Junk(System.String)"
          "System.String Sample21.Traditional::DoSomething()"
          "System.Void Sample21.Product::.ctor(System.String)"
          "System.Void Sample21.Tests::.ctor()"
          "System.Void Sample21.Tests::Setup()"
          "System.Void Sample21.Tests::Test1()"
          "System.Void Sample21.Traditional::.ctor()" ]

      test <@ methods = expectedMethods @>
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let sample21 =
      Path.Combine(SolutionDir(), "./_Binaries/Sample21/Debug+AnyCPU/net9.0/Sample21.dll")

    Assert.That(File.Exists sample21, "Test file Sample21 for net9.0 not built")

    try
      "Program"
      |> (Regex
          >> FilterRegex.Exclude
          >> FilterClass.Build FilterScope.Type
          >> CoverageParameters.nameFilters.Add)

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = sample21
            Identity = Hallmark.Build()
            Destinations = [] })

      let doc = makeDocument document

      let classes =
        doc.Descendants(XName.Get "FullName")
        |> Seq.filter (
          _.Parent.Attribute(XName.Get "skippedDueTo")
          >> isNull
        )
        |> Seq.map _.Value
        |> Seq.filter (Seq.head >> Char.IsLetterOrDigit)
        |> Seq.sort
        |> Seq.filter (
          _.EndsWith("Attribute", StringComparison.Ordinal)
          >> not
        )
        |> Seq.toList

      let methods =
        doc.Descendants(XName.Get "Name")
        |> Seq.map _.Value
        |> Seq.filter (_.Contains("Attribute::.ctor") >> not)
        |> Seq.sort
        |> Seq.toList

      // document.Save(@"C:\Users\steve\Documents\GitHub\altcover\Sample21Modern.xml")

      let expectedTypes =
        [ "Sample21.IModern"
          "Sample21.Modern1"
          "Sample21.Modern2"
          "Sample21.Product"
          "Sample21.Tests"
          "Sample21.Traditional" ]

      test <@ classes = expectedTypes @>

      let expectedMethods =
        [ "System.String Sample21.IModern::DoSomething()"
          "System.String Sample21.Modern2::DoSomething()"
          "System.String Sample21.Product::Junk(System.String)"
          "System.String Sample21.Traditional::DoSomething()"
          "System.Void Sample21.Modern1::.ctor()"
          "System.Void Sample21.Modern2::.ctor()"
          "System.Void Sample21.Product::.ctor(System.String)"
          "System.Void Sample21.Tests::.ctor()"
          "System.Void Sample21.Tests::Setup()"
          "System.Void Sample21.Tests::Test1()"
          "System.Void Sample21.Traditional::.ctor()" ]

      test <@ methods = expectedMethods @>
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldSortFileIds () =
    let visitor, document =
      OpenCover.reportGenerator ()

    let xn name = XName.Get(name)

    let path =
      Path.Combine(
        SolutionDir(),
        "_Binaries/AltCover/Debug+AnyCPU/net8.0/AltCover.Engine.dll"
      )

    Visitor.visit
      [ visitor ]
      (Visitor.I.toSeq
        { AssemblyPath = path
          Identity = Hallmark.Build()
          Destinations = [] })

    let doc = makeDocument document
    Assert.That(doc.Descendants(xn "Module") |> Seq.length, Is.EqualTo 1)
    Assert.That(doc.Descendants(xn "File") |> Seq.length, Is.GreaterThan 1)

    doc.Descendants(xn "File")
    |> Seq.iteri (fun i x ->
      Assert.That(x.Attribute(xn "uid").Value, Is.EqualTo(string (1 + i))))

  let MonoBaseline =
    "<?xml-stylesheet type='text/xsl' href='coverage.xsl'?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
  <module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null\">
    <method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"false\" instrumented=\"true\" fullname=\"System.Void TouchTest.Program.Main(System.String[])\">
      <seqpnt visitcount=\"0\" line=\"11\" column=\"3\" endline=\"11\" endcolumn=\"4\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"12\" column=\"23\" endline=\"12\" endcolumn=\"24\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"4\" endline=\"13\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"12\" endline=\"13\" endcolumn=\"13\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"14\" column=\"4\" endline=\"14\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"5\" endline=\"15\" endcolumn=\"6\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"60\" endline=\"15\" endcolumn=\"61\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"13\" endline=\"15\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"16\" column=\"4\" endline=\"16\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"18\" column=\"4\" endline=\"18\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"5\" endline=\"19\" endcolumn=\"6\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"13\" endline=\"19\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"20\" column=\"4\" endline=\"20\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"21\" column=\"3\" endline=\"21\" endcolumn=\"4\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
    </method>
  </module>
</coverage>"

  [<TailCall>]
  let rec RecursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length
    test' <@ rcount = ecount @> ("Mismatch at depth " + depth.ToString())

    Seq.zip result expected
    |> Seq.iter (fun (r: XElement, e: XElement) ->
      test <@ r.Name = e.Name @>
      let ra = r.Attributes()
      let ea = e.Attributes()

      Seq.zip ra ea
      |> Seq.iter (fun (a1: XAttribute, a2: XAttribute) ->
        test <@ a1.Name = a2.Name @>

        match a1.Name.ToString() with
        | "profilerVersion"
        | "driverVersion"
        | "moduleId"
        | "metadataToken"
        | "startTime"
        | "measureTime" -> ()
        | "document" ->
          test'
            <@ a1.Value.Replace("\\", "/").EndsWith(a2.Value.Replace("\\", "/")) @>
            (a1.Name.ToString()
             + " : "
             + r.ToString()
             + " -> document")
        | "visitcount" ->
          let expected = Maybe zero "0" a2.Value
          test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
        | _ ->
          test'
            <@ a1.Value.Replace("\\", "/") = a2.Value.Replace("\\", "/") @>
            (r.ToString() + " -> " + a1.Name.ToString()))

      RecursiveValidate (r.Elements()) (e.Elements()) (depth + 1) zero)

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromMono () =
    let visitor, documentSource =
      Report.reportGenerator ()

    let path = monoSample1path
    maybeIgnore (fun () -> path |> File.Exists |> not)

    Visitor.visit
      [ visitor ]
      (Visitor.I.toSeq
        { AssemblyPath = path
          Identity = Hallmark.Build()
          Destinations = [] })

    let expectedText =
      MonoBaseline.Replace(
        "name=\"Sample1.exe\"",
        "name=\"" + (path |> Path.GetFullPath) + "\""
      )

    let baseline =
      XDocument.Load(new System.IO.StringReader(expectedText))

    let document =
      use stash = new MemoryStream()
      stash |> documentSource
      stash.Position <- 0L
      XDocument.Load stash

    let result = document.Elements()
    let expected = baseline.Elements()
    RecursiveValidate result expected 0 true

  [<TailCall>]
  let rec RecursiveValidateOpenCover result expected' depth zero expectSkipped =
    let xn name = XName.Get(name)
    let rcount = result |> Seq.length

    let expected =
      expected'
      |> Seq.filter (fun (el: XElement) ->
        el.Name.LocalName <> "Module"
        || expectSkipped
        || "skippedDueTo"
           |> xn
           |> el.Attributes
           |> Seq.isEmpty)
      |> Seq.toList

    let ecount = expected |> Seq.length

    test'
      <@ rcount = ecount @>
      ("Mismatch at depth "
       + depth.ToString()
       + " : "
       + expected.ToString()
       + " but got"
       + (result |> Seq.toList).ToString())

    Seq.zip result expected
    |> Seq.iter (fun (r: XElement, e: XElement) ->
      test <@ r.Name = e.Name @>
      let ra = r.Attributes()
      let ea = e.Attributes()

      Seq.zip ra ea
      |> Seq.iter (fun (a1: XAttribute, a2: XAttribute) ->
        test <@ a1.Name = a2.Name @>

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
        | "fullPath" ->
          test'
            <@
              a1.Value
                .Replace("\\", "/")
                .Replace("altcover", "AltCover")
                .Replace("Samples/", String.Empty)
                .EndsWith(a2.Value.Replace("\\", "/").Replace("altcover", "AltCover"))
            @>
            (a1.Name.ToString()
             + " : "
             + r.ToString()
             + " -> document")
        | "vc" ->
          let expected = Maybe zero "0" a2.Value
          test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
        | _ ->
          test' <@ a1.Value = a2.Value @> (r.ToString() + " -> " + a1.Name.ToString()))

      RecursiveValidateOpenCover
        (r.Elements())
        (e.Elements())
        (depth + 1)
        zero
        expectSkipped)

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle () =
    let visitor, documentSource =
      OpenCover.reportGenerator ()

    let path = monoSample1path
    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("HandRolledMonoCoverage.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)

      ("ModulePath"
       |> XName.Get
       |> baseline.Descendants
       |> Seq.head)
        .SetValue
        path

      let document =
        use stash = new MemoryStream()
        stash |> documentSource
        stash.Position <- 0L
        XDocument.Load stash

      let result = document.Elements()
      let expected = baseline.Elements()
      RecursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None