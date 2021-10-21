namespace Tests
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
open N

#nowarn "25"

[<NUnit.Framework.IncludeExcludeAttribute>]
type ProxyObject() =
  inherit MarshalByRefObject()

  member val Type: Type option = None with get, set
  member val Object = null with get, set

#if !NET472
  member val Context: System.Runtime.Loader.AssemblyLoadContext = null with get, set
#endif

  member this.InstantiateObject(assemblyPath: string, typeName: string, args: obj []) =
#if !NET472
    let assembly =
      this.Context.LoadFromAssemblyPath(assemblyPath) //LoadFrom loads dependent DLLs (assuming they are in the app domain's base directory
#else
    let assembly = Assembly.LoadFrom(assemblyPath) //LoadFrom loads dependent DLLs (assuming they are in the app domain's base directory
#endif
    let t =
      assembly.ExportedTypes
      |> Seq.filter (fun t -> t.FullName = typeName)

    this.Type <- Seq.tryHead t
    this.Object <- Activator.CreateInstance(this.Type |> Option.get, args)

  member this.InvokeMethod(methodName: string, args: obj []) =
    let t = this.Type |> Option.get
    let methodinfo = t.GetMethod(methodName)
    methodinfo.Invoke(this.Object, args)

[<AutoOpen>]
module Extensions =
  type internal Exemption with
    static member internal OfInt i =
      if i > 0 then
        Exemption.Visited
      else if i < -4 then
        Exemption.None
      else
        i
        |> sbyte
        |> Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<sbyte, Exemption>

module AltCoverTests =

#if !NET472
  let dir =
    Path.Combine(SolutionDir(), "_Binaries/AltCover.Tests/Debug+AnyCPU/net5.0")
#else
  let dir =
    Path.Combine(SolutionDir(), "_Binaries/AltCover.Tests/Debug+AnyCPU/net472")
#endif

  let monoSample1path =
    Path.Combine(SolutionDir(), "_Mono/Sample1/Sample1.exe")
#if !NET472
  let sample1path =
    Path.Combine(
      SolutionDir(),
      "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0/Sample1.dll"
    )

  let sample4path =
    Path.Combine(
      SolutionDir(),
      "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"
    )

  let sample8path =
    Path.Combine(
      SolutionDir(),
      "_Binaries/Sample8/Debug+AnyCPU/netcoreapp2.0/Sample8.dll"
    )
#else
  let sample1path =
    Path.Combine(SolutionDir(), "_Binaries/Sample1/Debug+AnyCPU/net20/Sample1.exe")

  let sample4path =
    Path.Combine(SolutionDir(), "_Binaries/Sample4/Debug+AnyCPU/net472/Sample4.dll")

  let sample8path =
    Path.Combine(SolutionDir(), "_Binaries/Sample8/Debug+AnyCPU/net20/Sample8.exe")
#endif
  let recorderSnk =
    typeof<AltCover.Node>.Assembly.GetManifestResourceNames ()
    |> Seq.find (fun n -> n.EndsWith(".Recorder.snk", StringComparison.Ordinal))

  let infrastructureSnk =
    Assembly
      .GetExecutingAssembly()
      .GetManifestResourceNames()
    |> Seq.find (fun n -> n.EndsWith("Infrastructure.snk", StringComparison.Ordinal))

  let private ff (a, b, c) = { Scope = a; Regex = b; Sense = c }

  // Augment.fs
  [<Test>]
  let ZeroIsNotVisited () =
    test <@ Exemption.OfInt 0 = Exemption.None @>

  [<Test>]
  let PositiveIsVisited () =
    test
      <@ [ 1 .. 255 ]
         |> Seq.map Exemption.OfInt
         |> Seq.tryFind (fun x -> x <> Exemption.Visited) = None @>

  [<Test>]
  let NegativesSpray () =
    test
      <@ [ 0 .. 5 ]
         |> Seq.map ((~-) >> Exemption.OfInt)
         |> Seq.toList = [ Exemption.None
                           Exemption.Declared
                           Exemption.Automatic
                           Exemption.StaticAnalysis
                           Exemption.Excluded
                           Exemption.None ] @>

  // ProgramDatabase.fs
  [<Test>]
  let ShouldGetPdbFromImage () =
    let files =
      Directory.GetFiles(dir)
      |> Seq.filter
           (fun x ->
             x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
             || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun f ->
             f |> Path.GetFileNameWithoutExtension
             <> "testhost")
      |> Seq.filter (fun f -> f |> Path.GetFileName <> "AltCover.Tests.exe")
      |> Seq.filter (fun f -> f |> Path.GetFileName <> "CompilerAttributes.dll")
      |> Seq.map (fun x -> (x, Mono.Cecil.AssemblyDefinition.ReadAssembly x))
      |> Seq.filter (fun x -> (fst x) + ".mdb" |> File.Exists |> not)
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("altcode.", StringComparison.OrdinalIgnoreCase))
#if !NET472
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("Expecto", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("ICSharp", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("Mono.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("BlackFox.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("Microsoft.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("Manatee.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("Newtonsoft.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("NuGet.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("nunit", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("FSharp.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("testcentric.", StringComparison.OrdinalIgnoreCase))
      // for coverlet
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("coverlet", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("AltCover,", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("System.", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("Unquote", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName.StartsWith("xunit", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter
           (fun x ->
             not
             <| (snd x)
               .FullName
               .StartsWith(
                 "AltCover.Recorder",
                 StringComparison.OrdinalIgnoreCase
               ))
#else
      |> Seq.filter
           (fun x ->
             (snd x)
               .FullName
               .EndsWith(
                 "PublicKeyToken=c02b1a9f5b7cade8",
                 StringComparison.OrdinalIgnoreCase
               ))
#endif
      |> Seq.toList

    test <@ files <> [] @>

    files
    |> Seq.iter
         (fun x ->
           let pdb =
             AltCover.ProgramDatabase.getPdbFromImage (snd x)

           match pdb with
           //| None -> Assert.Fail(sprintf "%A" x)
           | Some name ->
               let probe = Path.ChangeExtension((fst x), ".pdb")
               let file = FileInfo(probe)
               let filename = file.Name.Replace("\\", "/")

               Assert.That(
                 "/" + name.Replace("\\", "/"),
                 Does.EndWith("/" + filename),
                 (fst x) + " -> " + name
               ))

  [<Test>]
  let ShouldGetEmbeddedPdbFromImage () =
    let target = sample8path
    maybeIgnore (fun () -> target |> File.Exists |> not)

    use image =
      Mono.Cecil.AssemblyDefinition.ReadAssembly target

    let pdb =
      AltCover.ProgramDatabase.getPdbFromImage image

    match pdb with
    | Some name -> Assert.That(name, Is.EqualTo "Sample8.pdb", target + " -> " + name)

  [<Test>]
  let ShouldGetNoMdbFromMonoImage () =
    let path = Path.GetDirectoryName monoSample1path
    maybeIgnore (fun () -> path |> Directory.Exists |> not)

    let files =
      Directory.GetFiles(path)
      |> Seq.filter
           (fun x ->
             x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
             || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
      |> Seq.map (fun x -> (x, Mono.Cecil.AssemblyDefinition.ReadAssembly x))
      |> Seq.toList

    Assert.That(files, Is.Not.Empty)

    files
    |> Seq.iter
         (fun x ->
           let probe = (fst x) + ".mdb"

           let pdb =
             AltCover.ProgramDatabase.getPdbFromImage (snd x)

           match pdb with
           | None -> Assert.That(File.Exists probe, probe + " not found"))

  [<Test>]
  let ShouldGetPdbWithFallback () =
    Directory.GetFiles(dir)
    |> Seq.filter
         (fun x ->
           x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
           || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x -> (x + ".mdb") |> File.Exists |> not)
    |> Seq.filter
         (fun f ->
           f |> Path.GetFileNameWithoutExtension
           <> "testhost")
    |> Seq.filter (fun f -> f |> Path.GetFileName <> "AltCover.Tests.exe")
    |> Seq.iter
         (fun x ->
           use def =
             Mono.Cecil.AssemblyDefinition.ReadAssembly x

           let pdb =
             AltCover.ProgramDatabase.getPdbWithFallback (def)

           match pdb with
           | None ->
               Assert.That(
                 File.Exists(Path.ChangeExtension(x, ".pdb")),
                 Is.Not.True,
                 "No .pdb for " + x
               )
           | Some name ->
               let probe = Path.ChangeExtension(x, ".pdb")
               let file = FileInfo(probe)
               let filename = file.Name.Replace("\\", "/")

               Assert.That(
                 "/" + name.Replace("\\", "/"),
                 Does.EndWith("/" + filename),
                 x + " -> " + name
               ))

  [<Test>]
  let ShouldGetForeignPdbWithFallback () =
    let path = Path.Combine(SolutionDir(), "packages")
    // Looking for the Mono.Options symbols
    let files =
      Directory.GetFiles(path, "*.pdb", SearchOption.AllDirectories)

    files
    |> Seq.filter (fun p -> Path.ChangeExtension(p, ".dll") |> File.Exists)
    |> Seq.iter
         (fun p ->
           let dll = Path.ChangeExtension(p, ".dll")

           try
             use def =
               Mono.Cecil.AssemblyDefinition.ReadAssembly dll

             let pdb =
               AltCover.ProgramDatabase.getPdbWithFallback (def)

             let normalized =
               Path.Combine(Path.GetDirectoryName p, Path.GetFileName p)

             match pdb with
             | Some name -> Assert.That(name, Is.EqualTo normalized)
           with :? BadImageFormatException -> ())

  [<Test>]
  let ShouldGetForeignPdbWithFallbackWhenNotColocated () =
    try
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(SolutionDir(), "packages")
      // Looking for the Mono.Options symbols
      let files =
        Directory.GetFiles(path, "*.pdb", SearchOption.AllDirectories)

      files
      |> Seq.filter (fun p -> Path.ChangeExtension(p, ".dll") |> File.Exists)
      |> Seq.iter
           (fun p ->
             let dll0 = Path.ChangeExtension(p, ".dll")
             let unique = Guid.NewGuid().ToString()

             let output =
               Path.Combine(Path.GetDirectoryName(where), unique)

             Directory.CreateDirectory(output) |> ignore

             let dll =
               Path.Combine(output, Path.GetFileName dll0)

             System.IO.File.Copy(dll0, dll)
             ProgramDatabase.symbolFolders.Clear()

             p
             |> Path.GetDirectoryName
             |> ProgramDatabase.symbolFolders.Add

             try
               use def =
                 Mono.Cecil.AssemblyDefinition.ReadAssembly dll

               let pdb =
                 AltCover.ProgramDatabase.getPdbWithFallback (def)

               let normalized =
                 Path.Combine(Path.GetDirectoryName p, Path.GetFileName p)

               match pdb with
               | Some name ->
                   Assert.That(name, Is.EqualTo normalized)
                   AltCover.ProgramDatabase.readSymbols def
                   Assert.That(def.MainModule.HasSymbols, def.MainModule.FileName)
             with :? BadImageFormatException -> ())
    finally
      ProgramDatabase.symbolFolders.Clear()

  [<Test>]
  let ShouldGetMdbWithFallback () =
    let path = Path.GetDirectoryName monoSample1path
    maybeIgnore (fun () -> path |> Directory.Exists |> not)
    let files = Directory.GetFiles(path)

    files
    |> Seq.filter
         (fun x ->
           x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
           || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter
         (fun x ->
           use def =
             Mono.Cecil.AssemblyDefinition.ReadAssembly x

           let mdb =
             AltCover.ProgramDatabase.getPdbWithFallback (def)

           match mdb with
           //           | None -> Assert.That(File.Exists(x + ".mdb"), Is.Not.True, "No .mdb for " + x)
           | Some name ->
               let probe = x + ".mdb"
               let file = FileInfo(probe)
               let filename = file.Name.Replace("\\", "/")

               Assert.That(
                 name.Replace("\\", "/") + ".mdb",
                 Does.EndWith("/" + filename),
                 x + " -> " + name
               ))

  [<Test>]
  let ShouldGetSymbolsFromPdb () =
    Directory.GetFiles(dir)
    |> Seq.filter
         (fun x ->
           x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
           || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter
         (fun f ->
           f |> Path.GetFileNameWithoutExtension
           <> "testhost")
    |> Seq.filter (fun f -> f |> Path.GetFileName <> "AltCover.Tests.exe")
    |> Seq.map Mono.Cecil.AssemblyDefinition.ReadAssembly
    |> Seq.filter
         (fun x ->
           not
           <| x.FullName.StartsWith("altcode.", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter
         (fun x ->
           not
           <| x.FullName.StartsWith("AltCover,", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter
         (fun x ->
           not
           <| x.FullName.StartsWith(
             "AltCover.Recorder",
             StringComparison.OrdinalIgnoreCase
           ))
    |> Seq.filter
         (fun x ->
           x.FullName.EndsWith(
             "PublicKeyToken=c02b1a9f5b7cade8",
             StringComparison.OrdinalIgnoreCase
           ))
    |> Seq.iter
         (fun def ->
           AltCover.ProgramDatabase.readSymbols def
           Assert.That(def.MainModule.HasSymbols, def.MainModule.FileName))

  [<Test>]
  let ShouldGetSymbolsFromEmbeddedPdb () =
    let target = sample8path
    maybeIgnore (fun () -> target |> File.Exists |> not)

    use image =
      Mono.Cecil.AssemblyDefinition.ReadAssembly target

    AltCover.ProgramDatabase.readSymbols image
    Assert.That(image.MainModule.HasSymbols, image.MainModule.FileName)

  [<Test>]
  let ShouldNotGetSymbolsWhenNoPdb () =
    Directory.GetFiles(dir)
    |> Seq.filter
         (fun x ->
           x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
           || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter
         (fun x ->
           Path
             .GetFileName(x)
             .StartsWith("BlackFox", StringComparison.OrdinalIgnoreCase))
    |> Seq.map Mono.Cecil.AssemblyDefinition.ReadAssembly
    |> Seq.filter
         (fun x ->
           not
           <| x.FullName.EndsWith(
             "PublicKeyToken=c02b1a9f5b7cade8",
             StringComparison.OrdinalIgnoreCase
           ))
    |> Seq.iter
         (fun def ->
           AltCover.ProgramDatabase.readSymbols def
           Assert.That(not def.MainModule.HasSymbols, def.MainModule.FileName))

  [<Test>]
  let ShouldGetSymbolsFromMdb () =
    let where = Assembly.GetExecutingAssembly().Location
    let pdb = Path.ChangeExtension(where, ".pdb")
    maybeIgnore (fun () -> monoSample1path |> File.Exists |> not)
    let path = Path.GetDirectoryName monoSample1path

    let files = Directory.GetFiles(path)

    files
    |> Seq.filter
         (fun x ->
           x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
           || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter
         (fun x ->
           use def =
             Mono.Cecil.AssemblyDefinition.ReadAssembly x

           AltCover.ProgramDatabase.readSymbols def
           Assert.That(def.MainModule.HasSymbols, def.MainModule.FileName))

  // Filter.fs
  [<Test>]
  let NoneOfTheAboveMatchesNoType () =
    Assert.That(Filter.``match`` () (ff(FilterScope.Type, Regex "23", Exclude)), Is.False)
    Assert.That(Filter.``match`` () (ff(FilterScope.Type, Regex "23", Include)), Is.False)

  [<Test>]
  let NoneOfTheAboveMatchesNoAttribute () =
    Assert.That(
      Filter.``match`` () (ff(FilterScope.Attribute, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff(FilterScope.Attribute, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoAssembly () =
    Assert.That(
      Filter.``match`` () (ff(FilterScope.Assembly, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff(FilterScope.Assembly, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoModule () =
    Assert.That(
      Filter.``match`` () (ff(FilterScope.Module, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff(FilterScope.Module, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let NoneOfTheAboveMatchesNoFile () =
    Assert.That(Filter.``match`` () (ff(FilterScope.File, Regex "23", Exclude)), Is.False)
    Assert.That(Filter.``match`` () (ff(FilterScope.File, Regex "23", Include)), Is.False)

  [<Test>]
  let NoneOfTheAboveMatchesNoPath () =
    Assert.That(Filter.``match`` () (ff(FilterScope.Path, Regex "23", Exclude)), Is.False)
    Assert.That(Filter.``match`` () (ff(FilterScope.Path, Regex "23", Include)), Is.False)

  [<Test>]
  let NoneOfTheAboveMatchesNoMethod () =
    Assert.That(
      Filter.``match`` () (ff(FilterScope.Method, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` () (ff(FilterScope.Method, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let FileDoesNotMatchNonFileClass () =
    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff(FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

  [<Test>]
  let FileDoesMatchFileClass () =
    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff(FilterScope.File, Regex "Cove", Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff(FilterScope.File, Regex "Cove", Include)),
      Is.False
    )

  [<Test>]
  let PathDoesNotMatchNonPathClass () =
    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff(FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

  [<Test>]
  let PathDoesMatchPathClass () =
    let x =
      String [| '\\'
                Path.DirectorySeparatorChar |]

    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff(FilterScope.Path, Regex(x + "_Binaries" + x), Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match``
        (Assembly.GetExecutingAssembly().Location)
        (ff(FilterScope.Path, Regex(x + "_Binaries" + x), Include)),
      Is.False
    )

  [<Test>]
  let AssemblyDoesNotMatchNonAssemblyClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def (ff(FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

    Assert.That(
      Filter.``match`` def (ff(FilterScope.Type, Regex "23", Include)),
      Is.False
    )

  [<Test>]
  let AssemblyDoesMatchAssemblyClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def (ff(FilterScope.Assembly, Regex "Cove", Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match`` def (ff(FilterScope.Assembly, Regex "Cove", Include)),
      Is.False
    )

  [<Test>]
  let ModuleDoesNotMatchNonModuleClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def.MainModule (ff(FilterScope.Type, Regex "23", Exclude)),
      Is.False
    )

  [<Test>]
  let ModuleDoesMatchModuleClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      Filter.``match`` def.MainModule (ff(FilterScope.Module, Regex "Cove", Exclude)),
      Is.True
    )

    Assert.That(
      Filter.``match`` def.MainModule (ff(FilterScope.Module, Regex "Cove", Include)),
      Is.False
    )

  [<Test>]
  let TypeDoesNotMatchNonTypeClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.iter
         (fun t ->
           Assert.That(
             Filter.``match`` t (ff(FilterScope.File, Regex "23", Exclude)),
             Is.False,
             t.FullName
           ))

  [<Test>]
  let TypeDoesMatchTypeClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.IsPublic && t.Name.Contains("AltCover")) // exclude the many compiler generted chaff classes
    |> Seq.iter
         (fun t ->
           Assert.That(
             Filter.``match`` t (ff(FilterScope.Type, Regex "Cove", Exclude)),
             Is.True,
             t.FullName
           )

           Assert.That(
             Filter.``match`` t (ff(FilterScope.Type, Regex "Cove", Include)),
             Is.False,
             t.FullName
           ))

  [<Test>]
  let MethodDoesNotMatchNonMethodClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.IsPublic)
    |> Seq.collect (fun t -> t.Methods)
    |> Seq.iter
         (fun m ->
           Assert.That(
             Filter.``match`` m (ff(FilterScope.Type, Regex "23", Exclude)),
             Is.False
           ))

  [<Test>]
  let MethodDoesMatchMethodClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    Assert.That(
      def.MainModule.Types
      |> Seq.filter (fun t -> t.IsPublic) // exclude the many compiler generated chaff classes
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.filter (fun m -> m.IsPublic && (not m.IsConstructor))
      |> Seq.filter
           (fun m ->
             Filter.``match``
               m
               (ff(FilterScope.Method, Regex "MethodDoesMatchMethodClass", Exclude)))
      |> Seq.length,
      Is.EqualTo(1)
    )

    Assert.That(
      def.MainModule.Types
      |> Seq.filter (fun t -> t.IsPublic) // exclude the many compiler generated chaff classes
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.filter (fun m -> m.IsPublic && (not m.IsConstructor))
      |> Seq.filter
           (fun m ->
             Filter.``match``
               m
               (ff(FilterScope.Method, Regex "MethodDoesMatchMethodClass", Include))
             |> not)
      |> Seq.length,
      Is.EqualTo(1)
    )

  [<Test>]
  let AttributeDoesNotMatchNonAttributeClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.iter
         (fun t ->
           Assert.That(
             Filter.``match``
               t.CustomAttributes
               (ff(FilterScope.File, Regex "23", Exclude)),
             Is.False,
             t.FullName
           ))

  [<Test>]
  let AttributeDoesMatchAttributeClass () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    def.MainModule.Types
    |> Seq.filter
         (fun t ->
           t.IsPublic
           && t.Name.Contains("ProxyObject")
           && (not (t.FullName.Contains("Coverlet.Core.Instrumentation")))) // exclude the many compiler generted chaff classes
    |> Seq.iter
         (fun t ->
           Assert.That(
             Filter.``match`` t (ff(FilterScope.Attribute, Regex "Exclu", Exclude)),
             Is.True,
             t.FullName
           )

           Assert.That(
             Filter.``match`` t (ff(FilterScope.Attribute, Regex "Exclu", Include)),
             Is.False,
             t.FullName
           ))

  [<Test>]
  let CanExcludeCSharpPropertiesByAttribute () =
    let location =
      typeof<Sample11.Class1>.Assembly.Location

    let sourceAssembly =
      AssemblyDefinition.ReadAssembly(location)

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
      |> Seq.map (fun x -> x.Name)
      |> Seq.sort
      |> Seq.toList

    let expected = [ ".ctor" ]
    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)

  [<Test>]
  let Sample3Class1IsCSharpAutoproperty () =
    let sample3 = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class1")
    |> Seq.collect (fun t -> t.Methods)
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (Filter.isCSharpAutoProperty >> Assert.That)

  [<Test>]
  let Sample3Class2IsNotCSharpAutoproperty () =
    let sample3 = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class2")
    |> Seq.collect (fun t -> t.Methods)
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (fun m -> Assert.That(Filter.isCSharpAutoProperty m, Is.False))

  [<Test>]
  let CanIdentifyExcludedFSharpMethods () =
    let tracer = DU.returnFoo 23
    let location = tracer.GetType().Assembly.Location

    let sourceAssembly =
      AssemblyDefinition.ReadAssembly(location)

    let direct =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x -> x.Namespace = "N")
      |> Seq.toList

    let indirect =
      direct
      |> Seq.filter (fun t -> t.HasNestedTypes)
      |> Seq.collect (fun t -> t.NestedTypes)
      |> Seq.toList // MyUnion, MyThing

    let indirect2 =
      indirect
      |> Seq.filter (fun t -> t.HasNestedTypes)
      |> Seq.collect (fun t -> t.NestedTypes)
      |> Seq.toList // Foo, Bar, ...

    let indirect3 =
      indirect2
      |> Seq.filter (fun t -> t.HasNestedTypes)
      // |> Seq.collect (fun t -> t.NestedTypes)
      // |> Seq.map (fun t -> t.FullName)
      |> Seq.toList

    Assert.That(indirect3 |> Seq.isEmpty, sprintf "Third order types found %A" indirect3)

    let pass =
      Seq.concat [ direct
                   indirect
                   indirect2 ]
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.filter (not << Filter.isFSharpInternal)
      |> Seq.map (fun x -> x.Name)
      |> Seq.sort
      |> Seq.toList

    let expected =
      [ ".ctor"
        ".ctor"
        "Invoke"
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

    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)

  [<Test>]
  let CanIdentifyExcludedCSharpAutoProperties () =
    let location = typeof<Sample3.Class1>.Assembly.Location

    let sourceAssembly =
      AssemblyDefinition.ReadAssembly(location)

    let direct =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x -> x.Name = "Class1")
      |> Seq.head

    let pass =
      direct.Methods
      |> Seq.filter (not << Filter.isCSharpAutoProperty)
      |> Seq.map (fun x -> x.Name)
      |> Seq.sort
      |> Seq.toList

    let expected = [ ".ctor" ]
    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)

  [<Test>]
  let CanIdentifyIncludedCSharpProperties () =
    let location = typeof<Sample3.Class1>.Assembly.Location

    let sourceAssembly =
      AssemblyDefinition.ReadAssembly(location)

    let direct =
      sourceAssembly.MainModule.Types
      |> Seq.filter (fun x -> x.Name = "Class2")
      |> Seq.head

    let pass =
      direct.Methods
      |> Seq.filter (not << Filter.isCSharpAutoProperty)
      |> Seq.map (fun x -> x.Name)
      |> Seq.sort
      |> Seq.toList

    let expected =
      [ ".ctor"
        "get_Property"
        "set_Property" ]

    Assert.That(pass, Is.EquivalentTo(expected), sprintf "Got sequence %A" pass)

  // Visitor.fs
  [<Test>]
  let ReportFileShouldBeCorrectlySuffixed () =
    try
      let path1 = Path.Combine(SolutionRoot.location, "test.xml")
      let path2 = Path.Combine(SolutionRoot.location, "test")
      let path3 = Path.Combine(SolutionRoot.location, "test.json")

      CoverageParameters.theReportFormat <- Some ReportFormat.NCover
      CoverageParameters.theReportPath <- Some path1
      test <@ CoverageParameters.reportPath () = path1 @>

      CoverageParameters.theReportPath <- Some path2
      test <@ CoverageParameters.reportPath () = path2 @>

      CoverageParameters.theReportPath <- Some path3
      test <@ CoverageParameters.reportPath () = path1 @>

      CoverageParameters.theReportFormat <- Some ReportFormat.NativeJson
      CoverageParameters.theReportPath <- Some path1
      test <@ CoverageParameters.reportPath () = path3 @>

      CoverageParameters.theReportPath <- Some path2
      test <@ CoverageParameters.reportPath () = path2 @>

      CoverageParameters.theReportPath <- Some path3
      test <@ CoverageParameters.reportPath () = path3 @>

    finally
      CoverageParameters.theReportPath <- None
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let CanSwitchSampling () =
    let save = CoverageParameters.single

    try
      CoverageParameters.single <- true
      test <@ CoverageParameters.sampling () = 1 @>
      CoverageParameters.single <- false
      test <@ CoverageParameters.sampling () = 0 @>
    finally
      CoverageParameters.single <- save

  [<Test>]
  let ValidateStaticExemption () =
    let result =
      [ StaticFilter.AsCovered
        StaticFilter.Hidden
        StaticFilter.NoFilter ]
      |> List.map (fun k -> Visitor.I.selectExemption k [] Exemption.None)

    test
      <@ result = [ Exemption.StaticAnalysis
                    Exemption.None
                    Exemption.None ] @>

  [<Test>]
  let ValidateStaticClass () =
    let where =
      typeof<AltCover.CommandLine.Format>
        .Assembly
        .Location

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly where

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
      CoverageParameters.showGenerated := true
      let path = Path.Combine(dir, "Sample4.dll")

      use def =
        Mono.Cecil.AssemblyDefinition.ReadAssembly path

      let items =
        def.MainModule.GetAllTypes()
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.filter (fun m -> m.Name = "testMakeUnion")
        |> Seq.toList

      let result =
        [ StaticFilter.AsCovered
          StaticFilter.Hidden
          StaticFilter.NoFilter ]
        |> List.map (fun k -> Visitor.I.selectExemption k items Exemption.None)

      test
        <@ result = [ Exemption.StaticAnalysis
                      Exemption.Automatic
                      Exemption.Automatic ] @>
    finally
      CoverageParameters.showGenerated := false

  [<Test>]
  let DetectLocalSource () =

    let toolPackages =
      let xml =
        Path.Combine(SolutionDir(), "./Build/NuGet.csproj")
        |> Path.GetFullPath
        |> XDocument.Load

      xml.Descendants(XName.Get("PackageReference"))
      |> Seq.map
           (fun x ->
             (x
               .Attribute(XName.Get("Include"))
                .Value.ToLowerInvariant(),
              x.Attribute(XName.Get("version")).Value))
      |> Map.ofSeq

    CoverageParameters.local := false
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

    let fdll = Path.Combine(fscore, "FSharp.Core.dll")
    Assert.That(File.Exists fdll, Is.True, "FSharp.Core.dll not found")
    //let pdb2 = Path.Combine(fscore, "FSharp.Core.pdb")
    //Assert.That(File.Exists pdb2, Is.True, "FSharp.Core.pdb not found")

    let dll = Path.Combine(mono, "Mono.Options.dll")
    Assert.That(File.Exists dll, Is.True, "Mono.Options.dll not found")
    //let pdb3 = Path.Combine(mono, "Mono.Options.pdb")
    //Assert.That(File.Exists pdb3, Is.True, "Mono.Options.pdb not found")

    let a = AssemblyDefinition.ReadAssembly exe
    ProgramDatabase.readSymbols a

    let m = AssemblyDefinition.ReadAssembly dll
    ProgramDatabase.readSymbols m

    let f = AssemblyDefinition.ReadAssembly fdll
    ProgramDatabase.readSymbols f

    // work round the instrumented assemblies having unreliable symbols
#if !NET472
    let dir =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/AltCover.Engine/Debug+AnyCPU/netstandard2.0"
      )
#else
    let dir =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/AltCover.Engine/Debug+AnyCPU/net472"
      )
#endif

    let localAssembly = Path.Combine(dir, "AltCover.Engine.dll")
                        |> AssemblyDefinition.ReadAssembly
    ProgramDatabase.readSymbols localAssembly

    Assert.That(localAssembly.LocalFilter, Is.False, "local engine Assembly non-local")
    Assert.That(localAssembly.MainModule.LocalFilter, Is.False, "local engine  MainModule non-local")
    Assert.That(a.LocalFilter, Is.False, "Assembly non-local")
    Assert.That(a.MainModule.LocalFilter, Is.False, "MainModule non-local")
    Assert.That(m.LocalFilter, Is.False, "dll Assembly non-local")
    Assert.That(m.MainModule.LocalFilter, Is.False, "dll MainModule non-local")
    Assert.That(f.LocalFilter, Is.False, "f# Assembly non-local")
    Assert.That(f.MainModule.LocalFilter, Is.False, "f# MainModule non-local")

    try
      CoverageParameters.local := true
      Assert.That(localAssembly.LocalFilter, Is.False, "local engine  Assembly local")
      Assert.That(localAssembly.MainModule.LocalFilter, Is.False, "local engine  MainModule local")
      Assert.That(a.LocalFilter, Is.True, "Assembly local")
      Assert.That(a.MainModule.LocalFilter, Is.False, "MainModule local")
      Assert.That(m.LocalFilter, Is.True, "dll Assembly local")
      Assert.That(m.MainModule.LocalFilter, Is.False, "dll MainModule local")
      Assert.That(f.LocalFilter, Is.True, "f# Assembly local")
      Assert.That(f.MainModule.LocalFilter, Is.False, "f# MainModule local")

    finally
      CoverageParameters.local := false

  [<Test>]
  let LocateMatchShouldChooseLongerWildCardPath () =
    let dict =
      System.Collections.Generic.Dictionary<string, string>()

    let file = Assembly.GetExecutingAssembly().Location
    let p1 = Path.GetDirectoryName file
    let p2 = Path.GetDirectoryName p1
    let pp1 = Path.Combine(p1, "*")
    let pp2 = Path.Combine(p2, "*")
    dict.Add(pp1, pp1)
    dict.Add(pp2, pp2)
    let find = Visitor.I.findClosestMatch file dict
    Assert.That(find, Is.EqualTo(Some(pp1, String.Empty)))

  [<Test>]
  let AsyncTestInContext () =
    let sample23 = Path.Combine(dir, "Sample23.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample23)

    let symbols23 = Path.ChangeExtension(sample23, ".pdb")

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, symbols23)

    def.MainModule.ReadSymbols(rr)

    let types = def.MainModule.GetAllTypes()

    let synch =
      types
      |> Seq.filter (fun t -> t.Name = "Async97")
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.filter (fun m -> m.Name = "DoSomethingSynch")
      |> Seq.head

    //synch.Body.Instructions
    //|> Seq.iter (fun i ->
    //  let sp = synch.DebugInformation.GetSequencePoint(i)
    //           |> Option.ofObj
    //           |> Option.map (fun s -> s.StartLine)
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
    //           |> Option.map (fun s -> s.StartLine)
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
      |> List.iteri
           (fun i node ->
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
      |> List.iteri
           (fun i node ->
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
    let sample24 = Path.Combine(dir, "Sample24.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample24)

    let symbols24 = Path.ChangeExtension(sample24, ".pdb")

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, symbols24)

    def.MainModule.ReadSymbols(rr)

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.filter (fun m -> m.Name = "MoveNext")
      |> Seq.toList

    methods
    |> Seq.iter
         (fun method ->
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
               <@ deeper
                  |> List.forall
                       (fun n ->
                         match n with
                         | MethodPoint _ -> true
                         | _ -> false) @>
           finally
             CoverageParameters.nameFilters.Clear()
             CoverageParameters.theReportFormat <- None)

  [<Test>]
  let DebugBuildTernaryTestInContext () =
    let sample23 = Path.Combine(dir, "Sample23.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample23)

    let symbols23 = Path.ChangeExtension(sample23, ".pdb")

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
      |> List.iteri
           (fun i node ->
             match node with
             | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 1
      |> List.iteri
           (fun i node ->
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
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("issue37.dl_", StringComparison.Ordinal))

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(res)

    let res2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("issue37.pd_", StringComparison.Ordinal))

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(res2)

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly stream

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
      |> List.iteri
           (fun i node ->
             match node with
             | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 1
      |> List.iteri
           (fun i node ->
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
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("issue37.dl_", StringComparison.Ordinal))

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(res)

    let res2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("issue37.pd_", StringComparison.Ordinal))

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(res2)

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly stream

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
      CoverageParameters.coalesceBranches := true
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
      |> List.iteri
           (fun i node ->
             match node with
             | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 1
      |> List.iteri
           (fun i node ->
             match node with
             | MethodPoint { Instruction = _
                             SeqPnt = _
                             Uid = uid
                             DefaultVisitCount = Exemption.Automatic
                             Interesting = true } ->
                 Assert.That(uid, Is.EqualTo i, "point number"))
    finally
      CoverageParameters.coalesceBranches := false
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let CSharpNestedMethods () =
    let sample3 = Path.Combine(dir, "Sample5.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample3)

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.toList

    let containing =
      methods |> Seq.map Visitor.I.containingMethod

    let result =
      containing
      |> Seq.map (fun (mo: MethodDefinition option) -> mo |> Option.map id)

    let expected =
      [ None // System.Int32 Sample5.Class1::F1(System.String)
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
        None // "System.Collections.Generic.IEnumerable`1<K> Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values()"
        None // "System.Collections.Generic.IEnumerable`1<K> Sample5.RecursiveSyntheticInvocation`2::get_ValuesWorks()"
        None // "System.Collections.Generic.IEnumerable`1<T> Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Keys()"
        None // "K Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Item(T)"
        None // "System.Int32 Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyCollection<System.Collections.Generic.KeyValuePair<T,K>>.get_Count()"
        None // "System.Boolean Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.ContainsKey(T)"
        None // "System.Collections.Generic.IEnumerator`1<System.Collections.Generic.KeyValuePair`2<T,K>> Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<T,K>>.GetEnumerator()"
        None // "System.Collections.IEnumerator Sample5.RecursiveSyntheticInvocation`2::System.Collections.IEnumerable.GetEnumerator()"
        None // "System.Boolean Sample5.RecursiveSyntheticInvocation`2::System.Collections.Generic.IReadOnlyDictionary<T,K>.TryGetValue(T,K&)"
        None // "System.Void Sample5.RecursiveSyntheticInvocation`2::.ctor()"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "System.Void Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::.ctor(System.Int32)"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "System.Void Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.IDisposable.Dispose()"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "System.Boolean Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::MoveNext()"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "K Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.Generic.IEnumerator<K>.get_Current()"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "System.Void Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.IEnumerator.Reset()"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "System.Object Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.IEnumerator.get_Current()"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "System.Collections.Generic.IEnumerator`1<K> Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.Generic.IEnumerable<K>.GetEnumerator()"
        Some "System.Collections.Generic.IReadOnlyDictionary<T,K>.get_Values" // "System.Collections.IEnumerator Sample5.RecursiveSyntheticInvocation`2/<System-Collections-Generic-IReadOnlyDictionary<T,K>-get_Values>d__2::System.Collections.IEnumerable.GetEnumerator()"
        Some "get_ValuesWorks" // "System.Void Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::.ctor(System.Int32)"
        Some "get_ValuesWorks" // "System.Void Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.IDisposable.Dispose()"
        Some "get_ValuesWorks" // "System.Boolean Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::MoveNext()"
        Some "get_ValuesWorks" // "K Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.Generic.IEnumerator<K>.get_Current()"
        Some "get_ValuesWorks" // "System.Void Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.IEnumerator.Reset()"
        Some "get_ValuesWorks" // "System.Object Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.IEnumerator.get_Current()"
        Some "get_ValuesWorks" // "System.Collections.Generic.IEnumerator`1<K> Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.Generic.IEnumerable<K>.GetEnumerator()"
        Some "get_ValuesWorks" ] // "System.Collections.IEnumerator Sample5.RecursiveSyntheticInvocation`2/<get_ValuesWorks>d__4::System.Collections.IEnumerable.GetEnumerator()"

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
    |> Seq.toList
    |> List.zip expected
    |> List.iteri
         (fun i (x, y) ->
           Assert.That(
             y |> Option.map toName,
             x |> Is.EqualTo,
             sprintf "%A %A %d" x (y |> Option.map toFullName) i
           ))
    // Disambiguation checks
    let g3 = methods.[10]

    Assert.That(
      methods
      |> Seq.map Visitor.I.containingMethod
      |> Seq.choose id
      |> Seq.filter (fun m -> m.Name = "G3"),
      Is.EquivalentTo [ g3; g3; g3 ]
    )

    let g1 = methods.[6]

    Assert.That(
      methods
      |> Seq.map Visitor.I.containingMethod
      |> Seq.choose id
      |> Seq.filter (fun m -> m.Name = "G1"),
      Is.EquivalentTo [ g1; g1 ]
    )

  [<Test>]
  let FSharpNestedMethodsClassic () =
    let sample3 = Path.Combine(SolutionRoot.location, "Samples/Sample6/Sample6Classic.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample3)

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
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

    result
    |> List.zip expected
    |> List.iteri
         (fun i (x, y) ->
           Assert.That(y, Is.EqualTo x, sprintf "%A %A %d %s" x y i methods.[i].FullName))

  [<Test>]
  let FSharpNestedMethods5x0x201 () =
    // let sample3 = Path.Combine(SolutionRoot.location, "Samples/Sample6/Sample6_5_0_201.dll")
    let sample3 = Path.Combine(dir, "Sample6.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample3)

    let methods =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
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
        Some "fetchUrlAsync@25-4::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@26-5::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@25-4::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@26-5::Invoke(System.IO.StreamReader)"
        Some "fetchUrlAsync@23-3::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@25-4::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@23-3::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@25-4::Invoke(System.IO.Stream)"
        Some "fetchUrlAsync@23-2::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-3::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@23-2::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-3::Invoke(System.Net.WebResponse)"
        Some "fetchUrlAsync@22-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-2::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@22-1::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@23-2::Invoke(System.Net.WebResponse)"
        Some "fetchUrlAsync@22-1::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@23-6::.ctor(Microsoft.FSharp.Control.FSharpAsync`1<System.Net.WebResponse>,Microsoft.FSharp.Core.FSharpFunc`2<System.Net.WebResponse,Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit>>)"
        Some "fetchUrlAsync@22-1::Invoke" //"Microsoft.FSharp.Control.AsyncReturn Sample6.Module/fetchUrlAsync@23-6::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<Microsoft.FSharp.Core.Unit>)"
        Some "fetchUrlAsync@21::Invoke" //"System.Void Sample6.Module/fetchUrlAsync@22-1::.ctor(System.String,Microsoft.FSharp.Control.FSharpAsyncBuilder)"
        Some "fetchUrlAsync@21::Invoke" //"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@22-1::Invoke(Microsoft.FSharp.Core.Unit)"
        Some "Module::F2" //*//"System.Void Sample6.Module/fetchUrlAsync@21::.ctor()"
        Some "Module::F2" //*//"Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.Unit> Sample6.Module/fetchUrlAsync@21::Invoke(System.String)"
        Some "Module::F2" //*//"System.Void Sample6.Module/fetchUrlAsync@21::.cctor()"
        ]

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

    result
    |> List.zip expected
    |> List.iteri
         (fun i (x, y) ->
           Assert.That(y, Is.EqualTo x, sprintf "%A %A %d %s" x y i methods.[i].FullName))

  [<Test>]
  let ValidateSeqPntFixUp () = // HACK HACK HACK
    let location = typeof<Sample3.Class1>.Assembly.Location

    use sourceAssembly =
      AssemblyDefinition.ReadAssembly(location)

    let i =
      sourceAssembly.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.filter (fun m -> m.HasBody && m.Body.Instructions.Any())
      |> Seq.map (fun m -> m.Body.Instructions |> Seq.head)
      |> Seq.head

    let dummy = Cil.Document("dummy")
    let before = Cil.SequencePoint(i, dummy)

    before
      .GetType()
      .GetProperty("StartLine")
      .SetValue(before, 23)

    before
      .GetType()
      .GetProperty("StartColumn")
      .SetValue(before, 42)

    before
      .GetType()
      .GetProperty("EndLine")
      .SetValue(before, -1)

    before
      .GetType()
      .GetProperty("EndColumn")
      .SetValue(before, -1)

    let after = SeqPnt.Build(before)
    Assert.That(after.EndLine, Is.EqualTo before.StartLine)
    Assert.That(after.EndColumn, Is.EqualTo(before.StartColumn + 1))

  [<Test>]
  let EmptyArrayHasExpectedHash () =
    Assert.That(
      (KeyStore.I.tokenOfArray [||]),
      Is.EquivalentTo [| 9uy
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
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(infrastructureSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    StrongNameKeyData.Make(buffer.ToArray())

  [<Test>]
  let KeyHasExpectedToken () =
    let token = KeyStore.tokenOfKey <| provideKeyPair()

    let token' =
      String.Join(String.Empty, token |> List.map (fun x -> x.ToString("x2")))

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
    let token = KeyStore.keyToIndex <| provideKeyPair()
    Assert.That(token, Is.EqualTo(0xe8ad7c5b9f1a2bc0UL), sprintf "%x" token)

  [<Test>]
  let EmptyArrayHasExpectedIndex () =
    Assert.That((KeyStore.arrayToIndex [||]), Is.EqualTo(0x95601890afd80709UL))

  [<Test>]
  let KeyHasExpectedRecord () =
    let pair = provideKeyPair()
#if NET472  // Strong-name signing is not supported on this platform.
    let computed = pair.PublicKey

    let definitive =
      StrongNameKeyPair(
        pair.Blob |> List.toArray
      )
        .PublicKey

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

      let pair = provideKeyPair()
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

  [<Test>]
  let NonEmptyFiltersCatchAnExpectedValue () =
    try
      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo(0))

      CoverageParameters.nameFilters.AddRange(
        [ ff(FilterScope.File, Regex "Cove", Exclude)
          ff(FilterScope.Method, Regex "Augment", Exclude) ]
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
        [ ff(FilterScope.File, Regex "System", Exclude)
          ff(FilterScope.Method, Regex "Augment", Exclude) ]
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
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    let inputs =
      [ Node.Start []
        Node.Assembly
          { Assembly = def
            Inspection = Inspections.Instrument
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
            Destinations = [] }
        Node.Finish ]

    let outputs =
      inputs
      |> Seq.map (fun n -> n.After() |> Seq.toList)
      |> Seq.toList

    let expected =
      [ [ Finish ]
        [ AfterAssembly
            { Assembly = def
              Inspection = Inspections.Instrument
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
    let sample3 = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class1")
    |> Seq.collect (fun t -> t.Methods)
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (fun m -> Assert.That(Visitor.I.significant m, Is.False))

  [<Test>]
  let Sample3Class2IPropertyIsSignificant () =
    let sample3 = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(sample3)

    def.MainModule.Types
    |> Seq.filter (fun t -> t.Name = "Class2")
    |> Seq.collect (fun t -> t.Methods)
    |> Seq.filter (fun m -> m.IsGetter || m.IsSetter)
    |> Seq.iter (Visitor.I.significant >> Assert.That)

  [<Test>]
  let TerminalCasesGoNoDeeper () =
    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(Assembly.GetExecutingAssembly().Location)

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
            Destinations = [] }
        Node.Finish ]

    let outputs =
      inputs
      |> Seq.map (Visitor.I.deeper >> Seq.toList)
      |> Seq.toList

    let expected : Node list list = [ []; []; []; []; [] ]
    //Assert.That(outputs, Is.EquivalentTo(expected))
    test <@ outputs = expected @>

  [<Test>]
  let MethodPointsAreDeeperThanMethods () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = sample1path

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      (def.MainModule.Types
       |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))
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
      |> List.iteri
           (fun i node ->
             match node with
             | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 10
      |> List.iteri
           (fun i node ->
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

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

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
      CoverageParameters.coalesceBranches := true
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
        |> List.filter
             (fun n ->
               match n with
               | BranchPoint b -> b.Representative = Reporting.Representative
               | _ -> true)
      //reported |> List.skip 21 |> Seq.iter (printfn "reported = %A")
      Assert.That(reported.Length, Is.EqualTo 29)

      let branches =
        reported
        |> List.skip 21
        |> List.mapi
             (fun i node ->
               match node with
               | (BranchPoint b) ->
                   Assert.That(b.Uid, Is.EqualTo i, "branch point number")
                   Some b)
        |> List.choose id

      deeper
      |> List.take 21
      |> List.iteri
           (fun i node ->
             match node with
             | MethodPoint { Instruction = _
                             SeqPnt = _
                             Uid = uid
                             DefaultVisitCount = Exemption.Declared
                             Interesting = true } ->
                 Assert.That(uid, Is.EqualTo i, "point number"))

      Assert.That(
        branches |> List.map (fun b -> b.Path),
        Is.EquivalentTo [ 0
                          1
                          0
                          1
                          2
                          3
                          0
                          1 ]
      )
    finally
      CoverageParameters.coalesceBranches := false
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let BranchPointsAreComputedForMatch () =
    let path = Path.Combine(dir, "Sample17.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

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
      CoverageParameters.coalesceBranches := true

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
        |> List.filter
             (fun n ->
               match n with
               | BranchPoint b -> b.Representative = Reporting.Representative
               | _ -> true)

      Assert.That(reported.Length, Is.EqualTo 14)

      reported
      |> List.skip 9
      |> List.iteri
           (fun i node ->
             match node with
             | (BranchPoint b) -> Assert.That(b.Uid, Is.EqualTo i, "branch point number"))

      deeper
      |> List.take 9
      |> List.iteri
           (fun i node ->
             match node with
             | MethodPoint { Instruction = _
                             SeqPnt = _
                             Uid = uid
                             DefaultVisitCount = Exemption.StaticAnalysis
                             Interesting = true } ->
                 Assert.That(uid, Is.EqualTo i, "point number"))

    finally
      CoverageParameters.coalesceBranches := false
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let MethodsAreDeeperThanTypes () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = sample1path

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    ProgramDatabase.readSymbols def

    let type' =
      (def.MainModule.Types
       |> Seq.skipWhile (fun t -> t.Name.StartsWith("<"))
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
        |> Seq.map
             (fun m ->
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

               List.concat [ [ node ]
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
    let where = Assembly.GetExecutingAssembly().Location
    let path = sample1path

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

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
        |> Seq.map
             (fun t ->
               let flag =
                 Maybe (t.Name <> "Program") Inspections.Instrument Inspections.Ignore

               let node =
                 Node.Type
                   { VisibleType = t
                     Type = t
                     Inspection = flag
                     DefaultVisitCount = Exemption.None }

               List.concat [ [ node ]
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
      let where = Assembly.GetExecutingAssembly().Location
      let path = sample1path

      use def =
        Mono.Cecil.AssemblyDefinition.ReadAssembly path

      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      ProgramDatabase.readSymbols def
      Visitor.visit [] [] // cheat reset

      let deeper =
        Visitor.I.deeper
        <| Node.Assembly
             { Assembly = def
               Inspection = Inspections.Instrument
               Destinations = [] }
        |> Seq.toList

      Visitor.visit [] [] // cheat reset

      let expected =
        def.Modules // we have no nested types in this test
        |> Seq.map
             (fun t ->
               let node =
                 Node.Module
                   { Module = t
                     Inspection = Inspections.Instrument }

               List.concat [ [ node ]
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

      let where = Assembly.GetExecutingAssembly().Location
      let path = sample1path

      let deeper =
        Visitor.I.deeper
        <| Node.Start [ { AssemblyPath = path
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
            Destinations = [] }

      let expected =
        List.concat [ [ assembly ]
                      (Visitor.I.deeper >> Seq.toList) assembly
                      [ AfterAssembly
                          { Assembly = def
                            Inspection = Inspections.Instrument
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
    let where = Assembly.GetExecutingAssembly().Location
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
        <| Node.Start [ { AssemblyPath = path
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
            Destinations = [] }

      let expected =
        List.concat [ [ assembly ]
                      (Visitor.I.deeper >> Seq.toList) assembly
                      [ AfterAssembly
                          { Assembly = def
                            Inspection = Inspections.Ignore
                            Destinations = [] } ] ]

      Assert.That(deeper.Length, Is.EqualTo 4)
      Assert.That(deeper, Is.EquivalentTo expected)
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let TestFixPointInvoke () =
    let mutable called = 0

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
      CoverageParameters.showGenerated := true
      let where = Assembly.GetExecutingAssembly().Location
      let path = sample1path
      let accumulator = System.Collections.Generic.List<Node>()

      let fix =
        Visitor.encloseState
          (fun (x: System.Collections.Generic.List<Node>) t ->
            x.Add t
            x)
          accumulator

      let u = Guid.NewGuid().ToString()
      let u2 = Guid.NewGuid().ToString()
      let ux = [ u; u2 ]

      Visitor.visit [ fix ] [
        { AssemblyPath = path
          Destinations = ux }
      ]
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
            Destinations = ux }

      let expected =
        List.concat [ [ Start [ { AssemblyPath = path
                                  Destinations = ux } ]
                        assembly ]
                      (Visitor.I.deeper >> Seq.toList) assembly
                      [ AfterAssembly
                          { Assembly = def
                            Inspection = Inspections.Instrument
                            Destinations = ux }
                        Finish ] ]

      Assert.That(
        accumulator |> Seq.map string,
        Is.EquivalentTo(expected |> Seq.map string)
      )
    finally
      CoverageParameters.showGenerated := false

  [<Test>]
  let TrackingDetectsTests () =
    let path = Path.Combine(dir, "Sample2.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    ProgramDatabase.readSymbols def

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.AddRange([ "Junk"; "[MoreJunk"; "[Test" ])
      Visitor.visit [] [] // cheat reset

      let tracks =
        def.MainModule.GetAllTypes()
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.map (Visitor.I.track)
        |> Seq.choose id
        |> Seq.toList

      Assert.That(
        tracks,
        Is.EquivalentTo [ (1, "[Test")
                          (2, "[Test") ]
      )
    finally
      CoverageParameters.trackingNames.Clear()
      Visitor.visit [] [] // cheat reset

  [<Test>]
  let TrackingDetectsExpectedTests () =
    let path = Path.Combine(dir, "Sample2.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    ProgramDatabase.readSymbols def

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.AddRange([ "Junk"; "[MoreJunk"; "[Test" ])
      Visitor.visit [] [] // cheat reset

      let tracks =
        def.MainModule.GetAllTypes()
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.filter (Visitor.I.track >> Option.isSome)
        |> Seq.map (fun m -> m.Name)
        |> Seq.toList

      Assert.That(
        tracks,
        Is.EquivalentTo [ "testMakeUnion"
                          "testMakeThing" ]
      )
    finally
      CoverageParameters.trackingNames.Clear()
      Visitor.visit [] [] // cheat reset

  [<Test>]
  let TrackingDetectsTestsByFullType () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample2.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

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
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.map (Visitor.I.track)
        |> Seq.choose id
        |> Seq.toList

      Assert.That(
        tracks,
        Is.EquivalentTo [ (1, "[NUnit.Framework.TestAttribute]")
                          (2, "[NUnit.Framework.TestAttribute]") ]
      )
    finally
      CoverageParameters.trackingNames.Clear()
      Visitor.visit [] [] // cheat reset

  [<Test>]
  let TrackingDetectsMethods () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample2.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

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
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.map (Visitor.I.track)
        |> Seq.choose id
        |> Seq.toList

      Assert.That(
        tracks,
        Is.EquivalentTo [ (1, "returnFoo")
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
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map Naming.I.typeName
      |> Seq.toList

    let expected =
      [ "<Module>"
        "Class1"
        "Class2"
        "Class3"
        "Class4" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let FullTypeNamesAreExtracted () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map Naming.fullTypeName
      |> Seq.toList

    let expected =
      [ "<Module>"
        "Sample3.Class1"
        "Sample3.Class2"
        "Sample3.Class3"
        "Sample3.Class3+Class4" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let TypeRefNamesAreExtracted () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map
           (fun td ->
             Naming.I.typeRefName (
               TypeReference(td.Namespace, td.Name, def.MainModule, null)
             ))
      |> Seq.toList

    let expected =
      [ "<Module>"
        "Class1"
        "Class2"
        "Class3"
        "Class4" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let FullTypeRefNamesAreExtracted () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.map
           (fun td ->
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
        "Sample3.Class1"
        "Sample3.Class2"
        "Sample3.Class3"
        "Sample3.Class3+Class4" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let MethodNamesAreExtracted () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.map Naming.I.methodName
      |> Seq.toList

    let expected =
      [ "get_Property"
        "set_Property"
        "#ctor"
        "get_Property"
        "set_Property"
        "#ctor"
        "get_Visits"
        "Log"
        "GetOperandType"
        "#ctor"
        ".cctor"
        "get_Defer"
        "set_Defer"
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
        "ToList"
        "#ctor" ]

    Assert.That(names, Is.EquivalentTo expected)

  [<Test>]
  let FullMethodNamesAreExtracted () =
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(dir, "Sample3.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let names =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.map Naming.fullMethodName
      |> Seq.toList

    let expected =
      [ "System.Int32 Sample3.Class1.get_Property()"
        "System.Void Sample3.Class1.set_Property(System.Int32)"
        "System.Void Sample3.Class1.#ctor()"
        "System.Int32 Sample3.Class2.get_Property()"
        "System.Void Sample3.Class2.set_Property(System.Int32)"
        "System.Void Sample3.Class2.#ctor()"
        "System.Collections.Generic.List`1 Sample3.Class3.get_Visits()"
        "System.Void Sample3.Class3.Log(System.String,System.Int32)"
        "System.Int32 Sample3.Class3.GetOperandType(Mono.Cecil.Cil.Instruction)"
        "System.Void Sample3.Class3.#ctor()"
        "System.Void Sample3.Class3..cctor()"
        "System.Boolean Sample3.Class3+Class4.get_Defer()"
        "System.Void Sample3.Class3+Class4.set_Defer(System.Boolean)"
        "Sample3.Class1 Sample3.Class3+Class4.get_Property()"
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
        "System.Collections.Generic.List`1 Sample3.Class3+Class4.ToList<T>(T)"
        "System.Void Sample3.Class3+Class4.#ctor()" ]

    Assert.That(names, Is.EquivalentTo expected)

  // Report.fs
  let TTBaseline = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
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

  let rec private recursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length
    Assert.That(rcount, Is.EqualTo(ecount), "Mismatch at depth " + depth.ToString())

    Seq.zip result expected
    |> Seq.iter
         (fun (r: XElement, e: XElement) ->
           Assert.That(r.Name, Is.EqualTo(e.Name), "Expected name " + e.Name.ToString())
           let ra = r.Attributes()
           let ea = e.Attributes()

           Seq.zip ra ea
           |> Seq.iter
                (fun (a1: XAttribute, a2: XAttribute) ->
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

                      Assert.That(
                        a1.Value,
                        Is.EqualTo(expected),
                        r.ToString() + " -> visitcount"
                      )
                  | _ ->
                      Assert.That(
                        a1.Value.Replace("\\", "/"),
                        Is.EqualTo(a2.Value.Replace("\\", "/")),
                        r.ToString() + " -> " + a1.Name.ToString()
                      ))

           recursiveValidate(r.Elements()) (e.Elements()) (depth + 1) zero)

  let makeDocument (f: Stream -> unit) =
    use stash = new MemoryStream()
    stash |> f
    stash.Position <- 0L
    XDocument.Load stash

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNet () =
    let visitor, document = Report.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      use def =
        Mono.Cecil.AssemblyDefinition.ReadAssembly path

      let xml = TTBaseline

      let xml' =
        xml.Replace("Version=1.0.0.0", "Version=" + def.Name.Version.ToString())

      let xml'' =
        xml'.Replace("name=\"Sample1.exe\"", "name=\"" + path + "\"")

      let baseline =
        XDocument.Load(new System.IO.StringReader(xml''))

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidate result expected 0 true
    finally
      CoverageParameters.nameFilters.Clear()

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
      let visitor, document = Main.I.selectReportGenerator ()

      let path =
        Path.Combine(
          SolutionDir(),
          "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"
        )

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
            Destinations = [] })

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo ReportFormat.NativeJsonWithTracking
      )

      let result = makeJson document

      let nativeJson =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("Sample4.native.json", StringComparison.Ordinal))

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(nativeJson)

      use reader = new StreamReader(stream)

      let expected =
        reader
          .ReadToEnd()
          .Replace(
            "Tests.fs",
            Path
              .GetFullPath(Path.Combine(SolutionRoot.location,
                                        "Samples/Sample4",
                                        "Tests.fs"))
              .Replace("\\", "\\\\")
          )
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])
      //printfn "%s" result
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
  let ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly () =
    let visitor, document = Report.reportGenerator ()
    let path = sample4path

    try
      CoverageParameters.methodPoint := true

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      (makeDocument document)
        .Descendants(XName.Get "method")
      |> Seq.iter
           (fun mx ->
             let sx = mx.Descendants(XName.Get "seqpnt")
             test <@ sx |> Seq.length = 1 @>)
    finally
      CoverageParameters.methodPoint := false

  [<Test>]
  let ShouldGenerateExpectedXmlReportForNCoverWithTopLevel () =
    let path = sample4path

    let path5 =
      sample4path
        .Replace("4", "5")
        .Replace("572", "472")
        .Replace("netcoreapp2.1", "netstandard2.0")

    let path6 =
      sample4path
        .Replace("4", "6")
        .Replace("672", "472")
        .Replace("2.1", "2.0")

    try
      AltCoverRunnerTests.mainInit ()

      let visitor1, document1 = Report.reportGenerator ()

      Visitor.visit
        [ visitor1 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      let names1 =
        (makeDocument document1)
          .Descendants(XName.Get "method")
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

      let visitor2, document2 = Report.reportGenerator ()

      Visitor.visit
        [ visitor2 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      let names2 =
        (makeDocument document2)
          .Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map (fun mx -> mx.Attribute(XName.Get "name").Value)
        |> Seq.filter (fun n -> n <> "Main")
        |> Seq.sortBy
             (fun n ->
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
        <@ names2 = [ "bytes"
                      "makeThing"
                      "testMakeThing" ] @>

      { Scope = Attribute
        Regex = Regex "AutoSerializable"
        Sense = Exclude }
      |> CoverageParameters.topLevel.Add

      let visitor3, document3 = Report.reportGenerator ()

      Visitor.visit
        [ visitor3 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      let names3 =
        (makeDocument document3)
          .Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map (fun mx -> mx.Attribute(XName.Get "name").Value)
        |> Seq.filter (fun n -> n <> "Main")
        |> Seq.sortBy
             (fun n ->
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

      let visitor5, document5 = Report.reportGenerator ()

      Visitor.visit
        [ visitor5 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      let names5 =
        (makeDocument document5)
          .Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map (fun mx -> mx.Attribute(XName.Get "name").Value)
        |> Seq.filter (fun n -> n <> "Main")
        |> Seq.sortBy
             (fun n ->
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

      let visitor6, document6 = Report.reportGenerator ()

      Visitor.visit
        [ visitor6 ]
        (Visitor.I.toSeq
          { AssemblyPath = path6
            Destinations = [] })

      let names6 =
        (makeDocument document6)
          .Descendants(XName.Get "method")
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

      let visitor7, document7 = Report.reportGenerator ()

      Visitor.visit
        [ visitor7 ]
        (Visitor.I.toSeq
          { AssemblyPath = path6
            Destinations = [] })

      let names7 =
        (makeDocument document7)
          .Descendants(XName.Get "method")
        |> Seq.filter (fun mx -> mx.Attribute(XName.Get "excluded").Value = "true")
        |> Seq.map
             (fun mx ->
               (mx.Attribute(XName.Get "name").Value + "    ",
                mx.Attribute(XName.Get "class").Value))
        |> Seq.sortBy
             (fun (n, _) ->
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
        <@ names7 = [ "Sample6.Module.F1"
                      "Sample6.Module.F2"
                      "Sample6.Module+FII@12T.Invoke"
                      "Sample6.Module+FI@11T.Invoke"
                      "Sample6.Module+F1@17.Invoke" ] @>

      CoverageParameters.topLevel.Clear()
      CoverageParameters.nameFilters.Clear()

      { Scope = Attribute
        Regex = Regex "Exclude"
        Sense = Exclude }
      |> CoverageParameters.nameFilters.Add

      let visitor8, document8 = Report.reportGenerator ()

      Visitor.visit
        [ visitor8 ]
        (Visitor.I.toSeq
          { AssemblyPath = path5
            Destinations = [] })

      let names8 =
        (makeDocument document8)
          .Descendants(XName.Get "method")
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

      let seqTrim (s: String seq) = s |> Seq.map (fun n -> n.Trim())

      let visitor9, document9 = Report.reportGenerator ()

      Visitor.visit
        [ visitor9 ]
        (Visitor.I.toSeq
          { AssemblyPath = path5
            Destinations = [] })

      let names9 =
        (makeDocument document9)
          .Descendants(XName.Get "method")
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
      let visitor4, document4 = Report.reportGenerator ()

      Visitor.visit
        [ visitor4 ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      let names4 =
        (makeDocument document4)
          .Descendants(XName.Get "method")
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
    let where = Assembly.GetExecutingAssembly().Location
    let path = sample4path

    try
      CoverageParameters.methodPoint := true
      CoverageParameters.theReportFormat <- None

      let visitor, document = Main.I.selectReportGenerator () //OpenCover.reportGenerator()

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      (makeDocument document)
        .Descendants(XName.Get "Method")
      |> Seq.iter
           (fun mx ->
             let sx =
               mx.Descendants(XName.Get "SequencePoint")

             test <@ sx |> Seq.length = 1 @>)
    finally
      CoverageParameters.methodPoint := false

  [<Test>]
  let LocateMatchFallsBackOK () =
    let file = Assembly.GetExecutingAssembly().Location
    let empty = Dictionary<string, string>()
    test <@ Visitor.I.locateMatch file empty = file @>

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithSourceLink () =
    let visitor, document = Report.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let here = SolutionDir()

    let path =
      Path.Combine(here, "_SourceLink/Sample14.dll")

    try
      CoverageParameters.sourcelink := true
      CoverageParameters.staticFilter <- Some StaticFilter.NoFilter

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      Assert.That(
        Visitor.sourceLinkDocuments |> Option.isSome,
        "Documents should be present"
      )

      let map =
        Visitor.sourceLinkDocuments |> Option.get

      let url =
        map.Values
        |> Seq.find (fun f -> f.EndsWith("*", StringComparison.Ordinal))

      let files =
        (makeDocument document)
          .Descendants(XName.Get "seqpnt")
        |> Seq.map (fun s -> s.Attribute(XName.Get "document").Value)
        |> Seq.distinct
        |> Seq.filter (fun f -> f.StartsWith("https://", StringComparison.Ordinal))
        |> Seq.sort
        |> Seq.toList

      let expected =
        [ url.Replace("*", "Samples/Sample14/Sample14/Program.cs")
          url.Replace("*", "Samples/Sample5/Class1.cs") ]

      Assert.That(files, Is.EquivalentTo expected)

      let untracked =
        (makeDocument document)
          .Descendants(XName.Get "seqpnt")
        |> Seq.map (fun s -> s.Attribute(XName.Get "document").Value)
        |> Seq.distinct
        |> Seq.filter
             (fun f ->
               f.StartsWith("https://", StringComparison.Ordinal)
               |> not)
        |> Seq.map Path.GetFileName
        |> Seq.sort
        |> Seq.toList

      let expected2 =
        [ "Class2.cs"
          "Sample14.SourceLink.Class3.cs" ]

      Assert.That(untracked, Is.EquivalentTo expected2)

    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.sourcelink := false
      CoverageParameters.staticFilter <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter () =
    let visitor, document = Report.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      use def =
        Mono.Cecil.AssemblyDefinition.ReadAssembly path

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

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidate result expected 0 true
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded () =
    let visitor, document = Report.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      use def =
        Mono.Cecil.AssemblyDefinition.ReadAssembly path

      let xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
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

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidate result expected 0 true
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked () =
    let visitor, document = Report.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      use def =
        Mono.Cecil.AssemblyDefinition.ReadAssembly path

      let xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
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

      let result = (makeDocument document).Elements()
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

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let target =
      def
        .MainModule
        .GetType("Sample3.Class2")
        .GetMethods()
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

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let target =
      def
        .MainModule
        .GetType("Sample3.Class3")
        .GetMethods()
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
    let where = Assembly.GetExecutingAssembly().Location

    let path =
      Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
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
        |> Seq.map
             (fun n ->
               match n with
               | BranchPoint b -> Some b
               | _ -> None)
        |> Seq.choose id
        |> Seq.toList
      // The only overt branching in this function are the 4 match cases
      // Internal IL conditional branching is a compiler thing from inlining "string"
      Assert.That(branches |> Seq.length, Is.EqualTo 4)
      let branch = branches |> Seq.head
      Assert.That(branch.Target.Length, Is.EqualTo 2)
      let xbranch = XElement(XName.Get "test")
      OpenCover.I.setChain xbranch branch
      Assert.That(xbranch.ToString(), Is.EqualTo """<test offsetchain="29" />""")
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let BranchChainsTerminate () =
    let where = Assembly.GetExecutingAssembly().Location

    let path =
      Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")

    use def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      def.MainModule.GetAllTypes()
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.find (fun m -> m.Name = "as_bar")

    let fin = method.Body.Instructions |> Seq.last
    let list = Visitor.I.getJumpChain fin fin
    Assert.That(list, Is.EquivalentTo [ fin ])

  let rec private recursiveValidateOpenCover result expected' depth zero expectSkipped =
    let xn name = XName.Get(name)
    let rcount = result |> Seq.length

    let expected =
      expected'
      |> Seq.filter
           (fun (el: XElement) ->
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
    |> Seq.iter
         (fun (r: XElement, e: XElement) ->
           Assert.That(r.Name, Is.EqualTo(e.Name), "Expected name " + e.Name.ToString())
           let ra = r.Attributes()
           let ea = e.Attributes()

           Seq.zip ra ea
           |> Seq.iter
                (fun (a1: XAttribute, a2: XAttribute) ->
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
                        a1
                          .Value
                          .Replace("\\", "/")
                          .Replace("Samples/", String.Empty)
                          .Replace("altcover", "AltCover"),
                        Does.EndWith(
                          a2
                            .Value
                            .Replace("\\", "/")
                            .Replace("altcover", "AltCover")
                        ),
                        a1.Name.ToString()
                        + " : "
                        + r.ToString()
                        + " -> document"
                      )
                  | "vc" ->
                      let expected = Maybe zero "0" a2.Value

                      Assert.That(
                        a1.Value,
                        Is.EqualTo(expected),
                        r.ToString() + " -> visitcount"
                      )
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
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let here = SolutionDir()

    let path =
      Path.Combine(here, "_SourceLink/Sample14.dll")

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover
      CoverageParameters.sourcelink := true

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      Assert.That(
        Visitor.sourceLinkDocuments |> Option.isSome,
        "Documents should be present"
      )

      let map =
        Visitor.sourceLinkDocuments |> Option.get

      let url =
        map.Values
        |> Seq.find (fun f -> f.EndsWith("*", StringComparison.Ordinal))

      let files =
        (makeDocument document)
          .Descendants(XName.Get "File")
        |> Seq.map (fun s -> s.Attribute(XName.Get "fullPath").Value)
        |> Seq.filter (fun f -> f.StartsWith("https://", StringComparison.Ordinal))
        |> Seq.sort
        |> Seq.toList

      let expected =
        [ url.Replace("*", "Samples/Sample14/Sample14/Program.cs")
          url.Replace("*", "Samples/Sample5/Class1.cs") ]

      Assert.That(files, Is.EquivalentTo expected)

      let untracked =
        (makeDocument document)
          .Descendants(XName.Get "File")
        |> Seq.map (fun s -> s.Attribute(XName.Get "fullPath").Value)
        |> Seq.filter
             (fun f ->
               f.StartsWith("https://", StringComparison.Ordinal)
               |> not)
        |> Seq.map Path.GetFileName
        |> Seq.sort
        |> Seq.toList

      let expected2 =
        [ "Class2.cs"
          "Sample14.SourceLink.Class3.cs" ]

      Assert.That(untracked, Is.EquivalentTo expected2)
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.sourcelink := false
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = sample1path

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Destinations = [] })

      let resource =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)
      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      let resource =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

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
      |> Seq.iter (fun x -> x.Remove())

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      let resource =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)
      // strip out line information
      baseline.Descendants(xn "Summary")
      |> Seq.filter (fun x -> x.Attribute(xn "numSequencePoints").Value = "10")
      |> Seq.iter (fun x -> x.Attribute(xn "numSequencePoints").Value <- "0")

      baseline.Descendants(xn "SequencePoint")
      |> Seq.toList
      |> Seq.iter (fun x -> x.Remove())

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

  let AddTrackingForMain xml =
    let resource =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith(xml, StringComparison.Ordinal))

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let tail =
      baseline.Descendants(XName.Get "Module")
      |> Seq.last

    let tracked = XElement(XName.Get "TrackedMethods")
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
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      let baseline =
        AddTrackingForMain "Sample1WithOpenCover.xml"

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.theReportFormat <- None
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

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

      let baseline =
        XDocument.Load(new System.IO.StringReader(raw))

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true true
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

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

      let baseline =
        XDocument.Load(new System.IO.StringReader(raw))

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true true
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      let resource =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1ClassExclusion.xml", StringComparison.Ordinal))

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)
      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = sample1path

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.Add("Main")

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo ReportFormat.OpenCoverWithTracking
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
            Destinations = [] })

      let baseline =
        AddTrackingForMain "Sample1ClassExclusion.xml"

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      let resource =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1MethodExclusion.xml", StringComparison.Ordinal))

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)
      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
            Destinations = [] })

      let resource =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1MethodExclusion.xml", StringComparison.Ordinal))

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)

      baseline.Descendants(XName.Get "Method")
      |> Seq.filter (fun x -> x.Attributes(XName.Get "skippedDueTo").Any())
      |> Seq.iter
           (fun x ->
             x.SetAttributeValue(XName.Get "skippedDueTo", "File")

             x.Descendants(XName.Get "Summary")
             |> Seq.toList
             |> Seq.iter (fun x -> x.Remove()))
      //|> Seq.iter (fun s -> s.SetAttributeValue(XName.Get "maxCyclomaticComplexity", "2")
      //                      s.SetAttributeValue(XName.Get "minCyclomaticComplexity", "2")))
      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()
    CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = sample1path

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.trackingNames.Add("Main")

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo ReportFormat.OpenCoverWithTracking
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
            Destinations = [] })

      let baseline =
        AddTrackingForMain "Sample1MethodExclusion.xml"

      let result = (makeDocument document).Elements()
      let expected = baseline.Elements()
      recursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()

    let sample21trad =
      Path.Combine(SolutionDir(), "./Samples/Sample21/bin/Debug/net472/Sample21.dll")

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
            Destinations = [] })

      let doc = makeDocument document

      let classes =
        doc.Descendants(XName.Get "FullName")
        |> Seq.map (fun x -> x.Value)
        |> Seq.filter (Seq.head >> Char.IsLetterOrDigit)
        |> Seq.sort
        |> Seq.toList

      let methods =
        doc.Descendants(XName.Get "Name")
        |> Seq.map (fun x -> x.Value)
        |> Seq.sort
        |> Seq.toList

      test
        <@ classes = [ "Sample21.Tests"
                       "Sample21.Traditional" ] @>

      let expectedMethods =
        [ "System.String Sample21.Traditional::DoSomething()"
          "System.Void Sample21.Tests::.ctor()"
          "System.Void Sample21.Tests::Setup()"
          "System.Void Sample21.Tests::Test1()"
          "System.Void Sample21.Traditional::.ctor()" ]

      test <@ methods = expectedMethods @>
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle () =
    let visitor, document = OpenCover.reportGenerator ()

    let sample21 =
      Path.Combine(SolutionDir(), "./Samples/Sample21/bin/Debug/net5.0/Sample21.dll")

    Assert.That(File.Exists sample21, "Test file Sample21 for net5.0 not built")

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
            Destinations = [] })

      let doc = makeDocument document

      let classes =
        doc.Descendants(XName.Get "FullName")
        |> Seq.filter
             (fun x ->
               x.Parent.Attribute(XName.Get "skippedDueTo")
               |> isNull)
        |> Seq.map (fun x -> x.Value)
        |> Seq.filter (Seq.head >> Char.IsLetterOrDigit)
        |> Seq.sort
        |> Seq.toList

      let methods =
        doc.Descendants(XName.Get "Name")
        |> Seq.map (fun x -> x.Value)
        |> Seq.sort
        |> Seq.toList

      // document.Save(@"C:\Users\steve\Documents\GitHub\altcover\Sample21Modern.xml")

      let expectedTypes =
        [ "Sample21.IModern"
          "Sample21.Modern1"
          "Sample21.Modern2"
          "Sample21.Tests"
          "Sample21.Traditional" ]

      test <@ classes = expectedTypes @>

      let expectedMethods =
        [ "System.String Sample21.IModern::DoSomething()"
          "System.String Sample21.Modern2::DoSomething()"
          "System.String Sample21.Traditional::DoSomething()"
          "System.Void Sample21.Modern1::.ctor()"
          "System.Void Sample21.Modern2::.ctor()"
          "System.Void Sample21.Tests::.ctor()"
          "System.Void Sample21.Tests::Setup()"
          "System.Void Sample21.Tests::Test1()"
          "System.Void Sample21.Traditional::.ctor()" ]

      test <@ methods = expectedMethods @>
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ShouldSortFileIds () =
    let visitor, document = OpenCover.reportGenerator ()
    let xn name = XName.Get(name)

    let path =
      Path.Combine(
        SolutionDir(),
        "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.Engine.dll"
      )

    Visitor.visit
      [ visitor ]
      (Visitor.I.toSeq
        { AssemblyPath = path
          Destinations = [] })

    let doc = makeDocument document
    Assert.That(doc.Descendants(xn "Module") |> Seq.length, Is.EqualTo 1)
    Assert.That(doc.Descendants(xn "File") |> Seq.length, Is.GreaterThan 1)

    doc.Descendants(xn "File")
    |> Seq.iteri
         (fun i x -> Assert.That(x.Attribute(xn "uid").Value, Is.EqualTo(string (1 + i))))