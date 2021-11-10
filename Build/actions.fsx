open System
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq

open Fake.Core
open Fake.DotNet
open Fake.IO.FileSystemOperators
open Fake.IO
open Fake.IO.Globbing.Operators

open Markdig
open NUnit.Framework
open Swensen.Unquote
open YamlDotNet.RepresentationModel

open AltCoverFake.DotNet.Testing

module Actions =
    let Clean () =
        let rec clean1 depth =
            try
                (DirectoryInfo ".")
                    .GetDirectories("*", SearchOption.AllDirectories)
                |> Seq.filter
                    (fun x ->
                        x.Name.StartsWith "_"
                        || x.Name = "bin"
                        || x.Name = "obj")
                |> Seq.filter
                    (fun n ->
                        "packages"
                        |> Path.GetFullPath
                        |> n.FullName.StartsWith
                        |> not)
                |> Seq.map (fun x -> x.FullName)
                |> Seq.distinct
                // arrange so leaves get deleted first, avoiding "does not exist" warnings
                |> Seq.groupBy
                    (fun x ->
                        x
                        |> Seq.filter (fun c -> c = '\\' || c = '/')
                        |> Seq.length)
                |> Seq.map (fun (n, x) -> (n, x |> Seq.sort))
                |> Seq.sortBy (fst >> ((*) -1))
                |> Seq.collect snd
                |> Seq.iter
                    (fun n ->
                        printfn "Deleting %s" n
                        Directory.Delete(n, true))

                !!(@"./*Tests/*.tests.core.fsproj")
                |> Seq.map
                    (fun f ->
                        (Path.GetDirectoryName f)
                        @@ "coverage.opencover.xml")
                |> Seq.iter File.Delete

                let temp = Environment.environVar "TEMP"

                if not <| String.IsNullOrWhiteSpace temp then
                    Directory.GetFiles(temp, "*.tmp.dll.mdb")
                    |> Seq.iter File.Delete
            with
            | :? System.IO.IOException as x -> clean' (x :> Exception) depth
            | :? System.UnauthorizedAccessException as x -> clean' (x :> Exception) depth

        and clean' x depth =
            printfn "looping after %A" x
            System.Threading.Thread.Sleep(500)

            if depth < 10 then
                clean1 (depth + 1)
            else
                Assert.Fail "Could not clean all the files"

        clean1 0

    let template = """namespace AltCover
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyDescription("Part of a cross-platform coverage gathering and processing tool set for .net/.net core and Mono")>]

#if DEBUG
[<assembly: AssemblyConfiguration("Debug {0}")>]
[<assembly: InternalsVisibleTo("AltCover.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Api.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Api.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Recorder.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Recorder.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Recorder2.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Recorder2.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Tests.Visualizer, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Tests.Visualizer, PublicKey={2}")>]
#else
[<assembly: AssemblyConfiguration("Release {0}")>]
#endif
do ()"""

    let prefix =
        [| 0x00uy
           0x24uy
           0x00uy
           0x00uy
           0x04uy
           0x80uy
           0x00uy
           0x00uy
           0x94uy
           0x00uy
           0x00uy
           0x00uy |]

    let GetPublicKey (stream: Stream) =
        // see https://social.msdn.microsoft.com/Forums/vstudio/en-US/d9ef264e-1a74-4f48-b93f-3e2c7902f660/determine-contents-of-a-strong-name-key-file-snk?forum=netfxbcl
        // for the exact format; this is a stripped down hack

        let buffer = Array.create 148 0uy
        let size = stream.Read(buffer, 0, buffer.Length)
        Assert.That(size, Is.EqualTo buffer.Length)
        Assert.That(buffer.[0], Is.EqualTo 7uy) // private key blob
        buffer.[0] <- 6uy // public key blob
        Assert.That(buffer.[11], Is.EqualTo 0x32uy) // RSA2 magic number
        buffer.[11] <- 0x31uy // RSA1 magic number
        Array.append prefix buffer

    let InternalsVisibleTo version =
        let stream2 =
            new System.IO.FileStream("./Build/SelfTest.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)

        //let pair2 = StrongNameKeyPair(stream2)
        //let key2 = BitConverter.ToString pair2.PublicKey
        let key2 =
            stream2 |> GetPublicKey |> BitConverter.ToString

        let stream =
            new System.IO.FileStream("./Build/Infrastructure.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)

        //let pair = StrongNameKeyPair(stream)
        //let key = BitConverter.ToString pair.PublicKey
        let key =
            stream |> GetPublicKey |> BitConverter.ToString

        let file =
            String.Format(
                System.Globalization.CultureInfo.InvariantCulture,
                template,
                version,
                key.Replace("-", String.Empty),
                key2.Replace("-", String.Empty)
            )

        let path = "_Generated/VisibleToTest.fs"

        // Update the file only if it would change
        let old =
            if File.Exists(path) then
                File.ReadAllText(path)
            else
                String.Empty

        if not (old.Equals(file)) then
            File.WriteAllText(path, file)

    let GetVersionFromYaml () =
        use yaml =
            new FileStream(
                "appveyor.yml",
                FileMode.Open,
                FileAccess.ReadWrite,
                FileShare.None,
                4096,
                FileOptions.SequentialScan
            )

        use yreader = new StreamReader(yaml)
        let ystream = new YamlStream()
        ystream.Load(yreader)

        let mapping =
            ystream.Documents.[0].RootNode :?> YamlMappingNode

        (mapping.Children :> System.Collections.Generic.IDictionary<YamlNode, YamlNode>).[YamlScalarNode("version")]
        |> string

    let LocalVersion appveyor (version: string) =
        let now = DateTimeOffset.UtcNow

        let epoch =
            DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan(int64 0))

        let diff = now.Subtract(epoch)

        let fraction =
            diff.Subtract(TimeSpan.FromDays(float diff.Days))

        let revision = ((int fraction.TotalSeconds) / 3)

        let majmin =
            String.Join(".", version.Split('.') |> Seq.take 2)

        let result =
            if String.IsNullOrWhiteSpace appveyor then
                sprintf "%s.%d.%d" majmin diff.Days revision
            else
                appveyor

        printfn "Build version : %s" version
        (result, majmin, now.Year)

    let FixMVId files =
        // Fix up symbol file to have the MVId emitted by the System.Reflection.Emit code
        files
        |> Seq.iter
            (fun f ->
                let assembly =
                    System.Reflection.Assembly.LoadFrom(Path.GetFullPath f)

                let mvid = assembly.ManifestModule.ModuleVersionId
                let symbols = System.IO.File.ReadAllBytes(f + ".mdb")

                mvid.ToByteArray()
                |> Array.iteri (fun i x -> symbols.[i + 16] <- x)

                System.IO.File.WriteAllBytes(f + ".mdb", symbols))

    let ValidateFSharpTypes simpleReport others =
        use coverageFile =
            new FileStream(
                simpleReport,
                FileMode.Open,
                FileAccess.Read,
                FileShare.None,
                4096,
                FileOptions.SequentialScan
            )
        // Edit xml report to store new hits
        let coverageDocument =
            XDocument.Load(XmlReader.Create(coverageFile))

        let recorded =
            coverageDocument.Descendants(XName.Get("method"))
            |> Seq.map (fun x -> x.Attribute(XName.Get("name")).Value)
            |> Seq.filter (fun x -> others |> Seq.exists (fun y -> x = y) |> not)
            |> Seq.sort
            |> Seq.toList

        let expected =
            ".ctor Invoke as_bar bytes get_MyBar makeThing returnBar returnFoo testMakeThing testMakeUnion"

        Assert.That(recorded, expected.Split() |> Is.EquivalentTo, sprintf "Bad method list %A" recorded)

    let ValidateFSharpTypesCoverage simpleReport =
        use coverageFile =
            new FileStream(
                simpleReport,
                FileMode.Open,
                FileAccess.Read,
                FileShare.None,
                4096,
                FileOptions.SequentialScan
            )
        // Edit xml report to store new hits
        let coverageDocument =
            XDocument.Load(XmlReader.Create(coverageFile))

        let recorded =
            coverageDocument.Descendants(XName.Get("seqpnt"))
            |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
            |> Seq.toList

        let expected =
          "0 1 1 1 0 1 0 1 0 1 1 1 0 0 0 0 0 0 0 0 0 2 1 0 1 0 1"
        //"0 1 1 1 0 1 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 2 1 0 1 0 1"
        //"0 1 1 1 0 1 0 1 0 1 0 0 0 0 0 0 0 2 1 0 1 0 1"
        Assert.That(String.Join(" ", recorded), expected |> Is.EqualTo, sprintf "Bad visit list %A" recorded)

    let ValidateSample1 simpleReport sigil =
        // get recorded details from here
        use coverageFile =
            new FileStream(
                simpleReport,
                FileMode.Open,
                FileAccess.Read,
                FileShare.None,
                4096,
                FileOptions.SequentialScan
            )

        let coverageDocument =
            XDocument.Load(XmlReader.Create(coverageFile))

        let recorded =
            coverageDocument.Descendants(XName.Get("seqpnt"))
            |> Seq.toList

        let zero =
            recorded
            |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "0")
            |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
            |> Seq.sort
            |> Seq.toList

        let ones =
            recorded
            |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "1")
            |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
            |> Seq.sort
            |> Seq.toList

        Assert.That(
            List.length recorded,
            Is.EqualTo((List.length ones) + (List.length zero)),
            "unexpected visits in " + sigil
        )

        let zero' = zero |> Seq.distinct |> Seq.toList

        Assert.That(
            zero',
            Is.EquivalentTo [ "18"; "19"; "20" ],
            "wrong unvisited in "
            + sigil
            + " : "
            + (sprintf "%A" zero')
        )

        let ones' = ones |> Seq.distinct |> Seq.toList

        Assert.That(
            ones',
            Is.EquivalentTo [ "11"
                              "12"
                              "13"
                              "14"
                              "15"
                              "16"
                              "21" ],
            "wrong number of visited in "
            + sigil
            + " : "
            + (sprintf "%A" zero')
        )

    let HandleResults (msg: string) (result: Fake.Core.ProcessResult) =
        String.Join(Environment.NewLine, result.Messages)
        |> printfn "%s"

        let save =
            (Console.ForegroundColor, Console.BackgroundColor)

        match result.Errors |> Seq.toList with
        | [] -> ()
        | errors ->
            try
                Console.ForegroundColor <- ConsoleColor.Black
                Console.BackgroundColor <- ConsoleColor.White

                String.Join(Environment.NewLine, errors)
                |> printfn "ERR : %s"
            finally
                Console.ForegroundColor <- fst save
                Console.BackgroundColor <- snd save

        Assert.That(result.ExitCode, Is.EqualTo 0, msg)

    let AssertResult (msg: string) (result: Fake.Core.ProcessResult<'a>) =
        Assert.That(result.ExitCode, Is.EqualTo 0, msg)

    let Run (file, dir, args) msg =
        CreateProcess.fromRawCommand file args
        |> CreateProcess.withWorkingDirectory dir
        |> CreateProcess.withFramework
        |> Proc.run
        |> (AssertResult msg)

    let RunDotnet (o: DotNet.Options -> DotNet.Options) cmd args msg =
        DotNet.exec o cmd args |> (HandleResults msg)

    let SimpleInstrumentingRun (samplePath: string) (binaryPath: string) (reportSigil: string) =
        printfn "Instrument and run a simple executable"
        Directory.ensure "./_Reports"

        let simpleReport =
            (Path.getFullName "./_Reports")
            @@ (reportSigil + ".xml")

        let binRoot = Path.getFullName binaryPath
        let sampleRoot = Path.getFullName samplePath
        let instrumented = "__Instrumented." + reportSigil

        let framework =
            Fake.DotNet.ToolType.CreateFullFramework()

        let prep =
            AltCover.PrepareOptions.Primitive
                { Primitive.PrepareOptions.Create() with
                      TypeFilter = [ """System\.""" ]
                      Report = simpleReport
                      OutputDirectories = [| "./" + instrumented |]
                      ReportFormat = "NCover"
                      InPlace = false
                      Save = false }
            |> AltCoverCommand.Prepare

        let parameters =
            { AltCoverCommand.Options.Create prep with
                  ToolPath = binRoot @@ "AltCover.exe"
                  ToolType = framework
                  WorkingDirectory = sampleRoot }

        AltCoverCommand.run parameters
        System.Threading.Thread.Sleep(1000)

        Run(sampleRoot @@ (instrumented + "/Sample1.exe"), (sampleRoot @@ instrumented), []) "Instrumented .exe failed"
        System.Threading.Thread.Sleep(1000)

        ValidateSample1 simpleReport reportSigil

    let SimpleInstrumentingRunUnderMono
        (samplePath: string)
        (binaryPath: string)
        (reportSigil': string)
        (monoOnWindows: string option)
        =
        printfn "Instrument and run a simple executable under mono"

        match monoOnWindows with
        | None -> Assert.Fail "Mono executable expected"
        | _ ->
            Directory.ensure "./_Reports"
            let reportSigil = reportSigil' + "UnderMono"

            let simpleReport =
                (Path.getFullName "./_Reports")
                @@ (reportSigil + ".xml")

            let binRoot = Path.getFullName binaryPath
            let sampleRoot = Path.getFullName samplePath
            let instrumented = "__Instrumented." + reportSigil

            let framework =
                Fake.DotNet.ToolType.CreateFullFramework()

            let prep =
                AltCover.PrepareOptions.Primitive
                    { Primitive.PrepareOptions.Create() with
                          TypeFilter = [ """System\.""" ]
                          Report = simpleReport
                          OutputDirectories = [| "./" + instrumented |]
                          ReportFormat = "NCover"
                          InPlace = false
                          Save = false }
                |> AltCoverCommand.Prepare

            let parameters =
                { AltCoverCommand.Options.Create prep with
                      ToolPath = binRoot @@ "AltCover.exe"
                      ToolType = framework
                      WorkingDirectory = sampleRoot }

            AltCoverCommand.runWithMono monoOnWindows parameters

            Run
                (sampleRoot @@ (instrumented + "/Sample1.exe"), (sampleRoot @@ instrumented), [])
                "Instrumented .exe failed"

            ValidateSample1 simpleReport reportSigil

    let PrepareReadMe packingCopyright readmemd =
        let readme = Path.getFullName readmemd
        let name = Path.GetFileNameWithoutExtension readmemd
        let document = File.ReadAllText readme
        let markdown = Markdown.ToHtml(document)

        let docHtml =
            """<?xml version="1.0"  encoding="utf-8"?>
<!DOCTYPE html>
<html lang="en">
<head>
<title>AltCover README</title>
<style>
body, html {
color: #000; background-color: #eee;
font-family: 'Segoe UI', 'Open Sans', Calibri, verdana, helvetica, arial, sans-serif;
position: absolute; top: 0px; width: 50em;margin: 1em; padding:0;
}
a {color: #444; text-decoration: none; font-weight: bold;}
a:hover {color: #ecc;}
</style>
</head>
<body>
"""
            + markdown
            + """
<footer><p style="text-align: center">"""
            + packingCopyright
            + """</p>
</footer>
</body>
</html>
"""

        let xmlform = XDocument.Parse docHtml
        let body = xmlform.Descendants(XName.Get "body")

        let eliminate =
            [ "Continuous Integration"
              "Building"
              "Thanks to" ]

        let keep = ref true

        let kill =
            body.Elements()
            |> Seq.map
                (fun x ->
                    match x.Name.LocalName with
                    | "h2" ->
                        keep.Value <-
                          (List.tryFind (fun e -> e = String.Concat(x.Nodes())) eliminate)
                           |> Option.isNone
                    | "footer" -> keep.Value <- true
                    | _ -> ()

                    if keep.Value then None else Some x)
            |> Seq.toList

        kill
        |> Seq.iter
            (fun q ->
                match q with
                | Some x -> x.Remove()
                | _ -> ())

        let packable =
            Path.getFullName ("./_Binaries/" + name + ".html")

        xmlform.Save packable

    let Check4Content (coverageDocument: XDocument) =
        let recorded =
            coverageDocument.Descendants(XName.Get("Method"))
            |> Seq.collect (fun x -> x.Descendants(XName.Get("Name")))
            |> Seq.map (fun x -> x.Value.Replace("Tests.Program", "Program/Program"))
            |> Seq.sort
            |> Seq.toList

        let expected =
            [ "Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,Tests.DU/MyUnion> Tests.DU/MyUnion::get_MyBar()"
              "System.Byte[] Tests.M/Thing::bytes()"
              "System.Int32 Program/Program::main(System.String[])"
              "System.Void Tests.DU/MyClass::.ctor()"
              // "System.Void Tests.DU/get_MyBar@31::.ctor(Tests.DU/MyUnion)"
              "System.Void Tests.DU::testMakeUnion()"
              "System.Void Tests.M::testMakeThing()"
              "Tests.DU/MyUnion Tests.DU/MyUnion::as_bar()"
              "Tests.DU/MyUnion Tests.DU/get_MyBar@44::Invoke(Microsoft.FSharp.Core.Unit)"
              "Tests.DU/MyUnion Tests.DU::returnBar(System.String)"
              "Tests.DU/MyUnion Tests.DU::returnFoo(System.Int32)"
              "Tests.M/Thing Tests.M::makeThing(System.String)" ]

        Assert.That(recorded, expected |> Is.EquivalentTo, sprintf "Bad method list %A" recorded)
        printfn "Content OK"

    let CheckSample4Content x =
        do
            use coverageFile =
                new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)

            let coverageDocument =
                XDocument.Load(XmlReader.Create(coverageFile))

            Check4Content coverageDocument

    let ticksNow () =
        let now = System.DateTime.UtcNow
        now.Ticks

    let Check4Visits path before (coverageDocument: XDocument) =
        let recorded =
            coverageDocument.Descendants(XName.Get("SequencePoint"))
            |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
            |> Seq.toList

        let expected =
            "0 1 1 1 0 1 0 1 0 1 1 1 0 0 0 0 0 0 0 0 0 2 1 0 1 0 1"
            // "0 1 1 1 0 1 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 2 1 0 1 0 1"
            //"0 1 1 1 0 1 0 1 0 1 0 0 0 0 0 0 0 2 1 0 1 0 1"
        Assert.That(String.Join(" ", recorded), expected |> Is.EqualTo, sprintf "Bad visit list %A in %s" recorded path)
        printfn "Visits OK"

        coverageDocument.Descendants(XName.Get("SequencePoint"))
        |> Seq.iter
            (fun sp ->
                let vc =
                    Int32.Parse(sp.Attribute(XName.Get("vc")).Value)

                let vx =
                    sp.Descendants(XName.Get("Time"))
                    |> Seq.sumBy (fun x -> x.Attribute(XName.Get("vc")).Value |> Int32.Parse)

                Assert.That(vc, Is.EqualTo vx, sp.Value))

        let trackedFormat = """<TrackedMethods>
        <TrackedMethod uid="1" token="100663300" name="System.Void Tests.DU::testMakeUnion()" strategy="[Fact]" entry="{0}" exit="{1}" />
        <TrackedMethod uid="2" token="100663345" name="System.Void Tests.M::testMakeThing()" strategy="[Fact]" entry="{2}" exit="{3}" />
      </TrackedMethods>"""

        coverageDocument.Descendants(XName.Get("TrackedMethods"))
        |> Seq.iter
            (fun x ->
                // entry and exit times need to be tested
                let times =
                    x.Descendants(XName.Get("TrackedMethod"))
                    |> Seq.map
                        (fun m ->
                            (m.Attribute(XName.Get("uid")).Value,
                             m.Attribute(XName.Get("entry")).Value,
                             m.Attribute(XName.Get("exit")).Value))
                    |> Seq.sortBy (fun (u, _, _) -> Int32.Parse u)
                    |> Seq.collect
                        (fun (_, entry, exit) ->
                            let first = entry.TrimEnd('L') |> Int64.Parse
                            let second = exit.TrimEnd('L') |> Int64.Parse
                            test <@ before <= first @>
                            test <@ first <= second @>
                            test <@ second <= ticksNow () @>
                            [ entry; exit ])
                    |> Seq.toArray

                let tracked =
                    String.Format(trackedFormat, times.[0], times.[1], times.[2], times.[3])

                Assert.That(x.ToString().Replace("\r\n", "\n"), Is.EqualTo <| tracked.Replace("\r\n", "\n")))

        printfn "Tracked OK"

        Assert.That(
            coverageDocument.Descendants(XName.Get("TrackedMethodRef"))
            |> Seq.map (fun x -> x.ToString()),
            Is.EquivalentTo [ "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"1\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"2\" vc=\"2\" />"
                              "<TrackedMethodRef uid=\"2\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"2\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"2\" vc=\"1\" />"
                              "<TrackedMethodRef uid=\"2\" vc=\"1\" />" ]
        )

        printfn "TrackRefs OK"

    let CheckSample4Visits before x =
        do
            use coverageFile =
                new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)

            let coverageDocument =
                XDocument.Load(XmlReader.Create(coverageFile))

            Check4Visits x before coverageDocument

    let CheckSample4 before x =
        do
            use coverageFile =
                new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)

            let coverageDocument =
                XDocument.Load(XmlReader.Create(coverageFile))

            Check4Content coverageDocument
            Check4Visits x before coverageDocument