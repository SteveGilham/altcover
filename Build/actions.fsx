open System
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq

open Fake.Core
open Fake.DotNet
open Fake.IO.FileSystemOperators
open Fake.IO

open HeyRed.MarkdownSharp
open NUnit.Framework
open YamlDotNet.RepresentationModel

module Actions =
  let Clean () =
    let rec Clean1 depth =
      try
        (DirectoryInfo ".").GetDirectories("*", SearchOption.AllDirectories)
        |> Seq.filter (fun x -> x.Name.StartsWith "_" || x.Name = "bin" || x.Name = "obj")
        |> Seq.filter (fun n -> match n.Name with
                                | "obj" -> Path.Combine(n.FullName, "dotnet-fake.fsproj.nuget.g.props") |> File.Exists |> not
                                | _ -> true)
        |> Seq.filter (fun n -> "packages" |> Path.GetFullPath |> n.FullName.StartsWith |> not)
        |> Seq.map (fun x -> x.FullName)
        |> Seq.distinct
        // arrange so leaves get deleted first, avoiding "does not exist" warnings
        |> Seq.groupBy (fun x -> x |> Seq.filter (fun c -> c='\\' || c = '/') |> Seq.length)
        |> Seq.map (fun (n,x) -> (n, x |> Seq.sort))
        |> Seq.sortBy (fun p -> -1 * (fst p))
        |> Seq.map snd
        |> Seq.concat
        |> Seq.iter (fun n -> printfn "Deleting %s" n
                              Directory.Delete(n, true))

        let temp = Environment.environVar "TEMP"
        if not <| String.IsNullOrWhiteSpace temp then
            Directory.GetFiles(temp, "*.tmp.dll.mdb")
            |> Seq.iter File.Delete
       with
       | :? System.IO.IOException as x -> Clean' (x :> Exception) depth
       | :? System.UnauthorizedAccessException as x -> Clean' (x :> Exception) depth
    and Clean' x depth =
      printfn "looping after %A" x
      System.Threading.Thread.Sleep(500)
      if depth < 10 then Clean1 (depth + 1)
      else Assert.Fail "Could not clean all the files"

    Clean1 0

  let template ="""namespace AltCover
open System.Reflection
open System.Runtime.CompilerServices
#if DEBUG
[<assembly: AssemblyConfiguration("Debug {0}")>]
#else
[<assembly: AssemblyConfiguration("Release {0}")>]
#endif
#if NETSTANDARD2_0
[<assembly: InternalsVisibleTo("AltCover.Shadow.Adapter")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests")>]
#else
#if NETCOREAPP2_0
[<assembly: InternalsVisibleTo("AltCover.Tests")>]
[<assembly: InternalsVisibleTo("AltCover.XTests")>]

#else
[<assembly: InternalsVisibleTo("AltCover.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.XTests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.XTests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Adapter, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Adapter, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests2, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests2, PublicKey={2}")>]
#endif
#endif
()"""

  let prefix = [| 0x00uy; 0x24uy; 0x00uy; 0x00uy; 0x04uy; 0x80uy; 0x00uy; 0x00uy; 0x94uy; 0x00uy; 0x00uy; 0x00uy |]

  let GetPublicKey (stream:Stream) =
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
    let stream2 = new System.IO.FileStream("./Build/SelfTest.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    //let pair2 = StrongNameKeyPair(stream2)
    //let key2 = BitConverter.ToString pair2.PublicKey
    let key2 = stream2 |> GetPublicKey |> BitConverter.ToString

    let stream = new System.IO.FileStream("./Build/Infrastructure.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    //let pair = StrongNameKeyPair(stream)
    //let key = BitConverter.ToString pair.PublicKey
    let key = stream |> GetPublicKey |> BitConverter.ToString

    let file = String.Format(System.Globalization.CultureInfo.InvariantCulture,
                template, version, key.Replace("-", String.Empty), key2.Replace("-", String.Empty))

    let path = "_Generated/VisibleToTest.fs"
    // Update the file only if it would change
    let old = if File.Exists(path) then File.ReadAllText(path) else String.Empty
    if not (old.Equals(file)) then File.WriteAllText(path, file)

  let GetVersionFromYaml () =
    use yaml = new FileStream("appveyor.yml", FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
    use yreader = new StreamReader(yaml)
    let ystream = new YamlStream()
    ystream.Load(yreader)
    let mapping = ystream.Documents.[0].RootNode :?> YamlMappingNode
    string mapping.Children.[YamlScalarNode("version")]

  let LocalVersion appveyor (version:string) =
    let now = DateTimeOffset.UtcNow
    let epoch = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan(int64 0))
    let diff = now.Subtract(epoch)
    let fraction = diff.Subtract(TimeSpan.FromDays(float diff.Days))
    let revision= ((int fraction.TotalSeconds) / 3)

    let majmin = String.Join(".", version.Split('.') |> Seq.take 2)
    let result = if String.IsNullOrWhiteSpace appveyor then sprintf "%s.%d.%d" majmin diff.Days revision else appveyor
    printfn "Build version : %s" version
    (result, majmin, now.Year)

  let FixMVId files =
    // Fix up symbol file to have the MVId emitted by the System.Reflection.Emit code
    files
    |> Seq.iter (fun f -> let assembly = System.Reflection.Assembly.LoadFrom (Path.GetFullPath f)
                          let mvid = assembly.ManifestModule.ModuleVersionId
                          let symbols = System.IO.File.ReadAllBytes(f + ".mdb")
                          mvid.ToByteArray() |> Array.iteri (fun i x -> symbols.[i+16] <- x)
                          System.IO.File.WriteAllBytes(f + ".mdb", symbols))

  let ValidateFSharpTypes simpleReport others =
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    // Edit xml report to store new hits
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("method"))
                   |> Seq.map (fun x -> x.Attribute(XName.Get("name")).Value)
                   |> Seq.filter (fun x -> others |> Seq.exists (fun y -> x = y) |> not)
                   |> Seq.sort
                   |> Seq.toList
    let expected = ".ctor Invoke as_bar bytes get_MyBar makeThing returnBar returnFoo testMakeThing testMakeUnion"
    Assert.That(recorded, expected.Split() |> Is.EquivalentTo, sprintf "Bad method list %A" recorded)

  let ValidateFSharpTypesCoverage simpleReport =
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    // Edit xml report to store new hits
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
                   |> Seq.toList
    let expected = "0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1"
    Assert.That(recorded, expected.Split() |> Is.EquivalentTo, sprintf "Bad method list %A" recorded)

  let ValidateSample1 simpleReport sigil =
    // get recorded details from here
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let zero = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "0")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList
    let ones = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "1")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList

    Assert.That(List.length recorded, Is.EqualTo ((List.length ones) + (List.length zero)),
                        "unexpected visits in " + sigil)
    let zero' = zero |> Seq.distinct |> Seq.toList

    Assert.That (zero', Is.EquivalentTo ["18"; "19"; "20"], "wrong unvisited in " + sigil + " : " + (sprintf "%A" zero'))

    let ones' = ones |> Seq.distinct |> Seq.toList
    Assert.That (ones', Is.EquivalentTo ["11"; "12"; "13"; "14"; "15"; "16"; "21"], "wrong number of visited in " + sigil + " : " + (sprintf "%A" zero'))

  let HandleResults (msg:string) (result:Fake.Core.ProcessResult) =
    String.Join (Environment.NewLine, result.Messages) |> printfn "%s"
    let save = (Console.ForegroundColor, Console.BackgroundColor)
    match result.Errors |> Seq.toList with
    | [] -> ()
    | errors ->
        try
            Console.ForegroundColor <- ConsoleColor.Black
            Console.BackgroundColor <- ConsoleColor.White
            String.Join (Environment.NewLine, errors) |> printfn "ERR : %s"
        finally
            Console.ForegroundColor <- fst save
            Console.BackgroundColor <- snd save
    Assert.That(result.ExitCode, Is.EqualTo 0, msg)

  let Run (f:Fake.Core.ProcStartInfo -> Fake.Core.ProcStartInfo) msg =
    Fake.Core.Process.execWithResult (f >> Fake.Core.Process.withFramework) (TimeSpan.FromMinutes 10.0)
    |> (HandleResults msg)

  let RunDotnet (o:DotNet.Options -> DotNet.Options) cmd args msg =
    DotNet.exec o cmd args
    |> (HandleResults msg)

  let SimpleInstrumentingRun (samplePath:string) (binaryPath:string) (reportSigil:string) =
    printfn "Instrument and run a simple executable"
    Directory.ensure "./_Reports"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( reportSigil + ".xml")
    let binRoot = Path.getFullName binaryPath
    let sampleRoot = Path.getFullName samplePath
    let instrumented = "__Instrumented." + reportSigil
    Run (fun info -> { info with
                            FileName = binRoot @@ "AltCover.exe"
                            WorkingDirectory = sampleRoot
                            Arguments = ("\"-t=System\\.\" -x=" + simpleReport + " /o=./" + instrumented)})
                            "Simple instrumentation failed"
    Run (fun info -> { info with
                            FileName = sampleRoot @@ (instrumented + "/Sample1.exe")
                            WorkingDirectory = (sampleRoot @@ instrumented)
                            Arguments = ""})  "Instrumented .exe failed"
    ValidateSample1 simpleReport reportSigil

  let SimpleInstrumentingRunUnderMono (samplePath:string) (binaryPath:string) (reportSigil':string) (monoOnWindows:string option)=
   printfn "Instrument and run a simple executable under mono"
   match monoOnWindows with
   | Some mono ->
    Directory.ensure "./_Reports"
    let reportSigil = reportSigil' + "UnderMono"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( reportSigil + ".xml")
    let binRoot = Path.getFullName binaryPath
    let sampleRoot = Path.getFullName samplePath
    let instrumented = "__Instrumented." + reportSigil
    Run (fun info -> { info with
                            FileName = mono
                            WorkingDirectory = sampleRoot
                            Arguments = ((binRoot @@ "AltCover.exe") + " \"-t=System\\.\" -x=" + simpleReport + " /o=./" + instrumented)})
                            "Simple instrumentation failed"
    Run (fun info -> { info with
                            FileName = sampleRoot @@ (instrumented + "/Sample1.exe")
                            WorkingDirectory = (sampleRoot @@ instrumented)
                            Arguments = ""}) "Instrumented .exe failed"
    ValidateSample1 simpleReport reportSigil
   | None -> Assert.Fail "Mono executable expected"

  let PrepareReadMe packingCopyright =
    let readme = Path.getFullName "README.md"
    let document = File.ReadAllText readme
    let markdown = Markdown()
    let docHtml = """<?xml version="1.0"  encoding="utf-8"?>
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
"""               + markdown.Transform document + """
<footer><p style="text-align: center">""" + packingCopyright + """</p>
</footer>
</body>
</html>
"""
    let xmlform = XDocument.Parse docHtml
    let body = xmlform.Descendants(XName.Get "body")
    let eliminate = [ "Continuous Integration"; "Building"; "Thanks to" ]
    let keep = ref true

    let kill = body.Elements()
               |> Seq.map (fun x -> match x.Name.LocalName with
                                    | "h2" -> keep := (List.tryFind (fun e -> e = String.Concat(x.Nodes())) eliminate) |> Option.isNone
                                    | "footer" -> keep := true
                                    | _ -> ()
                                    if !keep then None else Some x)
               |> Seq.toList
    kill |>
    Seq.iter (fun q -> match q with
                       | Some x -> x.Remove()
                       | _ -> ())

    let packable = Path.getFullName "./_Binaries/README.html"
    xmlform.Save packable