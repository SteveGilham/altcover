open System
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq

open Fake
open YamlDotNet.RepresentationModel

module Actions =
  let Clean ()  =
    (DirectoryInfo ".").GetDirectories("*", SearchOption.AllDirectories)
    |> Seq.filter (fun x -> x.Name.StartsWith "_" || x.Name = "bin" || x.Name = "obj")
    |> Seq.map (fun x -> x.FullName)
    |> Seq.distinct
    // arrange so leaves get deleted first, avoiding "does not exist" warnings
    |> Seq.groupBy (fun x -> x |> Seq.filter (fun c -> c='\\' || c = '/') |> Seq.length)
    |> Seq.map (fun (n,x) -> (n, x |> Seq.sort))
    |> Seq.sortBy (fun (n,x) -> -1 * n)
    |> Seq.map (fun (n,x) -> x)
    |> Seq.concat
    |> Seq.iter (fun n -> printfn "Deleting %s" n
                          Directory.Delete(n, true))

    Directory.GetFiles(Environment.GetEnvironmentVariable("TEMP"), "*.tmp.dll.mdb")
    |> Seq.iter File.Delete
  
  let template ="""namespace AltCover
open System.Reflection
open System.Runtime.CompilerServices
#if DEBUG
[<assembly: AssemblyConfiguration("Debug {0}")>]
#else
[<assembly: AssemblyConfiguration("Release {0}")>]
#endif
#if NETSTANDARD2_0
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests")>]
#else
#if NETCOREAPP2_0
[<assembly: InternalsVisibleTo("AltCover.Tests")>]

#else
[<assembly: InternalsVisibleTo("AltCover.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests, PublicKey={2}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests2, PublicKey={1}")>]
[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests2, PublicKey={2}")>]
#endif
#endif
()
"""

  let InternalsVisibleTo version =
    let stream2 = new System.IO.FileStream("./Build/SelfTest.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair2 = StrongNameKeyPair(stream2)
    let key2 = BitConverter.ToString pair2.PublicKey

    let stream = new System.IO.FileStream("./Build/Infrastructure.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair = StrongNameKeyPair(stream)
    let key = BitConverter.ToString pair.PublicKey

    let file = String.Format(System.Globalization.CultureInfo.InvariantCulture,
                template, version, key.Replace("-", String.Empty), key2.Replace("-", String.Empty))

    let path = "_Generated\VisibleToTest.fs"
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
    |> Seq.iter (fun f -> let assembly = System.Reflection.Assembly.ReflectionOnlyLoadFrom (Path.GetFullPath f)
                          let mvid = assembly.ManifestModule.ModuleVersionId.ToByteArray();
                          let symbols = System.IO.File.ReadAllBytes(f + ".mdb")
                          mvid |> Array.iteri (fun i x -> symbols.[i+16] <- x)
                          System.IO.File.WriteAllBytes(f + ".mdb", symbols))

  let ValidateFSharpTypes simpleReport = 
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    // Edit xml report to store new hits
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("method"))
                   |> Seq.map (fun x -> x.Attribute(XName.Get("name")).Value)
                   |> Seq.sort
                   |> Seq.toList
    let expected = "[\"Invoke\"; \"as_bar\"; \"bytes\"; \"get_MyBar\"; \"makeThing\"; \"returnBar\"; \"returnFoo\";\n \"testMakeThing\"; \"testMakeUnion\"]"
    if recorded.Length <> 9 then failwith (sprintf "Bad method list length %A" recorded)
    if (sprintf "%A" recorded) <> expected then failwith (sprintf "Bad method list %A" recorded)
