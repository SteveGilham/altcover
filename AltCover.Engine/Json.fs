namespace AltCover

open System
open System.IO
open System.Xml.Linq
open System.Globalization

open Manatee.Json

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Json' is jargon")>]
module internal Json =
  let internal path : Option<string> ref = ref None

  let internal convertReport (report : XDocument) (stream : Stream) =
    doWithStream (fun () -> new StreamWriter(stream)) (fun writer ->
        XmlExtensions.ToJson(report.Root).ToString() // ready minified
        |> writer.Write)

  let internal summary (report : XDocument) (_ : ReportFormat) result =
    doWithStream(fun () -> File.OpenWrite(!path |> Option.get))
      (convertReport report)
    (result, 0uy, String.Empty)