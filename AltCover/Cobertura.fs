namespace AltCover

open System
open System.Xml.Linq

module Cobertura =
  let internal path : Option<string> ref = ref None

  let NCover _  = //(report:XDocument)
    NotImplementedException() |> raise

  let OpenCover _  = //(report:XDocument)
    NotImplementedException() |> raise

  let Summary (report:XDocument) (format:Base.ReportFormat) result =
    match format with 
    | Base.ReportFormat.NCover -> NCover report
    | _ -> OpenCover report
    result

