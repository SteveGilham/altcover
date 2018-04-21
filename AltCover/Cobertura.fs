namespace AltCover

open System
open System.Xml.Linq

module Cobertura =
  let internal path : Option<string> ref = ref None
  let X = OpenCover.X

  let NCover (report:XDocument) (packages:XElement) =
    report.Descendants(X "module")
    |> Seq.iter (fun m -> let package = XElement(X "package",
                                                 XAttribute(X "name", m.Attribute(X "name").Value))
                          packages.Add(package)
                          let classes = XElement(X "classes")
                          package.Add(classes)
                          m.Descendants(X "method")
                          |> Seq.groupBy(fun mx -> (mx.Attribute(X "class").Value,
                                                    mx.Descendants(X "seqpnt")
                                                    |> Seq.map (fun s -> s.Attribute(X "document").Value)
                                                    |> Seq.head))
                          |> Seq.sortBy fst
                          |> Seq.iter (fun ((n,s),mx) -> let cx = XElement(X "class",
                                                                           XAttribute(X "name", n),
                                                                           XAttribute(X "filename", s))
                                                         classes.Add(cx))
                )
    packages.Parent.SetAttributeValue(X "branch-rate", null)

  let OpenCover (report:XDocument)  (packages:XElement) =
    report.Descendants(X "Module")
    |> Seq.filter(fun m -> m.Descendants(X "Class") |> Seq.isEmpty |> not)
    |> Seq.iter (fun m -> let package = XElement(X "package",
                                                 XAttribute(X "name",
                                                     m.Descendants(X "ModuleName")
                                                     |> Seq.map (fun x -> x.Value)
                                                     |> Seq.head))
                          let files = m.Descendants(X "File")
                                      |> Seq.fold(fun m x -> m |> 
                                                             Map.add (x.Attribute(X "uid").Value) (x.Attribute(X "fullPath").Value)) Map.empty
                          packages.Add(package)
                          let classes = XElement(X "classes")
                          package.Add(classes)
                          m.Descendants(X "Method")
                          |> Seq.filter(fun m -> m.Descendants(X "FileRef") |> Seq.isEmpty |> not)
                          |> Seq.groupBy(fun mx -> ((mx.Parent.Parent.Descendants(X "FullName") |> Seq.head).Value,
                                                    mx.Descendants(X "FileRef")
                                                    |> Seq.map (fun s -> files
                                                                         |> Map.find (s.Attribute(X "uid").Value))
                                                    |> Seq.head))
                          |> Seq.sortBy fst
                          |> Seq.iter (fun ((n,s),mx) -> let cx = XElement(X "class",
                                                                           XAttribute(X "name", n),
                                                                           XAttribute(X "filename", s))
                                                         classes.Add(cx))
    )

  let Summary (report:XDocument) (format:Base.ReportFormat) result =
    let rewrite = XDocument(XDeclaration("1.0", "utf-8", "yes"), [||])
    let element = XElement(X "coverage",
                            XAttribute(X "line-rate", 0),
                            XAttribute(X "branch-rate", 0),
                            XAttribute(X "version", AssemblyVersionInformation.AssemblyVersion),
                            XAttribute(X "timestamp", int((DateTime.UtcNow - DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)).TotalSeconds))
                )

    rewrite.Add(element)
    let packages = XElement(X "packages")
    element.Add(packages)

    match format with
    | Base.ReportFormat.NCover -> NCover report packages
    | _ -> OpenCover report packages

    rewrite.Save(!path |> Option.get)
    result