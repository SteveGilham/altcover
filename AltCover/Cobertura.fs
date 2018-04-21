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
                          packages.Add(package)
                          let classes = XElement(X "classes")
                          package.Add(classes)
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