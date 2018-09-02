namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Linq
open System.Xml
open System.Xml.XPath

module OpenCoverUtilities =

  let private CompressMethod withinSequencePoint sameSpan (m:XmlElement) =
    let sp = m.GetElementsByTagName("SequencePoint").OfType<XmlElement>() |> Seq.toList
    let bp = m.GetElementsByTagName("BranchPoint").OfType<XmlElement>() |> Seq.toList

    if sp |> List.isEmpty |> not && bp |> List.isEmpty |> not then
        let interleave = List.concat [ sp; bp ]
                        |> List.sortBy (fun x -> x.GetAttribute("offset") |> Int32.TryParse |> snd)
        interleave
        |> Seq.fold (fun (s:XmlElement, bs:XmlElement list) x -> match x.Name with
                                                                 | "SequencePoint" ->
                                                                   let bx = if withinSequencePoint then
                                                                              let next = x.GetAttribute("offset") |> Int32.TryParse |> snd
                                                                              let (kill, keep) = bs
                                                                                                 |> List.partition (fun b -> b.GetAttribute("offsetend")
                                                                                                                             |> Int32.TryParse |> snd < next)
                                                                              kill
                                                                              |> Seq.iter (fun b -> b.ParentNode.RemoveChild(b) |> ignore)
                                                                              keep
                                                                            else bs

                                                                   let by = if sameSpan then
                                                                              let (kill, keep) = bx
                                                                                                 |> List.groupBy (fun b -> (b.GetAttribute("offset"), b.GetAttribute("offsetchain"),b.GetAttribute("offsetend")))
                                                                                                 |> List.fold (fun (ki,ke) (_, bz) -> let totalVisits = bz
                                                                                                                                                        |> Seq.map (fun b -> b.GetAttribute("vc")
                                                                                                                                                                             |> Int32.TryParse |> snd)
                                                                                                                                                        |> Seq.sum
                                                                                                                                      let h = bz |> Seq.head
                                                                                                                                      h.SetAttribute ("vc", totalVisits.ToString(CultureInfo.InvariantCulture))
                                                                                                                                      (List.concat [ki; bz |> Seq.tail |> Seq.toList] ,h::ke)) ([], [])
                                                                              kill
                                                                              |> Seq.iter (fun b -> b.ParentNode.RemoveChild(b) |> ignore)
                                                                              keep
                                                                             else bx

                                                                   // Fix up what remains
                                                                   by
                                                                   |> List.rev // because the list will have been built up in reverse order
                                                                   |> Seq.mapi (fun i b -> (i,b))
                                                                   |> Seq.groupBy (fun (_,b) ->  b.GetAttribute("offset"))
                                                                   |> Seq.iter (fun (_, paths) -> paths  // assume likely ranges for these numbers!
                                                                                                  |> Seq.sortBy (fun (n,p) -> n + 100 * (p.GetAttribute("offsetend")
                                                                                                                                         |> Int32.TryParse |> snd))
                                                                                                  |> Seq.iteri (fun i (_,p) -> p.SetAttribute("path", (i + 1).ToString(CultureInfo.InvariantCulture))))

                                                                   s.SetAttribute("bec", by.Length.ToString(CultureInfo.InvariantCulture))
                                                                   s.SetAttribute("bev", "0")

                                                                   (x, [])
                                                                 | _ -> (s, x::bs)
                         ) (sp.Head, [])
        |> ignore

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="returns a specific concrete type")>]
  let CompressBranching (navigable:IXPathNavigable) withinSequencePoint sameSpan =
      // Validate
      let xmlDocument =  new XmlDocument()
      navigable.CreateNavigator().ReadSubtree() |> xmlDocument.Load
      xmlDocument.Schemas <- XmlUtilities.LoadSchema AltCover.Base.ReportFormat.OpenCover
      xmlDocument.Validate (null)

      // Get all the methods
      xmlDocument.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter (CompressMethod withinSequencePoint sameSpan)

      // tidy up here
      AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover xmlDocument
      XmlUtilities.PrependDeclaration xmlDocument
      xmlDocument