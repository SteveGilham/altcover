namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Xml
open System.Xml.Linq

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Smells", "RelaxedAvoidCodeDuplicatedInSameClassRule",
  Justification="Idiomatic F# code")>]
type internal XmlAttributeAbstraction =
  | XmlAttr of XmlAttribute
  | XAttr of XAttribute
  member self.Name =
    match self with
    | XmlAttr a -> a.Name
    | XAttr a -> a.Name.LocalName
  member self.Value =
    match self with
    | XmlAttr a -> a.Value
    | XAttr a -> a.Value

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Smells", "RelaxedAvoidCodeDuplicatedInSameClassRule",
  Justification="Idiomatic F# code")>]
type internal XmlElementAbstraction =
  | XmlEl of XmlElement
  | XEl of XElement
  member self.CreateElement name =
    match self with
    | XmlEl x -> name
                      |> x.OwnerDocument.CreateElement
                      |> XmlElementAbstraction.XmlEl
    | XEl _ -> name
               |> XName.Get
               |> Linq.XElement
               |> XmlElementAbstraction.XEl

  member self.GetElementsByTagName name =
    match self with
    | XmlEl x -> use elements = x.GetElementsByTagName name
                 elements
                 |> Seq.cast<XmlElement>
                 |> Seq.map XmlElementAbstraction.XmlEl
                 |> Seq.toList
    | XEl x -> x.Descendants(XName.Get name)
              |> Seq.map XmlElementAbstraction.XEl
              |> Seq.toList
  member self.GetAttribute name =
    match self with
    | XmlEl x -> x.GetAttribute name
    | XEl x -> x.Attribute(XName.Get name).Value
  member self.SetAttribute name value =
    match self with
    | XmlEl x -> x.SetAttribute(name, value)
    | XEl x ->
      let attr = x.Attribute(XName.Get name)
      if attr |> isNull
      then x.Add(Linq.XAttribute(XName.Get name, value))
      else attr.Value <- value
  member self.SetNSAttribute name ns value =
    match self with
    | XmlEl x -> x.SetAttribute(name, ns, value) |> ignore
    | XEl x ->
      let attr = x.Attribute(XName.Get(name, ns))
      if attr |> isNull
      then x.Add(Linq.XAttribute(XName.Get (name,ns), value))
      else attr.Value <- value

  member self.Attributes
    with get() =
      match self with
      | XmlEl x -> x.Attributes
                        |> Seq.cast<XmlAttribute>
                        |> Seq.map XmlAttributeAbstraction.XmlAttr
      | XEl x -> x.Attributes()
                      |> Seq.map XmlAttributeAbstraction.XAttr
  member self.Name
    with get() =
      match self with
      | XmlEl x -> x.Name
      | XEl x -> x.Name.LocalName
  member self.InnerText
    with get() =
      match self with
      | XmlEl x -> x.InnerText
      | XEl x -> x.Value

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Smells", "RelaxedAvoidCodeDuplicatedInSameClassRule",
  Justification="Idiomatic F# code")>]
[<SuppressMessage("Gendarme.Rules.Performance","AvoidUncalledPrivateCodeRule",
  Justification="Internals visible to...")>]
type internal XmlAbstraction =
  | XmlDoc of XmlDocument
  | XDoc of XDocument
  member self.Elements name =
    match self with
    | XmlDoc x ->
          use elements = x.DocumentElement.SelectNodes("//" + name)
          elements
          |> Seq.cast<XmlElement>
          |> Seq.map XmlElementAbstraction.XmlEl
          |> Seq.toList
    | XDoc x ->
      x.Descendants(XName.Get name)
      |> Seq.map XmlElementAbstraction.XEl
      |> Seq.toList
  member self.RootElement
    with get() =
      match self with
      | XmlDoc x -> x.DocumentElement |> XmlElementAbstraction.XmlEl
      | XDoc x -> (x.Elements() |> Seq.head) |> XmlElementAbstraction.XEl

[<RequireQualifiedAccess>]
module internal PostProcess =
  let internal copyFillMethodPoint (mp : XmlElementAbstraction seq)
                                   (sp : XmlElementAbstraction seq) =
    mp
    |> Seq.iter (fun m ->
          m.SetNSAttribute
             "type" "http://www.w3.org/2001/XMLSchema-instance" "SequencePoint"
          |> ignore
          sp
          |> Seq.take 1
          |> Seq.collect (fun p -> p.Attributes)
          |> Seq.iter (fun a -> m.SetAttribute a.Name a.Value))

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal lookUpVisitsByToken token (dict : Dictionary<int, PointVisit>) =
    let (ok, index) =
      Int32.TryParse
        (token, System.Globalization.NumberStyles.Integer,
          System.Globalization.CultureInfo.InvariantCulture)
    match dict.TryGetValue(if ok then index else -1) with
    | (false, _) -> PointVisit.Create()
    | (_, pair) -> pair

  let internal fillMethodPoint (mp : XmlElementAbstraction seq) (method : XmlElementAbstraction)
      (dict : Dictionary<int, PointVisit>) =
    let token =
      method.GetElementsByTagName("MetadataToken")
      |> Seq.map (fun m -> m.InnerText)
      |> Seq.head

    let vc = (lookUpVisitsByToken token dict).Total()
    mp
    |> Seq.iter (fun m ->
          m.SetAttribute "vc" (vc.ToString(CultureInfo.InvariantCulture))
          m.SetAttribute "uspid" token
          m.SetAttribute "ordinal" "0"
          m.SetAttribute "offset" "0")

  let visitCount (nodes: XmlElementAbstraction seq) =
    nodes
    |> Seq.filter (fun s ->
          Int64.TryParse
            (s.GetAttribute("vc"), NumberStyles.Integer, CultureInfo.InvariantCulture)
          |> snd
          <> 0L)
    |> Seq.length

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal tryGetValue (d : Dictionary<'a, 'b>) (key : 'a) =
    match d with
    | null -> (false, Unchecked.defaultof<'b>)
    | _ -> d.TryGetValue key

  let internal action
      orderAttr
      (counts : Dictionary<string, Dictionary<int, PointVisit>>)
      format
      (document : XmlAbstraction) =
    match format with
    | ReportFormat.OpenCoverWithTracking
    | ReportFormat.OpenCover ->
        let scoreToString raw =
          (sprintf "%.2f" raw).TrimEnd([| '0' |]).TrimEnd([| '.' |])

        let stringToScore (node : XmlElementAbstraction) name =
          node.GetAttribute(name).InvariantParseDouble()
          |> snd

        let percentCover visits points =
          if points = 0
          then "0"
          else ((float (visits * 100)) / (float points)) |> scoreToString

        let setSummary (x : XmlElementAbstraction) pointVisits branchVisits methodVisits classVisits
            ptcover brcover minCrap maxCrap =
          x.GetElementsByTagName("Summary")
          |> Seq.tryHead
          |> Option.iter (fun s ->
                let minc =
                  (if minCrap = Double.MaxValue
                  then 0.0
                  else minCrap)
                  |> scoreToString

                let maxc =
                  (if maxCrap = Double.MinValue then 0.0 else maxCrap)
                  |> scoreToString

                s.SetAttribute "visitedSequencePoints" (sprintf "%d" pointVisits)
                s.SetAttribute "visitedBranchPoints" (sprintf "%d" branchVisits)
                s.SetAttribute "visitedMethods" (sprintf "%d" methodVisits)
                classVisits
                |> Option.iter
                    (fun cvc -> s.SetAttribute "visitedClasses" (sprintf "%d" cvc))
                s.SetAttribute "branchCoverage" brcover
                s.SetAttribute "sequenceCoverage" ptcover
                s.SetAttribute "minCrapScore" minc
                s.SetAttribute "maxCrapScore" maxc)

        let computeBranchExitCount (anchor : XmlElementAbstraction) (sp : XmlElementAbstraction list) bp =
          let tail = anchor.CreateElement("SequencePoint")
          tail.SetAttribute
            orderAttr (Int32.MaxValue.ToString(CultureInfo.InvariantCulture))
          let nodes =
            List.concat
              [ sp
                [ tail ]
                bp ]

          let interleave =
            nodes
            |> Seq.sortBy (fun x ->
                  x.GetAttribute(orderAttr)
                  |> Int32.TryParse
                  |> snd)

          interleave
          |> Seq.fold (fun (bev, sq : XmlElementAbstraction) x ->
                match x.Name with
                | "SequencePoint" ->
                    sq.SetAttribute "bev" (sprintf "%d" bev)
                    (0, x)
                | _ ->
                    (bev + (if x.GetAttribute("vc") = "0" then 0 else 1), sq))
                (0, nodes.[0])
          |> ignore

        // https://blog.ndepend.com/crap-metric-thing-tells-risk-code/
        // Let CC(m) = cyclomatic complexity of a method and
        // U(m) = the percentage of a method not covered by unit tests.
        // CRAP(m) = CC(m)^2 * U(m)^3 + CC(m).
        let crapScore forceVisited (method : XmlElementAbstraction) =
          let coverage =
            if forceVisited then 100.0
            else
              let cover = stringToScore method "sequenceCoverage"
              if cover > 0.0 then cover else stringToScore method "branchCoverage"

          let complexity = stringToScore method "cyclomaticComplexity"

          let raw =
            Math.Pow(complexity, 2.0) * Math.Pow((1.0 - (coverage / 100.0)), 3.0)
              + complexity
          let score = raw |> scoreToString
          method.SetAttribute "crapScore" score
          raw

        let updateMethod (dict : Dictionary<int, PointVisit>)
            (vb, vs, vm:int, pt, br, minc, maxc) (method : XmlElementAbstraction) =
          let sp = method.GetElementsByTagName("SequencePoint")
          let bp = method.GetElementsByTagName("BranchPoint")
          let mp = method.GetElementsByTagName("MethodPoint")
          let count = sp |> List.length
          let rawCount = bp |> List.length

          // inconsistent name to shut Gendarme up
          let numBranches = rawCount + Math.Sign(count + rawCount)
          if count > 0 then
            copyFillMethodPoint mp sp
          else
            fillMethodPoint mp method dict
          let pointVisits = visitCount sp
          let b0 = visitCount bp
          let branchVisits = b0 + Math.Sign b0
          let mVisits = visitCount mp
          let methodVisit = if pointVisits > 0 || b0 > 0
                            then 1
                            else 0
          if methodVisit = 1 || mVisits > 0
          then method.SetAttribute "visited" "true"

          // zero visits still need to fill in CRAP score
          let cover = percentCover pointVisits count
          let bcover = percentCover branchVisits numBranches
          method.SetAttribute "sequenceCoverage" cover
          method.SetAttribute "branchCoverage" bcover
          let raw = crapScore (mVisits > count) method

          // degenerate methods don't contribute to counts by default
          let (maxcrap, mincrap) = if count > 0 || rawCount > 0
                                   then (Math.Max(maxc, raw),
                                         Math.Min(minc, raw))
                                   else (maxc, minc)
          setSummary method pointVisits branchVisits methodVisit None cover bcover raw raw
          computeBranchExitCount method sp bp
          (vb + branchVisits, vs + pointVisits, vm + methodVisit,
            pt + count, br + numBranches,
            mincrap, maxcrap)

        let updateClass (dict : Dictionary<int, PointVisit>)
            (vb, vs, vm, vc, pt, br, minc0, maxc0) (``class`` : XmlElementAbstraction) =
          let (cvb, cvs, cvm, cpt, cbr, minc, maxc) =
            ``class``.GetElementsByTagName("Method")
            |> Seq.fold (updateMethod dict)
                  (0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

          let cover = percentCover cvs cpt
          let bcover = percentCover cvb cbr

          let cvc =
            if cvm > 0 then 1 else 0
          setSummary ``class`` cvs cvb cvm (Some cvc) cover bcover minc maxc
          (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr,
            Math.Min(minc, minc0), Math.Max(maxc, maxc0))

        let updateModule (counts : Dictionary<string, Dictionary<int, PointVisit>>)
            (vb, vs, vm, vc, pt, br, minc0, maxc0) (``module`` : XmlElementAbstraction) =
          let dict =
            match (tryGetValue counts) <| ``module``.GetAttribute("hash") with
            | (false, _) -> Dictionary<int, PointVisit>()
            | (true, d) -> d

          let (cvb, cvs, cvm, cvc, cpt, cbr, minc, maxc) =
            ``module``.GetElementsByTagName("Class")
            |> Seq.fold (dict |> updateClass)
                  (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

          let cover = percentCover cvs cpt
          let bcover = percentCover cvb cbr
          setSummary ``module`` cvs cvb cvm (Some cvc) cover bcover minc maxc
          (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr,
            Math.Min(minc, minc0), Math.Max(maxc, maxc0))

        let (vb, vs, vm, vc, pt, br, minc, maxc) =
          document.Elements "Module"
          |> Seq.fold (updateModule counts)
                (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

        let cover = percentCover vs pt
        let bcover = percentCover vb br
        setSummary document.RootElement vs vb vm (Some vc) cover bcover minc maxc
    | _ -> ()