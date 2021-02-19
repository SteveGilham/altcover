namespace AltCover
#nowarn "25"

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Linq
open System.Reflection
open System.Xml
open System.Xml.Xsl
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

module internal Lcov =

  type LcovParseException (e:exn) =
    inherit exn(e.Message, e)

  type LRecord =
    | SF of string
    | FN of (int * string)
    | DA of (int * int)
    | BRDA of (int * int * int * int)
    | Other

  let buildModule (m:(int*LRecord) seq) =
    let result = XElement(XName.Get "module")
    m
    |> Seq.map snd
    |> Seq.filter (fun r -> match r with
                            | SF name ->
                              let fn = Path.GetFileNameWithoutExtension name
                              [|
                                XAttribute(XName.Get "moduleId", fn)
                                XAttribute(XName.Get "name", fn)
                                XAttribute(XName.Get "assembly", name)
                                XAttribute(XName.Get "assemblyIdentity", name)
                              |]
                              |> result.Add
                              false
                            | _ -> true)
    |> Seq.sortBy (fun r ->
      match r with
      | FN (a,_) -> 4 * a
      | DA (a,_) -> (4 * a) + 1
      | BRDA (a,_,_,_) -> (4 * a) + 2)
    |> Seq.fold (fun x r ->
      match r with
      | FN (_,n) -> // <method excluded="false" instrumented="true" name="Method1" class="Test.AbstractClass_SampleImpl1" fullname="Test.AbstractClass_SampleImpl1::Method1(...)" document="AbstractClass.cs">
        let mt = XElement(XName.Get "method",
                          XAttribute(XName.Get "excluded", "false"),
                          XAttribute(XName.Get "instrumented", "true"),
                          XAttribute(XName.Get "name", "todo"),
                          XAttribute(XName.Get "class", "todo"),
                          XAttribute(XName.Get "fullname", n),
                          XAttribute(XName.Get "document", result.Attribute(XName.Get "assembly").Value))
        result.Add mt
        mt
      | DA (l,n) ->
        let sp = XElement(XName.Get "seqpnt",
                          XAttribute(XName.Get "visitcount", n),
                          XAttribute(XName.Get "line", l),
                          XAttribute(XName.Get "column", 1),
                          XAttribute(XName.Get "endline", l),
                          XAttribute(XName.Get "endcolumn", 2),
                          XAttribute(XName.Get "offset", l),
                          XAttribute(XName.Get "excluded", "false"),
                          XAttribute(XName.Get "document", result.Attribute(XName.Get "assembly").Value))
        x.Add sp
        x
      | BRDA (l,_,n,v) ->
// BRDA:<line number>,<block number>,<branch number>,<taken>
        let br = XElement(XName.Get "branch",
                          XAttribute(XName.Get "visitcount", v),
                          XAttribute(XName.Get "line", l),
                          XAttribute(XName.Get "path", n),
                          XAttribute(XName.Get "offset", l),
                          XAttribute(XName.Get "offsetend", l),
                          XAttribute(XName.Get "document", result.Attribute(XName.Get "assembly").Value))
        x.Add br
        x) null
    |> ignore
    result

  let ofLines (lines: string array) =
    try
      lines
      |> Seq.map (fun line ->
        match line with
        | l when l.StartsWith("SF:", StringComparison.Ordinal) ->
          SF (l.Substring 3)
        | l when l.StartsWith("FN:", StringComparison.Ordinal) ->
          let n::name::_ = (l.Substring 3).Split(',') |> Array.toList
          FN (n |> Int32.TryParse |> snd, name)
        | l when l.StartsWith("DA:", StringComparison.Ordinal) ->
          let n::v::_ = (l.Substring 3).Split(',') |> Array.toList
          DA (n |> Int32.TryParse |> snd, v |> Int32.TryParse |> snd)
        | l when l.StartsWith("BRDA:", StringComparison.Ordinal) ->
          let n::v::x::y::_ = (l.Substring 5).Split(',') |> Array.toList
          BRDA (n |> Int32.TryParse |> snd, v |> Int32.TryParse |> snd,
                x |> Int32.TryParse |> snd, y |> Int32.TryParse |> snd)
        | _ -> Other)
      |> Seq.fold (fun (i,l) r ->
        match r with
        | SF _ -> (i+1, ((i+1), r) :: l)
        | DA _
        | BRDA _
        | FN _ -> (i, (i,r) :: l)
        | _ -> (i, l)) (0, [])
      |> snd
      |> Seq.groupBy fst
      |> Seq.map (fun (_, l) -> buildModule l)
      |> Seq.toArray
    with
    | x -> raise <| LcovParseException(x)

  let toXml file =
    let root = XElement(XName.Get "coverage",
                        XAttribute(XName.Get "profilerVersion", "LCov"),
                        XAttribute(XName.Get "driverVersion", "LCov"),
                        XAttribute(XName.Get "startTime", "now"),
                        XAttribute(XName.Get "measureTime", "name"),
                        XAttribute(XName.Get "lineonly", "true"))
    root.Add(file |> File.ReadAllLines |> ofLines)

    XDocument([| root |])