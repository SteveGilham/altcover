namespace AltCover

open System.Diagnostics.CodeAnalysis

open System
open System.IO
open System.Xml.Linq

module internal Lcov =

  [<SuppressMessage("Gendarme.Rules.Serialization",
                    "MissingSerializationConstructorRule",
                    Justification = "Would not be used")>]
  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "MissingExceptionConstructorsRule",
                    Justification = "Would not be used")>]
  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "ExceptionShouldBeVisibleRule",
                    Justification = "Would not be used")>]
  [<SuppressMessage("Microsoft.Design",
                    "CA1064:ExceptionsShouldBePublic",
                    Justification = "Would not be used")>]
  [<SuppressMessage("Microsoft.Design",
                    "CA1032:ImplementStandardExceptionConstructors",
                    Justification = "Would not be used")>]
  [<Sealed>]
  type LcovParseException(e: exn) =
    inherit exn(e.Message, e)

  type LRecord =
    //SF:<absolute path to the source file>
    | SF of string
    //FN:<line number of function start>,<function name>
    | FN of (int * string)
    //DA:<line number>,<execution count>
    | DA of (int * int)
    //BRDA:<line number>,<block number>,<branch number>,<taken>
    | BRDA of (int * int * int * int)
    | Other

  let buildModule (m: (int * LRecord) seq) =
    let result = XElement(XName.Get "module")

    m
    |> Seq.map snd
    |> Seq.filter
         (fun r ->
           match r with
           | SF name ->
               let fn = Path.GetFileNameWithoutExtension name

               [| XAttribute(XName.Get "moduleId", fn)
                  XAttribute(XName.Get "name", fn)
                  XAttribute(XName.Get "assembly", name)
                  XAttribute(XName.Get "assemblyIdentity", name) |]
               |> result.Add

               false
           | FN _
           | DA _
           | BRDA _ -> true
           | _ -> raise (r |> sprintf "%A" |> InvalidDataException ))
    |> Seq.sortBy
         (fun r ->
           match r with
           | FN (a, _) -> 4 * a
           | DA (a, _) -> (4 * a) + 1
           | BRDA (a, _, _, _) -> (4 * a) + 2
           | _ -> raise (r |> sprintf "%A" |> InvalidDataException))
    |> Seq.fold
         (fun x r -> // <method excluded="false" instrumented="true" name="Method1" class="Test.AbstractClass_SampleImpl1" fullname="Test.AbstractClass_SampleImpl1::Method1(...)" document="AbstractClass.cs">
           match r with
           | FN (_, n) -> // <method excluded="false" instrumented="true" name="Method1" class="Test.AbstractClass_SampleImpl1" fullname="Test.AbstractClass_SampleImpl1::Method1(...)" document="AbstractClass.cs">
               let endclass =
                 n.IndexOf("::", StringComparison.Ordinal)

               let startclass = n.LastIndexOf(' ', endclass) + 1

               let c =
                 n.Substring(startclass, endclass - startclass)

               let endname = n.IndexOf('(', endclass)

               let name =
                 n.Substring(endclass + 2, endname - endclass - 2)

               let mt =
                 XElement(
                   XName.Get "method",
                   XAttribute(XName.Get "excluded", "false"),
                   XAttribute(XName.Get "instrumented", "true"),
                   XAttribute(XName.Get "name", name),
                   XAttribute(XName.Get "class", c),
                   XAttribute(XName.Get "fullname", n),
                   XAttribute(
                     XName.Get "document",
                     result.Attribute(XName.Get "assembly").Value
                   )
                 )

               result.Add mt
               mt
           | DA (l, n) ->
               let sp =
                 XElement(
                   XName.Get "seqpnt",
                   XAttribute(XName.Get "visitcount", n),
                   XAttribute(XName.Get "line", l),
                   XAttribute(XName.Get "column", 1),
                   XAttribute(XName.Get "endline", l),
                   XAttribute(XName.Get "endcolumn", 2),
                   XAttribute(XName.Get "offset", l),
                   XAttribute(XName.Get "excluded", "false"),
                   XAttribute(
                     XName.Get "document",
                     result.Attribute(XName.Get "assembly").Value
                   )
                 )

               x.Add sp
               x
           // BRDA:<line number>,<block number>,<branch number>,<taken>
           | BRDA (l, _, n, v) ->
               let br =
                 XElement(
                   XName.Get "branch",
                   XAttribute(XName.Get "visitcount", v),
                   XAttribute(XName.Get "line", l),
                   XAttribute(XName.Get "path", n),
                   XAttribute(XName.Get "offset", l),
                   XAttribute(XName.Get "offsetend", l),
                   XAttribute(
                     XName.Get "document",
                     result.Attribute(XName.Get "assembly").Value
                   )
                 )

               x.Add br
               x
           | _ -> raise (r |> sprintf "%A" |> InvalidDataException))
         null
    |> ignore

    result

  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "DoNotSwallowErrorsCatchingNonSpecificExceptionsRule",
                    Justification = "Wrapped and rethrown")>]
  let ofLines (lines: string array) =
    try
      lines
      |> Seq.map
           (fun line ->
             match line with
             | l when l.StartsWith("SF:", StringComparison.Ordinal) -> SF(l.Substring 3)
             | l when l.StartsWith("FN:", StringComparison.Ordinal) ->
                 let trim = l.Substring 3
                 let comma = trim.IndexOf ','
                 let n = trim.Substring(0, comma)
                 let name = trim.Substring(comma + 1)
                 FN(n |> Int32.TryParse |> snd, name)
             | l when l.StartsWith("DA:", StringComparison.Ordinal) ->
                 match (l.Substring 3).Split(',') |> Array.toList with
                 | n :: v :: _ ->
                     DA(n |> Int32.TryParse |> snd, v |> Int32.TryParse |> snd)
                 | _ ->
                     raise (line
                            |> sprintf "%A"
                            |> InvalidDataException)
             | l when l.StartsWith("BRDA:", StringComparison.Ordinal) ->
                 match (l.Substring 5).Split(',') |> Array.toList with
                 | n :: v :: x :: y :: _ ->
                     BRDA(
                       n |> Int32.TryParse |> snd,
                       v |> Int32.TryParse |> snd,
                       x |> Int32.TryParse |> snd,
                       y |> Int32.TryParse |> snd
                     )
                 | _ ->
                     raise (line
                            |> sprintf "%A"
                            |> InvalidDataException)
             | _ -> Other)
      |> Seq.fold
           (fun (i, l) r ->
             match r with
             | SF _ -> (i + 1, ((i + 1), r) :: l)
             | DA _
             | BRDA _
             | FN _ -> (i, (i, r) :: l)
             | _ -> (i, l))
           (0, [])
      |> snd
      |> Seq.groupBy fst
      |> Seq.map (fun (_, l) -> buildModule l)
      |> Seq.toArray
    with x -> raise <| LcovParseException(x)

  let toXml file =
    let root =
      XElement(
        XName.Get "coverage",
        XAttribute(XName.Get "profilerVersion", "LCov"),
        XAttribute(XName.Get "driverVersion", "LCov"),
        XAttribute(XName.Get "startTime", "now"),
        XAttribute(XName.Get "measureTime", "name"),
        XAttribute(XName.Get "lineonly", "true")
      )

    root.Add(file |> File.ReadAllLines |> ofLines)

    XDocument([| root |])