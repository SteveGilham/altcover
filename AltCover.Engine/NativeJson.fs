namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq

#if RUNNER
open System.Text
open Mono.Cecil
#endif

#if GUI || RUNNER
open System.Globalization
open Manatee.Json
#endif

module
#if GUI || RUNNER
       internal
#endif
                NativeJson =

  type internal TimeStamp = string

#if !GUI
  let internal fromTracking (ticks: int64) : TimeStamp =
    ticks
    |> System.Net.IPAddress.HostToNetworkOrder
    |> BitConverter.GetBytes
    |> Convert.ToBase64String
#endif

  type internal Times = List<TimeStamp>

  type internal Tracks = List<int>

  [<ExcludeFromCodeCoverage; NoComparison>]
  [<SuppressMessage("Gendarme.Rules.Design.Generic",
                    "DoNotExposeGenericListsRule",
                    Justification = "Harmless in context")>]
  type
#if GUI || RUNNER
      internal
#endif
                SeqPnt =
    { VC: int
      SL: int
      SC: int
      EL: int
      EC: int
      Offset: int
      Id: int
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "DoNotExposeGenericListsRule",
                        Justification = "Harmless in context")>]
      Times: Times
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "DoNotExposeGenericListsRule",
                        Justification = "Harmless in context")>]
      Tracks: Tracks }

  type internal SeqPnts = List<SeqPnt>

  // Coverlet compatible -- src/coverlet.core/CoverageResult.cs
  // also round-trippable
  [<ExcludeFromCodeCoverage; NoComparison>]
  [<SuppressMessage("Gendarme.Rules.Design.Generic",
                    "DoNotExposeGenericListsRule",
                    Justification = "Harmless in context")>]
  type
#if GUI || RUNNER
      internal
#endif
                BranchInfo =
    { Line: int
      Offset: int
      EndOffset: int
      Path: int
      Ordinal: uint
      Hits: int
      // scope to expand
      Id: int
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "DoNotExposeGenericListsRule",
                        Justification = "Harmless in context")>]
      Times: Times
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "DoNotExposeGenericListsRule",
                        Justification = "Harmless in context")>]
      Tracks: Tracks }

  type internal Lines = SortedDictionary<int, int>

  type internal Branches = List<BranchInfo>

  [<ExcludeFromCodeCoverage; NoComparison>]
  [<SuppressMessage("Gendarme.Rules.Design.Generic",
                    "DoNotExposeGenericListsRule",
                    Justification = "Harmless in context")>]
  type
#if GUI || RUNNER
      internal
#endif
                Method =
    { Lines: Lines
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "DoNotExposeGenericListsRule",
                        Justification = "Harmless in context")>]
      Branches: Branches
      // scope to expand
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "DoNotExposeGenericListsRule",
                        Justification = "Harmless in context")>]
      SeqPnts: SeqPnts
      TId: Nullable<int>
      Entry: Times
      Exit: Times }
#if !GUI
    static member Create(track: (int * string) option) =
      { Lines = Lines()
        Branches = Branches()
        SeqPnts = SeqPnts()
        TId = track |> Option.map fst |> Option.toNullable
        Entry = if track.IsNone then null else Times()
        Exit = if track.IsNone then null else Times() }
#endif

  type internal Methods = Dictionary<string, Method>
  type internal Classes = Dictionary<string, Methods>
  type internal Documents = SortedDictionary<string, Classes>
  type internal Modules = SortedDictionary<string, Documents> // <= serialize this

#if RUNNER || GUI
  // Deserialization ---------------------------------------------------------

  let internal timesFromJsonValue (j: JsonValue) =
    j.Array |> Seq.map (fun a -> a.String) |> Times

  let internal tracksFromJsonValue (j: JsonValue) =
    j.Array
    |> Seq.map (fun a -> a.Number |> Math.Round |> int)
    |> Tracks

  let internal zero = JsonValue(0.0)

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal softFromKey (fallback: JsonValue) (o: JsonObject) (key: string) =
    let b, i = o.TryGetValue key
    if b then i else fallback

  let internal softNumberFromKey (o: JsonObject) (key: string) =
    (softFromKey zero o key).Number
    |> Math.Round
    |> int

  let internal softValueFromKey (o: JsonObject) (key: string) =
    softFromKey JsonValue.Null o key

  let internal valueFromKey (o: JsonObject) (key: string) fallback decoder =
    let t = softValueFromKey o key

    if t = JsonValue.Null then
      fallback
    else
      decoder t

  let internal timesByKey (o: JsonObject) =
    valueFromKey o "Times" null timesFromJsonValue

  let internal tracksByKey (o: JsonObject) =
    valueFromKey o "Tracks" null tracksFromJsonValue

  let internal seqpntFromJsonValue (j: JsonValue) =
    let o = j.Object

    {
      // extract
      VC = (softNumberFromKey o "VC")
      SL = (softNumberFromKey o "SL")
      SC = (softNumberFromKey o "SC")
      EL = (softNumberFromKey o "EL")
      EC = (softNumberFromKey o "EC")
      Offset = (softNumberFromKey o "Offset")
      Id = (softNumberFromKey o "Id")
      Times = timesByKey o
      Tracks = tracksByKey o }

  let internal seqpntsFromJsonValue (j: JsonValue) =
    j.Array |> Seq.map seqpntFromJsonValue |> SeqPnts

  let internal branchinfoFromJsonValue (j: JsonValue) =
    let o = j.Object

    { // extract
      Line = (softNumberFromKey o "Line")
      Offset = (softNumberFromKey o "Offset")
      EndOffset = (softNumberFromKey o "EndOffset")
      Path = (softNumberFromKey o "Path")
      Ordinal = (softNumberFromKey o "Ordinal") |> uint
      Hits = (softNumberFromKey o "Hits")
      // Optionals
      Id = valueFromKey o "Id" 0 (fun t -> t.Number |> Math.Round |> int)
      Times = timesByKey o
      Tracks = tracksByKey o }

  let internal linesFromJsonValue (j: JsonValue) =
    let result = Lines()

    j.Object
    |> Seq.iter
         (fun kvp ->
           let _, i = Int32.TryParse kvp.Key

           if i > 0 then
             result.[i] <- kvp.Value.Number |> Math.Round |> int)

    result

  let internal branchesFromJsonValue (j: JsonValue) =
    j.Array
    |> Seq.map branchinfoFromJsonValue
    |> Branches

  let internal methodFromJsonValue (j: JsonValue) =
    let o = j.Object

    let tid =
      valueFromKey
        o
        "TId"
        (System.Nullable())
        (fun t -> t.Number |> Math.Round |> int |> Nullable<int>)

    { Lines = valueFromKey o "Lines" (Lines()) linesFromJsonValue
      Branches = valueFromKey o "Branches" (Branches()) branchesFromJsonValue
      // Optionals
      SeqPnts = valueFromKey o "SeqPnts" null seqpntsFromJsonValue
      TId = tid
      Entry =
        valueFromKey o "Entry" (if tid.HasValue then Times() else null) timesFromJsonValue
      Exit =
        valueFromKey o "Exit" (if tid.HasValue then Times() else null) timesFromJsonValue }

  let internal methodsFromJsonValue (j: JsonValue) =
    let result = Methods()

    j.Object
    |> Seq.iter (fun kvp -> result.[kvp.Key] <- kvp.Value |> methodFromJsonValue)

    result

  let internal classesFromJsonValue (j: JsonValue) =
    let result = Classes()

    j.Object
    |> Seq.iter (fun kvp -> result.[kvp.Key] <- kvp.Value |> methodsFromJsonValue)

    result

  let internal documentsFromJsonValue (j: JsonValue) =
    let result = Documents()

    j.Object
    |> Seq.iter (fun kvp -> result.[kvp.Key] <- kvp.Value |> classesFromJsonValue)

    result

  let internal modulesFromJsonValue (j: JsonValue) =
    let result = Modules()

    j.Object
    |> Seq.iter (fun kvp -> result.[kvp.Key] <- kvp.Value |> documentsFromJsonValue)

    result

  let internal fromJsonText (report: string) =
    report
    |> Manatee.Json.JsonValue.Parse
    |> modulesFromJsonValue
#endif

#if RUNNER
  // Serialization ---------------------------------------------------------

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidReturningArraysOnPropertiesRule",
                    Justification = "Indexing required")>]
  let private allowed =
    [| 0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       0uy
       1uy
       1uy
       0uy
       1uy
       1uy
       1uy
       0uy
       0uy
       1uy
       1uy
       1uy
       0uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       0uy
       1uy
       0uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       0uy
       1uy
       1uy
       1uy
       0uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       1uy
       0uy |]

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let private jsonEscape (s: String) (builder: StringBuilder) =
    Seq.fold
      (fun (sb: StringBuilder) c ->
        match c with
        | '"' -> sb.Append("\\\"")
        | '\\' -> sb.Append("\\\\")
        | '\b' -> sb.Append("\\b")
        | '\f' -> sb.Append("\\f")
        | '\n' -> sb.Append("\\n")
        | '\r' -> sb.Append("\\r")
        | '\t' -> sb.Append("\\t")
        | h when (int h) >= 128 || Array.get allowed (int h) = 0uy ->
            sb
              .Append("\\u")
              .Append(
                ((int) c)
                  .ToString("X4", CultureInfo.InvariantCulture)
              )
        | _ -> sb.Append(c))
      builder
      s

  let private slugs =
    { 0 .. 14 }
    |> Seq.map (fun i -> (i, String(' ', i)))
    |> Map.ofSeq

  let private appendLine (str: string) (builder: StringBuilder) = builder.AppendLine str

  let private newLine (builder: StringBuilder) = builder.AppendLine()

  let private append (str: string) (builder: StringBuilder) = builder.Append str

  let private appendCharacter (c: char) (builder: StringBuilder) = builder.Append c

  let private fold2 f values state = Seq.fold f state values

  let private dictionaryToBuilder<'a>
    (depth: int)
    (next: 'a -> StringBuilder -> StringBuilder)
    (report: IDictionary<string, 'a>)
    (w: StringBuilder)
    =
    let mutable first = true

    report
    |> Seq.fold
         (fun b kvp ->
           b
           |> (if first then
                 first <- false
                 id
               else
                 appendLine ",")
           |> append slugs.[depth]
           |> appendCharacter '"'
           |> jsonEscape kvp.Key
           |> appendLine ("\": {")
           |> next kvp.Value
           |> append slugs.[depth + 1]
           |> appendCharacter '}')
         w
    |> newLine

  let private lineToBuilder (kvp: KeyValuePair<int, int>) (w: StringBuilder) =
    w
    |> append (slugs.[11])
    |> appendCharacter ('"')
    |> append (kvp.Key.ToString(CultureInfo.InvariantCulture))
    |> append ("\": ")
    |> append (kvp.Value.ToString(CultureInfo.InvariantCulture))

  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidMessageChainsRule",
                    Justification = "Fluent interface")>]
  let private itemToBuilder (i: int) (n: string) more (w: StringBuilder) =
    w
    |> append (slugs.[12])
    |> appendCharacter ('"')
    |> append (n)
    |> append ("\": ")
    |> append (i.ToString(CultureInfo.InvariantCulture))
    |> if more then appendLine (",") else id

  let private timeToBuilder depth (time: TimeStamp) (b: StringBuilder) =
    b
    |> append (slugs.[depth])
    |> appendCharacter ('"')
    |> append (time)
    |> appendCharacter ('"')

  let private timesToBuilder (times: Times) (w: StringBuilder) =
    let mutable firstTime = true

    if times.IsNotNull && times.Count > 0 then
      w
      |> appendLine (",")
      |> append (slugs.[12])
      |> append ("\"Times\": [")
      |> fold2
           (fun b t ->
             timeToBuilder
               14
               t
               (b
                |> if firstTime then
                     firstTime <- false
                     newLine
                   else
                     appendLine (",")))
           times
      |> newLine
      |> append (slugs.[13])
      |> append ("]")
    else
      w

  let private eeToBuilder (times: Times) (w: StringBuilder) =
    let mutable firstTime = true

    if times.IsNotNull && times.Count > 0 then
      w
      |> fold2
           (fun b t ->
             timeToBuilder
               11
               t
               (b
                |> if firstTime then
                     firstTime <- false
                     newLine
                   else
                     appendLine (",")))
           times
      |> newLine
      |> append (slugs.[10])
    else
      w

  let private tracksToBuilder (tracks: Tracks) (w: StringBuilder) =
    let mutable firstTime = true

    w
    |> if tracks.IsNotNull && tracks.Count > 0 then
         appendLine (",")
         >> append (slugs.[12])
         >> append ("\"Tracks\": [")
         >> fold2
              (fun b (t: int) ->
                b
                |> if firstTime then
                     firstTime <- false
                     newLine
                   else
                     appendLine (",")
                |> append (slugs.[14])
                |> append (t.ToString(CultureInfo.InvariantCulture)))
              tracks
         >> newLine
         >> append (slugs.[13])
         >> append ("]")
       else
         id

  let private branchToBuilder (b: BranchInfo) (w: StringBuilder) =
    w
    |> append (slugs.[11])
    |> appendLine ("{")
    |> itemToBuilder b.Line "Line" true
    |> itemToBuilder b.Offset "Offset" true
    |> itemToBuilder b.EndOffset "EndOffset" true
    |> itemToBuilder b.Path "Path" true
    |> itemToBuilder (int b.Ordinal) "Ordinal" true
    |> itemToBuilder b.Hits "Hits" (b.Id > 0)
    |> if b.Id > 0 then
         itemToBuilder b.Id "Id" false
       else
         id
    |> timesToBuilder b.Times
    |> tracksToBuilder b.Tracks
    |> newLine
    |> append (slugs.[11])
    |> append ("}")

  let private seqpntToBuilder (s: SeqPnt) (w: StringBuilder) =
    w
    |> append (slugs.[11])
    |> appendLine ("{")
    |> itemToBuilder s.VC "VC" true
    |> itemToBuilder s.SL "SL" true
    |> itemToBuilder s.SC "SC" true
    |> itemToBuilder s.EL "EL" true
    |> itemToBuilder s.EC "EC" true
    |> itemToBuilder s.Offset "Offset" true
    |> itemToBuilder s.Id "Id" false
    |> timesToBuilder s.Times
    |> tracksToBuilder s.Tracks
    |> newLine
    |> append (slugs.[11])
    |> append ("}")

  let private methodTrackingToBuilder (method: Method) (w: StringBuilder) =
    w
    |> if method.TId.HasValue then
         appendLine (",")
         >> append (slugs.[9])
         >> append ("\"TId\": ")
         >> append (method.TId.Value.ToString(CultureInfo.InvariantCulture))
         >> appendLine (",")
         >> append (slugs.[9])
         >> append ("\"Entry\": [")
         >> eeToBuilder method.Entry
         >> append ("]")
         >> appendLine (",")
         >> append (slugs.[9])
         >> append ("\"Exit\": [")
         >> eeToBuilder method.Exit
         >> appendLine ("]")
       else
         newLine

  let private methodToBuilder (method: Method) (w: StringBuilder) =
    let post = newLine >> append (slugs.[10])

    let ifNotEmpty
      (collection: 'a seq)
      (operation: 'a -> StringBuilder -> StringBuilder)
      preamble
      postamble
      =
      if collection.IsNotNull
         && collection |> Seq.isEmpty |> not then
        let mutable first = true

        preamble
        >> fold2
             (fun b kvp ->
               b
               |> if first then
                    first <- false
                    id
                  else
                    appendLine (",")
               |> operation kvp)
             collection
        >> postamble
      else
        id

    w
    |> append (slugs.[9])
    |> (if method.Lines |> Seq.isEmpty then append else appendLine) ("\"Lines\": {")
    |> ifNotEmpty method.Lines lineToBuilder id post
    |> appendLine ("},")

    // After Lines, now Branches

    |> append (slugs.[9])
    |> append ("\"Branches\": [")
    |> ifNotEmpty method.Branches branchToBuilder newLine post
    |> append ("]")

    // After Branches, now SeqPnts
    |> ifNotEmpty
         method.SeqPnts
         seqpntToBuilder
         (appendLine (",")
          >> append (slugs.[9])
          >> appendLine ("\"SeqPnts\": ["))
         id
    |> if Seq.isEmpty method.SeqPnts
       then id
       else
         newLine
         >> append (slugs.[10])
         >> append ("]")

    // After SeqPnts, now Tracking
    |> methodTrackingToBuilder method

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let private methodsToBuilder (methods: Methods) (w: StringBuilder) =
    (dictionaryToBuilder 7 methodToBuilder methods w)

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let private classesToBuilder (classes: Classes) (w: StringBuilder) =
    (dictionaryToBuilder 5 methodsToBuilder classes w)

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let private documentsToBuilder (documents: Documents) (w: StringBuilder) =
    (dictionaryToBuilder 3 classesToBuilder documents w)

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let private modulesToBuilder (report: Modules) (w: StringBuilder) =
    (dictionaryToBuilder 1 documentsToBuilder report w)

  let internal toText (report: Modules) =
    let w = StringBuilder()

    w
    |> appendLine ("{")
    |> modulesToBuilder report
    |> appendLine ("}")
    |> ignore

    let result = w.ToString()
    result

  let internal serializeToUtf8Bytes (document: Modules) =
    document
    |> toText
    |> System.Text.Encoding.UTF8.GetBytes

#endif

#if GUI || RUNNER
  // Conversion to XML ---------------------------------------------------------

  let internal summaryElement () =
    let zero name = XAttribute(XName.Get name, 0)
    XElement(
      XName.Get "Summary",
      zero "numBranchPoints",
      zero "visitedBranchPoints",
      zero "numSequencePoints",
      zero "visitedSequencePoints")

  let internal buildSummary (m: XContainer) =
    let sd = summaryElement ()
    m.Add sd
    sd

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal tryGetValueOrDefault
    (table: Dictionary<string, 'a>)
    (key: string)
    (f: unit -> 'a)
    =
    let ok, index = table.TryGetValue key

    if ok then
      index
    else
      let value = f ()
      table.Add(key, value)
      value

  let internal buildMethodElement
    (table:Dictionary<string, XElement>)  (item:XElement)
    name fileId =
        tryGetValueOrDefault
          table
          name
          (fun () ->
            let m2 =
                XElement(
                  XName.Get "Method",
                  XAttribute(XName.Get "visited", false),
                  XAttribute(XName.Get "cyclomaticComplexity", 1),
                  XAttribute(XName.Get "sequenceCoverage", 0),
                  XAttribute(XName.Get "branchCoverage", 0),
                  XAttribute(XName.Get "isConstructor", false),
                  XAttribute(XName.Get "isStatic", false),
                  XAttribute(XName.Get "isGetter", false),
                  XAttribute(XName.Get "isSetter", false)
                )

            let sd = buildSummary m2

            [ "MetadataToken", "0"; "Name", name ]
            |> Seq.iter
                 (fun (name, value) ->
                   let x = XElement(XName.Get name)
                   x.Value <- value
                   m2.Add x)

            let f =
              XElement(XName.Get "FileRef", XAttribute(XName.Get "uid", fileId))

            m2.Add f
            item.Add m2
            m2)

  let internal makeSummary (nb: int) (vb: int) (ns: int) (vs: int) (sd: XElement) =
    sd.Attribute(XName.Get "numBranchPoints").Value <- nb.ToString(
      CultureInfo.InvariantCulture
    )
    sd.Attribute(XName.Get "visitedBranchPoints").Value <- vb.ToString(
      CultureInfo.InvariantCulture
    )
    sd.Attribute(XName.Get "numSequencePoints").Value <- ns.ToString(
      CultureInfo.InvariantCulture
    )
    sd.Attribute(XName.Get "visitedSequencePoints").Value <- vs.ToString(
      CultureInfo.InvariantCulture
    )

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "InstantiateArgumentExceptionCorrectlyRule",
                    Justification = "Library method inlined")>]
  let internal methodToXml
    (table:Dictionary<string, XElement>)
    (fileId: int)
    (item: XElement)
    (method: KeyValuePair<string, Method>)
    =
    let m = buildMethodElement table item method.Key fileId

    // conditional
    let sp = m.Descendants("SequencePoints".X)
             |> Seq.tryHead
             |> Option.defaultWith (fun () -> let tmp = XElement(XName.Get "SequencePoints")
                                              m.Add tmp
                                              tmp)

    let bp = m.Descendants("BranchPoints".X)
             |> Seq.tryHead
             |> Option.defaultWith (fun () -> let tmp = XElement(XName.Get "BranchPoints")
                                              m.Add tmp
                                              tmp)

    let value = method.Value
    let bec = Dictionary<int, int>()
    let bev = Dictionary<int, int>()

    if value.Branches.IsNotNull then
      value.Branches
      |> Seq.iter
           (fun b ->
             let _, old = bec.TryGetValue b.Line
             bec.[b.Line] <- old + 1

             if b.Hits > 0 then
               let _, old = bev.TryGetValue b.Line
               bev.[b.Line] <- old + 1

             let bx =
               XElement(
                 XName.Get "BranchPoint",
                 XAttribute(XName.Get "vc", b.Hits),
                 XAttribute(XName.Get "sl", b.Line),
                 XAttribute(XName.Get "uspid", b.Id),
                 XAttribute(XName.Get "ordinal", b.Ordinal),
                 XAttribute(XName.Get "offset", b.Offset),
                 XAttribute(XName.Get "path", b.Path),
                 XAttribute(XName.Get "fileid", fileId)
               )

             bp.Add bx)

      let targets =
        value.Branches
        |> Seq.groupBy (fun b -> b.Line)
        |> Seq.sumBy
             (fun (_, x) ->
               x
               |> Seq.distinctBy (fun bx -> bx.EndOffset)
               |> Seq.length)
      m.Attribute(XName.Get "cyclomaticComplexity").Value <- (1 + targets)
        .ToString(CultureInfo.InvariantCulture)

    if value.SeqPnts.IsNotNull then
      value.SeqPnts
      |> Seq.iter
           (fun s ->
             let _, bec2 = bec.TryGetValue s.SL
             bec.[s.SL] <- 0
             let _, bev2 = bev.TryGetValue s.SL
             bev.[s.SL] <- 0

             let sx =
               XElement(
                 XName.Get "SequencePoint",
                 XAttribute(XName.Get "vc", s.VC),
                 XAttribute(XName.Get "offset", s.Offset),
                 XAttribute(XName.Get "sl", s.SL),
                 XAttribute(XName.Get "sc", s.SC),
                 XAttribute(XName.Get "el", s.EL),
                 XAttribute(XName.Get "ec", s.EC),
                 XAttribute(XName.Get "uspid", s.Id),
                 XAttribute(XName.Get "bec", bec2),
                 XAttribute(XName.Get "bev", bev2),
                 XAttribute(XName.Get "fileid", fileId)
               )

             sp.Add sx)
    else
      value.Lines
      |> Seq.iteri
           (fun i l ->
             let k = l.Key
             let _, bec2 = bec.TryGetValue k
             bec.[k] <- 0
             let _, bev2 = bev.TryGetValue k
             bev.[k] <- 0

             let sx =
               XElement(
                 XName.Get "SequencePoint",
                 XAttribute(XName.Get "vc", l.Value),
                 XAttribute(XName.Get "offset", i),
                 XAttribute(XName.Get "sl", k),
                 XAttribute(XName.Get "sc", 1),
                 XAttribute(XName.Get "el", k),
                 XAttribute(XName.Get "ec", 2),
                 XAttribute(XName.Get "uspid", i),
                 XAttribute(XName.Get "bec", bec2),
                 XAttribute(XName.Get "bev", bev2),
                 XAttribute(XName.Get "fileid", fileId)
               )

             sp.Add sx)

    // recompute points
    let sd = m.Descendants("Summary".X) |> Seq.head
    let branches = bp.Descendants("BranchPoint".X)
    let nb = branches |> Seq.length
    let vb = branches |> Seq.filter (fun x -> x.Attribute("vc".X).Value <> "0")
                      |> Seq.length
    let points = sp.Descendants("SequencePoint".X)
    let ns = points |> Seq.length
    let vs = points |> Seq.filter (fun x -> x.Attribute("vc".X).Value <> "0")
                    |> Seq.length

    if ns > 0 then
      let mp = m.Descendants("MethodPoint".X) |> Seq.tryHead
               |> Option.defaultWith (fun () -> let tmp = XElement(XName.Get "MethodPoint", XAttribute(XName.Get "vc", "0"))
                                                m.Add tmp
                                                tmp)
      let mvc = points // not Seq.max as that exposes repeated calls to enumerator.Current when bootstrapping
                |> Seq.fold (fun max x -> let tmp = x.Attribute("vc".X).Value |> Int32.TryParse |> snd
                                          if tmp > max then tmp else max) 0 // know this is a hard floor
      mp.Attribute("vc".X).Value <- mvc.ToString(CultureInfo.InvariantCulture)

    // adjust to match OpenCover values for branches
    makeSummary (nb.Increment (nb > 0 || ns > 0)) 
                (vb.Increment (vb > 0 || vs > 0)) ns vs sd

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal methodsToXml (fileId: int) (item: XElement) (table:Dictionary<string, XElement>) (methods: Methods) =
    methods |> Seq.iter (methodToXml table fileId item)

  let private valueOf (x: XElement) (name: string) =
    x.Attribute(XName.Get name).Value
    |> Int32.TryParse
    |> snd

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal summarize sd (m: XElement) name =
    let (nb, vb, ns, vs) =
      m.Descendants(XName.Get name)
      |> Seq.collect (fun m2 -> m2.Elements(XName.Get "Summary"))
      |> Seq.fold
           (fun (bn, bv, sn, sv) ms ->
             (bn + valueOf ms "numBranchPoints",
              bv + valueOf ms "visitedBranchPoints",
              sn + valueOf ms "numSequencePoints",
              sv + valueOf ms "visitedSequencePoints"))
           (0, 0, 0, 0)

    makeSummary nb vb ns vs sd


  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal classesToXml
    (fileId: int)
    (table: Dictionary<string, XElement>)
    (methodtable: Dictionary<string, XElement>)
    (classes: Classes)
    =
    classes
    |> Seq.iteri
         (fun i kvp ->
           let name = kvp.Key

           let item =
             tryGetValueOrDefault
               table
               name
               (fun () ->
                 XElement(
                   XName.Get "Class",
                   summaryElement (),
                   XElement(XName.Get "FullName", name),
                   XElement(XName.Get "Methods")
                 ))

           let next =
             item.Elements(XName.Get "Methods") |> Seq.head

           methodsToXml fileId next methodtable kvp.Value
           let sd = item.Descendants("Summary".X) |> Seq.head
           summarize sd item "Method")

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal documentsToXml
    (indexTable: Dictionary<string, int>)
    (key: string)
    (documents: Documents)
    =
    let m =
      XElement(XName.Get "Module", XAttribute(XName.Get "hash", key))

    let sd = buildSummary m

    [ "ModulePath", key
      "ModuleName", (key |> Path.GetFileNameWithoutExtension) ]
    |> Seq.iter
         (fun (name, value) ->
           let x = XElement(XName.Get name)
           x.Value <- value
           m.Add x)

    let files = XElement(XName.Get "Files")
    m.Add files
    let classes = XElement(XName.Get "Classes")
    m.Add classes
    let classTable = Dictionary<string, XElement>()
    let methodTable = Dictionary<string, XElement>()

    documents
    |> Seq.iter
         (fun kvp ->
           let name = kvp.Key

           let i =
             tryGetValueOrDefault indexTable name (fun () -> 1 + indexTable.Count)

           let item =
             XElement(
               XName.Get "File",
               XAttribute(XName.Get "uid", i),
               XAttribute(XName.Get "fullPath", name)
             )

           kvp.Value
           |> Seq.tryFind(fun kvp -> kvp.Key = "\u00ABAltCover.embed\u00BB")
           |> Option.map (fun kvp -> kvp.Value.Keys |> Seq.tryHead)
           |> Option.flatten
           |> Option.iter (fun embed -> item.Add(XAttribute(XName.Get "altcover.embed", embed)))

           files.Add item
           classesToXml i classTable methodTable kvp.Value)

    classTable
    |> Seq.filter (fun kvp -> kvp.Key |> Seq.head <> '\u00AB')
    |> Seq.iter (fun kvp -> classes.Add kvp.Value)

    summarize sd m "Method"
    m

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "InstantiateArgumentExceptionCorrectlyRule",
                    Justification = "Library method inlined")>]
  [<SuppressMessage("Microsoft.Usage",
                    "CA2208:InstantiateArgumentExceptionsCorrectly",
                    Justification = "Library method inlined")>]
  let internal jsonToXml (modules: Modules) =
    let x = XDocument()
    x.Add(XElement(XName.Get "CoverageSession"))
    let root = x.Root
    let sd = buildSummary root
    let mroot = XElement(XName.Get "Modules")
    root.Add mroot
    let fileRefs = Dictionary<string, int>()

    modules
    |> Seq.iter
         (fun kvp ->
           documentsToXml fileRefs kvp.Key kvp.Value
           |> mroot.Add)

    summarize sd root "Module"

    let mcc =
      x.Descendants(XName.Get "Method")
      |> Seq.fold
           (fun top x ->
             let value = valueOf x "cyclomaticComplexity"
             if value > top then value else top)
           1

    sd.Add(
      XAttribute(
        XName.Get "maxCyclomaticComplexity",
        mcc.ToString(CultureInfo.InvariantCulture)
      )
    )

    x

#if RUNNER
  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal orderXml (x: XDocument) =
    x.Descendants(XName.Get "SequencePoints")
    |> Seq.iter
         (fun sps ->
           let original = sps.Elements() |> Seq.toList
           sps.RemoveAll()

           original
           |> Seq.sortBy
                (fun sp ->
                  let offset = sp.Attribute(XName.Get "fileid")
                  let topword = offset
                              |> Option.ofObj
                              |> Option.map(fun x -> x.Value |> Int64.TryParse |> snd)
                              |> Option.defaultValue 0L

                  let sl =
                    sp.Attribute(XName.Get "sl").Value
                    |> Int32.TryParse
                    |> snd

                  let sc =
                    sp.Attribute(XName.Get "sc").Value
                    |> Int32.TryParse
                    |> snd

                  (topword <<< 32) + int64 ((sl <<< 16) + sc))
           |> sps.Add)

    x.Descendants(XName.Get "BranchPoints")
    |> Seq.iter
         (fun bps ->
           let original = bps.Elements() |> Seq.toList
           bps.RemoveAll()

           original
           |> Seq.sortBy
                (fun bp ->
                  let sl =
                    bp.Attribute(XName.Get "sl").Value
                    |> Int32.TryParse
                    |> snd

                  let offset =
                    bp.Attribute(XName.Get "ordinal").Value
                    |> Int32.TryParse
                    |> snd

                  (sl <<< 16) + offset)
           |> bps.Add)

    x
#endif

  let internal fileToJson filename =
    filename |> File.ReadAllText |> fromJsonText

#endif

#if RUNNER
  // Instrumentation ---------------------------------------------------------

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let injectEmbed (c:Classes) (embed:string) =
    if embed |> String.IsNullOrWhiteSpace |> not then
      let dummy = Method.Create(None)
      let m = Methods()
      m.Add (embed, dummy)
      c.Add ("\u00ABAltCover.embed\u00BB", m)

  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
  type internal JsonContext =
    { Documents: Documents
      Type: TypeDefinition
      VisibleType: TypeDefinition
      Method: MethodDefinition
      VisibleMethod: MethodDefinition
      Track: (int * string) option }
    static member Build() =
      { Documents = null
        Type = null
        VisibleType = null
        Method = null
        VisibleMethod = null
        Track = None }

  let internal reportGenerator () =
    let document = Modules()

    let startVisit = id

    let visitModule s (m: ModuleEntry) =
      let documents = Documents()
      document.Add(m.Module.FileName |> Path.GetFileName, documents)
      { s with Documents = documents }

    let visitType (s: JsonContext) (m: TypeEntry) =
      { s with
          Type = m.Type
          VisibleType = m.VisibleType }

    let visitMethod (s: JsonContext) (m: MethodEntry) =
      { s with
          Method = m.Method
          VisibleMethod = m.VisibleMethod
          Track = m.Track }

    let getMethodRecord (s: JsonContext) (doc: string) (record:Cil.Document) =
      let visibleMethodName = s.VisibleMethod.FullName
      let visibleTypeName = s.VisibleMethod.DeclaringType.FullName

      let classes =
        match s.Documents.TryGetValue doc with
        | true, c -> c
        | _ ->
            let c = Classes()
            s.Documents.Add(doc, c)
            record
            |> Metadata.getSource
            |> Option.iter (injectEmbed c)
            c

      let methods =
        match classes.TryGetValue visibleTypeName with
        | true, m -> m
        | _ ->
            let m = Methods()
            classes.Add(visibleTypeName, m)
            m

      match methods.TryGetValue visibleMethodName with
      | true, m -> m
      | _ ->
          let m = Method.Create(s.Track)
          methods.Add(visibleMethodName, m)
          m

    let visitMethodPoint (s: JsonContext) (e: StatementEntry) =
      if e.Interesting then
        e.SeqPnt
        |> Option.iter
             (fun codeSegment ->
               let doc =
                 codeSegment.Document.Url |> Visitor.sourceLinkMapping

               let mplus = getMethodRecord s doc codeSegment.Document
               mplus.Lines.[codeSegment.StartLine] <- int e.DefaultVisitCount

               mplus.SeqPnts.Add
                 { VC = int e.DefaultVisitCount
                   SL = codeSegment.StartLine
                   SC = codeSegment.StartColumn
                   EL = codeSegment.EndLine
                   EC = codeSegment.EndColumn
                   Offset = codeSegment.Offset
                   Id = e.Uid
                   Times = null
                   Tracks = null })

      s

    let visitBranchPoint (s: JsonContext) (b: GoTo) =
      if b.Included then
        let doc =
          b.SequencePoint.Document.Url
          |> Visitor.sourceLinkMapping

        let mplus = getMethodRecord s doc b.SequencePoint.Document

        mplus.Branches.Add
          { Line = b.SequencePoint.StartLine
            Offset = b.Offset
            EndOffset = b.Target.Head.Offset
            Path =
              mplus.Branches
              |> Seq.filter (fun k -> k.Offset = b.Offset)
              |> Seq.length
            Ordinal = uint mplus.Branches.Count
            Hits = int b.VisitCount
            // scope to expand
            Id = b.Uid
            Times = null
            Tracks = null }

      s

    let visitAfterMethod (s: JsonContext) _ =
      { s with
          Method = null
          VisibleMethod = null
          Track = None }

    let visitAfterType (s: JsonContext) =
      { s with
          Type = null
          VisibleType = null }

    let visitAfterModule s = { s with Documents = null }

    let reportVisitor (s: JsonContext) (node: Node) =
      match node with
      | Start _ -> startVisit s
      | Node.Module m -> visitModule s m
      | Node.Type t -> visitType s t
      | Node.Method m -> visitMethod s m
      | MethodPoint m -> visitMethodPoint s m
      | BranchPoint b -> visitBranchPoint s b
      | AfterMethod m -> visitAfterMethod s m
      | AfterType _ -> visitAfterType s
      | AfterModule _ -> visitAfterModule s
      | _ -> s

    let result =
      Visitor.encloseState reportVisitor (JsonContext.Build())

    (result,
     fun (s: System.IO.Stream) ->
       let encoded = serializeToUtf8Bytes document
       s.Write(encoded, 0, encoded.Length))

[<AutoSerializable(false)>]
type internal DocumentType =
  | XML of XDocument
  | JSON of NativeJson.Modules
  | Unknown
  static member internal LoadReport format report =
    if File.Exists report then
      if format = ReportFormat.NativeJson
         || format = ReportFormat.NativeJsonWithTracking then
        report |> NativeJson.fileToJson |> JSON
      else
        report |> XDocument.Load |> XML
    else
      Unknown
#endif

#if GUI || RUNNER
// FxCop ---------------------------------------------------------
#if GUI
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-UICommon>.$NativeJson.#.cctor()",
                            Justification = "Compiler Generated")>]
#endif

[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#Branches",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#SeqPnts",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+BranchInfo.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.UInt32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+BranchInfo.#Times",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+BranchInfo.#Tracks",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,System.Nullable`1<System.Int32>,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.String>)",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+SeqPnt.#Times",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+SeqPnt.#Tracks",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#Entry",
                            Justification = "Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1002:DoNotExposeGenericLists",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#Exit",
                            Justification = "Harmless in context")>]

[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#SeqPnts",
                            MessageId = "Pnts",
                            Justification = "Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,System.Nullable`1<System.Int32>,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.String>)",
                            MessageId = "t",
                            Justification = "Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,System.Nullable`1<System.Int32>,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.String>)",
                            MessageId = "Pnts",
                            Justification = "Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "type",
                            Target = "AltCover.NativeJson+SeqPnt",
                            MessageId = "Pnt",
                            Justification = "Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
                            MessageId = "e",
                            Justification = "Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
                            MessageId = "s",
                            Justification = "Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
                            MessageId = "v",
                            Justification = "Smaller JSON")>]
()
#endif