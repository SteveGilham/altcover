namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq

#if RUNNER
open System.Globalization
open System.Text
open Mono.Cecil

[<AutoSerializable(false)>]
type internal DocumentType =
| XML of XDocument
| JSON of String
| Unknown
#endif

#if GUI || RUNNER
open Manatee.Json
#endif

module NativeJson =

  type internal TimeStamp = string

  let FromTracking(ticks:int64) : TimeStamp =
    ticks
    |> BitConverter.GetBytes
    |> Convert.ToBase64String

  type internal Times = List<TimeStamp>

  type internal Tracks = List<int>

  [<ExcludeFromCodeCoverage; NoComparison>]
  [<SuppressMessage(
        "Gendarme.Rules.Design.Generic",
        "DoNotExposeGenericListsRule",
        Justification="Harmless in context")>]
  type SeqPnt =
    {
      VC:int
      SL:int
      SC:int
      EL:int
      EC:int
      Offset:int
      Id:int
      [<SuppressMessage(
            "Gendarme.Rules.Design.Generic",
            "DoNotExposeGenericListsRule",
            Justification="Harmless in context")>]
      Times: Times
      [<SuppressMessage(
            "Gendarme.Rules.Design.Generic",
            "DoNotExposeGenericListsRule",
            Justification="Harmless in context")>]
      Tracks: Tracks
    }

  type SeqPnts = List<SeqPnt>

  // Coverlet compatible -- src/coverlet.core/CoverageResult.cs
  // also round-trippable
  [<ExcludeFromCodeCoverage; NoComparison>]
  [<SuppressMessage(
        "Gendarme.Rules.Design.Generic",
        "DoNotExposeGenericListsRule",
        Justification="Harmless in context")>]
  type BranchInfo =
    {
      Line:int
      Offset:int
      EndOffset:int
      Path:int
      Ordinal:uint
      Hits:int
    // scope to expand
      Id:int
      [<SuppressMessage(
            "Gendarme.Rules.Design.Generic",
            "DoNotExposeGenericListsRule",
            Justification="Harmless in context")>]
      Times: Times
      [<SuppressMessage(
            "Gendarme.Rules.Design.Generic",
            "DoNotExposeGenericListsRule",
            Justification="Harmless in context")>]
      Tracks: Tracks
    }

  type Lines = SortedDictionary<int, int>

  type Branches = List<BranchInfo>

  [<ExcludeFromCodeCoverage; NoComparison>]
  [<SuppressMessage(
        "Gendarme.Rules.Design.Generic",
        "DoNotExposeGenericListsRule",
        Justification="Harmless in context")>]
  type Method =
    {
      Lines:Lines
      [<SuppressMessage(
            "Gendarme.Rules.Design.Generic",
            "DoNotExposeGenericListsRule",
            Justification="Harmless in context")>]
      Branches:Branches
      // scope to expand
      [<SuppressMessage(
            "Gendarme.Rules.Design.Generic",
            "DoNotExposeGenericListsRule",
            Justification="Harmless in context")>]
      SeqPnts:SeqPnts
      TId:Nullable<int> // tracking ID
      Entry:Times
      Exit:Times
    }
    static member Create(track:(int*string) option) =
      {
        Lines = Lines()
        Branches = Branches()
        SeqPnts = SeqPnts()
        TId = track |> Option.map fst |> Option.toNullable
        Entry = if track.IsNone then null else Times()
        Exit = if track.IsNone then null else Times()
      }

  type internal Methods = Dictionary<string, Method>
  type internal Classes = Dictionary<string, Methods>
  type internal Documents = SortedDictionary<string, Classes>
  type internal Modules = SortedDictionary<string, Documents> // <= serialize this

#if RUNNER || GUI
  // Deserialization ---------------------------------------------------------

  let internal timesFromJsonValue (j:JsonValue) =
    j.Array
    |> Seq.map (fun a -> a.String)
    |> Times

  let internal tracksFromJsonValue (j:JsonValue) =
    j.Array
    |> Seq.map (fun a -> a.Number |> Math.Round |> int)
    |> Tracks

  let internal zero = JsonValue(0.0)

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal softFromKey (fallback:JsonValue) (o:JsonObject) (key:string) =
    let b, i = o.TryGetValue key
    if b then i else fallback

  let internal softNumberFromKey (o:JsonObject) (key:string) =
    (softFromKey zero o key).Number |> Math.Round |> int

  let internal softValueFromKey (o:JsonObject) (key:string) =
    softFromKey JsonValue.Null o key

  let internal seqpntFromJsonValue (j:JsonValue) =
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
      Times = let t = softValueFromKey o" Times"
              if t = JsonValue.Null
              then null
              else timesFromJsonValue t
      Tracks = let t = softValueFromKey o "Tracks"
               if t = JsonValue.Null
               then null
               else tracksFromJsonValue t
    }

  let internal seqpntsFromJsonValue (j:JsonValue) =
    j.Array
    |> Seq.map seqpntFromJsonValue
    |> SeqPnts

  let internal branchinfoFromJsonValue (j:JsonValue) =
    let o = j.Object
    { // extract
      Line = (softNumberFromKey o "Line")
      Offset = (softNumberFromKey o "Offset")
      EndOffset = (softNumberFromKey o "EndOffset")
      Path = (softNumberFromKey o "Path")
      Ordinal = (softNumberFromKey o "Ordinal") |> uint
      Hits = (softNumberFromKey o "Hits")
      // Optionals
      Id = let t = softValueFromKey o "Id"
           if t = JsonValue.Null
           then 0
           else t.Number |> Math.Round |> int
      Times = let t = softValueFromKey o" Times"
              if t = JsonValue.Null
              then null
              else timesFromJsonValue t
      Tracks = let t = softValueFromKey o "Tracks"
               if t = JsonValue.Null
               then null
               else tracksFromJsonValue t
    }

  let internal linesFromJsonValue (j:JsonValue) =
    let result = SortedDictionary<int, int>()
    j.Object
    |> Seq.iter (fun kvp ->
      let _,i = Int32.TryParse kvp.Key
      if i > 0 then
        result.[i] <- kvp.Value.Number |> Math.Round |> int
    )
    result

  let internal branchesFromJsonValue (j:JsonValue) =
    j.Array
    |> Seq.map branchinfoFromJsonValue
    |> Branches

  let internal methodFromJsonValue (j:JsonValue) =
    let o = j.Object
    let tid = let t = softValueFromKey o "TId"
              if t = JsonValue.Null
              then System.Nullable()
              else t.Number |> Math.Round |> int |> Nullable<int>

    {
      Lines = (softValueFromKey o "Lines") |> linesFromJsonValue
      Branches = (softValueFromKey o "Branches") |> branchesFromJsonValue
      // Optionals
      SeqPnts = let t = softValueFromKey o "SeqPnts"
                if t = JsonValue.Null
                then null
                else seqpntsFromJsonValue t
      TId = tid
      Entry = let t = softValueFromKey o "Entry"
              if t = JsonValue.Null
              then if tid.HasValue then Times() else null
              else timesFromJsonValue t
      Exit = let t = softValueFromKey o "Exit"
             if t = JsonValue.Null
             then if tid.HasValue then Times() else null
             else timesFromJsonValue t
    }

  let internal methodsFromJsonValue (j:JsonValue) =
    let result = Methods()
    j.Object
    |> Seq.iter (fun kvp ->
        result.[kvp.Key] <- kvp.Value |> methodFromJsonValue
    )
    result

  let internal classesFromJsonValue (j:JsonValue) =
    let result = Classes()
    j.Object
    |> Seq.iter (fun kvp ->
        result.[kvp.Key] <- kvp.Value |> methodsFromJsonValue
    )
    result

  let internal documentsFromJsonValue (j:JsonValue) =
    let result = Documents()
    j.Object
    |> Seq.iter (fun kvp ->
        result.[kvp.Key] <- kvp.Value |> classesFromJsonValue
    )
    result

  let internal modulesFromJsonValue (j:JsonValue) =
    let result = Modules()
    j.Object
    |> Seq.iter (fun kvp ->
        result.[kvp.Key] <- kvp.Value |> documentsFromJsonValue
    )
    result

  let internal fromJsonText (report:string) =
    report
    |> Manatee.Json.JsonValue.Parse
    |> modulesFromJsonValue
#endif

#if RUNNER
  // Serialization ---------------------------------------------------------

  [<Sealed>]
  type private BuildWriter() =
    inherit TextWriter(CultureInfo.InvariantCulture)
    member val Builder = StringBuilder() with get, set
    member self.Clear() = let temp = self.Builder
                          self.Builder <- null
                          temp
    override self.Encoding = Encoding.Unicode // pointless but required
    [<SuppressMessage(
      "Gendarme.Rules.Exceptions", "UseObjectDisposedExceptionRule",
      Justification="Would be meaningless")>]
    override self.Write(value:Char) =
        value
        |> self.Builder.Append
        |> ignore

    [<SuppressMessage(
      "Gendarme.Rules.Exceptions", "UseObjectDisposedExceptionRule",
      Justification="Would be meaningless")>]
    override self.Write(value:String) =
        value
        |> self.Builder.Append
        |> ignore

  let private escapeString (builder:TextWriter) (s:String) =
    System.Text.Encodings.Web.JavaScriptEncoder.Default.Encode(builder, s)

  let private slugs =
    { 0 .. 14 }
    |> Seq.map (fun i -> (i, String(' ', i)))
    |> Map.ofSeq

  let private dictionaryToWriter<'a>
    (depth : int)
    (next : BuildWriter -> 'a -> BuildWriter )
    (w:BuildWriter)
    (report:IDictionary<string,'a>)
    =
    let mutable first = true
    report
    |> Seq.iter (fun kvp ->
      if not first then ("," |> w.Builder.AppendLine |> ignore)
      first <- false
      w.Builder
        .Append(slugs.[depth])
        .Append('"') |> ignore
      escapeString w kvp.Key
      w.Builder
        .AppendLine("\": {") |> ignore
      (next w kvp.Value)
        .Builder.Append(slugs.[depth + 1])
        .Append('}') |> ignore
    )
    w.Builder.AppendLine() |> ignore
    w

  [<SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidMessageChainsRule",
    Justification = "Fluent interface")>]
  let private lineToWriter (w:BuildWriter) (kvp:KeyValuePair<int,int>) =
    w.Builder.Append(slugs.[11])
      .Append('"')
      .Append(kvp.Key.ToString(CultureInfo.InvariantCulture))
      .Append("\": ")
      .Append(kvp.Value.ToString(CultureInfo.InvariantCulture))

  [<SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidMessageChainsRule",
    Justification = "Fluent interface")>]
  let private itemToWriter (w:BuildWriter) (i:int) (n:string) more =
    w.Builder.Append(slugs.[12])
      .Append('"')
      .Append(n)
      .Append("\": ")
      .Append(i.ToString(CultureInfo.InvariantCulture)) |> ignore
    if more then
      w.Builder.AppendLine(",") |> ignore

  let private timeToWriter (b:StringBuilder) depth (time:TimeStamp) =
    b.Append(slugs.[depth]).Append('"').Append(time).Append('"') |> ignore

  let private timesToWriter (w:BuildWriter) (times:Times) =
    if times.IsNotNull && times.Count > 0
    then
      w.Builder.AppendLine(",").Append(slugs.[12])
               .Append("\"Times\": [") |> ignore
      let mutable firstTime = true
      times
      |> Seq.iter (fun t ->
        timeToWriter  (if firstTime
                       then firstTime <- false
                            w.Builder.AppendLine()
                            else w.Builder.AppendLine(",")) 14 t
      )
      w.Builder.AppendLine()
        .Append(slugs.[13]).Append("]") |> ignore

  let private tracksToWriter (w:BuildWriter) (tracks:Tracks) =
    if tracks.IsNotNull && tracks.Count > 0
    then
      w.Builder.AppendLine(",").Append(slugs.[12]).Append("\"Tracks\": [")|> ignore
      let mutable firstTime = true
      tracks
      |> Seq.iter (fun t ->
        (if firstTime
         then firstTime <- false
              w.Builder.AppendLine()
         else w.Builder.AppendLine(","))
          .Append(slugs.[14]).Append(t.ToString(CultureInfo.InvariantCulture)) |> ignore
      )
      w.Builder.AppendLine()
        .Append(slugs.[13]).Append("]") |> ignore

  let private branchToWriter  (w:BuildWriter) (b:BranchInfo) =
    w.Builder.Append(slugs.[11]).AppendLine("{") |> ignore
    itemToWriter w b.Line "Line" true
    itemToWriter w b.Offset "Offset" true
    itemToWriter w b.EndOffset "EndOffset" true
    itemToWriter w b.Path "Path" true
    itemToWriter w (int b.Ordinal) "Ordinal" true
    itemToWriter w b.Hits "Hits" (b.Id > 0)
    if b.Id > 0 then
      itemToWriter w b.Id "Id" false

    timesToWriter w b.Times
    tracksToWriter w b.Tracks
    w.Builder.AppendLine().Append(slugs.[11]).Append("}") |> ignore

  let private seqpntToWriter (w:BuildWriter) (s:SeqPnt) =
    w.Builder.Append(slugs.[11]).AppendLine("{") |> ignore
    itemToWriter w s.VC "VC" true
    itemToWriter w s.SL "SL" true
    itemToWriter w s.SC "SC" true
    itemToWriter w s.EL "EL" true
    itemToWriter w s.EC "EC" true
    itemToWriter w s.Offset "Offset" true
    itemToWriter w s.Id "Id" false
    timesToWriter w s.Times
    tracksToWriter w s.Tracks
    w.Builder.AppendLine().Append(slugs.[11]).Append("}") |> ignore

  [<SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidMessageChainsRule",
    Justification = "Fluent interface")>]
  let private methodToWriter (w:BuildWriter) (method:Method) =
    w.Builder.Append(slugs.[9]).AppendLine("\"Lines\": {") |> ignore
    if method.Lines.IsNotNull && method.Lines.Count > 0
    then
      let mutable first = true
      method.Lines // TODO extract
      |> Seq.iter (fun kvp ->
        if not first
        then w.Builder.AppendLine(",") |> ignore
        first <- false
        lineToWriter w kvp |> ignore
      )
      w.Builder.AppendLine()
        .Append(slugs.[10]) |> ignore
    w.Builder.AppendLine("},") |> ignore

    // After Lines, now Branches

    w.Builder
      .Append(slugs.[9]).Append("\"Branches\": [") |> ignore
    if method.Branches.IsNotNull && method.Branches.Count > 0
    then
      let mutable first = true
      w.Builder.AppendLine() |> ignore
      method.Branches  // TODO extract
      |> Seq.iter (fun b ->
        if not first
        then w.Builder.AppendLine(",") |> ignore
        first <- false
        branchToWriter w b
      )
      w.Builder.AppendLine().Append(slugs.[10]).Append("]") |> ignore
    else
      w.Builder.Append(']') |> ignore

    // After Branches, now SeqPnts

    if method.SeqPnts.IsNotNull && method.SeqPnts.Count > 0
    then
      w.Builder.AppendLine(",")
        .Append(slugs.[9])
        .AppendLine("\"SeqPnts\": [") |> ignore

      let mutable first = true
      method.SeqPnts
      |> Seq.iter (fun s ->
        if not first
        then w.Builder.AppendLine(",") |> ignore
        first <- false
        seqpntToWriter w s
      )
      w.Builder.AppendLine().Append(slugs.[10]).Append("]") |> ignore

    // After SeqPnts, now Tracking

    if method.TId.HasValue
    then
      w.Builder.AppendLine(",").Append(slugs.[9]).Append("\"TId\": ")
        .Append(method.TId.Value.ToString(CultureInfo.InvariantCulture))
      |> ignore

      w.Builder.AppendLine(",").Append(slugs.[9]).Append("\"Entry\": [") |> ignore
      let mutable firstTime = true
      if method.Entry.IsNotNull && method.Entry.Count > 0
      then
        method.Entry
        |> Seq.iter (fun t ->
          timeToWriter
               (if firstTime
                then firstTime <- false
                     w.Builder.AppendLine()
                else w.Builder.AppendLine(",")) 11 t
        )
        w.Builder.AppendLine().Append(slugs.[10]) |> ignore
      w.Builder.Append("]") |> ignore

      w.Builder.AppendLine(",").Append(slugs.[9]).Append("\"Exit\": [") |> ignore
      let mutable firstTime = true
      if method.Exit.IsNotNull && method.Exit.Count > 0
      then
        method.Exit
        |> Seq.iter (fun t ->
          timeToWriter
               (if firstTime
                then firstTime <- false
                     w.Builder.AppendLine()
                else w.Builder.AppendLine(",")) 11 t
        )
        w.Builder.AppendLine().Append(slugs.[10]) |> ignore
      w.Builder.AppendLine("]") |> ignore
    else w.Builder.AppendLine() |> ignore
    w

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let private methodsToWriter (w:BuildWriter) (methods:Methods) =
    (dictionaryToWriter 7 methodToWriter w methods)

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let private classesToWriter (w:BuildWriter) (classes:Classes) =
    (dictionaryToWriter 5 methodsToWriter w classes)

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let private documentsToWriter (w:BuildWriter) (documents:Documents) =
    (dictionaryToWriter 3 classesToWriter w documents)

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let private modulesToWriter (w:BuildWriter) (report:Modules) =
    (dictionaryToWriter 1 documentsToWriter w report)

  [<SuppressMessage(
    "Gendarme.Rules.Correctness", "EnsureLocalDisposalRule",
    Justification = "Is this a bug?")>]
  let internal toText (report:Modules) =
    use w = new BuildWriter()
    w.Builder.AppendLine("{") |> ignore
    (modulesToWriter w report)
      .Builder.AppendLine("}") |> ignore
    let result = w.Clear().ToString()
    result

  let internal serializeToUtf8Bytes (document:Modules) =
    document
    |> toText
    |> System.Text.Encoding.UTF8.GetBytes

#endif

#if GUI
  // Conversion to XML ---------------------------------------------------------

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal methodsToXml (fileId:int) (item:XElement) (methods:Methods) =
    methods
    |> Seq.iter (fun kvp ->
      let m = XElement(XName.Get "Method",
                       XAttribute(XName.Get "visited", false),
                       XAttribute(XName.Get "cyclomaticComplexity", 0),
                       XAttribute(XName.Get "sequenceCoverage", 0),
                       XAttribute(XName.Get "branchCoverage", 0),
                       XAttribute(XName.Get "isConstructor", false),
                       XAttribute(XName.Get "isStatic", false),
                       XAttribute(XName.Get "isGetter", false),
                       XAttribute(XName.Get "isSetter", false))
      item.Add m
      let md = XElement(XName.Get "MetadataToken")
      md.Value <- "0"
      m.Add md
      let n = XElement(XName.Get "Name")
      n.Value <- kvp.Key
      m.Add n
      let f = XElement(XName.Get "FileRef",
                       XAttribute(XName.Get "uid", fileId))
      m.Add f

      let sp = XElement(XName.Get "SequencePoints")
      m.Add sp
      let bp = XElement(XName.Get "BranchPoints")
      m.Add bp
      let value = kvp.Value
      if value.Branches.IsNotNull
      then
        value.Branches
        |> Seq.iter (fun b ->
          let bx = XElement(XName.Get "BranchPoint",
                       XAttribute(XName.Get "vc", b.Hits),
                       XAttribute(XName.Get "sl", b.Line),
                       XAttribute(XName.Get "uspid", b.Id),
                       XAttribute(XName.Get "ordinal", b.Ordinal),
                       XAttribute(XName.Get "offset", b.Offset),
                       XAttribute(XName.Get "path", b.Path))
          bp.Add bx
        )

      if value.SeqPnts.IsNotNull
      then
        value.SeqPnts
        |> Seq.iter(fun s ->
          let sx = XElement(XName.Get "SequencePoint",
                       XAttribute(XName.Get "vc", s.VC),
                       XAttribute(XName.Get "offset", s.Offset),
                       XAttribute(XName.Get "sl", s.SL),
                       XAttribute(XName.Get "sc", s.SC),
                       XAttribute(XName.Get "el", s.EL),
                       XAttribute(XName.Get "ec", s.EC))
          sp.Add sx
        )
      else
        value.Lines
        |> Seq.iteri (fun i l ->
          let k = l.Key
          let sx = XElement(XName.Get "SequencePoint",
                       XAttribute(XName.Get "vc", l.Value),
                       XAttribute(XName.Get "offset", i),
                       XAttribute(XName.Get "sl", k),
                       XAttribute(XName.Get "sc", 1),
                       XAttribute(XName.Get "el", k),
                       XAttribute(XName.Get "ec", 2))
          sp.Add sx
        )

    )

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal classesToXml (fileId:int)
    (table:Dictionary<string, XElement>) (classes:Classes) =
    classes
    |> Seq.iteri (fun i kvp ->
      let name = kvp.Key
      let b,i = table.TryGetValue name
      let item = if b
                 then i
                 else
                   let i2 = XElement(XName.Get "Class")
                   let n = XElement(XName.Get "FullName")
                   n.Value <- name
                   i2.Add n
                   table.Add(name, i2)
                   let m = XElement(XName.Get "Methods")
                   i2.Add m
                   i2
      let next = item.Elements(XName.Get "Methods") |> Seq.head
      methodsToXml fileId next kvp.Value
    )

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal documentsToXml (indexTable:Dictionary<string, int>)
    (key:string) (documents:Documents) =
    let m = XElement(XName.Get "Module",
                     XAttribute(XName.Get "hash", key))
    let p = XElement(XName.Get "ModulePath")
    p.Value <- key
    m.Add p
    let n = XElement(XName.Get "ModuleName")
    n.Value <- (key |> Path.GetFileNameWithoutExtension)
    m.Add n
    let files = XElement(XName.Get "Files")
    m.Add files
    let classes = XElement(XName.Get "Classes")
    m.Add classes
    let classTable = Dictionary<string, XElement>()
    documents
    |> Seq.iter (fun kvp ->
      let name = kvp.Key
      let ok,index = indexTable.TryGetValue name
      let i = if ok
              then index
              else
                let n = 1 + indexTable.Count
                indexTable.Add(name, n)
                n
      let item = XElement(XName.Get "File",
                          XAttribute(XName.Get "uid", i),
                          XAttribute(XName.Get "fullPath", name))
      files.Add item
      classesToXml i classTable kvp.Value
      )

    classTable
    |> Seq.iter (fun kvp ->
      classes.Add kvp.Value
    )
    m

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal jsonToXml (modules:Modules) =
    let x = XDocument()
    x.Add(XElement(XName.Get "CoverageSession"))
    let mroot = XElement(XName.Get "Modules")
    x.Root.Add mroot
    let fileRefs = Dictionary<string, int>()
    modules
    |> Seq.iter (fun kvp ->
      documentsToXml fileRefs kvp.Key kvp.Value
      |> mroot.Add
    )
    x

  let internal fileToXml filename =
    filename
    |> File.ReadAllText
    |> fromJsonText
    |> jsonToXml

#endif

#if RUNNER
  // Instrumentation ---------------------------------------------------------

  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
  type internal JsonContext =
    {
      Documents: Documents
      Type : TypeDefinition
      VisibleType : TypeDefinition
      Method : MethodDefinition
      VisibleMethod : MethodDefinition
      Track : (int * string) option
    }
    static member Build() =
      {
        Documents = null
        Type = null
        VisibleType = null
        Method = null
        VisibleMethod = null
        Track = None
      }

  let internal reportGenerator () =
    let document = Modules()

    let startVisit = id
    let visitModule s (m:ModuleEntry) =
      let documents = Documents()
      document.Add(m.Module.FileName |> Path.GetFileName, documents)
      { s with Documents = documents }
    let visitType (s : JsonContext) (m:TypeEntry) =
      { s with Type = m.Type
               VisibleType = m.VisibleType }
    let visitMethod (s : JsonContext) (m:MethodEntry) =
      { s with Method = m.Method
               VisibleMethod = m.VisibleMethod
               Track = m.Track }

    let getMethodRecord (s : JsonContext) (doc:string) =
      let visibleMethodName = s.VisibleMethod.FullName
      let visibleTypeName = s.VisibleMethod.DeclaringType.FullName
      let classes = match s.Documents.TryGetValue doc with
                    | true, c -> c
                    | _ -> let c = Classes()
                           s.Documents.Add(doc, c)
                           c
      let methods = match classes.TryGetValue visibleTypeName with
                    | true, m -> m
                    | _ -> let m = Methods()
                           classes.Add(visibleTypeName, m)
                           m
      match methods.TryGetValue visibleMethodName with
      | true, m -> m
      | _ -> let m = Method.Create(s.Track)
             methods.Add(visibleMethodName, m)
             m

    let visitMethodPoint (s : JsonContext) (e:StatementEntry) =
      if e.Interesting then
        e.SeqPnt
        |> Option.iter (fun codeSegment ->
          let doc = codeSegment.Document |> Visitor.sourceLinkMapping
          let mplus = getMethodRecord s doc
          mplus.Lines.[codeSegment.StartLine] <- int e.DefaultVisitCount
          mplus.SeqPnts.Add {
            VC = int e.DefaultVisitCount
            SL = codeSegment.StartLine
            SC = codeSegment.StartColumn
            EL = codeSegment.EndLine
            EC = codeSegment.EndColumn
            Offset = codeSegment.Offset
            Id = e.Uid
            Times = null
            Tracks = null}
        )
      s
    let visitBranchPoint  (s : JsonContext) (b:GoTo) =
      if b.Included then
        let doc = b.SequencePoint.Document.Url |> Visitor.sourceLinkMapping
        let mplus = getMethodRecord s doc
        mplus.Branches.Add {
                              Line = b.SequencePoint.StartLine
                              Offset = b.Offset
                              EndOffset = b.Target.Head.Offset
                              Path = mplus.Branches
                                     |> Seq.filter (fun k -> k.Offset = b.Offset)
                                     |> Seq.length
                              Ordinal = uint mplus.Branches.Count
                              Hits = int b.VisitCount
                            // scope to expand
                              Id= b.Uid
                              Times = null
                              Tracks = null
                           }
      s

    let visitAfterMethod (s : JsonContext) _ =
      { s with Method = null
               VisibleMethod = null
               Track = None }
    let visitAfterType (s : JsonContext) =
      { s with Type = null
               VisibleType = null }

    let visitAfterModule s =
      { s with Documents = null }
//    let afterAll = id

    let reportVisitor (s : JsonContext) (node : Node) =
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
//      | Finish -> afterAll s
      | _ -> s

    let result = Visitor.encloseState reportVisitor (JsonContext.Build())
    (result, fun (s:System.IO.Stream) -> let encoded = serializeToUtf8Bytes document
                                         s.Write(encoded, 0, encoded.Length))
#endif

#if GUI || RUNNER
  // FxCop ---------------------------------------------------------
#if GUI
[<assembly: SuppressMessage("Microsoft.Performance", "CA1810:InitializeReferenceTypeStaticFieldsInline", Scope="member",
  Target="<StartupCode$AltCover-UICommon>.$NativeJson.#.cctor()",
  Justification="Compiler Generated")>]
#endif

[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+Method.#Branches",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+Method.#SeqPnts",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+BranchInfo.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.UInt32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+BranchInfo.#Times",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+BranchInfo.#Tracks",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,System.Nullable`1<System.Int32>,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.String>)",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+SeqPnt.#Times",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+SeqPnt.#Tracks",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+Method.#Entry",
  Justification="Harmless in context")>]
[<assembly: SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope="member",
  Target="AltCover.NativeJson+Method.#Exit",
  Justification="Harmless in context")>]

[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.NativeJson+Method.#SeqPnts",
  MessageId="Pnts", Justification="Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,System.Nullable`1<System.Int32>,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.String>)",
  MessageId="t", Justification="Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.NativeJson+Method.#.ctor(System.Collections.Generic.SortedDictionary`2<System.Int32,System.Int32>,System.Collections.Generic.List`1<AltCover.NativeJson+BranchInfo>,System.Collections.Generic.List`1<AltCover.NativeJson+SeqPnt>,System.Nullable`1<System.Int32>,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.String>)",
  MessageId="Pnts", Justification="Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="type",
  Target="AltCover.NativeJson+SeqPnt",
  MessageId="Pnt", Justification="Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
  MessageId="e", Justification="Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
  MessageId="s", Justification="Smaller JSON")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.NativeJson+SeqPnt.#.ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Collections.Generic.List`1<System.String>,System.Collections.Generic.List`1<System.Int32>)",
  MessageId="v", Justification="Smaller JSON")>]
()
#endif