namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Text.Json
#if GUI
open System.Linq
open System.Xml.Linq
#endif

#if RUNNER
open Mono.Cecil

[<AutoSerializable(false)>]
type internal DocumentType =
| XML of System.Xml.Linq.XDocument
| JSON of String
| Unknown
#endif

module NativeJson =
#if !GUI
  let internal options =
    let o = JsonSerializerOptions()
    o.WriteIndented <- true
    o.IgnoreNullValues <- true
    o
#endif

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
#if GUI
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
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

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
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

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
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

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
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
    |> JsonSerializer.Deserialize<Modules>
    |> jsonToXml

#endif

#if RUNNER
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
    (result, fun (s:System.IO.Stream) -> let encoded = JsonSerializer.SerializeToUtf8Bytes(document, options)
                                         s.Write(encoded, 0, encoded.Length))
#endif

#if GUI || RUNNER

  [<SuppressMessage("Gendarme.Rules.BadPractice",
                    "AvoidCallingProblematicMethodsRule",
                    Justification = "Not a lot of alteratives")>]
  [<SuppressMessage("Microsoft.Reliability", "CA2001:AvoidCallingProblematicMethods",
                    Justification = "Not a lot of alteratives")>]
  let internal assemblyResolve (_:Object) (args:ResolveEventArgs) =
    sprintf "AssemblyResolve Name %s from %A" args.Name args.RequestingAssembly
    |> System.Diagnostics.Debug.WriteLine
    let n = AssemblyName(args.Name)
    match AppDomain.CurrentDomain.GetAssemblies()
          |> Seq.tryFind(fun a -> a.GetName().Name = n.Name) with
    | Some a ->
      System.Diagnostics.Debug.WriteLine "Found loaded"
      a
    | _ ->
      let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let file = Path.Combine(here, n.Name) + ".dll"
      System.Diagnostics.Debug.WriteLine ("Looking for {0}", file)
      if File.Exists file
      then
        System.Diagnostics.Debug.WriteLine "Found file"
        file |> Assembly.LoadFile
      else
        System.Diagnostics.Debug.WriteLine "**FAILED**"
        //if args.Name.Contains(".resources, V") |> not
        //then eprintfn "AssemblyResolve Name %s from %A" args.Name args.RequestingAssembly
        null
#if RUNNER
  do
    AppDomain.CurrentDomain.add_AssemblyResolve <| ResolveEventHandler(assemblyResolve)
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