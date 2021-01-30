namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

open Mono.Cecil

module NativeJson =
  let internal options =
    let o = JsonSerializerOptions()
    o.WriteIndented <- true
    o.IgnoreNullValues <- true
    o

  type internal TimeStamp = string

  let FromTracking(ticks:int64) : TimeStamp =
    ticks
    |> BitConverter.GetBytes
    |> Convert.ToBase64String

  type internal Times = List<TimeStamp>

  type internal Tracks = List<int>

  [<ExcludeFromCodeCoverage; NoComparison>]
  type internal SeqPnt =
    {
      VC:int
      SL:int
      SC:int
      EL:int
      EC:int
      Offset:int
      Id:int
      Times: Times option
      Tracks: Tracks option
    }
  type internal SeqPnts = List<SeqPnt>

  // Coverlet compatible -- src/coverlet.core/CoverageResult.cs
  // also round-trippable
  [<ExcludeFromCodeCoverage; NoComparison>]
  type internal BranchInfo =
    {
      Line:int
      Offset:int
      EndOffset:int
      Path:int
      Ordinal:uint
      Hits:int
    // scope to expand
      Id:int
      Times: Times option
      Tracks: Tracks option
    }

  type Lines = SortedDictionary<int, int>

  type internal Branches = List<BranchInfo>

  [<ExcludeFromCodeCoverage; NoComparison>]
  type internal Method =
    {
      Lines:Lines
      Branches:Branches
      // scope to expand
      SeqPnts:SeqPnts
      TId:int option // tracking ID
    }
    static member Create() =
      {
        Lines = null
        Branches = null
        SeqPnts = null
        TId = None
      }

  type internal Methods = Dictionary<string, Method>
  type internal Classes = Dictionary<string, Methods>
  type internal Documents = SortedDictionary<string, Classes>
  type internal Modules = SortedDictionary<string, Documents> // <= serialize this

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
      let documents = Documents(StringComparer.Ordinal)
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
      let visibleTypeName = s.VisibleType.FullName
      let visibleMethodName = s.VisibleMethod.FullName
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
      (methods, visibleMethodName,
        match methods.TryGetValue visibleMethodName with
        | true, m -> m
        | _ -> let m = Method.Create()
               methods.Add(visibleMethodName, m)
               m)

    let visitMethodPoint (s : JsonContext) (e:StatementEntry) =
      if e.Interesting then
        e.SeqPnt
        |> Option.iter (fun codeSegment ->
          let doc = codeSegment.Document |> Visitor.sourceLinkMapping
          let (methods, visibleMethodName, ``method``) = getMethodRecord s doc
          let mplus = { ``method`` with Lines = if isNull ``method``.Lines
                                                then Lines()
                                                else ``method``.Lines
                                        SeqPnts = if isNull ``method``.SeqPnts
                                                  then SeqPnts()
                                                  else ``method``.SeqPnts
                                        TId = if s.Track |> Option.isSome &&
                                                   Option.isNone ``method``.TId
                                              then Some (s.Track.Value |> fst)
                                              else None
                                         }
          mplus.Lines.[codeSegment.StartLine] <- int e.DefaultVisitCount
          mplus.SeqPnts.Add {
            VC = int e.DefaultVisitCount
            SL = codeSegment.StartLine
            SC = codeSegment.StartColumn
            EL = codeSegment.EndLine
            EC = codeSegment.EndColumn
            Offset = codeSegment.Offset
            Id = e.Uid
            Times = None
            Tracks = None}
          methods.[visibleMethodName] <- mplus
        )
      s
    let visitBranchPoint  (s : JsonContext) (b:GoTo) =
      if b.Included then
        let doc = b.SequencePoint.Document.Url |> Visitor.sourceLinkMapping
        let (methods, visibleMethodName, ``method``) = getMethodRecord s doc
        let mplus = { ``method`` with Branches = if isNull ``method``.Branches
                                                 then Branches()
                                                 else ``method``.Branches
                                      TId = if s.Track |> Option.isSome &&
                                                  Option.isNone ``method``.TId
                                            then Some (s.Track.Value |> fst)
                                            else None
                                        }
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
                              Times = None
                              Tracks = None
                           }
        methods.[visibleMethodName] <- mplus
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
    (result, fun (s:System.IO.Stream) -> let encoded = JsonSerializer.Serialize(document, options)
                                                       |> System.Text.Encoding.UTF8.GetBytes
                                         s.Write(encoded, 0, encoded.Length))