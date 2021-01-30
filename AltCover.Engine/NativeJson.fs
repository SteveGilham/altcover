namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

open Mono.Cecil

module NativeJson =
  let options =
    let o = JsonSerializerOptions()
    o.WriteIndented <- true
    o.IgnoreNullValues <- true
    o

  type internal TimeStamp = string

  let FromTracking(l:int64) : TimeStamp =
    l
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
      From: string option
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
      From: string option
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

    let visitMethodPoint (s : JsonContext) (e:StatementEntry) =
      if e.Interesting then
        e.SeqPnt
        |> Option.iter (fun codeSegment ->
          let doc = codeSegment.Document |> Visitor.sourceLinkMapping
          let classes = match s.Documents.TryGetValue doc with
                        | true, c -> c
                        | _ -> let c = Classes()
                               s.Documents.Add(doc, c)
                               c
          let methods = match classes.TryGetValue s.VisibleType.FullName with
                        | true, m -> m
                        | _ -> let m = Methods()
                               classes.Add(s.VisibleType.FullName, m)
                               m
          let ``method`` = match methods.TryGetValue s.VisibleMethod.FullName with
                           | true, m -> m
                           | _ -> let m = Method.Create()
                                  methods.Add(s.VisibleMethod.FullName, m)
                                  m
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
            From=  if s.Method <> s.VisibleMethod
                   then Some s.Method.FullName
                   else None
            Times = None
            Tracks = None}
          methods.[s.VisibleMethod.FullName] <- mplus
        )
      s
    let visitBranchPoint s _ = s

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