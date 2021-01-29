namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Text.Json

module NativeJson =
  let options =
    let o = JsonSerializerOptions()
    o.WriteIndented <- true
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
      Times: Times
      Tracks: Tracks
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
      Times: Times
      Tracks: Tracks
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
      TId:int // tracking ID
    }

  type internal Methods = Dictionary<string, Method>
  type internal Classes = Dictionary<string, Methods>
  type internal Documents = Dictionary<string, Classes>
  type internal Modules = Dictionary<string, Documents> // <= serialize this

  type internal JsonContext =
    {
      placeholder:int
    }
    static member Build() =
      {
        placeholder = 0
      }

  let internal reportGenerator () =
    let document = Modules()

    let startVisit = id
    let visitModule s _ = s
    let visitType s _ = s
    let visitMethod s _ _ = s
    let visitMethodPoint s _ = s
    let visitBranchPoint s _ = s
    let visitAfterMethod s _ = s
    let visitAfterType = id
    let visitAfterModule = id
    let afterAll = id

    let reportVisitor (s : JsonContext) (node : Node) =
      match node with
      | Start _ -> startVisit s
      | Node.Module m -> visitModule s m
      | Node.Type t -> visitType s t
      | Node.Method m -> visitMethod s m.Method m.Inspection
      | MethodPoint m -> visitMethodPoint s m
      | BranchPoint b -> visitBranchPoint s b
      | AfterMethod m ->
          visitAfterMethod s m
      | AfterType _ -> visitAfterType s
      | AfterModule _ -> visitAfterModule s
      | Finish -> afterAll s
      | _ -> s

    let result = Visitor.encloseState reportVisitor (JsonContext.Build())
    (result, fun (s:System.IO.Stream) -> let encoded = JsonSerializer.Serialize(document, options)
                                         use writer = new System.IO.StreamWriter(s)
                                         writer.Write(encoded))