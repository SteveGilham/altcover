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

  type internal Methods = Dictionary<string, Method>
  type internal Classes = Dictionary<string, Methods>
  type internal Documents = Dictionary<string, Classes>
  type internal Modules = Dictionary<string, Documents> // <= serialize this

  type internal JsonContext =
    {
      Documents: Documents
      Method : MethodDefinition
      VisibleMethod : MethodDefinition
    }
    static member Build() =
      {
        Documents = null
        Method = null
        VisibleMethod = null
      }

  let internal reportGenerator () =
    let document = Modules()

    let startVisit = id
    let visitModule s (m:ModuleEntry) =
      let documents = Documents()
      document.Add(m.Module.FileName |> Path.GetFileName, documents)
      { s with Documents = documents }

//    let visitType s _ = s
    let visitMethod (s : JsonContext) (m:MethodEntry) =
      { s with Method = m.Method
               VisibleMethod = m.VisibleMethod }

    let visitMethodPoint s _ = s
    let visitBranchPoint s _ = s

    let visitAfterMethod (s : JsonContext) _ =
      { s with Method = null
               VisibleMethod = null }
//    let visitAfterType = id
    let visitAfterModule s =
      { s with Documents = null }
//    let afterAll = id

    let reportVisitor (s : JsonContext) (node : Node) =
      match node with
      | Start _ -> startVisit s
      | Node.Module m -> visitModule s m
//      | Node.Type t -> visitType s t
      | Node.Method m -> visitMethod s m
      | MethodPoint m -> visitMethodPoint s m
      | BranchPoint b -> visitBranchPoint s b
      | AfterMethod m ->
          visitAfterMethod s m
//      | AfterType _ -> visitAfterType s
      | AfterModule _ -> visitAfterModule s
//      | Finish -> afterAll s
      | _ -> s

    let result = Visitor.encloseState reportVisitor (JsonContext.Build())
    (result, fun (s:System.IO.Stream) -> let encoded = JsonSerializer.Serialize(document, options)
                                                       |> System.Text.Encoding.UTF8.GetBytes
                                         s.Write(encoded, 0, encoded.Length))