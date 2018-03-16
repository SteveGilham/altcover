namespace AltCover.Shadow

open System.Collections.Generic
open AltCover.Recorder

module Adapter =
  let VisitsClear () = Instance.Visits.Clear()
  let VisitsAdd name line number =
    if name |> Instance.Visits.ContainsKey |> not then
        let entry = Dictionary<int, int * (int64 option * int option) list>()
        Instance.Visits.Add(name, entry)

    Instance.Visits.[name].Add(line, (number, []))

  let VisitsSeq() = Instance.Visits |> Seq.cast<obj>
  let VisitsEntrySeq key = Instance.Visits.[key] |> Seq.cast<obj>
  let VisitCount key key2 = Instance.Visits.[key].[key2] |> fst
  let Lock = Instance.Visits :> obj

  let VisitImplNone moduleId hitPointId = Instance.VisitImpl moduleId hitPointId (None,None)
