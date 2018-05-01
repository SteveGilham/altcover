namespace AltCover.Shadow

open System.Collections.Generic
open AltCover.Recorder

module Adapter =
  let DoPause () =
    Instance.DoPause null

  let DoResume () =
    Instance.DoResume null

  let VisitsClear () = Instance.Visits.Clear()

  let internal prepareName name =
    if name |> Instance.Visits.ContainsKey |> not then
        let entry = Dictionary<int, int * Track list>()
        Instance.Visits.Add(name, entry)

  let VisitsAdd name line number =
    prepareName name
    Instance.Visits.[name].Add(line, (number, []))

  let VisitsAddTrack name line number =
    prepareName name

    Instance.Visits.[name].Add(line, (number, [Call 17; Call 42]))

  let VisitsSeq() = Instance.Visits |> Seq.cast<obj>
  let VisitsEntrySeq key = Instance.Visits.[key] |> Seq.cast<obj>
  let VisitCount key key2 = Instance.Visits.[key].[key2] |> fst
  let Lock = Instance.Visits :> obj

  let VisitImplNone moduleId hitPointId = Instance.VisitImpl moduleId hitPointId Track.Null
  let VisitImplMethod moduleId hitPointId mId = Instance.VisitImpl moduleId hitPointId (Call mId)