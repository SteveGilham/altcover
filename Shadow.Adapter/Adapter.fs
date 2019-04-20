namespace AltCover.Shadow

open System.Collections.Generic
open AltCover.Recorder

module Adapter =
  let DoPause() = Instance.DoPause null
  let DoResume() = Instance.DoResume null
  let VisitsClear() = Instance.Visits.[0].Clear()
  let SamplesClear() = Instance.Samples.Clear()
  let FlushAll() = Instance.FlushAll ProcessExit

  let internal prepareName name =
    if name
       |> Instance.Visits.[0].ContainsKey
       |> not
    then
      let entry = Dictionary<int, PointVisit>()
      Instance.Visits.[0].Add(name, entry)

  let VisitsAdd name line number =
    prepareName name
    let v = PointVisit.Init number []
    Instance.Visits.[0].[name].Add(line, v)

  let VisitsAddTrack name line number =
    prepareName name
    let v1 = PointVisit.Init number [ Call 17
                                      Call 42 ]
    Instance.Visits.[0].[name].Add(line, v1)

    let v2 = PointVisit.Init (number + 1) [ Time 17L
                                            Both(42L, 23) ]
    Instance.Visits.[0].[name].Add(line + 1, v2)

  let VisitsSeq() = Instance.Visits.[0] |> Seq.cast<obj>
  let VisitsEntrySeq key = Instance.Visits.[0].[key] |> Seq.cast<obj>
  let VisitCount key key2 = (Instance.Visits.[0].[key].[key2]).Count
  let VisitTracks key key2 = (Instance.Visits.[0].[key].[key2]).Tracks |> Seq.cast<obj>
  let VisitTrack key key2 key3 =
    let raw = (Instance.Visits.[0].[key].[key2]).Tracks.[key3]
    match raw with
    | Null -> [| int64 Tag.Null |]
    | Call t -> [| int64 Tag.Call; int64 t |]
    | Time t -> [| int64 Tag.Time; t |]
    | Both (t, t2) -> [| int64 Tag.Both; t; int64 t2 |]
    | Table _ -> [| int64 Tag.Table |]
  let Lock = Instance.Visits :> obj

  let VisitImplNone moduleId hitPointId =
    Instance.VisitImpl moduleId hitPointId Track.Null
  let VisitImplMethod moduleId hitPointId mId =
    Instance.VisitImpl moduleId hitPointId (Call mId)

  let AddSample moduleId hitPointId =
    Instance.TakeSample Sampling.Single moduleId hitPointId
  let internal NewBoth time track = Both(time, track)