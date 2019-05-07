namespace AltCover.Shadow

open System.Collections.Generic
open AltCover.Recorder

module Adapter =
  let DoPause() = Instance.DoPause null
  let DoResume() = Instance.DoResume null
  let VisitsClear() = Instance.Visits.Clear()
  let SamplesClear() = Instance.Samples.Clear()
  let FlushAll() = Instance.FlushFinish ()
  let Reset () =
    Instance.IsRunner <- false
    Instance.Visits.Clear()
    Instance.Samples.Clear()

  let internal prepareName name =
    if name
       |> Instance.Visits.ContainsKey
       |> not
    then
      let entry = Dictionary<int, PointVisit>()
      Instance.Visits.Add(name, entry)

  let VisitsAdd name line number =
    prepareName name
    let v = PointVisit.Init number []
    Instance.Visits.[name].Add(line, v)

  let VisitsAddTrack name line number =
    prepareName name
    let v1 = PointVisit.Init number [ Call 17
                                      Call 42 ]
    Instance.Visits.[name].Add(line, v1)

    let v2 = PointVisit.Init (number + 1L) [ Time 17L
                                             Both(42L, 23) ]
    Instance.Visits.[name].Add(line + 1, v2)

  let VisitsSeq() = Instance.Visits |> Seq.cast<obj>
  let VisitsEntrySeq key = Instance.Visits.[key] |> Seq.cast<obj>
  let VisitCount key key2 = (Instance.Visits.[key].[key2]).Count
  let VisitTracks key key2 = (Instance.Visits.[key].[key2]).Tracks |> Seq.cast<obj>
  let VisitTrack key key2 key3 =
    let raw = (Instance.Visits.[key].[key2]).Tracks.[key3]
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
  let AddSampleUnconditional moduleId hitPointId =
    Instance.TakeSample Sampling.All moduleId hitPointId
  let internal NewBoth time track = Both(time, track)