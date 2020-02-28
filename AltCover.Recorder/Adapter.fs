namespace AltCover.Recorder

open System.Collections.Generic

#if DEBUG
module Adapter =
  let DoPause() = Instance.DoPause
  let DoResume() = Instance.DoResume
  let DoUnload() = Instance.DoUnload
  let DoExit() = Instance.DoExit
  let VisitsClear() = Instance.Clear()
  let SamplesClear() = Instance.Samples.Clear()
  let FlushAll() = Instance.FlushFinish()

  let Reset() =
    Instance.IsRunner <- false
    Instance.Clear()
    Instance.Samples.Clear()

  let internal prepareName name =
    if name
       |> Instance.Visits.ContainsKey
       |> not
    then
      let entry = Dictionary<int, PointVisit>()
      Instance.Visits.Add(name, entry)

  let internal Init n l = let tmp = { PointVisit.Create() with Count = n }
                          tmp.Tracks.AddRange l
                          tmp

  let VisitsAdd name line number =
    prepareName name
    let v = Init number []
    Instance.Visits.[name].Add(line, v)

  let VisitsAddTrack name line number =
    prepareName name
    let v1 =
      Init number
        [ Call 17
          Call 42 ]
    Instance.Visits.[name].Add(line, v1)

    let v2 =
      Init (number + 1L)
        [ Time 17L
          Both
            { Time = 42L
              Call = 23 } ]
    Instance.Visits.[name].Add(line + 1, v2)

  let VisitsSeq() = Instance.Visits |> Seq.cast<obj>
  let VisitsEntrySeq key = Instance.Visits.[key] |> Seq.cast<obj>
  let VisitCount key key2 = (Instance.Visits.[key].[key2]).Count
  let Lock = Instance.Visits :> obj

  let VisitImplNone moduleId hitPointId =
    Instance.VisitImpl moduleId hitPointId Track.Null
  let VisitImplMethod moduleId hitPointId mId =
    Instance.VisitImpl moduleId hitPointId (Call mId)

  let AddSample moduleId hitPointId =
    Instance.TakeSample Sampling.Single moduleId hitPointId
  let AddSampleUnconditional moduleId hitPointId =
    Instance.TakeSample Sampling.All moduleId hitPointId

  let internal NewBoth time call =
    Both
      { Time = time
        Call = call }

  let internal Call track = Call track
  let internal Time at = Time at

  let internal untime at =
    let r = List<System.Int64>()
    match at with
    | Time t -> r.Add(t)
    | _ -> ()
    r

  let internal Null() = Null
  let internal Table t = Table t

  let internal untable t =
    let r = List<System.Object>()
    match t with
    | (n, p, Table d) ->
        r.Add(n)
        r.Add(p)
        r.Add(d)
    | _ -> ()
    r

  let internal DoFlush visits format report output =
    let output' = //if System.String.IsNullOrEmpty output
      //then None // this case gets tested elsewhere
      (*else*) Some output
    Counter.DoFlush ignore (fun _ _ -> ()) true visits format report output'

  let internal UpdateReport counts format coverageFile outputFile =
    Counter.UpdateReport ignore (fun _ _ -> ()) true counts format coverageFile
      outputFile
  let internal PayloadSelector x = Instance.PayloadSelector(fun _ -> x)
  let internal PayloadControl x y = Instance.PayloadControl (fun _ -> x) (fun _ -> y)
  let internal PayloadSelection x y z =
    Instance.PayloadSelection (fun _ -> x) (fun _ -> y) (fun _ -> z)

  let internal MakeNullTrace name =
    { Tracer = name
      Stream = null
      Formatter = null
      Runner = false
      Definitive = false }

  let internal MakeStreamTrace s1 =
    { Tracer = null
      Stream = new System.IO.MemoryStream()
      Formatter = new System.IO.BinaryWriter(s1)
      Runner = true
      Definitive = false }

  let internal InvokeIssue71Wrapper<'T when 'T :> System.Exception> (unique : string)
      (called : bool array) =
    let constructor = typeof<'T>.GetConstructor([| typeof<System.String> |])
    let pitcher =
      fun _ _ _ _ -> constructor.Invoke([| unique |]) :?> System.Exception |> raise

    let catcher =
      fun _ _ _ (x : System.Exception) ->
        called.[0] <- true
        called.[1] <- match x with
                      | :? System.ArgumentNullException as ane -> ane.ParamName = unique
                      | _ -> x.Message = unique

    Instance.Issue71Wrapper () () () () catcher pitcher

#endif