namespace AltCover.Recorder

open System.Collections.Generic

#if DEBUG
[<RequireQualifiedAccess>]
module Adapter =
  let DoPause () = Instance.I.doPause
  let DoResume () = Instance.I.doResume
  let DoUnload () = Instance.I.doUnload
  let DoExit () = Instance.I.doExit

  let VisitsClear () =
    Instance.I.clear ()
    Counter.branchVisits <- 0L
    Counter.totalVisits <- 0L

  let SamplesClear () = Instance.I.samples.Clear()

  let Reset () =
    Instance.I.isRunner <- false
    VisitsClear()
    SamplesClear()

  let internal prepareName name =
    if name |> Instance.I.visits.ContainsKey |> not then
      let entry = Dictionary<int, PointVisit>()
      Instance.I.visits.Add(name, entry)

  let internal init (n, l) =
    let tmp = { PointVisit.Create() with Count = n }
    tmp.Tracks.AddRange l
    tmp

  let VisitsAdd (name, line, number) =
    prepareName name
    let v = init(number, [])
    Instance.I.visits.[name].Add(line, v)

  let VisitsAddTrack (name, line, number) =
    prepareName name
    let v1 = init(number, [ Call 17; Call 42 ])
    Instance.I.visits.[name].Add(line, v1)

    let v2 =
      init(
        (number + 1L),
        [ Time 17L
          Both { Time = 42L; Call = 23 } ]
      )

    Instance.I.visits.[name].Add(line + 1, v2)

  let VisitsSeq () = Instance.I.visits |> Seq.cast<obj>

  let VisitsEntrySeq key =
    Instance.I.visits.[key] |> Seq.cast<obj>

  let VisitCount (key, key2) = (Instance.I.visits.[key].[key2]).Count
  let Lock = Instance.I.visits :> obj

  let VisitImplNone (moduleId, hitPointId) =
    Instance.I.visitImpl moduleId hitPointId Track.Null

  let VisitImplMethod (moduleId, hitPointId, mId) =
    Instance.I.visitImpl moduleId hitPointId (Call mId)
  //let internal VisitImpl (a, b, c) =
  //  Instance.I.visitImpl a b c

  let internal addSample (moduleId, hitPointId, context) =
    Instance.I.takeSample Sampling.Single moduleId hitPointId context

  let internal addSampleUnconditional (moduleId, hitPointId, context) =
    Instance.I.takeSample Sampling.All moduleId hitPointId context

  let internal newBoth (time, call) = Both { Time = time; Call = call }

  let internal asCall track = Call track
  let internal time at = Time at

  let internal untime at =
    let r = List<System.Int64>()

    match at with
    | Time t -> r.Add(t)
    | _ -> ()

    r

  let internal asNull () = Null
  let internal table t = Table t

  let internal untable t =
    let r = List<System.Object>()

    match t with
    | (n, p, Table d) ->
        r.Add(n)
        r.Add(p)
        r.Add(d)
    | _ -> ()

    r

  let internal doFlush (visits, format, report, output) =
    let output' =
      if System.String.IsNullOrEmpty output then
        None
      else
        Some output

    Counter.doFlushFile ignore (fun _ _ -> ()) true visits format report output'

  let internal updateReport (counts, format, coverageFile, outputFile) =
    Counter.I.updateReport
      ignore
      (fun _ _ -> ())
      true
      counts
      format
      coverageFile
      outputFile

  let internal payloadSelector x = Instance.I.payloadSelector (fun _ -> x)

  let internal payloadControl (x, y) =
    Instance.I.payloadControl (fun _ -> x) (fun _ -> y)

  let internal payloadSelection (x, y, z) =
    Instance.I.payloadSelection (fun _ -> x) (fun _ -> y) (fun _ -> z)

  let internal makeNullTrace name =
    { Tracer = name
      Stream = null
      Formatter = null
      Runner = false
      Definitive = false }

  let internal makeStreamTrace s1 =
    { Tracer = null
      Stream = new System.IO.MemoryStream()
      Formatter = new System.IO.BinaryWriter(s1)
      Runner = true
      Definitive = false }

  let internal invokeIssue71Wrapper<'T when 'T :> System.Exception>
    (
      (unique: string),
      (called: bool array)
    ) =
    let constructor =
      typeof<'T>.GetConstructor ([| typeof<System.String> |])

    let pitcher =
      fun _ _ _ _ ->
        constructor.Invoke([| unique |]) :?> System.Exception
        |> raise

    let catcher =
      fun _ _ _ (x: System.Exception) ->
        called.[0] <- true

        called.[1] <-
          match x with
          | :? System.ArgumentNullException as ane -> ane.ParamName = unique
          | _ -> x.Message = unique

    Instance.I.issue71Wrapper () () () () catcher pitcher
    
  let internal invokeCurriedIssue71Wrapper<'T when 'T :> System.Exception>
      (unique: string) =
    let constructor =
      typeof<'T>.GetConstructor ([| typeof<System.String> |])

    let pitcher =
      fun _ _ _ _ ->
        constructor.Invoke([| unique |]) :?> System.Exception
        |> raise

    Instance.I.curriedIssue71Wrapper "a" "b" "c" "d" pitcher

  let internal tracePush (a, b, c) = Instance.I.trace.Push a b c
//let LogException (a, b, c, d) = Instance.I.logException a b c d
//let FindIndexFromUspid (a,b) = Counter.I.findIndexFromUspid a b
#endif