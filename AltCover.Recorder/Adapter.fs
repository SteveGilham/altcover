namespace AltCover.Recorder

open System.Collections.Generic

#if DEBUG
[<RequireQualifiedAccess>]
module Adapter =
  let DoPause() = Instance.I.doPause
  let DoResume() = Instance.I.doResume
  let DoUnload() = Instance.I.doUnload
  let DoExit() = Instance.I.doExit
  let VisitsClear() = Instance.I.clear()
  let SamplesClear() = Instance.I.samples.Clear()

  let Reset() =
    Instance.I.isRunner <- false
    Instance.I.clear()
    Instance.I.samples.Clear()

  let internal prepareName name =
    if name
       |> Instance.I.visits.ContainsKey
       |> not
    then
      let entry = Dictionary<int, PointVisit>()
      Instance.I.visits.Add(name, entry)

  let internal Init (n, l) = let tmp = { PointVisit.Create() with Count = n }
                             tmp.Tracks.AddRange l
                             tmp

  //let internal VisitSelection (a, b, c) = Instance.I.visitSelection a b c
  //let Visit (a,b) = Instance.Visit a b
  let VisitsAdd (name, line, number) =
    prepareName name
    let v = Init (number, [])
    Instance.I.visits.[name].Add(line, v)

  let VisitsAddTrack (name, line, number) =
    prepareName name
    let v1 =
      Init (number,
        [ Call 17
          Call 42 ])
    Instance.I.visits.[name].Add(line, v1)

    let v2 =
      Init ((number + 1L),
        [ Time 17L
          Both
            { Time = 42L
              Call = 23 } ])
    Instance.I.visits.[name].Add(line + 1, v2)

  let VisitsSeq() = Instance.I.visits |> Seq.cast<obj>
  let VisitsEntrySeq key = Instance.I.visits.[key] |> Seq.cast<obj>
  let VisitCount (key, key2) = (Instance.I.visits.[key].[key2]).Count
  let Lock = Instance.I.visits :> obj

  let VisitImplNone (moduleId, hitPointId) =
    Instance.I.visitImpl moduleId hitPointId Track.Null
  let VisitImplMethod (moduleId, hitPointId, mId) =
    Instance.I.visitImpl moduleId hitPointId (Call mId)
  //let internal VisitImpl (a, b, c) =
  //  Instance.I.visitImpl a b c

  let AddSample (moduleId, hitPointId) =
    Instance.I.takeSample Sampling.Single moduleId hitPointId
  let AddSampleUnconditional (moduleId, hitPointId) =
    Instance.I.takeSample Sampling.All moduleId hitPointId

  let internal NewBoth (time, call) =
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

  let internal DoFlush (visits, format, report, output) =
    let output' = //if System.String.IsNullOrEmpty output
      //then None // this case gets tested elsewhere
      (*else*) Some output
    Counter.doFlush ignore (fun _ _ -> ()) true visits format report output'

  let internal UpdateReport (counts, format, coverageFile, outputFile) =
    Counter.I.updateReport ignore (fun _ _ -> ()) true counts format coverageFile
      outputFile
  let internal PayloadSelector x = Instance.I.payloadSelector(fun _ -> x)
  let internal PayloadControl (x, y) = Instance.I.payloadControl (fun _ -> x) (fun _ -> y)
  let internal PayloadSelection (x, y, z) =
    Instance.I.payloadSelection (fun _ -> x) (fun _ -> y) (fun _ -> z)

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

  let internal InvokeIssue71Wrapper<'T when 'T :> System.Exception> ((unique : string), (called : bool array)) =
    let constructor = typeof<'T>.GetConstructor([| typeof<System.String> |])
    let pitcher =
      fun _ _ _ _ -> constructor.Invoke([| unique |]) :?> System.Exception |> raise

    let catcher =
      fun _ _ _ (x : System.Exception) ->
        called.[0] <- true
        called.[1] <- match x with
                      | :? System.ArgumentNullException as ane -> ane.ParamName = unique
                      | _ -> x.Message = unique

    Instance.I.issue71Wrapper () () () () catcher pitcher

  let internal tracePush (a, b, c) =
    Instance.I.trace.Push a b c
  //let LogException (a, b, c, d) = Instance.I.logException a b c d
  //let FindIndexFromUspid (a,b) = Counter.I.findIndexFromUspid a b
#endif