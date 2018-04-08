namespace AltCover

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Xml

open Mono.Cecil
open Mono.Options
open Augment

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
type Tracer = { Tracer : string }

type TypeBinder (``type``:Type) =
  inherit System.Runtime.Serialization.SerializationBinder()
  override self.BindToType (_:string, n:string) =
    match n with
    | both when both.StartsWith("System.Tuple`2[[System.Int64") -> typeof<(int64*int)>
    | t2 when t2.StartsWith("System.Tuple`2") -> ``type``
    | t3 when t3.StartsWith("System.Tuple`3") -> typeof<(string*int*Base.Track)>
    | "AltCover.Recorder.Track+Call"
    | "AltCover.Base.Track+Call"-> (Base.Track.Call 0).GetType()
    | "AltCover.Recorder.Track+Time"
    | "AltCover.Base.Track+Time" -> (Base.Track.Time 0L).GetType()
    | "AltCover.Recorder.Track+Both"
    | "AltCover.Base.Track+Both" -> (Base.Track.Both (0L, 0)).GetType()
    | _ -> typeof<Base.Track>

module Runner =

  let mutable internal recordingDirectory : Option<string> = None
  let mutable internal workingDirectory : Option<string> = None
  let mutable internal executable : Option<string> ref = ref None
  let mutable internal collect = false

  let internal DeclareOptions () =
    [ ("r|recorderDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome recordingDirectory then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--recorderDirectory") :: CommandLine.error

                    else
                      recordingDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "DirectoryNotFound",
                                                         "--recorderDirectory",
                                                         x) :: CommandLine.error ))
      ("w|workingDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome workingDirectory then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--workingDirectory") :: CommandLine.error

                    else
                      workingDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "DirectoryNotFound",
                                                         "--workingDirectory",
                                                         x) :: CommandLine.error ))
      ("x|executable=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome !executable then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--executable") :: CommandLine.error
                    else
                      executable := Some x
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "--executable",
                                                         x) :: CommandLine.error))
      ("collect",
       (fun _ ->  if collect then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--collect") :: CommandLine.error

                  else
                      collect <- true))
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))
      ("<>", (fun x -> CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "AltCover",
                                                         x) :: CommandLine.error))         ]// default end stop
      |> List.fold (fun (o:OptionSet) (p, a) -> o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a))) (OptionSet())

  let internal RequireExe (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right (l, options) -> match (!executable, collect) with
                            | (None, false)
                            | (Some _, true) ->
                               CommandLine.error <- (CommandLine.resources.GetString "executableRequired") ::
                                                     CommandLine.error
                               Left ("UsageError", options)
                            | (None, _) -> Right ([], options)
                            | (Some exe, _) -> Right (exe::l, options)
    | fail -> fail

  let internal RequireRecorder (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right (_, options) -> match recordingDirectory with
                            | None -> CommandLine.error <- (CommandLine.resources.GetString "recorderRequired") ::
                                                            CommandLine.error
                                      Left ("UsageError", options)
                            | Some path -> let dll = Path.Combine (path, "AltCover.Recorder.g.dll")
                                           if File.Exists dll then parse
                                           else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "recorderNotFound",
                                                         dll) :: CommandLine.error
                                                Left ("UsageError", options)
    | fail -> fail

  let internal RequireWorker (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right _ -> match workingDirectory with
                 | None -> workingDirectory <- Directory.GetCurrentDirectory() |> Some
                 | _ -> ()
                 parse
    | fail -> fail

  // mocking point
  let mutable internal RecorderName = "AltCover.Recorder.g.dll"

  let RecorderInstance () =
    let recorderPath = Path.Combine (Option.get recordingDirectory, RecorderName)
    let definition = AssemblyDefinition.ReadAssembly recorderPath
    definition.MainModule.GetType("AltCover.Recorder.Instance")

  let GetMethod (t:TypeDefinition) (name:string) =
    t.Methods
    |> Seq.filter (fun m -> m.Name = name)
    |> Seq.head

  let GetFirstOperandAsString (m:MethodDefinition) =
     m.Body.Instructions
     |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldstr)
     |> Seq.map (fun i -> i.Operand :?> string)
     |> Seq.head

  let GetFirstOperandAsNumber (m:MethodDefinition) =
     m.Body.Instructions
     |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldc_I4)
     |> Seq.map (fun i -> i.Operand :?> int)
     |> Seq.head

  let PayloadBase (rest:string list)  =
    CommandLine.doPathOperation (fun () ->
        CommandLine.ProcessTrailingArguments rest (DirectoryInfo(Option.get workingDirectory))) 255

  let WriteResource =
    CommandLine.resources.GetString >> Output.Info

  let WriteResourceWithFormatItems s x =
    String.Format (CultureInfo.CurrentCulture, s |> CommandLine.resources.GetString, x) |> Output.Info

  let internal SetRecordToFile report =
      let binpath = report + ".acv"
      use _stream = File.Create(binpath)
      ()

  let internal RunProcess report (payload: string list -> int) (args : string list) =
      SetRecordToFile report

      "Beginning run..." |> WriteResource
      let result = payload args
      "Getting results..." |> WriteResource
      result

  let internal CollectResults (hits:ICollection<(string*int*Base.Track)>) report =
      let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
      formatter.Binder <- TypeBinder(typeof<(string*int)>)

      Directory.GetFiles( Path.GetDirectoryName(report),
                          Path.GetFileName(report) + ".*.acv")
      |> Seq.iter (fun f ->
          sprintf "... %s" f |> Output.Info
          use results = new DeflateStream(File.OpenRead f, CompressionMode.Decompress)
          let rec sink() =
            let hit = try
                          let raw = formatter.Deserialize(results)
                          match raw with
                          | :? (string * int * Base.Track) as x -> x
                          | _ -> let pair = raw :?> (string * int)
                                 (fst pair, snd pair, Base.Null)
                      with
                      | :? System.InvalidCastException
                      | :? System.ArgumentException
                      | :? System.Runtime.Serialization.SerializationException -> (null, -1, Base.Null)
            let (key, _, _) = hit
            if key |> String.IsNullOrWhiteSpace  |> not then
              hit |> hits.Add
              sink()
          sink()
      )

      WriteResourceWithFormatItems "%d visits recorded" [|hits.Count|]

  let internal MonitorBase (hits:ICollection<(string*int*Base.Track)>) report (payload: string list -> int) (args : string list) =
      let result = if collect then 0 else RunProcess report payload args
      CollectResults hits report
      result

  let internal CopyFillMethodPoint (mp:XmlElement seq) sp =
    mp
    |> Seq.iter(fun m ->
        m.SetAttribute("type", "http://www.w3.org/2001/XMLSchema-instance", "SequencePoint") |> ignore
        sp
        |> Seq.cast<XmlElement>
        |> Seq.take 1
        |> Seq.collect (fun p -> p.Attributes |> Seq.cast<XmlAttribute>)
        |> Seq.iter (fun a -> m.SetAttribute(a.Name, a.Value)))

  let internal LookUpVisitsByToken token (dict:Dictionary<int, int * Base.Track list>) =
    let (ok, index) = Int32.TryParse(token,
                                        System.Globalization.NumberStyles.Integer,
                                        System.Globalization.CultureInfo.InvariantCulture)
    match dict.TryGetValue(if ok then index else -1) with
    | (false, _) -> (0, [])
    | (_, pair) -> pair

  let internal FillMethodPoint (mp:XmlElement seq) (``method``:XmlElement) (dict:Dictionary<int, int * Base.Track list>) =
    let token = ``method``.GetElementsByTagName("MetadataToken")
                |> Seq.cast<XmlElement>
                |> Seq.map(fun m -> m.InnerText)
                |> Seq.head
    let (vc0, l) = LookUpVisitsByToken token dict
    let vc = vc0 + (List.length l)

    mp
    |> Seq.iter (fun m -> m.SetAttribute("vc", vc.ToString(System.Globalization.CultureInfo.InvariantCulture))
                          m.SetAttribute("uspid", token)
                          m.SetAttribute("ordinal", "0")
                          m.SetAttribute("offset", "0"))

  let VisitCount nodes =
    nodes
    |> Seq.cast<XmlElement>
    |> Seq.filter(fun s -> Int32.TryParse( s.GetAttribute("vc") ,
                                        System.Globalization.NumberStyles.Integer,
                                        System.Globalization.CultureInfo.InvariantCulture) |> snd
                            <> 0)
    |> Seq.length

  let internal PostProcess (counts:Dictionary<string, Dictionary<int, int  * Base.Track list>>) format (document:XmlDocument) =
    match format with
    | Base.ReportFormat.OpenCoverWithTracking
    | Base.ReportFormat.OpenCover ->
        let percentCover visits points =
          if points = 0 then "0"
          else (sprintf "%.2f" ((float (visits * 100))/(float points))).TrimEnd([| '0' |]).TrimEnd([|'.'|])

        let setSummary (x:XmlElement) pointVisits branchVisits methodVisits classVisits ptcover brcover =
          x.GetElementsByTagName("Summary")
          |> Seq.cast<XmlElement>
          |> Seq.tryHead
          |> Option.iter(fun s -> s.SetAttribute("visitedSequencePoints", sprintf "%d" pointVisits)
                                  s.SetAttribute("visitedBranchPoints", sprintf "%d" branchVisits)
                                  s.SetAttribute("visitedMethods", sprintf "%d" methodVisits)
                                  classVisits
                                  |> Option.iter (fun cvc -> s.SetAttribute("visitedClasses", sprintf "%d" cvc))
                                  s.SetAttribute("branchCoverage", brcover)
                                  s.SetAttribute("sequenceCoverage", ptcover))

        let computeBranchExitCount (sp:XmlNodeList) bp =
          let interleave = Seq.concat [ sp |> Seq.cast<XmlElement>
                                        bp |> Seq.cast<XmlElement>]
                           |> Seq.sortBy (fun x -> x.GetAttribute("offset") |> Int32.TryParse |> snd)
          interleave
          |> Seq.fold(fun (bev, (sq:XmlElement)) x ->
                               match x.Name with
                               | "SequencePoint" -> sq.SetAttribute("bev", sprintf "%d" bev)
                                                    (0, x)
                               | _ -> (bev + (if x.GetAttribute("vc") = "0" then 0 else 1), sq))
                               (0, sp.[0] :?> XmlElement)
          |> ignore

        let updateMethod (dict:Dictionary<int, int * Base.Track list>) (vb, vs, vm, pt, br) (``method``:XmlElement) =
            let sp = ``method``.GetElementsByTagName("SequencePoint")
            let bp = ``method``.GetElementsByTagName("BranchPoint")
            let mp = ``method``.GetElementsByTagName("MethodPoint")
                        |> Seq.cast<XmlElement>

            let count = sp.Count
            let bCount = bp.Count + Math.Sign count
            if count > 0 then
                CopyFillMethodPoint mp sp
            else
                FillMethodPoint mp ``method`` dict

            let pointVisits = VisitCount sp
            if pointVisits > 0 then
                let b0 = VisitCount bp
                let branchVisits = b0 + Math.Sign b0
                let cover = percentCover pointVisits count
                let bcover = percentCover branchVisits bCount

                ``method``.SetAttribute("visited", "true")
                ``method``.SetAttribute("sequenceCoverage", cover)
                ``method``.SetAttribute("branchCoverage", bcover)
                setSummary ``method`` pointVisits branchVisits 1 None cover bcover
                computeBranchExitCount sp bp
                (vb + branchVisits, vs + pointVisits, vm + 1, pt + count, br+bCount)
            else (vb, vs, vm, pt + count, br+bCount)

        let updateClass (dict:Dictionary<int, int * Base.Track list>) (vb, vs, vm, vc, pt, br) (``class``:XmlElement) =
            let (cvb, cvs, cvm, cpt, cbr) = ``class``.GetElementsByTagName("Method")
                                            |> Seq.cast<XmlElement>
                                            |> Seq.fold (updateMethod dict) (0,0,0,0,0)
            let cover = percentCover cvs cpt
            let bcover = percentCover cvb cbr
            let cvc = if cvm > 0 then 1 else 0
            setSummary ``class`` cvs cvb cvm (Some cvc) cover bcover
            (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr)

        let updateModule (counts:Dictionary<string, Dictionary<int, int * Base.Track list>>) (vb, vs, vm, vc, pt, br) (``module``:XmlElement) =
            let dict =  match counts.TryGetValue <| ``module``.GetAttribute("hash") with
                        | (false, _) -> Dictionary<int, int * Base.Track list>()
                        | (true, d) -> d
            let (cvb, cvs, cvm, cvc, cpt, cbr) = ``module``.GetElementsByTagName("Class")
                                                |> Seq.cast<XmlElement>
                                                |> Seq.fold (dict |> updateClass) (0,0,0,0,0,0)
            let cover = percentCover cvs cpt
            let bcover = percentCover cvb cbr
            setSummary ``module`` cvs cvb cvm (Some cvc) cover bcover
            (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr)

        let (vb, vs, vm, vc, pt, br) = document.DocumentElement.SelectNodes("//Module")
                                       |> Seq.cast<XmlElement>
                                       |> Seq.fold (updateModule counts) (0, 0, 0, 0, 0, 0)
        let cover = percentCover vs pt
        let bcover = percentCover vb br
        setSummary document.DocumentElement vs vb vm (Some vc) cover bcover
    | _ -> ()

  let internal Point (pt:XmlElement) items outername innername attribute =
    match items with
    | [] -> ()
    | _ -> let outer = pt.OwnerDocument.CreateElement(outername)
           outer |> pt.AppendChild |> ignore
           items
           |> Seq.choose id
           |> Seq.countBy id
           |> Seq.sortBy fst
           |> Seq.iter (fun (t,n) -> let inner = pt.OwnerDocument.CreateElement(innername)
                                     inner |> outer.AppendChild |> ignore
                                     inner.SetAttribute(attribute, t.ToString())
                                     inner.SetAttribute("vc", sprintf "%d" n))

  let internal PointProcess (pt:XmlElement) tracks =
    let (times, calls) = tracks
                         |> List.map (fun t -> match t with
                                               | Base.Time x -> (Some x, None)
                                               | Base.Both (x, y) -> (Some x, Some y)
                                               | Base.Call y -> (None, Some y)
                                               | _ -> (None, None))
                         |> List.unzip
    Point pt times "Times" "Time" "time"
    Point pt calls "TrackedMethodRefs" "TrackedMethodRef" "uid"

  let internal WriteReportBase (hits:ICollection<(string*int*Base.Track)>) report =
    let counts = Dictionary<string, Dictionary<int, int * Base.Track list>>()
    hits |> Seq.iter(fun (moduleId, hitPointId, hit) ->
                        AltCover.Base.Counter.AddVisit counts moduleId hitPointId hit)
    AltCover.Base.Counter.DoFlush (PostProcess counts report) PointProcess true counts report

  // mocking points
  let mutable internal GetPayload = PayloadBase
  let mutable internal GetMonitor = MonitorBase
  let mutable internal DoReport = WriteReportBase

(*
param ([string]$OpenCoverPath)

$x = [xml](Get-Content $OpenCoverPath)
$x.CoverageSession.Modules.Module.Files.File | % {
  Write-Output "TN:"
  Write-Output "SF:$($_.fullPath)"
  $uid = $_.uid
  $p = $_.ParentNode.ParentNode
  $methods = $p.Classes.Class.Methods.Method | ? { $_.FileRef.uid -eq $uid } 
  $methods | % {
    $s = $_.SequencePoints.SequencePoint
    if ($s) {
        $l = $s[0].sl
        if($l) {
            Write-Output "FN:$l,$($_.Name)"
        }
    }
  }

  $methods | % {
      $s = $_.SequencePoints.SequencePoint
      if ($s) {
        $l = $s[0].sl
        if($l) {
            Write-Output "FNDA:$($_.MethodPoint.vc),$($_.Name)"
        }
      }
  }

  Write-Output "FNF:$($Methods.Length)"
  $hit = $methods | ? { $_.visited -eq "true" }
  Write-Output "FNH:$($hit.Length)"

  $brf = 0
  $brh = 0
  $methods | % {
      $_.Branchpoints.BranchPoint | % {
         if ($_.sl) {
            $brf += 1
            if ([int]($_.vc)) { $brh += 1 }
            Write-Output "BRDA:$($_.sl),$($_.offset),$($_.path),$($_.vc)"
        }
      }
  }
  Write-Output "BRF:$brf"
  Write-Output "BRH:$brh"

  $lf = 0
  $lh = 0
  $methods | % {
      $_.SequencePoints.SequencePoint | % {
         if ($_.sl) {
            $lf += 1
            if ([int]($_.vc)) { $lh += 1 }
            Write-Output "DA:$($_.sl),$($_.vc)"
        }
      }
  }
  Write-Output "LF:$lf"
  Write-Output "LH:$lh"

  Write-Output "end_of_record"
}*)

  let DoCoverage arguments options1 =
    let check1 = DeclareOptions ()
                 |> CommandLine.ParseCommandLine (arguments |> Array.skip 1)
                 |> CommandLine.ProcessHelpOption
                 |> RequireExe
                 |> RequireRecorder
                 |> RequireWorker
    match check1 with
    | Left (intro, options) -> CommandLine.HandleBadArguments arguments intro options1 options
                               255
    | Right (rest, _) ->
        let value = CommandLine.doPathOperation( fun () ->
            let instance = RecorderInstance()
            let report = (GetMethod instance "get_ReportFile")
                         |> GetFirstOperandAsString
                         |> Path.GetFullPath
            let format = (GetMethod instance "get_CoverageFormat")
                         |> GetFirstOperandAsNumber
            let hits = List<(string*int*Base.Track)>()

            let payload = GetPayload
            let result = GetMonitor hits report payload rest
            let delta = DoReport hits (enum format) report
            WriteResourceWithFormatItems "Coverage statistics flushing took {0:N} seconds" [|delta.TotalSeconds|]

            // And tidy up after everything's done
            File.Delete (report + ".acv")
            Directory.GetFiles( Path.GetDirectoryName(report),
                                Path.GetFileName(report) + ".*.acv")
            |> Seq.iter File.Delete
            result                             ) 255
        CommandLine.ReportErrors()
        value