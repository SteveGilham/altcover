namespace AltCover

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Xml

open Mono.Cecil
open Mono.Options
open Augment

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
type Tracer = { Tracer : string }

type MonoTypeBinder (``type``:Type) =
  inherit System.Runtime.Serialization.SerializationBinder()
  override self.BindToType (_:string, _:string) =
    ``type``

module Runner =

  let mutable internal recordingDirectory : Option<string> = None
  let mutable internal workingDirectory : Option<string> = None
  let mutable internal executable : Option<string> ref = ref None

  let internal DeclareOptions () =
    [ ("r|recorderDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome recordingDirectory then
                      CommandLine.error <- true
                    else
                      recordingDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- true))
      ("w|workingDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome workingDirectory then
                      CommandLine.error <- true
                    else
                      workingDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- true))
      ("x|executable=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome !executable then
                      CommandLine.error <- true
                    else
                      executable := Some x
                 else CommandLine.error <- true))
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))
      ("<>", (fun x -> CommandLine.error <- true))         ]// default end stop
      |> List.fold (fun (o:OptionSet) (p, a) -> o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a))) (OptionSet())

  let HandleBadArguments arguments intro options1 options =
        String.Join (" ", arguments |> Seq.map (sprintf "%A"))
        |> CommandLine.WriteErr
        CommandLine.Usage intro options1 options

  let internal RequireExe (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right (l, options) -> match !executable with
                            | None -> Left ("UsageError", options)
                            | Some exe -> Right (exe::l, options)
    | fail -> fail

  let internal RequireRecorder (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right (_, options) -> match recordingDirectory with
                            | None -> Left ("UsageError", options)
                            | Some path -> let dll = Path.Combine (path, "AltCover.Recorder.g.dll")
                                           if File.Exists dll then parse
                                           else Left ("UsageError", options)
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
    CommandLine.resources.GetString >> Console.WriteLine

  let WriteResourceWithFormatItems s x =
    Console.WriteLine (s |> CommandLine.resources.GetString, x)

  let MonitorBase (hits:ICollection<(string*int)>) report (payload: string list -> int) (args : string list) =
      let binpath = report + ".bin"
      do
        use stream = File.Create(binpath)
        ()

      "Beginning run..." |> WriteResource
      let result = payload args
      "Getting results..."  |> WriteResource

      let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
      formatter.Binder <- MonoTypeBinder(typeof<(string*int)>) // anything else is an error

      Directory.GetFiles( Path.GetDirectoryName(report),
                          Path.GetFileName(report) + ".*.bin")
      |> Seq.iter (fun f ->
          printfn "... %s" f
          use results = new DeflateStream(File.OpenRead f, CompressionMode.Decompress)
          let rec sink() =
            let hit = try formatter.Deserialize(results) :?> (string*int)
                      with | :? System.Runtime.Serialization.SerializationException as x -> (null, -1)
            if hit|> fst |> String.IsNullOrWhiteSpace  |> not then
              hit |> hits.Add
              sink()
          sink()
      )

      WriteResourceWithFormatItems "%d visits recorded" [|hits.Count|]
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

  let internal LookUpVisitsByToken token (dict:Dictionary<int, int>) =
    let (ok, index) = Int32.TryParse( token,
                                        System.Globalization.NumberStyles.Integer,
                                        System.Globalization.CultureInfo.InvariantCulture)
    dict.TryGetValue(if ok then index else -1)

  let internal FillMethodPoint (mp:XmlElement seq) (``method``:XmlElement) (dict:Dictionary<int, int>) =
    let token = ``method``.GetElementsByTagName("MetadataToken")
                |> Seq.cast<XmlElement>
                |> Seq.map(fun m -> m.InnerText)
                |> Seq.head
    let (_, vc) = LookUpVisitsByToken token dict
    mp
    |> Seq.iter (fun m -> m.SetAttribute("vc", vc.ToString(System.Globalization.CultureInfo.InvariantCulture))
                          m.SetAttribute("uspid", token)
                          m.SetAttribute("ordinal", "0")
                          m.SetAttribute("offset", "0"))

  let internal PostProcess (counts:Dictionary<string, Dictionary<int, int>>) format (document:XmlDocument) =
    match format with
    | Base.ReportFormat.OpenCover ->
        let updateMethod (dict:Dictionary<int, int>) (vs, vm, pt) (``method``:XmlElement) =
            let sp = ``method``.GetElementsByTagName("SequencePoint")
            let count = sp.Count
            let mp = ``method``.GetElementsByTagName("MethodPoint")
                        |> Seq.cast<XmlElement>
            if count > 0 then
                CopyFillMethodPoint mp sp
            else
                FillMethodPoint mp ``method`` dict

            let visitPoints = sp
                            |> Seq.cast<XmlElement>
                            |> Seq.filter(fun s -> Int32.TryParse( s.GetAttribute("vc") ,
                                                             System.Globalization.NumberStyles.Integer,
                                                             System.Globalization.CultureInfo.InvariantCulture) |> snd
                                                   <> 0)
                            |> Seq.length
            if visitPoints > 0 then
                let cover = (sprintf "%.2f" ((float (visitPoints * 100))/(float count))).TrimEnd([| '0' |]).TrimEnd([|'.'|])
                ``method``.SetAttribute("visited", "true")
                ``method``.SetAttribute("sequenceCoverage", cover)
                ``method``.GetElementsByTagName("Summary")
                |> Seq.cast<XmlElement>
                |> Seq.iter(fun s -> s.SetAttribute("visitedSequencePoints", sprintf "%d" visitPoints)
                                     s.SetAttribute("visitedMethods", "1")
                                     s.SetAttribute("sequenceCoverage", cover))
                (vs + visitPoints, vm + 1, pt + count)
            else (vs, vm, pt + count)

        let updateClass (dict:Dictionary<int, int>) (vs, vm, vc, pt) (``class``:XmlElement) =
            let (cvs, cvm, cpt) = ``class``.GetElementsByTagName("Method")
                                     |> Seq.cast<XmlElement>
                                     |> Seq.fold (updateMethod dict) (0,0,0)
            let csum = ``class``.GetElementsByTagName("Summary")
                         |> Seq.cast<XmlElement> |> Seq.head
            let cover = if cpt = 0 then "0"
                         else (sprintf "%.2f" ((float (cvs * 100))/(float cpt))).TrimEnd([| '0' |]).TrimEnd([|'.'|])
            let cvc = if cvm > 0 then 1 else 0
            csum.SetAttribute("visitedSequencePoints", sprintf "%d" cvs)
            csum.SetAttribute("visitedMethods", sprintf "%d" cvm)
            csum.SetAttribute("visitedClasses", sprintf "%d" cvc)
            csum.SetAttribute("sequenceCoverage", cover)
            (vs + cvs, vm + cvm, vc + cvc, pt + cpt)

        let updateModule (counts:Dictionary<string, Dictionary<int, int>>) (vs, vm, vc, pt) (``module``:XmlElement) =
            let dict =  match counts.TryGetValue <| ``module``.GetAttribute("hash") with
                        | (false, _) -> Dictionary<int, int>()
                        | (true, d) -> d
            let (cvs, cvm, cvc, cpt) = ``module``.GetElementsByTagName("Class")
                                         |> Seq.cast<XmlElement>
                                         |> Seq.fold (updateClass dict) (0,0,0,0)
            let cover = if cpt = 0 then "0"
                         else (sprintf "%.2f" ((float (cvs * 100))/(float cpt))).TrimEnd([| '0' |]).TrimEnd([|'.'|])
            ``module``.GetElementsByTagName("Summary")
            |> Seq.cast<XmlElement> |> Seq.tryFind (fun _ -> true)
            |> Option.iter (fun msum ->
                msum.SetAttribute("visitedSequencePoints", sprintf "%d" cvs)
                msum.SetAttribute("visitedMethods", sprintf "%d" cvm)
                msum.SetAttribute("visitedClasses", sprintf "%d" cvc)
                msum.SetAttribute("sequenceCoverage", cover))
            (vs + cvs, vm + cvm, vc + cvc, pt + cpt)

        let (vs, vm, vc, pt) = document.DocumentElement.SelectNodes("//Module")
                                   |> Seq.cast<XmlElement>
                                   |> Seq.fold (updateModule counts) (0, 0, 0, 0)
        let cover = if pt = 0 then "0"
                    else (sprintf "%.2f" ((float (vs * 100))/(float pt))).TrimEnd([| '0' |]).TrimEnd([|'.'|])
        let msum = document.DocumentElement.GetElementsByTagName("Summary")
                         |> Seq.cast<XmlElement> |> Seq.head
        msum.SetAttribute("visitedSequencePoints", sprintf "%d" vs)
        msum.SetAttribute("visitedMethods", sprintf "%d" vm)
        msum.SetAttribute("visitedClasses", sprintf "%d" vc)
        msum.SetAttribute("sequenceCoverage", cover)
    | _ -> ()

  let WriteReportBase (hits:ICollection<(string*int)>) report =
    let counts = Dictionary<string, Dictionary<int, int>>()
    hits |> Seq.iter(fun (moduleId, hitPointId) ->
                        AltCover.Base.Counter.AddVisit counts moduleId hitPointId)
    AltCover.Base.Counter.DoFlush (PostProcess counts report) true counts report

  // mocking points
  let mutable internal GetPayload = PayloadBase
  let mutable internal GetMonitor = MonitorBase
  let mutable internal DoReport = WriteReportBase

  let DoCoverage arguments options1 =
    let check1 = DeclareOptions ()
                 |> CommandLine.ParseCommandLine (arguments |> Array.skip 1)
                 |> CommandLine.ProcessHelpOption
                 |> RequireExe
                 |> RequireRecorder
                 |> RequireWorker
    match check1 with
    | Left (intro, options) -> HandleBadArguments arguments intro options1 options
                               255
    | Right (rest, _) ->
          let instance = RecorderInstance()
          let report = (GetMethod instance "get_ReportFile")
                       |> GetFirstOperandAsString
                       |> Path.GetFullPath
          let format = (GetMethod instance "get_CoverageFormat")
                       |> GetFirstOperandAsNumber
          let hits = List<(string*int)>()

          let payload = GetPayload
          let result = GetMonitor hits report payload rest
          let delta = DoReport hits (enum format) report
          WriteResourceWithFormatItems "Coverage statistics flushing took {0:N} seconds" [|delta.TotalSeconds|]

          // And tidy up after everything's done
          File.Delete (report + ".bin")
          Directory.GetFiles( Path.GetDirectoryName(report),
                              Path.GetFileName(report) + ".*.bin")
          |> Seq.iter File.Delete
          result