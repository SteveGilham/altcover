namespace AltCover

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Xml
open System.Xml.Linq

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
  let internal executable : Option<string> ref = ref None
  let internal lcov : Option<string> ref = ref None
  let mutable internal collect = false

  let DoWithFile (create: unit -> FileStream) (action : Stream -> unit) =
    use stream = create()
    action stream

  let LCovSummary (report:XDocument) (format:Base.ReportFormat) =
    DoWithFile 
      (fun () -> File.OpenWrite(!lcov |> Option.get))
      (fun stream ->
        use writer = new StreamWriter(stream)
        //If available, a tracefile begins with the testname which
        //   is stored in the following format:
        //
        //     TN:<test name>
        writer.WriteLine "TN:"
        let X = OpenCover.X

        match format with
        | Base.ReportFormat.NCover ->
            report.Descendants(X "method")
            |> Seq.filter (fun m -> m.Descendants(X "seqpnt") |> Seq.isEmpty |> not)
            |> Seq.groupBy (fun m -> (m.Descendants(X "seqpnt") |> Seq.head).Attribute(X "document").Value)
            |> Seq.map (fun (f, ms) -> (f, ms
                                            |> Seq.sortBy (fun m -> (m.Descendants(X "seqpnt") |> Seq.head).Attribute(X "line").Value |> Int32.TryParse |> snd)
                                            |> Seq.toList))
            |> Seq.sortBy (fun (f, _) -> f)
            |> Seq.iter (fun (f, methods) ->
                           // For each source file referenced in the .da file,  there  is  a  section
                           // containing filename and coverage data:
                           //
                           //  SF:<absolute path to the source file>
                           writer.WriteLine ("SF:" + f)

                           // Following is a list of line numbers for each function name found in the
                           // source file:
                           //
                           // FN:<line number of function start>,<function name>
                           methods
                           |> Seq.iter (fun m ->
                                           let l = (m.Descendants(X "seqpnt") |> Seq.head).Attribute(X "line").Value
                                           let name = m.Attribute(X "fullname").Value
                                           writer.WriteLine ("FN:" + l + "," + name))

                           // Next, there is a list of execution counts for each  instrumented  function:
                           //
                           // FNDA:<execution count>,<function name>
                           let hit = methods
                                     |> Seq.fold (fun n m ->
                                           let v = (m.Descendants(X "seqpnt") |> Seq.head).Attribute(X "visitcount").Value
                                           let name = m.Attribute(X "fullname").Value
                                           writer.WriteLine ("FNDA:" + v + "," + name)
                                           n + (if v = "0" then 0 else 1)
                                           ) 0
                           // This  list  is followed by two lines containing the number of functions
                           // found and hit:
                           //
                           // FNF:<number of functions found>
                           // FNH:<number of function hit>
                           writer.WriteLine ("FNF:" + methods.Length.ToString(CultureInfo.InvariantCulture))
                           writer.WriteLine ("FNH:" + hit.ToString(CultureInfo.InvariantCulture))

                           // Branch coverage information is stored which one line per branch:
                           //
                           // BRDA:<line number>,<block number>,<branch number>,<taken>
                           //
                           // Block number and branch number are gcc internal  IDs  for  the  branch.
                           // Taken  is either '-' if the basic block containing the branch was never
                           // executed or a number indicating how often that branch was taken.

                           // Branch coverage summaries are stored in two lines:
                           //
                           // BRF:<number of branches found>
                           // BRH:<number of branches hit>
                           writer.WriteLine ("BRF:0")
                           writer.WriteLine ("BRH:0")

                           // Then there is a list of execution counts  for  each  instrumented  line
                           // (i.e. a line which resulted in executable code):
                           //
                           // DA:<line number>,<execution count>[,<checksum>]
                           //
                           // Note  that  there  may be an optional checksum present for each instru‐
                           // mented line. The current geninfo implementation uses  an  MD5  hash  as
                           // checksumming algorithm.
                           let (lf, lh) = methods
                                               |> Seq.collect (fun m -> m.Descendants(X "seqpnt"))
                                               |> Seq.filter (fun b -> b.Attribute(X "line").Value |> String.IsNullOrWhiteSpace |> not)
                                               |> Seq.fold (fun (f,h) b -> let sl = b.Attribute(X "line").Value
                                                                           let v = b.Attribute(X "visitcount")
                                                                           let vc = if v |> isNull then "0" else v.Value
                                                                           writer.WriteLine ("DA:" + sl + "," + vc)
                                                                           (f+1, h + if vc = "0" then 0 else 1))
                                                                           (0,0)
                           // At  the  end of a section, there is a summary about how many lines were
                           // found and how many were actually instrumented:
                           //
                           // LH:<number of lines with a non-zero execution count>
                           // LF:<number of instrumented lines>
                           writer.WriteLine ("LH:" + lh.ToString(CultureInfo.InvariantCulture))
                           writer.WriteLine ("LF:" + lf.ToString(CultureInfo.InvariantCulture))

                           // Each sections ends with:
                           //
                           // end_of_record
                           writer.WriteLine "end_of_record"

                        )

        | _ ->
            // For each source file referenced in the .da file,  there  is  a  section
            // containing filename and coverage data:
            //
            //  SF:<absolute path to the source file>

            report.Descendants(X "File")
            |> Seq.iter (fun f ->
                           writer.WriteLine ("SF:" + f.Attribute(X "fullPath").Value)
                           let uid = f.Attribute(X "uid").Value
                           let p = f.Parent.Parent

                           // Following is a list of line numbers for each function name found in the
                           // source file:
                           //
                           // FN:<line number of function start>,<function name>
                           let methods = p.Descendants(X "Method")
                                         |> Seq.filter (fun m -> m.Descendants(X "FileRef")
                                                                 |> Seq.exists (fun r -> r.Attribute(X "uid").Value = uid))
                                         |> Seq.toList

                           let FN (ms : XElement list) =
                               ms
                               |> Seq.iter (fun m ->
                                             m.Descendants(X "SequencePoint") |> Seq.tryHead
                                             |> Option.iter (fun s -> let n = (m.Descendants(X "Name")
                                                                               |> Seq.head).Value
                                                                      let mp = m.Descendants(X "MethodPoint") |> Seq.head
                                                                      let sl = s.Attribute(X "sl").Value
                                                                      if sl |> String.IsNullOrWhiteSpace |> not then
                                                                          writer.WriteLine ("FN:" + s.Attribute(X "sl").Value +
                                                                                            "," + n)))
                           FN methods
                           // Next, there is a list of execution counts for each  instrumented  function:
                           //
                           // FNDA:<execution count>,<function name>
                           let FNDA (ms : XElement list) =
                               ms
                               |> Seq.iter (fun m ->
                                             m.Descendants(X "SequencePoint") |> Seq.tryHead
                                             |> Option.iter (fun s -> let n = (m.Descendants(X "Name")
                                                                               |> Seq.head).Value
                                                                      let mp = m.Descendants(X "MethodPoint") |> Seq.head
                                                                      let sl = s.Attribute(X "sl").Value
                                                                      if sl |> String.IsNullOrWhiteSpace |> not then
                                                                          writer.WriteLine ("FNDA:" + mp.Attribute(X "vc").Value +
                                                                                            "," + n)))

                           FNDA methods
                           // This  list  is followed by two lines containing the number of functions
                           // found and hit:
                           //
                           // FNF:<number of functions found>
                           // FNH:<number of function hit>
                           writer.WriteLine ("FNF:" + methods.Length.ToString(CultureInfo.InvariantCulture))
                           let hit = methods
                                     |> List.filter (fun m -> m.Attribute(X "visited").Value = "true")
                           writer.WriteLine ("FNH:" + hit.Length.ToString(CultureInfo.InvariantCulture))

                           // Branch coverage information is stored which one line per branch:
                           //
                           // BRDA:<line number>,<block number>,<branch number>,<taken>
                           //
                           // Block number and branch number are gcc internal  IDs  for  the  branch.
                           // Taken  is either '-' if the basic block containing the branch was never
                           // executed or a number indicating how often that branch was taken.
                           let Branch (ms : XElement list) =
                               let (brf, brh, _) = ms
                                                   |> Seq.collect (fun m -> m.Descendants(X "BranchPoint"))
                                                   |> Seq.filter (fun b -> b.Attribute(X "sl").Value |> String.IsNullOrWhiteSpace |> not)
                                                   |> Seq.fold (fun (f,h,(o,u)) b -> let sl = b.Attribute(X "sl").Value
                                                                                     let off = b.Attribute(X "offset").Value
                                                                                     let usp = b.Attribute(X "uspid").Value
                                                                                     let path = b.Attribute(X "path").Value
                                                                                     let vc = b.Attribute(X "vc").Value
                                                                                     writer.WriteLine ("BRDA:" + sl + "," +
                                                                                                       (if o = off then u else usp) +
                                                                                                       "," + path  + "," +
                                                                                                       (if vc = "0" then "-" else vc))
                                                                                     (f+1, h + (if vc = "0" then 0 else 1), if o = off then (o,u) else (off,usp)))
                                                                                     (0,0,("?","?"))
                               // Branch coverage summaries are stored in two lines:
                               //
                               // BRF:<number of branches found>
                               // BRH:<number of branches hit>
                               writer.WriteLine ("BRF:" + brf.ToString(CultureInfo.InvariantCulture))
                               writer.WriteLine ("BRH:" + brh.ToString(CultureInfo.InvariantCulture))

                           Branch methods

                           // Then there is a list of execution counts  for  each  instrumented  line
                           // (i.e. a line which resulted in executable code):
                           //
                           // DA:<line number>,<execution count>[,<checksum>]
                           //
                           // Note  that  there  may be an optional checksum present for each instru‐
                           // mented line. The current geninfo implementation uses  an  MD5  hash  as
                           // checksumming algorithm.
                           let (lf, lh) = methods
                                               |> Seq.collect (fun m -> m.Descendants(X "SequencePoint"))
                                               |> Seq.filter (fun b -> b.Attribute(X "sl").Value |> String.IsNullOrWhiteSpace |> not)
                                               |> Seq.fold (fun (f,h) b -> let sl = b.Attribute(X "sl").Value
                                                                           let vc = b.Attribute(X "vc").Value
                                                                           writer.WriteLine ("DA:" + sl + "," + vc)
                                                                           (f+1, h + if vc = "0" then 0 else 1))
                                                                           (0,0)
                           // At  the  end of a section, there is a summary about how many lines were
                           // found and how many were actually instrumented:
                           //
                           // LH:<number of lines with a non-zero execution count>
                           // LF:<number of instrumented lines>
                           writer.WriteLine ("LH:" + lh.ToString(CultureInfo.InvariantCulture))
                           writer.WriteLine ("LF:" + lf.ToString(CultureInfo.InvariantCulture))

                           // Each sections ends with:
                           //
                           // end_of_record
                           writer.WriteLine "end_of_record"
                           ))

  let StandardSummary (_:XDocument) (format:Base.ReportFormat) =
    match format with
    | Base.ReportFormat.NCover -> ()
    | _ ->

(*
        private static void CalculateResults(CoverageSession coverageSession, Results results)
        {
            foreach (var @class in
                                from module in coverageSession.Modules.Where(x => x.Classes != null)
                                from @class in module.Classes.Where(c => !c.ShouldSerializeSkippedDueTo())
                                select @class)
            {
                if (@class.Methods == null)
                    continue;

                if (!@class.Methods.Any(x => !x.ShouldSerializeSkippedDueTo() && x.SequencePoints.Any(y => y.VisitCount > 0))
                    && @class.Methods.Any(x => x.FileRef != null))
                {
                    results.unvisitedClasses.Add(@class.FullName);
                }

                if (@class.Methods.Any(x => x.Visited))
                {
                    results.altVisitedClasses += 1;
                    results.altTotalClasses += 1;
                }
                else if (@class.Methods.Any())
                {
                    results.altTotalClasses += 1;
                }

                foreach (var method in @class.Methods.Where(x => !x.ShouldSerializeSkippedDueTo()))
                {
                    if (method.FileRef != null && !method.SequencePoints.Any(x => x.VisitCount > 0))
                        results.unvisitedMethods.Add(string.Format("{0}", method.FullName));

                    results.altTotalMethods += 1;
                    if (method.Visited)
                    {
                        results.altVisitedMethods += 1;
                    }
                }
            }
        }

        private static void DisplayResults(CoverageSession coverageSession, ICommandLine parser, Results results)
        {
            if (coverageSession.Summary.NumClasses > 0)
            {
                Logger.InfoFormat("Visited Classes {0} of {1} ({2})", coverageSession.Summary.VisitedClasses,
                                  coverageSession.Summary.NumClasses, Math.Round(coverageSession.Summary.VisitedClasses * 100.0 / coverageSession.Summary.NumClasses, 2));
                Logger.InfoFormat("Visited Methods {0} of {1} ({2})", coverageSession.Summary.VisitedMethods,
                                  coverageSession.Summary.NumMethods, Math.Round(coverageSession.Summary.VisitedMethods * 100.0 / coverageSession.Summary.NumMethods, 2));
                Logger.InfoFormat("Visited Points {0} of {1} ({2})", coverageSession.Summary.VisitedSequencePoints,
                                  coverageSession.Summary.NumSequencePoints, coverageSession.Summary.SequenceCoverage);
                Logger.InfoFormat("Visited Branches {0} of {1} ({2})", coverageSession.Summary.VisitedBranchPoints,
                                  coverageSession.Summary.NumBranchPoints, coverageSession.Summary.BranchCoverage);

                Logger.InfoFormat("");
                Logger.InfoFormat(
                    "==== Alternative Results (includes all methods including those without corresponding source) ====");
                Logger.InfoFormat("Alternative Visited Classes {0} of {1} ({2})", results.altVisitedClasses,
                                  results.altTotalClasses, results.altTotalClasses == 0 ? 0 : Math.Round(results.altVisitedClasses * 100.0 / results.altTotalClasses, 2));
                Logger.InfoFormat("Alternative Visited Methods {0} of {1} ({2})", results.altVisitedMethods,
                                  results.altTotalMethods, results.altTotalMethods == 0 ? 0 : Math.Round(results.altVisitedMethods * 100.0 / results.altTotalMethods, 2));

*)
    ()

  let mutable internal Summaries : (XDocument -> Base.ReportFormat -> unit) list = []

  let internal DeclareOptions () =
    Summaries <- []
    Summaries <- StandardSummary :: Summaries
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
      ("l|lcovReport=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome !lcov then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--lcovReport") :: CommandLine.error
                    else
                      lcov := Some x
                      Summaries <- LCovSummary :: Summaries
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "--lcovReport",
                                                         x) :: CommandLine.error))
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
    DoWithFile (fun () -> 
          let binpath = report + ".acv"
          File.Create(binpath))
          ignore

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

  let DoSummaries (document:XDocument) (format:Base.ReportFormat) =
    Summaries
    |> List.iter (fun summary -> summary document format)

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
            let format' = enum format
            let delta = DoReport hits format' report
            WriteResourceWithFormatItems "Coverage statistics flushing took {0:N} seconds" [|delta.TotalSeconds|]

            // And tidy up after everything's done
            File.Delete (report + ".acv")
            Directory.GetFiles( Path.GetDirectoryName(report),
                                Path.GetFileName(report) + ".*.acv")
            |> Seq.iter File.Delete

            let document = if File.Exists report then XDocument.Load report else XDocument()
            DoSummaries document format'
            result                             ) 255
        CommandLine.ReportErrors()
        value