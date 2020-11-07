namespace AltCover

open System
open System.Globalization
open System.IO
open System.Xml.Linq

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
  "AvoidSpeculativeGeneralityRule",
  Justification="Delegation = first class functions")>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'LCov' is jargon")>]
module internal LCov =
  let internal path : Option<string> ref = ref None
  let internal sortByFirst s = s |> Seq.sortBy fst

  module internal I =

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal lineOfMethod (m : XElement) =
      (m.Descendants("seqpnt".X) |> Seq.head).Attribute("line".X).Value
      |> Int32.TryParse
      |> snd

    let internal multiSort (by : 'a -> int) (l : (string * 'a seq) seq) =
      l
      |> Seq.map (fun (f, ms) ->
           (f,
            ms
            |> Seq.sortBy by
            |> Seq.toList))
      |> sortByFirst

    let internal multiSortByNameAndStartLine (l : (string * XElement seq) seq) =
      multiSort lineOfMethod l

  // from e.g. https://manpages.debian.org/unstable/lcov/geninfo.1.en.html
  // Following is a quick description of the tracefile format as used by 
  // genhtml, geninfo and lcov.

  // A tracefile is made up of several human-readable lines of text, divided into sections.   

  let internal convertReport (report : XDocument) (format : ReportFormat) (stream : Stream) =
    doWithStream (fun () -> new StreamWriter(stream)) (fun writer ->
      match format with
      | ReportFormat.NCover ->
          report.Descendants("method".X)
          |> Seq.filter (fun m ->
                m.Attribute("excluded".X).Value <> "true"
                && m.Descendants("seqpnt".X)
                  |> Seq.exists (fun s -> s.Attribute("excluded".X).Value <> "true"))
          |> Seq.groupBy (fun m ->
                (m.Descendants("seqpnt".X) |> Seq.head).Attribute("document".X).Value)
          |> I.multiSortByNameAndStartLine
          |> Seq.iter (fun (f, methods) ->
                //If available, a tracefile begins with the testname which
                //   is stored in the following format:
                //
                // TN:<test name>
                writer.WriteLine "TN:"
                // For each source file referenced in the .da file,  there  is  a  section
                // containing filename and coverage data:
                //
                //  SF:<absolute path to the source file>
                writer.WriteLine("SF:" + f)
                let fullname (m : XElement) =
                  let fna = m.Attribute("fullname".X)
                  if fna |> isNull
                  then m.Attribute("class".X).Value + "." + m.Attribute("name".X).Value
                  else fna.Value
                // Following is a list of line numbers for each function name found in the
                // source file:
                //
                // FN:<line number of function start>,<function name>
                methods
                |> Seq.iter (fun m ->
                    let l = (I.lineOfMethod m).ToString(CultureInfo.InvariantCulture)
                    let name = fullname m
                    writer.WriteLine("FN:" + l + "," + name))
                // Next, there is a list of execution counts for each  instrumented  function:
                //
                // FNDA:<execution count>,<function name>
                let hit =
                  methods
                  |> Seq.fold (fun (n:int) m ->
                      let v =
                        (m.Descendants("seqpnt".X) |> Seq.head).Attribute("visitcount".X).Value
                      let name = fullname m
                      writer.WriteLine("FNDA:" + v + "," + name)
                      n.Increment(v <> "0")) 0
                // This  list  is followed by two lines containing the number of functions
                // found and hit:
                //
                // FNF:<number of functions found>
                // FNH:<number of function hit>
                writer.WriteLine
                  ("FNF:" + methods.Length.ToString(CultureInfo.InvariantCulture))
                writer.WriteLine("FNH:" + hit.ToString(CultureInfo.InvariantCulture))
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
                writer.WriteLine("BRF:0")
                writer.WriteLine("BRH:0")
                // Then there is a list of execution counts  for  each  instrumented  line
                // (i.e. a line which resulted in executable code):
                //
                // DA:<line number>,<execution count>[,<checksum>]
                //
                // Note  that  there  may be an optional checksum present for each instru‐
                // mented line. The current geninfo implementation uses  an  MD5  hash  as
                // checksumming algorithm.
                let (lf, lh) =
                  methods
                  |> Seq.collect (fun m -> m.Descendants("seqpnt".X))
                  |> Seq.filter (fun b ->
                      b.Attribute("line".X).Value
                      |> String.IsNullOrWhiteSpace
                      |> not)
                  |> Seq.fold (fun (f, h) b ->
                      let sl = b.Attribute("line".X).Value
                      let v = b.Attribute("visitcount".X)

                      let vc =
                        if v |> isNull then "0" else v.Value
                      writer.WriteLine("DA:" + sl + "," + vc)
                      (f + 1,
                        h + if vc = "0" then 0 else 1)) (0, 0)
                // At  the  end of a section, there is a summary about how many lines were
                // found and how many were actually instrumented:
                //
                // LH:<number of lines with a non-zero execution count>
                // LF:<number of instrumented lines>
                writer.WriteLine("LH:" + lh.ToString(CultureInfo.InvariantCulture))
                writer.WriteLine("LF:" + lf.ToString(CultureInfo.InvariantCulture))
                // Each sections ends with:
                //
                // end_of_record
                writer.WriteLine "end_of_record")
      | _ ->  // ReportFormat.OpenCover, ReportFormat.OpenCoverWithTracking
          report.Descendants("File".X)
          |> Seq.iter (fun f ->
                //If available, a tracefile begins with the testname which
                //   is stored in the following format:
                //
                // TN:<test name>
                writer.WriteLine "TN:"
                // For each source file referenced in the .da file,  there  is  a  section
                // containing filename and coverage data:
                //
                //  SF:<absolute path to the source file>
                writer.WriteLine("SF:" + f.Attribute("fullPath".X).Value)
                let uid = f.Attribute("uid".X).Value
                let p = f.Parent.Parent

                // Following is a list of line numbers for each function name found in the
                // source file:
                //
                // FN:<line number of function start>,<function name>
                let methods =
                  p.Descendants("Method".X)
                  |> Seq.filter (fun m ->
                      m.Descendants("FileRef".X)
                      |> Seq.exists (fun r -> r.Attribute("uid".X).Value = uid))
                  |> Seq.toList

                let FN(ms : XElement list) = // fsharplint:disable-line NonPublicValuesNames
                  ms
                  |> Seq.iter (fun m ->
                      m.Descendants("SequencePoint".X)
                      |> Seq.tryHead
                      |> Option.iter (fun s ->
                            let n = (m.Descendants("Name".X) |> Seq.head).Value
                            let mp = m.Descendants("MethodPoint".X) |> Seq.head
                            let sl = s.Attribute("sl".X).Value
                            if sl
                              |> String.IsNullOrWhiteSpace
                              |> not
                            then
                              writer.WriteLine("FN:" + s.Attribute("sl".X).Value + "," + n)))

                FN methods
                // Next, there is a list of execution counts for each  instrumented  function:
                //
                // FNDA:<execution count>,<function name>
                let FNDA(ms : XElement list) = // fsharplint:disable-line NonPublicValuesNames
                  ms
                  |> Seq.iter (fun m ->
                      m.Descendants("SequencePoint".X)
                      |> Seq.tryHead
                      |> Option.iter (fun s ->
                            let n = (m.Descendants("Name".X) |> Seq.head).Value
                            let mp = m.Descendants("MethodPoint".X) |> Seq.head
                            let sl = s.Attribute("sl".X).Value
                            if sl
                              |> String.IsNullOrWhiteSpace
                              |> not
                            then
                              writer.WriteLine
                                ("FNDA:" + mp.Attribute("vc".X).Value + "," + n)))
                FNDA methods
                // This  list  is followed by two lines containing the number of functions
                // found and hit:
                //
                // FNF:<number of functions found>
                // FNH:<number of function hit>
                writer.WriteLine
                  ("FNF:" + methods.Length.ToString(CultureInfo.InvariantCulture))
                let hit =
                  methods |> List.filter (fun m -> m.Attribute("visited".X).Value = "true")
                writer.WriteLine
                  ("FNH:" + hit.Length.ToString(CultureInfo.InvariantCulture))
                // Branch coverage information is stored which one line per branch:
                //
                // BRDA:<line number>,<block number>,<branch number>,<taken>
                //
                // Block number and branch number are gcc internal  IDs  for  the  branch.
                // Taken  is either '-' if the basic block containing the branch was never
                // executed or a number indicating how often that branch was taken.
                let branch(ms : XElement list) =
                  let (brf, brh, _) =
                    ms
                    |> Seq.collect (fun m -> m.Descendants("BranchPoint".X))
                    |> Seq.filter (fun b ->
                        b.Attribute("sl".X).Value
                        |> String.IsNullOrWhiteSpace
                        |> not)
                    |> Seq.fold (fun (f, h, (o, u)) b ->
                        let sl = b.Attribute("sl".X).Value
                        let off = b.Attribute("offset".X).Value
                        let usp = b.Attribute("uspid".X).Value
                        let path = b.Attribute("path".X).Value
                        let vc = b.Attribute("vc".X).Value
                        writer.WriteLine
                          ("BRDA:" + sl + "," + (if o = off then u else usp) + "," + path
                            + "," + (if vc = "0" then "-" else vc))
                        (f + 1,
                          h + (if vc = "0" then 0 else 1),
                          (if o = off then (o, u) else (off, usp)))) (0, 0, ("?", "?"))
                  // Branch coverage summaries are stored in two lines:
                  //
                  // BRF:<number of branches found>
                  // BRH:<number of branches hit>
                  writer.WriteLine("BRF:" + brf.ToString(CultureInfo.InvariantCulture))
                  writer.WriteLine("BRH:" + brh.ToString(CultureInfo.InvariantCulture))
                branch methods
                // Then there is a list of execution counts  for  each  instrumented  line
                // (i.e. a line which resulted in executable code):
                //
                // DA:<line number>,<execution count>[,<checksum>]
                //
                // Note  that  there  may be an optional checksum present for each instru‐
                // mented line. The current geninfo implementation uses  an  MD5  hash  as
                // checksumming algorithm.
                let (lf, lh) =
                  methods
                  |> Seq.collect (fun m -> m.Descendants("SequencePoint".X))
                  |> Seq.filter (fun b ->
                      b.Attribute("sl".X).Value
                      |> String.IsNullOrWhiteSpace
                      |> not)
                  |> Seq.fold (fun (f, h) b ->
                      let sl = b.Attribute("sl".X).Value
                      let vc = b.Attribute("vc".X).Value
                      writer.WriteLine("DA:" + sl + "," + vc)
                      (f + 1,
                        h + if vc = "0" then 0 else 1)) (0, 0)
                // At  the  end of a section, there is a summary about how many lines were
                // found and how many were actually instrumented:
                //
                // LH:<number of lines with a non-zero execution count>
                // LF:<number of instrumented lines>
                writer.WriteLine("LH:" + lh.ToString(CultureInfo.InvariantCulture))
                writer.WriteLine("LF:" + lf.ToString(CultureInfo.InvariantCulture))
                // Each sections ends with:
                //
                // end_of_record
                writer.WriteLine "end_of_record"))

  let internal summary (report : XDocument) (format : ReportFormat) result =
    doWithStream(fun () -> File.OpenWrite(!path |> Option.get))
      (convertReport report format)
    (result, 0uy, String.Empty)
