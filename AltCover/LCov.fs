namespace AltCover

open System
open System.Globalization
open System.IO
open System.Xml.Linq

module internal LCov =
  let internal path : Option<string> ref = ref None

  let DoWith (create : unit -> 'a) (action : 'a -> unit) =
    use stream = create()
    action stream

  let X = OpenCover.X

  let lineOfMethod (m : XElement) =
    (m.Descendants(X "seqpnt") |> Seq.head).Attribute(X "line").Value
    |> Int32.TryParse
    |> snd

  let SortByFirst s = s |> Seq.sortBy fst

  let multiSort (by : 'a -> int) (l : (string * 'a seq) seq) =
    l
    |> Seq.map (fun (f, ms) ->
         (f,
          ms
          |> Seq.sortBy by
          |> Seq.toList))
    |> SortByFirst

  let multiSortByNameAndStartLine (l : (string * XElement seq) seq) =
    multiSort lineOfMethod l

  let ConvertReport (report : XDocument) (format : Base.ReportFormat) (stream : Stream) =
    DoWith (fun () -> new StreamWriter(stream)) (fun writer ->
      //If available, a tracefile begins with the testname which
      //   is stored in the following format:
      //
      //     TN:<test name>
      writer.WriteLine "TN:"
      match format with
      | Base.ReportFormat.NCover ->
          report.Descendants(X "method")
          |> Seq.filter (fun m ->
               m.Attribute(X "excluded").Value <> "true"
               && m.Descendants(X "seqpnt")
                  |> Seq.exists (fun s -> s.Attribute(X "excluded").Value <> "true"))
          |> Seq.groupBy (fun m ->
               (m.Descendants(X "seqpnt") |> Seq.head).Attribute(X "document").Value)
          |> multiSortByNameAndStartLine
          |> Seq.iter (fun (f, methods) ->
               // For each source file referenced in the .da file,  there  is  a  section
               // containing filename and coverage data:
               //
               //  SF:<absolute path to the source file>
               writer.WriteLine("SF:" + f)
               let fullname (m : XElement) =
                 let fna = m.Attribute(X "fullname")
                 if fna |> isNull
                 then m.Attribute(X "class").Value + "." + m.Attribute(X "name").Value
                 else fna.Value
               // Following is a list of line numbers for each function name found in the
               // source file:
               //
               // FN:<line number of function start>,<function name>
               methods
               |> Seq.iter (fun m ->
                    let l = (lineOfMethod m).ToString(CultureInfo.InvariantCulture)
                    let name = fullname m
                    writer.WriteLine("FN:" + l + "," + name))
               // Next, there is a list of execution counts for each  instrumented  function:
               //
               // FNDA:<execution count>,<function name>
               let hit =
                 methods
                 |> Seq.fold (fun n m ->
                      let v =
                        (m.Descendants(X "seqpnt") |> Seq.head).Attribute(X "visitcount").Value
                      let name = fullname m
                      writer.WriteLine("FNDA:" + v + "," + name)
                      n + (Augment.Increment(v <> "0"))) 0
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
                 |> Seq.collect (fun m -> m.Descendants(X "seqpnt"))
                 |> Seq.filter (fun b ->
                      b.Attribute(X "line").Value
                      |> String.IsNullOrWhiteSpace
                      |> not)
                 |> Seq.fold (fun (f, h) b ->
                      let sl = b.Attribute(X "line").Value
                      let v = b.Attribute(X "visitcount")

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
      | _ ->
          // For each source file referenced in the .da file,  there  is  a  section
          // containing filename and coverage data:
          //
          //  SF:<absolute path to the source file>
          report.Descendants(X "File")
          |> Seq.iter (fun f ->
               writer.WriteLine("SF:" + f.Attribute(X "fullPath").Value)
               let uid = f.Attribute(X "uid").Value
               let p = f.Parent.Parent

               // Following is a list of line numbers for each function name found in the
               // source file:
               //
               // FN:<line number of function start>,<function name>
               let methods =
                 p.Descendants(X "Method")
                 |> Seq.filter (fun m ->
                      m.Descendants(X "FileRef")
                      |> Seq.exists (fun r -> r.Attribute(X "uid").Value = uid))
                 |> Seq.toList

               let FN(ms : XElement list) =
                 ms
                 |> Seq.iter (fun m ->
                      m.Descendants(X "SequencePoint")
                      |> Seq.tryHead
                      |> Option.iter (fun s ->
                           let n = (m.Descendants(X "Name") |> Seq.head).Value
                           let mp = m.Descendants(X "MethodPoint") |> Seq.head
                           let sl = s.Attribute(X "sl").Value
                           if sl
                              |> String.IsNullOrWhiteSpace
                              |> not
                           then
                             writer.WriteLine("FN:" + s.Attribute(X "sl").Value + "," + n)))

               FN methods
               // Next, there is a list of execution counts for each  instrumented  function:
               //
               // FNDA:<execution count>,<function name>
               let FNDA(ms : XElement list) =
                 ms
                 |> Seq.iter (fun m ->
                      m.Descendants(X "SequencePoint")
                      |> Seq.tryHead
                      |> Option.iter (fun s ->
                           let n = (m.Descendants(X "Name") |> Seq.head).Value
                           let mp = m.Descendants(X "MethodPoint") |> Seq.head
                           let sl = s.Attribute(X "sl").Value
                           if sl
                              |> String.IsNullOrWhiteSpace
                              |> not
                           then
                             writer.WriteLine
                               ("FNDA:" + mp.Attribute(X "vc").Value + "," + n)))
               FNDA methods
               // This  list  is followed by two lines containing the number of functions
               // found and hit:
               //
               // FNF:<number of functions found>
               // FNH:<number of function hit>
               writer.WriteLine
                 ("FNF:" + methods.Length.ToString(CultureInfo.InvariantCulture))
               let hit =
                 methods |> List.filter (fun m -> m.Attribute(X "visited").Value = "true")
               writer.WriteLine
                 ("FNH:" + hit.Length.ToString(CultureInfo.InvariantCulture))
               // Branch coverage information is stored which one line per branch:
               //
               // BRDA:<line number>,<block number>,<branch number>,<taken>
               //
               // Block number and branch number are gcc internal  IDs  for  the  branch.
               // Taken  is either '-' if the basic block containing the branch was never
               // executed or a number indicating how often that branch was taken.
               let Branch(ms : XElement list) =
                 let (brf, brh, _) =
                   ms
                   |> Seq.collect (fun m -> m.Descendants(X "BranchPoint"))
                   |> Seq.filter (fun b ->
                        b.Attribute(X "sl").Value
                        |> String.IsNullOrWhiteSpace
                        |> not)
                   |> Seq.fold (fun (f, h, (o, u)) b ->
                        let sl = b.Attribute(X "sl").Value
                        let off = b.Attribute(X "offset").Value
                        let usp = b.Attribute(X "uspid").Value
                        let path = b.Attribute(X "path").Value
                        let vc = b.Attribute(X "vc").Value
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
               Branch methods
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
                 |> Seq.collect (fun m -> m.Descendants(X "SequencePoint"))
                 |> Seq.filter (fun b ->
                      b.Attribute(X "sl").Value
                      |> String.IsNullOrWhiteSpace
                      |> not)
                 |> Seq.fold (fun (f, h) b ->
                      let sl = b.Attribute(X "sl").Value
                      let vc = b.Attribute(X "vc").Value
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

  let Summary (report : XDocument) (format : Base.ReportFormat) result =
    DoWith (fun () -> File.OpenWrite(!path |> Option.get))
      (ConvertReport report format)
    result