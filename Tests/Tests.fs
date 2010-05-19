namespace Tests

open System
open System.IO
open System.Reflection
open System.Xml.Linq
 
open AltCover
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() = class
  [<Test>]
  member self.CheckPdbMapping() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location;
    let dir = match AltCover.ProgramDatabase.PdbPath(where) with
              | None -> "\\.."
              | _ -> String.Empty
  
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + dir)
    files 
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) 
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x -> not (x.EndsWith("nunit.framework.dll", StringComparison.OrdinalIgnoreCase))
                         && not (x.EndsWith("Microsoft.Cci.dll", StringComparison.OrdinalIgnoreCase)))
    |> Seq.iter( fun x ->
      let pdb = AltCover.ProgramDatabase.PdbPath(x)
      match pdb with
      | None -> Assert.Fail("No .pdb for " + x)
      | Some name -> 
         let probe = Path.ChangeExtension(x, ".pdb")
         let file = new FileInfo(probe)
         let filename = file.Name
         Assert.That(name, Does.EndWith("\\" + filename), x + " -> " + name)
    )

  [<Test; Ignore("awaits refactoring")>]
  member self.CheckPdbLoading() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location;
    let dir = match AltCover.ProgramDatabase.PdbPath(where) with
              | None -> "\\.."
              | _ -> String.Empty
    let path = Path.Combine(Path.GetDirectoryName(where) + dir, "Sample1.exe")
    let symbols = AltCover.ProgramDatabase.LoadAssembly(path)
    Assert.That(symbols.Symbols, Is.Not.Empty, "should be some symbols")
    Assert.That(symbols.Symbols.Keys.Count, Is.EqualTo(1), "the number we expect from NCoverExplorer")
    
  [<Test; Explicit>] // Until we instrument recorder
  member self.FindVisitMethod() =
    let trace  = typeof<AltCover.Recorder.Tracer>
    let other = trace.Assembly.GetExportedTypes()
                |> Seq.find (fun (t:Type) -> t.Name.Contains("Instance"))
    Assert.That(other.FullName, Is.EqualTo("AltCover.Recorder.Instance"))
    let v = other.GetMethod("Visit")
    Assert.That(v, Is.Not.Null)

  static member TTBaseline = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"coverage.xsl\" type=\"text/xsl\"?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
<module moduleId=\"1\" name=\"TouchTest.exe\" assembly=\"TouchTest\" assemblyIdentity=\"TouchTest, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\">
<method name=\"Main\" excluded=\"false\" instrumented=\"true\" class=\"TouchTest.Program\">
<seqpnt visitcount=\"1\" line=\"21\" column=\"9\"  endline=\"21\" endcolumn=\"10\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"20\" column=\"13\" endline=\"20\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"19\" column=\"17\" endline=\"19\" endcolumn=\"62\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"0\" line=\"18\" column=\"13\" endline=\"18\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"16\" column=\"13\" endline=\"16\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"15\" column=\"17\" endline=\"15\" endcolumn=\"63\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"14\" column=\"13\" endline=\"14\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"13\" column=\"13\" endline=\"13\" endcolumn=\"33\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"12\" column=\"13\" endline=\"12\" endcolumn=\"36\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
<seqpnt visitcount=\"1\" line=\"11\" column=\"9\"  endline=\"11\" endcolumn=\"10\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
</method>
</module>
</coverage>"
  
    
  static member private RecursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length
    
    Assert.That(rcount, Is.EqualTo(ecount), "Mismatch at depth " + depth.ToString())
    
    Seq.zip result expected |> Seq.iter (fun ((r:XElement), (e:XElement)) -> 
            Assert.That(r.Name, Is.EqualTo(e.Name), "Expected name " + e.Name.ToString())
            let ra = r.Attributes()
            let ea = e.Attributes()
            Seq.zip ra ea |> Seq.iter (fun ((a1:XAttribute), (a2:XAttribute)) ->
                    Assert.That(a1.Name, Is.EqualTo(a2.Name))
                    match a1.Name.ToString() with
                    | "startTime"
                    | "measureTime" -> ()
                    | "document" -> Assert.That(a1.Value, Does.EndWith(a2.Value), 
                                      a1.Name.ToString() + " : " + r.ToString() + " -> document")
                    | "visitcount" -> let expected = if zero then "0" else a2.Value 
                                      Assert.That(a1.Value, Is.EqualTo(expected), r.ToString() + " -> visitcount")
                    | _ -> Assert.That(a1.Value, Is.EqualTo(a2.Value), r.ToString() + " -> " + a1.Name.ToString())
                )

            AltCoverTests.RecursiveValidate (r.Elements()) (e.Elements()) (depth+1) zero)
  

  [<Test; Ignore("awaits refactoring")>]
  member self.ValidateTouchTestReportOnly() =
    let visitor = Report.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location;
    let dir = match AltCover.ProgramDatabase.PdbPath(where) with
              | None -> "\\.."
              | _ -> String.Empty
    let path = Path.Combine(Path.GetDirectoryName(where) + dir, "Sample1.exe")
    
    Visitor.Visit [ visitor ] (Visitor.ToSeq path)
  
    let doc = Report.ReportDocument()
    Console.WriteLine(doc.ToString())
    let baseline = XDocument.Load(new System.IO.StringReader(AltCoverTests.TTBaseline))
    let result = doc.Elements()
    let expected = baseline.Elements()
    AltCoverTests.RecursiveValidate result expected 0 true
    
end    
