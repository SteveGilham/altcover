namespace Tests

open System.Xml.Linq

open AltCover

[<AutoOpen>]
module CoverageFiles =

  let MonoBaseline =
    "<?xml-stylesheet type='text/xsl' href='coverage.xsl'?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
  <module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null\">
    <method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"false\" instrumented=\"true\" fullname=\"System.Void TouchTest.Program.Main(System.String[])\">
      <seqpnt visitcount=\"0\" line=\"11\" column=\"3\" endline=\"11\" endcolumn=\"4\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"12\" column=\"23\" endline=\"12\" endcolumn=\"24\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"4\" endline=\"13\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"12\" endline=\"13\" endcolumn=\"13\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"14\" column=\"4\" endline=\"14\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"5\" endline=\"15\" endcolumn=\"6\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"60\" endline=\"15\" endcolumn=\"61\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"13\" endline=\"15\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"16\" column=\"4\" endline=\"16\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"18\" column=\"4\" endline=\"18\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"5\" endline=\"19\" endcolumn=\"6\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"13\" endline=\"19\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"20\" column=\"4\" endline=\"20\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"21\" column=\"3\" endline=\"21\" endcolumn=\"4\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
    </method>
  </module>
</coverage>"

  [<TailCall>]
  let rec RecursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length
    test' <@ rcount = ecount @> ("Mismatch at depth " + depth.ToString())

    Seq.zip result expected
    |> Seq.iter (fun (r: XElement, e: XElement) ->
      test <@ r.Name = e.Name @>
      let ra = r.Attributes()
      let ea = e.Attributes()

      Seq.zip ra ea
      |> Seq.iter (fun (a1: XAttribute, a2: XAttribute) ->
        test <@ a1.Name = a2.Name @>

        match a1.Name.ToString() with
        | "profilerVersion"
        | "driverVersion"
        | "moduleId"
        | "metadataToken"
        | "startTime"
        | "measureTime" -> ()
        | "document" ->
          test'
            <@ a1.Value.Replace("\\", "/").EndsWith(a2.Value.Replace("\\", "/")) @>
            (a1.Name.ToString()
             + " : "
             + r.ToString()
             + " -> document")
        | "visitcount" ->
          let expected = Maybe zero "0" a2.Value
          test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
        | _ ->
          test'
            <@ a1.Value.Replace("\\", "/") = a2.Value.Replace("\\", "/") @>
            (r.ToString() + " -> " + a1.Name.ToString()))

      RecursiveValidate (r.Elements()) (e.Elements()) (depth + 1) zero)