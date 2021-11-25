open System
open System.IO
open System.Reflection

open Mono.CSharp

let FixMVId f =
  // Fix up symbol file to have the MVId emitted by the System.Reflection.Emit code
  let assembly = Assembly.LoadFrom(Path.GetFullPath f)

  let mvid = assembly.ManifestModule.ModuleVersionId
  let symbols = System.IO.File.ReadAllBytes(f + ".mdb")

  mvid.ToByteArray()
  |> Array.iteri (fun i x -> symbols.[i + 16] <- x)

  System.IO.File.WriteAllBytes(f + ".mdb", symbols)

[<EntryPoint>]
let main argv =
  Location.InEmacs <- (Environment.GetEnvironmentVariable("EMACS") = "t")
  let commandLineParser = CommandLineParser(Console.Out)
  let compilerSettings = commandLineParser.ParseArguments(argv);
  if isNull compilerSettings
  then 1
  else
    if commandLineParser.HasBeenStopped
    then 0
    else
      let context = CompilerContext(compilerSettings, ConsoleReportPrinter())
      let assembly = context.GetType().Assembly
      let driverType =
        assembly.GetTypes()
        |> Seq.filter (fun t -> t.FullName = "Mono.CSharp.Driver")
        |> Seq.head
      let makeDriver = driverType.GetConstructors() |> Seq.head
      let driver = makeDriver.Invoke([| context :> obj |])
      let compile = driverType.GetMethod("Compile")
      let ok = compile.Invoke(driver, [||]) :?> bool
      let getReport = driverType.GetProperty("Report", BindingFlags.NonPublic ||| BindingFlags.Instance)
      let report = getReport.GetValue(driver) :?> Report
      if (ok && report.Errors = 0)
      then
        if (report.Warnings > 0)
        then
          Console.WriteLine("Compilation succeeded - {0} warning(s)", report.Warnings)
        FixMVId compilerSettings.OutputFile
        Environment.Exit(0)
        0
      else
        Console.WriteLine("Compilation failed: {0} error(s), {1} warnings", report.Errors, report.Warnings);
        Environment.Exit(1);
        1