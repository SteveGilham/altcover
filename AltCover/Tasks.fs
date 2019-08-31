namespace AltCover

open System
#if NETCOREAPP2_0
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
#endif

open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open Augment

[<RequireQualifiedAccess>]
module Api =
  let Prepare (args : FSApi.PrepareParams) (log : FSApi.Logging) =
    log.Apply()
    args
    |> FSApi.Args.Prepare
    |> List.toArray
    |> Main.EffectiveMain

  let Collect (args : FSApi.CollectParams) (log : FSApi.Logging) =
    log.Apply()
    FSApi.Args.Collect args
    |> List.toArray
    |> Main.EffectiveMain

  let Summary () =
    Runner.Summary.ToString()

  let mutable internal store = String.Empty
  let private writeToStore s = store <- s
  let internal LogToStore = FSApi.Logging.Primitive { Primitive.Logging.Create() with Info = writeToStore }

  let internal GetStringValue s =
    writeToStore String.Empty
    LogToStore.Apply()
    [| s |]
    |> Main.EffectiveMain
    |> ignore
    store

  let Ipmo() = GetStringValue "ipmo"
  let Version() = GetStringValue "version"

  let internal Colourize name =
    let ok, colour = Enum.TryParse<ConsoleColor>(name, true)
    if ok then Console.ForegroundColor <- colour

type Prepare() =
  inherit Task(null)
  member val internal ACLog : FSApi.Logging option = None with get, set

  member val InputDirectories : string array = [||] with get, set
  member val OutputDirectories : string array = [||] with get, set
  member val SymbolDirectories : string array = [||] with get, set
  member val Dependencies : string array = [||] with get, set
  member val Keys : string array = [| |] with get, set
  member val StrongNameKey = String.Empty with get, set
  member val XmlReport = String.Empty with get, set
  member val FileFilter : string array = [||] with get, set
  member val AssemblyFilter : string array = [||] with get, set
  member val AssemblyExcludeFilter : string array = [||] with get, set
  member val TypeFilter : string array = [||] with get, set
  member val MethodFilter : string array = [||] with get, set
  member val AttributeFilter : string array = [||] with get, set
  member val PathFilter : string array = [||] with get, set
  member val CallContext : string array = [||] with get, set
  member val LocalSource = false with get, set
  member val OpenCover = true with get, set
  member val InPlace = true with get, set
  member val Save = true with get, set
  member val Single = false with get, set // work around Gendarme insistence on non-default values only
  member val LineCover = false with get, set
  member val BranchCover = false with get, set
  member val CommandLine : string array = [||] with get, set
  member val SourceLink = false with get, set
  member val Defer = false with get, set

  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      Option.getOrElse (FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                  Warn = base.Log.LogWarning
                                                                                  Info = self.Message }) self.ACLog

    let task =
      FSApi.PrepareParams.Primitive { InputDirectories = self.InputDirectories
                                      OutputDirectories = self.OutputDirectories
                                      SymbolDirectories = self.SymbolDirectories
                                      Dependencies = self.Dependencies
                                      Keys = self.Keys
                                      StrongNameKey = self.StrongNameKey
                                      XmlReport = self.XmlReport
                                      FileFilter = self.FileFilter
                                      AssemblyFilter = self.AssemblyFilter
                                      AssemblyExcludeFilter = self.AssemblyExcludeFilter
                                      TypeFilter = self.TypeFilter
                                      MethodFilter = self.MethodFilter
                                      AttributeFilter = self.AttributeFilter
                                      PathFilter = self.PathFilter
                                      CallContext = self.CallContext
                                      OpenCover = self.OpenCover
                                      InPlace = self.InPlace
                                      Save = self.Save
                                      Single = self.Single
                                      LineCover = self.LineCover
                                      BranchCover = self.BranchCover
                                      CommandLine = self.CommandLine
                                      ExposeReturnCode = true
                                      SourceLink = self.SourceLink
                                      Defer = self.Defer
                                      LocalSource = self.LocalSource}

    Api.Prepare task log = 0

type Collect() =
  inherit Task(null)

  member val internal ACLog : FSApi.Logging option = None with get, set

  [<Required>]
  member val RecorderDirectory = String.Empty with get, set

  member val WorkingDirectory = String.Empty with get, set
  member val Executable = String.Empty with get, set
  member val LcovReport = String.Empty with get, set
  member val Threshold = String.Empty with get, set
  member val Cobertura = String.Empty with get, set
  member val OutputFile = String.Empty with get, set
  member val CommandLine : string array = [||] with get, set
  member val SummaryFormat = String.Empty with get, set

  [<Output>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance",
                                                    "CA1822",
                                                    Justification = "Instance property needed")>]
  member self.Summary with get() = Api.Summary()
  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      Option.getOrElse (FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                  Warn = base.Log.LogWarning
                                                                                  Info = self.Message }) self.ACLog

    let task =
      FSApi.CollectParams.Primitive { RecorderDirectory = self.RecorderDirectory
                                      WorkingDirectory = self.WorkingDirectory
                                      Executable = self.Executable
                                      LcovReport = self.LcovReport
                                      Threshold = self.Threshold
                                      Cobertura = self.Cobertura
                                      OutputFile = self.OutputFile
                                      CommandLine = self.CommandLine
                                      ExposeReturnCode = true
                                      SummaryFormat = self.SummaryFormat }

    Api.Collect task log = 0

type PowerShell() =
  inherit Task(null)

  member val internal IO = FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                     Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Ipmo()
    self.IO.Apply()
    r |> Output.Warn
    true

type GetVersion() =
  inherit Task(null)

  member val internal IO = FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                     Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Version()
    self.IO.Apply()
    r |> Output.Warn
    true

type Echo() =
  inherit Task(null)

  [<Required>]
  member val Text = String.Empty with get, set
  member val Colour = String.Empty with get, set

  override self.Execute() =
    if self.Text |> String.IsNullOrWhiteSpace |>  not then
      let original = Console.ForegroundColor
      try
        Api.Colourize self.Colour
        printfn "%s" self.Text
      finally
        Console.ForegroundColor <- original

    true
#if NETCOREAPP2_0
type RunSettings() =
  inherit Task(null)

  member val TestSetting  = String.Empty with get, set
  [<Output>]
  member val Extended = String.Empty with get, set

  member val internal DataCollector = "AltCover.DataCollector.dll" with get, set

  override self.Execute() =
    let tempFile = Path.GetTempFileName()

    try
      let settings =
        if self.TestSetting |> String.IsNullOrWhiteSpace |> not &&
           self.TestSetting |> File.Exists
        then
          try
            use s = File.OpenRead(self.TestSetting)
            XDocument.Load(s)
          with
          | :? IOException
          | :? XmlException -> XDocument()
        else XDocument()

      let X n = XName.Get n

      let ensureHas (parent:XContainer) childName =
        match parent.Descendants(X childName) |> Seq.tryHead with
        | Some child -> child
        | _ -> let extra = XElement(X childName)
               parent.Add extra
               extra
      let here = Assembly.GetExecutingAssembly().Location
      let expected = Path.Combine(Path.GetDirectoryName(here),
                                   self.DataCollector)

      let result = File.Exists(expected)

      if result then
        let rs = ensureHas settings "RunSettings"
        let ip1 = ensureHas rs "InProcDataCollectionRunSettings"
        let ip2 = ensureHas ip1 "InProcDataCollectors"

        let name = AssemblyName.GetAssemblyName(expected)
        let altcover = XElement(X "InProcDataCollector",
                        XAttribute(X "friendlyName", "AltCover"),
                        XAttribute(X "uri", "InProcDataCollector://AltCover/Recorder/" + name.Version.ToString()),
                        XAttribute(X "assemblyQualifiedName", "AltCover.DataCollector, " + name.FullName),
                        XAttribute(X "codebase", expected),
                        XElement(X "Configuration",
                            XElement(X "Offload", XText("true"))))
        ip2.Add(altcover)

        self.Extended <- Path.ChangeExtension(tempFile, ".altcover.runsettings")
        settings.Save(self.Extended)

      result
    finally
      File.Delete(tempFile)
#endif