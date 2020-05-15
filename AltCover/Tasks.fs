namespace AltCover

open System
#if NETCOREAPP2_0
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
#endif
open System.Diagnostics.CodeAnalysis

open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open TaskIO

[<SuppressMessage(
  "Gendarme.Rules.Smells",
  "AvoidLargeClassesRule",
  Justification="So many options available, so many compiler generated fields");
  AutoSerializable(false)>]
type Prepare() =
  inherit Task(null)
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  member val internal ACLog : AltCover.LoggingOptions option = None with get, set

  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val InputDirectories : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val OutputDirectories : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val SymbolDirectories : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val Dependencies : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val Keys : string array = [||] with get, set
  member val StrongNameKey = String.Empty with get, set
  member val XmlReport = String.Empty with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val FileFilter : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val AssemblyFilter : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val AssemblyExcludeFilter : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val TypeFilter : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val MethodFilter : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val AttributeFilter : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val PathFilter : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val TopLevel : string array = [||] with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val CallContext : string array = [||] with get, set
  member val LocalSource = false with get, set
  member val ReportFormat = "OpenCover" with get, set
  member val InPlace = true with get, set
  member val Save = true with get, set
  member val ZipFile = false with get, set // work around Gendarme insistence on non-default values only
  member val MethodPoint = false with get, set // work around Gendarme insistence on non-default values only
  member val SingleVisit = false with get, set // work around Gendarme insistence on non-default values only
  member val LineCover = false with get, set
  member val BranchCover = false with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val CommandLine : string array = [||] with get, set
  member val SourceLink = false with get, set
  member val Defer = false with get, set
  member val VisibleBranches = false with get, set
  member val ShowStatic = "-" with get, set
  member val ShowGenerated = false with get, set
  member val ExposeReturnCode = true with get, set

  member self.Message text = base.Log.LogMessage(MessageImportance.High, text)
  override self.Execute() =
    let log =
      Option.getOrElse
        (AltCover.LoggingOptions.Primitive
          { Primitive.LoggingOptions.Create() with
              Failure = base.Log.LogError
              Warn = base.Log.LogWarning
              Info = self.Message }) self.ACLog

    let task =
      AltCover.PrepareOptions.Primitive
        { InputDirectories = self.InputDirectories
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
          TopLevel = self.TopLevel
          CallContext = self.CallContext
          ReportFormat = self.ReportFormat
          InPlace = self.InPlace
          Save = self.Save
          ZipFile = self.ZipFile
          MethodPoint = self.MethodPoint
          SingleVisit = self.SingleVisit
          LineCover = self.LineCover
          BranchCover = self.BranchCover
          CommandLine = self.CommandLine
          ExposeReturnCode = self.ExposeReturnCode
          SourceLink = self.SourceLink
          Defer = self.Defer
          LocalSource = self.LocalSource
          VisibleBranches = self.VisibleBranches
          ShowStatic = self.ShowStatic
          ShowGenerated = self.ShowGenerated }

    Command.Prepare task log = 0

[<AutoSerializable(false)>]
type Collect() =
  inherit Task(null)

  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  member val internal ACLog : AltCover.LoggingOptions option = None with get, set

  [<Required>]
  member val RecorderDirectory = String.Empty with get, set

  member val WorkingDirectory = String.Empty with get, set
  member val Executable = String.Empty with get, set
  [<SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'LCov' is jargon")>]
  member val LcovReport = String.Empty with get, set
  member val Threshold = String.Empty with get, set
  [<SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'  member val Cobertura = String.Empty with get, set
' is jargon")>]
  member val Cobertura = String.Empty with get, set
  member val OutputFile = String.Empty with get, set
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "MSBuild tasks use arrays")>]
  member val CommandLine : string array = [||] with get, set
  member val SummaryFormat = String.Empty with get, set
  member val ExposeReturnCode = true with get, set

  [<Output>]
  [<SuppressMessage("Microsoft.Performance", "CA1822:MarkMembersAsStatic",
                                                    Justification =
                                                      "Instance property needed");
    SuppressMessage("Gendarme.Rules.Correctness",
                                                      "MethodCanBeMadeStaticRule",
                                                      Justification =
                                                       "Instance property needed")>]
  member self.Summary = Command.Summary()

  member self.Message text = base.Log.LogMessage(MessageImportance.High, text)
  override self.Execute() =
    let log =
      Option.getOrElse
        (AltCover.LoggingOptions.Primitive
          { Primitive.LoggingOptions.Create() with
              Failure = base.Log.LogError
              Warn = base.Log.LogWarning
              Info = self.Message }) self.ACLog

    let task =
      AltCover.CollectOptions.Primitive
        { RecorderDirectory = self.RecorderDirectory
          WorkingDirectory = self.WorkingDirectory
          Executable = self.Executable
          LcovReport = self.LcovReport
          Threshold = self.Threshold
          Cobertura = self.Cobertura
          OutputFile = self.OutputFile
          CommandLine = self.CommandLine
          ExposeReturnCode = self.ExposeReturnCode
          SummaryFormat = self.SummaryFormat }

    Command.Collect task log = 0

[<AutoSerializable(false)>]
type PowerShell() =
  inherit Task(null)

  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  member val internal IO = AltCover.LoggingOptions.Primitive
                             { Primitive.LoggingOptions.Create() with
                                 Failure = base.Log.LogError
                                 Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Command.ImportModule()
    self.IO.Apply()
    r |> Output.warn
    true

[<AutoSerializable(false)>]
type GetVersion() =
  inherit Task(null)

  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  member val internal IO = AltCover.LoggingOptions.Primitive
                             { Primitive.LoggingOptions.Create() with
                                 Failure = base.Log.LogError
                                 Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Command.FormattedVersion()
    self.IO.Apply()
    r |> Output.warn
    true

type Echo() =
  inherit Task(null)

  [<Required>]
  member val Text = String.Empty with get, set

  [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
    Justification="Queen's English, m80!")>]
  member val Colour = String.Empty with get, set

  override self.Execute() =
    if self.Text
       |> String.IsNullOrWhiteSpace
       |> not
    then
      let original = Console.ForegroundColor
      try
        TaskIO.colourize self.Colour
        printfn "%s" self.Text
      finally
        Console.ForegroundColor <- original

    true
#if NETCOREAPP2_0
type RunSettings() =
  inherit Task(null)

  member val TestSetting = String.Empty with get, set

  [<Output>]
  member val Extended = String.Empty with get, set

  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  member val internal DataCollector = "AltCover.DataCollector.dll" with get, set

  override self.Execute() =
    let tempFile = Path.GetTempFileName()

    try
      let settings =
        if self.TestSetting
           |> String.IsNullOrWhiteSpace
           |> not
           && self.TestSetting |> File.Exists then
          try
            use s = File.OpenRead(self.TestSetting)
            XDocument.Load(s)
          with
          | :? IOException
          | :? XmlException -> XDocument()
        else
          XDocument()

      let X n = XName.Get n

      let ensureHas (parent : XContainer) childName =
        match parent.Descendants(X childName) |> Seq.tryHead with
        | Some child -> child
        | _ ->
            let extra = XElement(X childName)
            parent.Add extra
            extra

      let here = Assembly.GetExecutingAssembly().Location
      let expected = Path.Combine(Path.GetDirectoryName(here), self.DataCollector)

      let result = File.Exists(expected)

      if result then
        let rs = ensureHas settings "RunSettings"
        let ip1 = ensureHas rs "InProcDataCollectionRunSettings"
        let ip2 = ensureHas ip1 "InProcDataCollectors"

        let name = AssemblyName.GetAssemblyName(expected)
        let altcover =
          XElement
            (X "InProcDataCollector", XAttribute(X "friendlyName", "AltCover"),
             XAttribute
               (X "uri",
                "InProcDataCollector://AltCover/Recorder/" + name.Version.ToString()),
             XAttribute
               (X "assemblyQualifiedName", "AltCover.DataCollector, " + name.FullName),
             XAttribute(X "codebase", expected),
             XElement(X "Configuration", XElement(X "Offload", XText("true"))))
        ip2.Add(altcover)

        self.Extended <- Path.ChangeExtension(tempFile, ".altcover.runsettings")
        settings.Save(self.Extended)

      result
    finally
      File.Delete(tempFile)
#endif