// # namespace AltCover
// ```
namespace AltCover
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
// ```
// ## type Prepare
// ```
type Prepare =
  class
    inherit Task
    new : unit -> Prepare
    override Execute : unit -> bool
    member Message : text:string -> unit
    member AssemblyExcludeFilter : string array with get, set
    member AssemblyFilter : string array with get, set
    member AttributeFilter : string array with get, set
    member BranchCover : bool with get, set
    member CallContext : string array with get, set
    member CommandLine : string array with get, set
    member Defer : bool with get, set
    member Dependencies : string array with get, set
    member ExposeReturnCode : bool with get, set
    member FileFilter : string array with get, set
    member InPlace : bool with get, set
    member InputDirectories : string array with get, set
    member Keys : string array with get, set
    member LineCover : bool with get, set
    member LocalSource : bool with get, set
    member MethodFilter : string array with get, set
    member MethodPoint : bool with get, set
    member OutputDirectories : string array with get, set
    member PathFilter : string array with get, set
    member ReportFormat : string with get, set
    member Save : bool with get, set
    member ShowGenerated : bool with get, set
    member ShowStatic : string with get, set
    member Single : bool with get, set
    member SourceLink : bool with get, set
    member StrongNameKey : string with get, set
    member SymbolDirectories : string array with get, set
    member TypeFilter : string array with get, set
    member VisibleBranches : bool with get, set
    member XmlReport : string with get, set
    member ZipFile : bool with get, set
  end
// ```
// ## type Collect
// ```
type Collect =
  class
    inherit Task
    new : unit -> Collect
    override Execute : unit -> bool
    member Message : text:string -> unit
    [<Output>]
    member Summary : string
    member Cobertura : string with get, set
    member CommandLine : string array with get, set
    member Executable : string with get, set
    member ExposeReturnCode : bool with get, set
    member LcovReport : string with get, set
    member OutputFile : string with get, set
    [<Required>]
    member RecorderDirectory : string with get, set
    member SummaryFormat : string with get, set
    member Threshold : string with get, set
    member WorkingDirectory : string with get, set
  end
// ```
// ## type PowerShell
// ```
type PowerShell =
  class
    inherit Task
    new : unit -> PowerShell
    override Execute : unit -> bool
  end
// ```
// ## type GetVersion
// ```
type GetVersion =
  class
    inherit Task
    new : unit -> GetVersion
    override Execute : unit -> bool
  end
// ```
// ## type Echo
// ```
type Echo =
  class
    inherit Task
    new : unit -> Echo
    override Execute : unit -> bool
    member Colour : string with get, set
    [<Required>]
    member Text : string with get, set
  end
// ```
#if NETCOREAPP2_0
// ## type RunSettings
// ```
type RunSettings =
  class
    inherit Task
    new : unit -> RunSettings
    override Execute : unit -> bool
    [<Output>]
    member Extended : string with get, set
    member TestSetting : string with get, set
  end
#endif