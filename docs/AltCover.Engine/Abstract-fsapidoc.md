<!-- DO NOT EDIT: generated by ./Build/prepareDocumentation.ps1 for .\AltCover.Engine\Abstract.fsi -->


# namespace `AltCover`
```
namespace AltCover
```






## module `Abstract`

```
module Abstract =
```

This represents the weakly ("stringly") typed equivalent of the command line options in a C# friendly manner
as interfaces with the values expressed as read-only properties.

### interface `ICollectOptions`

The members correspond to the like-named command line options for `AltCover Runner`, except
* `ExposeReturnCode` being the converse of the `dropReturnCode` option
* `CommandLine` being the material after a `-- `

```
  type ICollectOptions =
    abstract member RecorderDirectory : String with get
    abstract member WorkingDirectory : String with get
    abstract member Executable : String with get
    abstract member LcovReport : String with get
    abstract member Threshold : String with get
    abstract member Cobertura : String with get
    abstract member Packages : IEnumerable<String> with get
    abstract member OutputFile : String with get
    abstract member CommandLine : IEnumerable<String> with get
    abstract member ExposeReturnCode : bool with get
    abstract member SummaryFormat : String with get
    abstract member Verbosity : System.Diagnostics.TraceLevel with get
```
### interface `IPrepareOptions`

The members correspond to the like-named command line options for `AltCover`, except
* `ExposeReturnCode` being the converse of the `dropReturnCode` option
* `CommandLine` being the material after a `-- `

```
  type IPrepareOptions =
    abstract member InputDirectories : IEnumerable<String> with get
    abstract member OutputDirectories : IEnumerable<String> with get
    abstract member SymbolDirectories : IEnumerable<String> with get
    abstract member Dependencies : IEnumerable<String> with get
    abstract member Keys : IEnumerable<String> with get
    abstract member StrongNameKey : String with get
    abstract member Report : String with get
    abstract member FileFilter : IEnumerable<String> with get
    abstract member AssemblyFilter : IEnumerable<String> with get
    abstract member AssemblyExcludeFilter : IEnumerable<String> with get
    abstract member TypeFilter : IEnumerable<String> with get
    abstract member MethodFilter : IEnumerable<String> with get
    abstract member AttributeFilter : IEnumerable<String> with get
    abstract member PathFilter : IEnumerable<String> with get
    abstract member AttributeTopLevel : IEnumerable<String> with get
    abstract member TypeTopLevel : IEnumerable<String> with get
    abstract member MethodTopLevel : IEnumerable<String> with get
    abstract member CallContext : IEnumerable<String> with get
    abstract member ReportFormat : String with get
    abstract member InPlace : bool with get
    abstract member Save : bool with get
    abstract member ZipFile : bool with get
    abstract member MethodPoint : bool with get
    abstract member All : bool with get
    abstract member LineCover : bool with get
    abstract member BranchCover : bool with get
    abstract member CommandLine : IEnumerable<String> with get
    abstract member ExposeReturnCode : bool with get
    abstract member SourceLink : bool with get
    abstract member Eager : bool with get
    abstract member LocalSource : bool with get
    abstract member VisibleBranches : bool with get
    abstract member ShowStatic : string with get
    abstract member ShowGenerated : bool with get
    abstract member Verbosity : System.Diagnostics.TraceLevel with get
    abstract member Trivia : bool with get
    abstract member OutputRoot : string with get
    abstract member Portable : bool with get
```

### interface `ILoggingOptions`

Destinations for user level output at various levels of success.  `Echo` is for the
command line and usage warninings only.

```
  type ILoggingOptions =
    abstract member Info : Action<String> with get
    abstract member Warn : Action<String> with get
    abstract member Failure : Action<String> with get
    abstract member Echo : Action<String> with get
    abstract member Verbose : Action<String> with get

