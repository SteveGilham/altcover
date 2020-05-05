

# namespace `AltCover`
```
namespace AltCover
```
## module `OptionApi`
```
  [<RequireQualifiedAccess>]
  module OptionApi = begin
```
### type `ValidatedCommandLine`
```
    [<NoComparison>]
    type ValidatedCommandLine =
      { Command: string list
        Errors: seq<string> }
      with
        override ToString : unit -> string
      end
```
Holds the composed command line in `Command`, and any validations errors in `Errors`.

The `ToString()` override formats the outcome for pretty-printing











### type `CollectOptions`


```
    [<NoComparison>]
    type CollectOptions =
      | Primitive of Primitive.CollectOptions
      | TypeSafe of TypeSafe.CollectOptions
      with
        member Cobertura : System.String
        member CommandLine : seq<string>
        member Executable : System.String
        member ExposeReturnCode : bool
        member LcovReport : System.String
        member OutputFile : System.String
        member RecorderDirectory : System.String
        member SummaryFormat : System.String
        member Threshold : System.String
        member WorkingDirectory : System.String
```

```
        member Validate : bool -> string []
      end
```
`Validate` does simple checking of the arguments without causing any changes to the system; set the input argument `true` if the Prepare step has already run (and there should be instrumented code the `RecorderDirectory`; returns all the problems that the application command-line could report, so empty is success.







The property members corresponding to the command line options read the values from the contained instance; null strings will be retrurned as null, but null sequences will be returned as empty ones.

 Values that are not applicable to the use case or platform are silently ignored.
### type `PrepareOptions`


```
    [<NoComparison>]
    type PrepareOptions =
      | Primitive of Primitive.PrepareOptions
      | TypeSafe of TypeSafe.PrepareOptions
      with
        member AssemblyExcludeFilter : System.String list
        member AssemblyFilter : System.String list
        member AttributeFilter : System.String list
        member BranchCover : bool
        member CallContext : System.String list
        member CommandLine : seq<System.String>
        member Defer : bool
        member Dependencies : System.String list
        member ExposeReturnCode : bool
        member FileFilter : System.String list
        member InPlace : bool
        member InputDirectories : System.String list
        member Keys : System.String list
        member LineCover : bool
        member LocalSource : bool
        member MethodFilter : System.String list
        member MethodPoint : bool
        member OutputDirectories : System.String list
        member PathFilter : System.String list
        member ReportFormat : string
        member Save : bool
        member ShowGenerated : bool
        member ShowStatic : string
        member Single : bool
        member SourceLink : bool
        member StrongNameKey : System.String
        member SymbolDirectories : System.String list
        member TypeFilter : System.String list
        member VisibleBranches : bool
        member XmlReport : System.String
        member ZipFile : bool
```

```
        member Validate : unit -> string []
      end
```
`Validate` does simple checking of the arguments without causing any changes to the system; returns all the problems that the application command-line could report, so empty is success.







The property members corresponding to the command line options read the values from the contained instance; null strings will be retrurned as null, but null sequences will be returned as empty ones.

 Values that are not applicable to the use case or platform are silently ignored.

### type `LoggingOptions`


```
    type LoggingOptions =
      | Primitive of Primitive.LoggingOptions
      with
        member Echo : (System.String -> unit)
        member Error : (System.String -> unit)
        member Info : (System.String -> unit)
        member Warn : (System.String -> unit)
        static member
          ActionAdapter : action:System.Action<System.String> ->
                            (System.String -> unit)
        static member Create : unit -> LoggingOptions
      end
```
`Create()` returns a pure sink instance; `ActionAdapter` is a helper for C# use, and the others just return from the underlying structure.

```
  end
```
