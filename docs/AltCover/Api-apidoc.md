
# namespace `AltCover`
```
namespace AltCover
```
## module `Api`
```
module Api = begin
  val Prepare : args:OptionApi.PrepareOptions -> log:OptionApi.LoggingOptions -> int
  val Collect : args:OptionApi.CollectOptions -> log:OptionApi.LoggingOptions -> int
  val ImportModule : unit -> string
  val Version : unit -> System.Version
  val FormattedVersion : unit -> string
end
```

where `int` results are 0 for success and otherwise for failure (this would be the return code of the operation if run as a command-line function)

`FormattedVersion` return is of the form "AltCover version #.#.##.##" as per the command-line "Version" option.
