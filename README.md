# altcover
Instrumenting coverage tool for .net, reimplemented and extended from [dot-net-coverage](https://github.com/SteveGilham/dot-net-coverage)

## Why altcover?
As the name suggests, it's an alternative coverage approach.  Rather than working by hooking the .net profiling API at run-time, it works by weaving the same sort of extra IL into the assemblies of interest ahead of execution.  This means that it should work pretty much everywhere, so long as the executing process has write access to the results file.

In particular, this approach supports Mono, as long as suitable `.mdb` symbols are available.  The major limitation here is that the `.mdb` format only stores the start location in the source of any code sequence point, and not the end; consequently nicely coloured reports may be a bit patchy.

## Building

You will need Visual Studio VS2017 (Community Edition) v15.5 or later with F# language support.  The NUnit3 Test Runner will simplify the basic in-IDE development cycle

### Bootstrapping

Running `.\Build\get-nuget.ps1` pre-installs all the NuGet packages, including, most importantly, the Fake build system.

### Normal builds

Running `.\fake.bat` performs a full build/test/package process.

## Usage

The full command line is 

```AltCover [/i[nputDirectory]=VALUE] [/o[utputDirectory]=VALUE] [/sn|strongNameKey=VALUE] [/x[mlReport]=VALUE] [/f[ileFilter]=VALUE] [/s|assemblyFilter=VALUE] [/t|typeFilter=VALUE] [/m|methodFilter=VALUE] [/a|attributeFilter=VALUE] [/?|h[elp]] [-- command arguments]```

In detail

* `i|inputDirectory=` : The folder containing assemblies to instrument (default: current directory)
* `o|outputDirectory=` : The folder to receive the instrumented assemblies and their companions (default: sub-folder `.\\__Instrumented` of the current directory)"
* `sn|strongNameKey=` : The default strong naming key to apply to the instrumented rewrites of strong-named imput assemblies (default: None)
* `k|key=` : any other strong-name key to use (default: None; may repeat)
* `x|xmlReport=` : The output report template (traditional NCover format) file (default: coverage.xml in the current directory)
* `f|fileFilter=` : file name to exclude from instrumentation (may repeat)
* `s|assemblyFilter=` : assembly name to exclude from instrumentation (may repeat)
* `t|typeFilter=` : type name to exclude from instrumentation (may repeat)
* `m|methodFilter=` : method name to exclude from instrumentation (may repeat)
* `a|attributeFilter=` : attribute name to exclude from instrumentation (may repeat)
* `?|help|h` : Prints out the options."
* `--` : the rest of the command line is treated as a command to execute after performing instrumentation

Coverage statistics are written to the file nominated by the `x|xmlReport=` parameter as instrumented assemblies are unloaded from an executing AppDomain, even if this is days or weeks later.  In practice the instrumented assemblies should be deleted after the relevant testing has been run, and the report file will thus be freed up.

