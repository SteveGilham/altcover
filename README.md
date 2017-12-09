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


