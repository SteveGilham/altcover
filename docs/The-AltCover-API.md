# AltCover API v7.x

`AltCover` exposes almost all its functionality through a series of APIs.

* In-process execution of the coverage operations
  * [Core functionality as an F# API](#f-core-api)
  * [Core functionality as a C# API](#c-core-api)
* [MSBuild tasks](MSBuild-tasks) 
* Utilities for manipulating coverage data
  * [As an API](The-AltCover-Utilities-API)
  * [As PowerShell cmdlets](PowerShell-integration)
* [`dotnet test` integration](%60dotnet-test%60-integration) 
* build script integration
  * Fake
    * [In the `altcover.api` package](Fake-and-Cake-integration#fake-integration)
    * [As a stand-alone Fake module](The-AltCover.Fake-package)
  * [Cake](Fake-and-Cake-integration#cake-integration)

## F# core API

By linking to `AltCover.exe` (`.dll` for .net core) all AltCover operations may be driven programmatically in-process.   This part of the API is available in all variants of the AltCover NuGet package.

* module `AltCover.Primitive` -- [This module](AltCover/Primitive-apidoc) contains the weakly-typed, all strings, data structures in the style of Fake, to specify the command-line arguments.
* module `AltCover.TypeSafe`-- [This module](AltCover/TypeSafe-apidoc) contains the strongly-typed equivalents of the above.
* module `AltCover.OptionApi` -- [This module](AltCover/OptionApi-apidoc) and [its extension](AltCover/OptionApiExtension-apidoc) takes the types above and provides command-line validation
* module `AltCover.Api` -- [This module](AltCover/Api-apidoc) exposes the actual instrumentation, collection and miscellaneous reporting functions of the `AltCover` executable.

# C# core API

The API is extended with a C# equivalent in `AltCover.CSApi.dll`in the `AltCover.Api` nuget package.  The C#-style API documentation is [here](AltCover.CSApi/AltCover.CSApi-apidoc). 