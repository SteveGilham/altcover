Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 6.0.700 (Fukurou series release 2)
* [BUGFIX] in `dotnet test` the pipe character `|` is used as a separator because the previous choice of `;` didn't play nice with MSBuild.  To escape pipe characters inside regular expressions, double them up `||`.  See the [Usage](https://github.com/SteveGilham/altcover/wiki/Usage) and [`dotnet test`](https://github.com/SteveGilham/altcover/wiki/%60dotnet-test%60-integration) wiki pages for more detail.

# 6.0.698 (Fukurou series release 1)
* [BREAKING] Allow multiple input and output directories for instrumentation into a single report.  This changes the types in API structures from `string` to `string seq` (F#) or `string[]` (C#) where appropriate.  Use case : [instrument multiple unit test assemblies in one go](https://github.com/SteveGilham/altcover/blob/9f6ed07e9d5d1c35d8c99a589fb77fe1868cecab/Build/targets.fsx#L702-L703) for running as [a single test step](https://github.com/SteveGilham/altcover/blob/9f6ed07e9d5d1c35d8c99a589fb77fe1868cecab/Build/targets.fsx#L724-L726) to collect coverage.
* Enable the `--sn` and `-k` arguments in the `dotnet` build.  This doesn't change any APIs, but it does mean that these arguments are no longer ignored for the .net core platform.  This work borrows from changes recently made in Mono.Cecil but not yet in a release build; in the fullness of time, my roll-my-own support will be replaced with the real deal.
* For what it's worth, enable the `-d` argument in the .net framework/Mono build (as the APIs already exposed this, the change is behavioural -- any values supplied are used rather than silently dropped)


For previous releases (5.x.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)