Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide and 
read the FAQ : https://github.com/SteveGilham/altcover/wiki/FAQ

# (Habu series release 27)
* [BUGFIX] Add `Json` member to the type-safe report format enumeration

# 8.7.3 (Habu series release 26)
* [Enhancement] [Discussion 202](https://github.com/SteveGilham/altcover/discussions/202) : More careful tidying of temporary `.runsettings` files, fixing long-standing errors of both commission and omission.
* [Enhancement] [Discussion 199](https://github.com/SteveGilham/altcover/discussions/199) : Add `/p:AltCoverOutputRoot=[path]` and associated APIs for `dotnet test` command line creation.  The `[path]` is a directory to be used instead of `$(TargetDir)` for the relative placing of the instrumented or saved files.  The use-case here is when `$(TargetDir)` is close to `MAX_PATH` and the generated sub-folders would overflow that limit.

⁋For previous releases (8.6.125 and earlier) go here -- [https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)