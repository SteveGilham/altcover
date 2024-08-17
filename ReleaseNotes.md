Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide and 
read the FAQ : https://github.com/SteveGilham/altcover/wiki/FAQ

# (Indori series release 1)
* [BREAKING] Minimum platforms net472, netstandard2.0 and net7.0
* [BREAKING] SDK updates to latest current for Cake and MSBuild-related packages

# 8.9.3 (Habu series release 33)
* Add `-p/--package` and equivalents to specify package roots for Cobertura output for all coverage collection methods, plus the PowerShell `ConvertTo-Cobertura` cmdlet
* [ADVISORY] The Fake.build related assemblies (in the `altcover.api` and `altcover.fake` packages) support Fake 6.1.0
* [PERFORMANCE] revise the OpenCover to LCov conversion to speed the mapping of methods from source files.

⁋For previous releases (8.8.173 and earlier) go here -- [https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)