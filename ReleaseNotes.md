Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide and 
read the FAQ : https://github.com/SteveGilham/altcover/wiki/FAQ

# (Indori series release 3)

* Fix issue #266 - allow an environment variable `ALTCOVER_REPORT_FILE` to supply a report file full path location for `--portable` mode. 
  * If not set, co-locate with the recorder assembly, and if that assembly is loaded from memory and has no location, expect it in the current directory. 
	* This fallback series is arranged to protect from null path exceptions (but not self-inflicted ones from invalid `ALTCOVER_REPORT_FILE` values) for the default case where the report file full path is stored in the recorder assembly but memory-loaded assemblies are used.

# 9.0.102 (Indori series release 2)
* Net10.0 support
* Fix issue #238 - some symbol errors cause instrumentation to abort rather than skip that assembly.
* Move to Fake 6.1.4 for all purposes, ending legacy support for 6.0.x
* Move to Cake 6.0.0 for all purposes, ending legacy support for 5.x

# 9.0.1 (Indori series release 1)
* Net9.0 support
* [BREAKING] Minimum platforms for the tools and API are `net472`, `netstandard2.0` and `net8.0`. *NOTE* the recorder assembly still targets `net20` (or `net46` when `async` is detected)
* [BREAKING] SDK updates to latest current for Cake (5.0.0) and MSBuild (17.12.6) packages; build with Fake.build 6.1.3 but supports back to 6.0.0
* [BREAKING] API change : `--all` and `--eager` replace `--single` and `--defer`, so the default behaviour is now deferred (Unload/Exit time) reporting, and first visit only per context. *NOTE* in some cases, use of .Net Framework `AppDomain`s may require the use of `--eager` as the unload handler gives no guarantees about execution time.

# 8.9.3 (Habu series release 33)
* Add `-p/--package` and equivalents to specify package roots for Cobertura output for all coverage collection methods, plus the PowerShell `ConvertTo-Cobertura` cmdlet
* [ADVISORY] The Fake.build related assemblies (in the `altcover.api` and `altcover.fake` packages) support Fake 6.1.0
* [PERFORMANCE] revise the OpenCover to LCov conversion to speed the mapping of methods from source files.

⁋For previous releases (8.8.173 and earlier) go here -- [https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)