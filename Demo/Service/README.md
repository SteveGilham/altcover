# Windows Service Demo

This is a bare-bones Windows service implementation, running as NETWORK SERVICE (so operating with reduced privilege) that can be used to demonstrate gathering coverage information from a Windows service.

## How To

Build the service in Debug, and ensure that the executable is in a location readable by NETWORK SERVICE : the Documents folder under `C:\Users\Public` serves for this.

Install the service with `istallutil <path to Service.exe>`

Test that the `AltCover Test Service` can be started and that it runs (and writes debug output that can be followed in [DbgView](https://docs.microsoft.com/en-us/sysinternals/downloads/debugview)).  If necessary (e.g. on getting Access denied/Error Code 5) ensure that `Service.exe` can be executed by NETWORK SERVICE, using the associated PowerShell script

Stop the service, and instrument it with AltCover (update the NuGet package as needed); if you're in the same folder as the executable, that would be `[path to]\AltCover.exe -t=Microsoft`

Ensure that the coverage.xml file is writeable by NETWORK SERVICE, and observe that all visit count values are zero.

Swap the instrumented binaries (`Service.exe`, `AltCover.Recorder.g`) into the location where you installed the service

Restart the service , watch the debug output for a while, then stop the service.  The coverage.xml file will now show visits for the service execution (with no coverage for the `AltCover.Test.ProjectInstaller` class).  

If you now uninstall the service with `istallutil /u <path to Service.exe>`, then the `AltCover.Test.ProjectInstaller` class will show that it has been visited.