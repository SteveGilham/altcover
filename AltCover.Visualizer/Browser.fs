namespace AltCover

open System
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.Runtime.InteropServices

[<AutoOpen>]
[<SuppressMessage("Gendarme.Rules.Smells",
                   "AvoidSpeculativeGeneralityRule",
                   Justification = "Delegation is DRYing the codebase")>]
module internal Extensions =
#if !NETCOREAPP2_1
  [<SuppressMessage("Gendarme.Rules.Globalization",
                    "PreferStringComparisonOverrideRule",
                    Justification = "Not available this platform")>]
#endif
  let
#if !DEBUG
      inline
#endif
             replace (y: string, z: string) (x: string) =
#if !NETCOREAPP2_1
             x.Replace(y, z)
#else
             x.Replace(y, z, System.StringComparison.Ordinal)
#endif

#if !NETCOREAPP2_1
  [<SuppressMessage("Gendarme.Rules.Globalization",
                    "PreferStringComparisonOverrideRule",
                    Justification = "Not available this platform")>]
#endif
  let
#if !DEBUG
      inline
#endif
             indexOf (y: char) (x: string) =
#if !NETCOREAPP2_1
             x.IndexOf y
#else
             x.IndexOf(y, System.StringComparison.Ordinal)
#endif

module Browser =

  [<SuppressMessage("Gendarme.Rules.Portability",
                    "DoNotHardcodePathsRule",
                    Justification = "I know what I'm doing here")>]
  // browser launch from Avalonia
  let private shellExec (cmd: string) waitForExit =
    let escapedArgs = cmd |> replace ("\"", "\\\"") // use Blackfox instead???
    let psi = ProcessStartInfo()
    psi.FileName <- "/bin/sh"
    psi.Arguments <- "-c \"" + escapedArgs + "\""
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    psi.WindowStyle <- ProcessWindowStyle.Hidden
    use proc = Process.Start(psi)
    if waitForExit then proc.WaitForExit()

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "Passed as a delegate")>]
  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "DoNotSwallowErrorsCatchingNonSpecificExceptionsRule",
                    Justification = "A call to native code")>]
  [<SuppressMessage("Microsoft.Design",
                    "CA1031:DoNotCatchGeneralExceptionTypes",
                    Justification = "A call to native code")>]
  let ShowUrl (url: Uri) =
    let isLinux =
      RuntimeInformation.IsOSPlatform(OSPlatform.Linux)

    let isWindows =
      RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

    let isOSX =
      RuntimeInformation.IsOSPlatform(OSPlatform.OSX)

    if isLinux then
      shellExec ("xdg-open " + url.ToString()) false
    else
      let psi = ProcessStartInfo()

      psi.FileName <-
        if isWindows then
          url.ToString()
        else
          "open"

      psi.Arguments <-
        if isOSX then
          ("-e " + url.ToString())
        else
          String.Empty

      psi.CreateNoWindow <- true
      psi.UseShellExecute <- isWindows
      use _proc = System.Diagnostics.Process.Start(psi)
      ()