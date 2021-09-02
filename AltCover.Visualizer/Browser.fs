namespace AltCover

open System
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.Runtime.InteropServices

module Browser =

  [<SuppressMessage("Gendarme.Rules.Portability",
                    "DoNotHardcodePathsRule",
                    Justification = "I know what I'm doing here")>]
  // browser launch from Avalonia
  let private shellExec (cmd: string) waitForExit =
    let escapedArgs = cmd.Replace("\"", "\\\"") // use Blackfox instead???
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