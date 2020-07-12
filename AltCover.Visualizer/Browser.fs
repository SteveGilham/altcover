namespace AltCover.Visualizer

open System
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.Runtime.InteropServices

#if !NETCOREAPP2_1
module internal NativeMethods =
  //From Managed.Windows.Forms/XplatUI
  [<DllImport ("libc")>]
  extern int private uname (IntPtr buf)
#endif

module Browser =

  [<SuppressMessage("Gendarme.Rules.Portability", "DoNotHardcodePathsRule",
                    Justification = "I know what I'm doing here")>]
  // browser launch from Avalonia
  let private shellExec (cmd:string) waitForExit =
    let escapedArgs = cmd.Replace("\"", "\\\"") // use Blackfox instead???
    let psi = ProcessStartInfo()
    psi.FileName <- "/bin/sh"
    psi.Arguments <- "-c \"" + escapedArgs + "\""
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    psi.WindowStyle <- ProcessWindowStyle.Hidden
    use proc = Process.Start(psi)
    if waitForExit
    then proc.WaitForExit();

  [<SuppressMessage("Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
                    Justification = "Passed as a delegate")>]
  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "DoNotSwallowErrorsCatchingNonSpecificExceptionsRule",
                    Justification = "A call to native code")>]
  [<SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes",
                    Justification = "A call to native code")>]
  let ShowUrl(url : Uri) =
#if NETCOREAPP2_1 // or net471+
    let isLinux = RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
    let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    let isOSX = RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
#else
    let p = Environment.OSVersion.Platform |> int
    let isWindows = p <= 3
      // System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"

    // from https://github.com/jpobst/Pinta/blob/1.6/Pinta.Core/Managers/SystemManager.cs#L125
    let isOSX =
      let mutable buf = IntPtr.Zero
      try
        buf <- Marshal.AllocHGlobal (8192)
        try
          // This is a hacktastic way of getting sysname from uname ()
          if (NativeMethods.uname (buf) = 0)
          then let os = Marshal.PtrToStringAnsi (buf);
               os = "Darwin"
          else false
          with
          | _ -> false
      finally
        if buf <> IntPtr.Zero
        then Marshal.FreeHGlobal buf

    //let isLinux =  (p = 4) || (p = 6) || (p = 128) // hack
    //               && System.Environment.GetEnvironmentVariable("OSTYPE") = "linux-gnu"
    let isLinux = (isWindows || isOSX) |>  not  // by default
#endif

    if isLinux
    then shellExec ("xdg-open " + url.ToString()) false
    else let psi = ProcessStartInfo()
         psi.FileName <- if isWindows
                         then url.ToString()
                         else "open"
         psi.Arguments <- if isOSX
                          then ("-e " + url.ToString())
                          else String.Empty
         psi.CreateNoWindow <- true
         psi.UseShellExecute <- isWindows
         use _proc = System.Diagnostics.Process.Start(psi)
         ()