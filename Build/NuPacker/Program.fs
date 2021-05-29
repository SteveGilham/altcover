namespace AltCover

open System
open System.Diagnostics
open System.IO
open System.Xml.Linq

// "...\_Binaries\Nupacker\Release+AnyCPU\net472\Nupacker.exe"
// pack -Version 8.2.7816.16055
// -OutputDirectory "...\_Packaging"
// "...\_Binaries\_Packaging\AltCover.8.2.7816.16055.nuspec"

// See https://docs.microsoft.com/en-us/nuget/reference/msbuild-targets#packing-using-a-nuspec-file
// dotnet pack <path to .csproj file> -p:NuspecFile=<path to nuspec file> -p:NuspecProperties=<> -p:NuspecBasePath=<Base path>

module internal Process =
  type System.Diagnostics.Process with
    // Work around observed unreliability of WaitForExit()
    // with an unbounded wait under mono on travis-ci
    member self.WaitForExitCustom() =
      let rec loop () =
        try
          if self.WaitForExit(1000) then
            // allow time for I/O redirection to complete
            System.Threading.Thread.Sleep(1000)
            if self.HasExited then () else loop ()
          else
            loop ()
        with
        | :? SystemException
        | :? InvalidOperationException
        | :? System.ComponentModel.Win32Exception -> ()

      if "Mono.Runtime" |> Type.GetType |> isNull then
        self.WaitForExit()
      else
        loop ()

open Process

module NuPacker =

  [<EntryPoint>]
  let main argv =
    let n = argv.Length
    let tail = argv |> Array.skip (n-2)
    let outdir = tail |> Seq.head
    let nuspec = tail |> Seq.last

    let doc = Path.Combine (SolutionRoot.location, "Build/NuPacker.xml")
              |> XDocument.Load
    doc.Descendants(XName.Get "NuspecFile")
    |> Seq.iter (fun x -> x.Value <- nuspec)

    [
      "NuspecProperties"
      "NuspecBasePath"
    ]
    |> Seq.iter (fun n -> doc.Descendants(XName.Get n)
                          |> Seq.iter (fun x -> x.Value <- String.Empty))

    Path.Combine (outdir, "NuPacker.g.fsproj") // maybe gen name from readme?
    |> doc.Save

    let psi = ProcessStartInfo("dotnet", "pack -o . -c Release")
    psi.WorkingDirectory <- outdir
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    //psi.RedirectStandardError <- true
    //psi.RedirectStandardOutput <- true
    use proc = new Process()
    proc.StartInfo <- psi

    proc.Start() |> ignore
    //proc.BeginErrorReadLine()
    //proc.BeginOutputReadLine()
    proc.WaitForExitCustom()
    proc.ExitCode