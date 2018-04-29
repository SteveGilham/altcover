namespace AltCover

open System
open Microsoft.Build.Utilities
open Microsoft.Build.Framework

module Args =
  let Item a x =
    if x |> String.IsNullOrWhiteSpace
       then []
       else [ a; x ]
  let ItemList a x =
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList
  let Flag a x =
    if x
       then [a]
       else []

type Prepare () =
  inherit Task(null)
  member val InputDirectory = String.Empty with get, set
  member val OutputDirectory = String.Empty with get, set
  member val SymbolDirectories : string array = [| |] with get, set
#if NETCOREAPP2_0
#else
  member val Keys  : string array = [| |] with get, set
  member val StrongNameKey = String.Empty with get, set
#endif
  member val XmlReport = String.Empty with get, set
  member val FileFilter  : string array = [| |] with get, set
  member val AssemblyFilter  : string array = [| |] with get, set
  member val AssemblyExcludeFilter  : string array = [| |] with get, set
  member val TypeFilter  : string array = [| |] with get, set
  member val MethodFilter  : string array = [| |] with get, set
  member val AttributeFilter  : string array = [| |] with get, set
  member val CallContext  : string array = [| |] with get, set

  member val OpenCover = true with get, set
  member val InPlace = true with get, set
  member val Save = true with get, set

  member val CommandLine = String.Empty with get, set

  member self.Message x =
    base.Log.LogMessage (MessageImportance.High, x)

  override self.Execute () =
    Output.Error <- base.Log.LogError
    Output.Info <- self.Message
    [
      Args.Item "-i" self.InputDirectory;
      Args.Item "-o" self.OutputDirectory;
      Args.ItemList "-y" self.SymbolDirectories;
#if NETCOREAPP2_0
#else
      Args.ItemList "-k" self.Keys;
      Args.Item "--sn" self.StrongNameKey;
#endif
      Args.Item "-x" self.XmlReport;
      Args.ItemList "-f" self.FileFilter;
      Args.ItemList "-s" self.AssemblyFilter;
      Args.ItemList "-e" self.AssemblyExcludeFilter;
      Args.ItemList "-t" self.TypeFilter;
      Args.ItemList "-m" self.MethodFilter;
      Args.ItemList "-a" self.AttributeFilter;
      Args.ItemList "-c" self.CallContext;

      Args.Flag "--opencover" self.OpenCover
      Args.Flag "--inplace" self.InPlace
      Args.Flag "--save" self.Save

      Args.Item "--" self.CommandLine;
    ]
    |> List.concat
    |> List.toArray
    |> AltCover.Main.EffectiveMain = 0

type Collect () =
  inherit Task(null)
  [<Required>]
  member val RecorderDirectory = String.Empty with get, set
  member val WorkingDirectory = String.Empty with get, set
  member val Executable = String.Empty with get, set
  member val LcovReport = String.Empty with get, set
  member val Threshold = String.Empty with get, set
  member val Cobertura = String.Empty with get, set
  member val OutputFile = String.Empty with get, set

  member val CommandLine = String.Empty with get, set

  member self.Message x =
    base.Log.LogMessage (MessageImportance.High, x)

  override self.Execute () =
    Output.Error <- base.Log.LogError
    Output.Info <- self.Message
    [
      ["Runner"];
      Args.Item "-r" self.RecorderDirectory;
      Args.Item "-w" self.WorkingDirectory;
      Args.Item "-x" self.Executable;
      Args.Item "-l" self.LcovReport;
      Args.Item "-t" self.Threshold;
      Args.Item "-c" self.Cobertura;
      Args.Item "-o" self.OutputFile;

      Args.Flag "--collect" (self.Executable |> String.IsNullOrWhiteSpace)

      Args.Item "--" self.CommandLine;
    ]
    |> List.concat
    |> List.toArray
    |> AltCover.Main.EffectiveMain = 0