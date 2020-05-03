// # namespace AltCoverFake.DotNet.Testing
// ```
namespace AltCoverFake.DotNet.Testing
// ```
// ## module AltCoverCommand
// ```
  module AltCoverCommand = begin
// ```
// ## type ArgumentType 
// ```
    [<NoComparison>]
    type ArgumentType =
      | Collect of AltCover.CollectOptions
      | Prepare of AltCover.PrepareOptions
      | ImportModule
      | GetVersion
// ```      
// This type defines what operation to use AltCover for
// ```
    val splitCommandLine : line:string -> string list
// ```      
// This module-level helper function decomposes a command line into its separate elements.
//
// Use to safely break up the argument list from e.g. `Fake.DotNet.Testing.NUnit3.buildArgs` for constructing a `CommandLine` ([see example here](https://github.com/SteveGilham/altcover/blob/bf291c9485a737ff4b1d7034f1bdf74374d1d3f9/Build/targets.fsx#L966-L985))
// ```
//       let nunitcmd = NUnit3.buildArgs nunitparams nunitAssemblies
//
//       try
//         let collect =
//           AltCover.CollectOptions.Primitive
//             { Primitive.CollectOptions.Create() with
//                 Executable = nunitConsole
//                 RecorderDirectory = testDirectory @@ outputDirectory
//                 CommandLine = AltCoverCommand.splitCommandLine nunitcmd }
//           |> AltCoverCommand.Collect
// ```
// or similarly for `Fake.DotNet.Testing.XUnit2.buildArgs` 
// ```
    val buildDotNetTestCommandLine :
      options:(Fake.DotNet.DotNet.TestOptions -> Fake.DotNet.DotNet.TestOptions) ->
        project:string -> string * string list
// ```      
// This module-level helper function composes a `dotnet test` command line from a `DotNet.TestOptions` transformer and aproject under test, and return the result as the `dotnet` executable followed by the separate arguments as a list.
// Intended for taking the arguments for `DotNet.test` and returning the `dotnet` path and the rest of the command line for again constructing a `CommandLine`; and `Executable` for `CollectOptions`.
// ```
//       let (dotnetexe, args) =  AltCoverCommand.buildDotNetTestCommandLine id "./Tests/altcover.tests.core.fsproj"
//
//       let collect =
//         AltCover.CollectOptions.Primitive
//           { Primitive.CollectOptions.Create() with
//               Executable = dotnetexe
//               RecorderDirectory = output
//               CommandLine = args }
//```
// ## type Options 
// ```
    [<NoComparison; NoEquality>]
    type Options =
      { ToolPath: string (* Path to the Altcover executable. *)
        ToolType: Fake.DotNet.ToolType (* Which style of tool *)
        WorkingDirectory: string (* Working directory for relative file paths.  Default is the current working directory *)
        Args: ArgumentType} (* Command arguments *)
      with
        member
          WithCreateProcess : command:Fake.Core.CreateProcess<'a> -> Options
        static member Create : argumentType:ArgumentType -> Options
      end
// ```   
// `Create` creates a default-initialised instance with the `Args` field set from input
//
// `WithCreateProcess` turns the specified process information into a suitable command line for AltCover to execute as a sub-process
//
// ```
    val composeCommandLine :
      options:Options ->
        Fake.Core.CreateProcess<Fake.Core.ProcessResult<unit>>
// ```
// Synthesises the command to execute, from which the full command line can be read
// ```        
    val run : options:Options -> unit
// ```
// Run the specified AltCover operation
// ```        
    val runWithMono :
      monoPath:Fake.Core.FilePath option -> options:Options -> unit
// ```
//  Runs AltCover expressed as a `Fake.DotNet.ToolType.FullFramework` under the supplied mono path (defaulting to just `mono`) on Windows
// ```        
  end
 // ```.