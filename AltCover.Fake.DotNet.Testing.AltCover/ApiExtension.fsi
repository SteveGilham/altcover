namespace AltCoverFake.DotNet.Testing
  module AltCoverCommand = begin
    [<NoComparison>]
    type ArgumentType =
      | Collect of AltCover.CollectOptions
      | Prepare of AltCover.PrepareOptions
      | ImportModule
      | GetVersion
    val splitCommandLine : line:string -> string list
    val buildDotNetTestCommandLine :
      options:(Fake.DotNet.DotNet.TestOptions -> Fake.DotNet.DotNet.TestOptions) ->
        project:string -> string * string list
    [<NoComparison; NoEquality>]
    type Options =
      { ToolPath: string
        ToolType: Fake.DotNet.ToolType
        WorkingDirectory: string
        Args: ArgumentType }
      with
        member
          WithCreateProcess : command:Fake.Core.CreateProcess<'a> -> Options
        static member Create : argumentType:ArgumentType -> Options
      end
    val composeCommandLine :
      options:Options ->
        Fake.Core.CreateProcess<Fake.Core.ProcessResult<unit>>
    val run : options:Options -> unit
    val runWithMono :
      monoPath:Fake.Core.FilePath option -> options:Options -> unit
  end