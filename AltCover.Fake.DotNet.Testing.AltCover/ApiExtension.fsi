namespace AltCoverFake.DotNet.Testing
  module AltCoverCommand = begin
    [<NoComparison>]
    type ArgumentType =
      | Collect of AltCover.CollectParameters
      | Prepare of AltCover.PrepareParameters
      | ImportModule
      | GetVersion
    val splitCommandLine : line:string -> string list
    val buildDotNetTestCommandLine :
      options:(Fake.DotNet.DotNet.TestOptions -> Fake.DotNet.DotNet.TestOptions) ->
        project:string -> string * string list
    [<NoComparison; NoEquality>]
    type Parameters =
      { ToolPath: string
        ToolType: Fake.DotNet.ToolType
        WorkingDirectory: string
        Args: ArgumentType }
      with
        member
          WithCreateProcess : command:Fake.Core.CreateProcess<'a> -> Parameters
        static member Create : argumentType:ArgumentType -> Parameters
      end
    val composeCommandLine :
      parameters:Parameters ->
        Fake.Core.CreateProcess<Fake.Core.ProcessResult<unit>>
    val run : parameters:Parameters -> unit
    val runWithMono :
      monoPath:Fake.Core.FilePath option -> parameters:Parameters -> unit
  end