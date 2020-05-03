namespace AltCover
  module DotNetCLI = begin
    val ToTestArgumentList :
      prepare:FSApi.PrepareOptions ->
        collect:FSApi.CollectOptions -> options:DotNet.CLIOptions -> string list
    val ToTestArguments :
      prepare:FSApi.PrepareOptions ->
        collect:FSApi.CollectOptions -> options:DotNet.CLIOptions -> string
  end