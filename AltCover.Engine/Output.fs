namespace AltCover

open System
open System.IO
open System.Reflection
open System.Resources

open AltCover.Shared
open Mono.Options

module PathOperation =
  let private handledPathOperationFault (x: exn) =
    (x :? ArgumentException)
    || (x :? NotSupportedException)
    || (x :? IOException)
    || (x :? System.Security.SecurityException)
    || (x :? UnauthorizedAccessException)

  let DoPathOperation (operation: unit -> 'TAny) (handle: exn -> 'TAny) =
    try
      operation ()
    with x when handledPathOperationFault x ->
      handle (x)

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage;
  NoComparison;
  AutoSerializable(false)>]
type internal UsageInfo =
  { Intro: String
    Options: OptionSet
    Options2: OptionSet }

module internal Output =
  let internal resources =
    ResourceManager("AltCover.Strings", Assembly.GetExecutingAssembly())

  let mutable internal info: String -> unit =
    ignore

  let mutable internal warn: String -> unit =
    ignore

  let mutable internal echo: String -> unit =
    ignore

  let mutable internal error: String -> unit =
    ignore

  let mutable internal usage: UsageInfo -> unit =
    ignore

  let mutable internal verbose: String -> unit =
    ignore

  let internal maybeVerbose p message =
    if p then
      verbose message

  let internal warnOn x = if x then warn else info

  let internal logExceptionToFile path e =
    Directory.CreateDirectory(path |> Path.GetDirectoryName)
    |> ignore

    use stream =
      File.Open(path, FileMode.Append, FileAccess.Write)

    use writer = new StreamWriter(stream)

    let rec logException padding ex =
      ex.ToString() |> writer.WriteLine

      ex.GetType().GetProperties()
      |> Seq.filter (fun p ->
        [ "Message"; "StackTrace" ]
        |> Seq.exists (fun n -> n == p.Name)
        |> not)
      |> Seq.iter (fun p ->
        (padding + p.Name + " = ") |> writer.WriteLine

        match p.GetValue(ex) with
        | :? Exception as exx -> logException ("  " + padding) exx
        | v -> v |> sprintf "%A" |> writer.WriteLine)

    logException String.Empty e