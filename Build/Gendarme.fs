/// Contains task a task which allows to merge .NET assemblies with [ILMerge](http://research.microsoft.com/en-us/people/mbarnett/ilmerge.aspx).
[<RequireQualifiedAccess>]
module Fake.DotNet.Gendarme

open System
open Fake.Core
open Fake.IO
open Fake.IO.Globbing
open System.Globalization

/// Nudge severity or confidence levels
type Grade =
    | Plus
    | Neutral
    | Minus

/// Filter defects for the specified severity levels.
/// Default is 'medium+'
type Severity =
    | All
    | Audit of Grade
    | Low of Grade
    | Medium of Grade
    | High of Grade
    | Critical

/// Filter defects for the specified confidence levels.
/// Default is 'normal+'
type Confidence =
    | All
    | Low of Grade
    | Normal of Grade
    | High of Grade
    | Total of Grade

/// Option type to configure ILMerge's target output.
type LogKind =
    | Text
    | Xml
    | Html

/// Parameter type for ILMerge
[<NoComparison>]
type Params =
    { /// Path to gendarme.exe
      ToolPath : string
      /// Working Directory
      WorkingDirectory : string
      /// Specify the rule sets and rule settings. Default is 'rules.xml'.
      Configuration : string
      /// Specify a rule set from configfile. Default is 'default'.
      RuleSet : string
      /// Save the report to the specified file.
      Log : string
      /// Report type.
      LogKind : LogKind
      /// Do not report defects listed in the specified file.
      Ignore : string seq
      /// Stop reporting after this many defects are found.
      Limit : uint8
      /// True -> Show defects on the console even if Log is specified
      Console : bool
      /// True -> Used to disable progress and other information which is normally written to stdout.
      Quiet : bool
      /// When present and > 0 additional progress information is written to stdout
      Verbosity : uint8
      /// Specify the assemblies to verify.
      Targets : string seq
      /// Filter defects for the specified severity levels.
      /// Default is 'medium+'
      Severity : Severity
      /// Filter defects for the specified confidence levels.
      /// Default is 'normal+'
      Confidence : Confidence }
    /// ILMerge default parameters. Tries to automatically locate ilmerge.exe in a subfolder.
    static member Create() =
        { ToolPath = Tools.findToolInSubPath "gendarme.exe" <| Shell.pwd()
          WorkingDirectory = String.Empty
          Configuration = String.Empty
          RuleSet = String.Empty
          Log = String.Empty
          LogKind = LogKind.Html
          Ignore = Seq.empty
          Limit = 0uy
          Console = true
          Quiet = false
          /// When present and > 0 additional progress information is written to stdout
          Verbosity = 0uy
          /// Specify the assemblies to verify.
          Targets = Seq.empty
          /// Filter defects for the specified severity levels.
          /// Default is 'medium+'
          Severity = Severity.Medium Grade.Plus
          /// Filter defects for the specified confidence levels.
          /// Default is 'normal+'
          Confidence = Confidence.Normal Grade.Plus }

/// Builds the arguments for the ILMerge task
/// [omit]
let internal getArguments parameters =
    let Item a x =
        if x |> String.IsNullOrWhiteSpace then []
        else [ a; x ]

    let ItemList a x =
        if x |> isNull then []
        else
            x
            |> Seq.collect (fun i -> [ a; i ])
            |> Seq.toList

    let Flag a predicate =
        if predicate then [ a ]
        else []

    [ Item "--config" parameters.Configuration
      Item "--set" parameters.RuleSet
      (match parameters.LogKind with
       | Text -> Item "--log"
       | Xml -> Item "--xml"
       | _ -> Item "--html") parameters.Log
      ItemList "--ignore" parameters.Ignore
      Item "--limit" <| parameters.Limit.ToString(CultureInfo.InvariantCulture)
      Flag "--console" parameters.Console
      Flag "--quiet" parameters.Quiet

      Item "--severity"
      <| (sprintf "%A" parameters.Severity).ToLowerInvariant().Replace(" plus", "+")
          .Replace(" minus", "-").Replace(" neutral", String.Empty)

      Item "--confidence"
      <| (sprintf "%A" parameters.Confidence).ToLowerInvariant().Replace(" plus", "+")
          .Replace(" minus", "-").Replace(" neutral", String.Empty)
      (if parameters.Verbosity > 0uy then
           { 0..int parameters.Verbosity }
           |> Seq.map (fun _ -> "--v")
           |> Seq.toList
       else [])
      (parameters.Targets |> Seq.toList) ]
    |> List.concat

/// Uses ILMerge to merge .NET assemblies.
/// ## Parameters
///
///  - `parameters` - A Gendarme.Params value with your required settings.
let run parameters =
    use __ = Trace.traceTask "Gendarme" ""
    let args = getArguments parameters

    let run =
        CreateProcess.fromRawCommand parameters.ToolPath args
        |> CreateProcess.withFramework
        |> if String.IsNullOrWhiteSpace parameters.WorkingDirectory then id
           else CreateProcess.withWorkingDirectory parameters.WorkingDirectory
        |> Proc.run
    if 0 <> run.ExitCode then failwithf "Gendarme %s failed." (String.separated " " args)
    __.MarkSuccess()