namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover
open Microsoft.FSharp.Reflection
open Mono.Options
open Mono.Cecil.Cil

#nowarn "25"

module Main =

  [<Test>]
  let ShouldHaveExpectedOptions () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let optionCount = 37

    let optionNames =
      options
      |> Seq.map (fun o -> (o.GetNames() |> Seq.maxBy _.Length).ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    // Options add "<>" and "help"
    Assert.That(
      options.Count,
      Is.EqualTo(optionCount + 2),
      String.Join("; ", optionNames)
    )

    let optionNames =
      options
      |> Seq.map (fun o -> (o.GetNames() |> Seq.maxBy _.Length).ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    let primitiveNames =
      typeof<Primitive.PrepareOptions>
      |> FSharpType.GetRecordFields
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    // add "commandline"
    testWithFallback
      <@ (primitiveNames) |> List.length = optionCount + 1 @> // adds optionroot
      (primitiveNames |> List.length)
      (Is.EqualTo(optionCount + 1))

    let typesafeNames =
      typeof<TypeSafe.PrepareOptions>
      |> FSharpType.GetRecordFields
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    testWithFallback
      <@ (typesafeNames) |> List.length = optionCount + 1 @> // adds optionroot
      (typesafeNames |> List.length)
      (Is.EqualTo(optionCount + 1))

    let fsapiNames =
      typeof<AltCover.PrepareOptions>.GetProperties()
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    let fsapiCases =
      (typeof<AltCover.PrepareOptions>
       |> FSharpType.GetUnionCases)
        .Length

    let args =
      { Primitive.PrepareOptions.Create() with
          Verbosity = System.Diagnostics.TraceLevel.Warning }
      |> AltCover.PrepareOptions.Primitive

    let commandFragments =
      [ Args.listItems >> (List.map fst)
        Args.plainItems >> (List.map fst)
        Args.options >> List.map (fun (a, _, _) -> a)
        Args.flagItems >> (List.map fst)
        Args.countItems >> (List.map fst) ]
      |> List.collect (fun f -> f args)
      |> List.map _.Trim('-')
      |> List.sort

    testWithFallback
      <@ (commandFragments) |> List.length = optionCount @> // drop -q/--verbose => verbosity
      (commandFragments |> List.length)
      (Is.EqualTo(optionCount))

    // Adds "Tag", "IsPrimitive", "IsTypeSafe"
    testWithFallback
      <@ (fsapiNames) |> List.length = optionCount + fsapiCases + 2 @> // drop -q/--verbose => verbosity
      (fsapiNames |> List.length)
      (Is.EqualTo(optionCount + fsapiCases + 2))

    let taskNames =
      typeof<Prepare>
        .GetProperties(
          BindingFlags.DeclaredOnly
          ||| BindingFlags.Public
          ||| BindingFlags.Instance
        )
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    testWithFallback
      <@ (taskNames) |> List.length = optionCount @> // drop -q/--verbose => verbosity
      (taskNames |> List.length)
      (Is.EqualTo(optionCount))

    let targets =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("AltCover.proj", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(targets)

    let doc = XDocument.Load stream

    let prepare =
      doc.Descendants()
      |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Prepare")
      |> Seq.head

    let attributeNames =
      prepare.Attributes()
      |> Seq.map _.Name.LocalName.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    // dotnet test loses commandline, eager, exposereturncode, save
    //                   N/A,         fixed, N/A,              fixed
    // inplace is explicitly hard-coded
    testWithFallback
      <@ (attributeNames) |> List.length = optionCount - 5 @> // drop --portable/-q/--verbose => verbosity
      (attributeNames |> List.length)
      (Is.EqualTo(optionCount - 4))

    Assert.That(
      options
      |> Seq.filter (fun x -> x.Prototype <> "<>")
      |> Seq.forall (_.Description >> String.IsNullOrWhiteSpace >> not),
      "empty description for one or more items"
    )

    Assert.That(
      options
      |> Seq.filter (fun x -> x.Prototype = "<>")
      |> Seq.length,
      Is.EqualTo 1,
      "more than one fallback"
    )