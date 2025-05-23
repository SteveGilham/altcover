﻿// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover

// Functional Visitor pattern

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Linq
open System.Text.RegularExpressions

open Manatee.Json
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open System.Net

open AltCover.Shared
open System.Globalization

[<Flags>]
[<SuppressMessage("Gendarme.Rules.Design",
                  "FlagsShouldNotDefineAZeroValueRule",
                  Justification = "Gives the unset state a name")>]
type internal Inspections =
  | Ignore = 0
  | Instrument = 1
  | Track = 2
  | TrackOnly = 4

type internal CoverStyle =
  | All = 0
  | LineOnly = 1
  | BranchOnly = 2

type internal Reporting =
  | None = 0
  | Representative = 1
  | Contributing = 2

[<ExcludeFromCodeCoverage>]
type internal SeqPnt =
  { StartLine: int
    StartColumn: int
    EndLine: int
    EndColumn: int
    [<NonSerialized>]
    Document: Cil.Document
    Offset: int }
  static member Build(codeSegment: Cil.SequencePoint) =
    { StartLine = codeSegment.StartLine
      StartColumn = codeSegment.StartColumn
      EndLine =
        if codeSegment.EndLine < 0 then
          codeSegment.StartLine
        else
          codeSegment.EndLine
      EndColumn =
        if codeSegment.EndLine < 0 then
          codeSegment.StartColumn + 1
        else
          codeSegment.EndColumn
      Document = codeSegment.Document
      Offset = codeSegment.Offset }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal GoTo =
  { Start: Instruction
    SequencePoint: SequencePoint
    Indexes: int list
    Uid: int
    Path: int
    Offset: int
    [<NonSerialized>]
    Target: Instruction list
    Included: bool
    VisitCount: Exemption
    Representative: Reporting
    Key: int }

[<ExcludeFromCodeCoverage>]
type internal Hallmark =
  { Assembly: string
    Configuration: string }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal AssemblyDespatch =
  { AssemblyPath: string
    Destinations: string list
    Identity: Hallmark }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal AssemblyEntry =
  { Assembly: AssemblyDefinition
    Inspection: Inspections
    Destinations: string list
    Identity: Hallmark }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal ModuleEntry =
  { Module: ModuleDefinition
    Inspection: Inspections }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal TypeEntry =
  { Type: TypeDefinition
    VisibleType: TypeDefinition
    Inspection: Inspections
    DefaultVisitCount: Exemption }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal MethodEntry =
  { Method: MethodDefinition
    VisibleMethod: MethodDefinition
    Inspection: Inspections
    Track: (int * string) option
    DefaultVisitCount: Exemption }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal StatementEntry =
  { Instruction: Instruction
    SeqPnt: SeqPnt option
    Uid: int
    Interesting: bool
    DefaultVisitCount: Exemption }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal Node =
  | Start of seq<AssemblyDespatch>
  | Assembly of AssemblyEntry
  | Module of ModuleEntry
  | Type of TypeEntry
  | Method of MethodEntry
  | MethodPoint of StatementEntry
  | BranchPoint of GoTo
  | AfterMethod of MethodEntry
  | AfterType
  | AfterModule
  | AfterAssembly of AssemblyEntry
  | Finish
  member this.After() =
    (match this with
     | Start _ -> [ Finish ]
     | Assembly a -> [ AfterAssembly a ]
     | Module _ -> [ AfterModule ]
     | Type _ -> [ AfterType ]
     | Method m -> [ AfterMethod m ]
     | _ -> [])
    |> List.toSeq

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal StrongNameKeyData =
  { Blob: byte list
    Parameters: System.Security.Cryptography.RSAParameters }

  member this.PublicKey =
    let lead =
      [ 0uy // RSA
        36uy
        0uy
        0uy
        4uy // SHA
        128uy
        0uy
        0uy
        148uy // modulus length + 20
        0uy
        0uy
        0uy
        6uy // public key
        2uy // version
        0uy // zero
        0uy
        0uy // RSA
        36uy
        0uy
        0uy
        82uy // "RSA1"
        83uy
        65uy
        49uy
        0uy // key bit length
        4uy
        0uy
        0uy ]
    // possibly reverse exponent too?
    let exponent =
      Seq.append this.Parameters.Exponent (Seq.initInfinite (fun _ -> 0uy))
      |> Seq.take 4
      |> Seq.toList

    Seq.concat
      [ lead
        exponent
        this.Parameters.Modulus |> Seq.toList |> List.rev ]

  static member Make(data: byte array) =
    use csp =
      new System.Security.Cryptography.RSACryptoServiceProvider()

    csp.ImportCspBlob(data)
    let blob = csp.ExportCspBlob(true)

    { Blob = blob |> Array.toList
      Parameters = csp.ExportParameters(true) }

  static member Empty() =
    { Blob = []
      Parameters = System.Security.Cryptography.RSAParameters() }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal KeyRecord =
  { Pair: StrongNameKeyData
    Token: byte list }

[<ExcludeFromCodeCoverage; NoComparison>]
type internal SequenceType =
  | Genuine
  | FakeAtReturn

[<RequireQualifiedAccess>]
module internal KeyStore =
  let private hash = sha1Hash ()

  let private publicKeyOfKey (key: StrongNameKeyData) = key.PublicKey

  module internal I =
    let internal tokenOfArray (key: byte seq) =
      key
      |> Seq.toArray
      |> hash.ComputeHash
      |> Array.rev
      |> Array.take 8

  let internal tokenOfKey (key: StrongNameKeyData) =
    key
    |> publicKeyOfKey
    |> I.tokenOfArray
    |> Array.toList

  let internal tokenAsULong (token: byte array) = BitConverter.ToUInt64(token, 0)

  let internal keyToIndex (key: StrongNameKeyData) =
    key |> tokenOfKey |> List.toArray |> tokenAsULong

  let internal arrayToIndex (key: byte array) = key |> I.tokenOfArray |> tokenAsULong

  let internal keyToRecord (key: StrongNameKeyData) =
    { Pair = key; Token = tokenOfKey key }

  let internal hashFile sPath =
    use stream = File.OpenRead sPath

    stream
    |> hash.ComputeHash
    |> BitConverter.ToString

//Stopped working in 9.0.200 [<ExcludeFromCodeCoverage;
[<SuppressMessage("Gendarme.Rules.Design.Generic",
                  "AvoidDeclaringCustomDelegatesRule",
                  Justification =
                    "Recursive type definition can't be done with Fix<'T> = Func<'T, Fix<'T>>")>]
[<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "Anonymous parameter")>]
type internal Fix<'T> = delegate of 'T -> Fix<'T>

[<RequireQualifiedAccess>]
[<SuppressMessage("Gendarme.Rules.Smells",
                  "AvoidLargeClassesRule",
                  Justification = "More complex to dismantle")>]
module internal CoverageParameters =
  let internal hash =
    System.Security.Cryptography.SHA256.Create()

  let internal methodPoint = ref false // ddFlag
  let internal collect = ref false // ddFlag

  let internal trackingNames = List<String>()

  let internal topLevel = List<FilterClass>()

  let internal nameFilters =
    List<FilterClass>()

  let mutable internal staticFilter: StaticFilter option =
    None

  let effectiveStaticFilter () =
    staticFilter
    |> Option.defaultValue StaticFilter.Hidden

  let internal showGenerated = ref false

  let generationFilter =
    [ "System.Runtime.CompilerServices.CompilerGeneratedAttribute"
      "System.CodeDom.Compiler.GeneratedCodeAttribute" ]
    |> List.map (
      Regex
      >> FilterRegex.Exclude
      >> (FilterClass.Build FilterScope.Attribute)
    )

  let internal inplace = ref false // ddFlag
  let internal coalesceBranches = ref false // ddFlag
  let internal local = ref false // ddFlag

  let mutable internal all = false // more complicated

  let internal sampling () =
    (if all then
       Sampling.All
     else
       Sampling.Single)
    |> int

  let internal sourcelink = ref false // ddFlag
  let internal eager = ref false
  let internal trivia = ref false
  let internal portable = ref false

  let internal eagerOpCode () =
    if eager.Value then
      OpCodes.Ldc_I4_1
    else
      OpCodes.Ldc_I4_0

  let internal theInputDirectories =
    List<string>()

  let private defaultInputDirectory = "."

  let internal inputDirectories () =
    if theInputDirectories.Any() then
      theInputDirectories |> Seq.toList
    else
      [ defaultInputDirectory ]
      |> List.map canonicalDirectory

  let internal inplaceSelection a b = if inplace.Value then a else b

  let internal theOutputDirectories =
    List<string>()

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUnusedParametersRule",
                    Justification = "meets an interface")>]
  let private defaultOutputDirectory _ =
    inplaceSelection "__Saved" "__Instrumented"

  let internal outputDirectories () =
    let paired = inputDirectories ()

    Seq.append
      (theOutputDirectories :> string seq)
      (Seq.initInfinite defaultOutputDirectory)
    |> Seq.zip paired
    |> Seq.map (fun (i, o) -> Path.Combine(i, o) |> canonicalDirectory)
    |> Seq.toList

  let internal instrumentDirectories () =
    (inplaceSelection inputDirectories outputDirectories) ()

  let internal sourceDirectories () =
    (inplaceSelection outputDirectories inputDirectories) ()

  let mutable internal theReportPath: Option<string> =
    None

  let internal zipReport = ref false // ddFlag

  let mutable internal theInterval: Option<int> =
    None

  let internal defaultInterval = 0

  let internal interval () =
    (Option.defaultValue defaultInterval theInterval)

  let mutable internal theReportFormat: Option<ReportFormat> =
    None

  let mutable internal coverstyle =
    CoverStyle.All

  let internal reportKind () =
    (Option.defaultValue ReportFormat.OpenCover theReportFormat)

  let internal defaultReportPath () =
    if reportKind () = ReportFormat.NativeJson then
      "coverage.json"
    else
      "coverage.xml"

  [<SuppressMessage("Gendarme.Rules.Globalization",
                    "PreferStringComparisonOverrideRule",
                    Justification = "Compiler generated")>]
  let internal reportPath () =
    let r =
      canonicalPath (Option.defaultValue (defaultReportPath ()) theReportPath)

    let suffix =
      (Path.GetExtension r).ToUpperInvariant()

    let path =
      match (suffix, reportKind ()) with
      | (".XML", ReportFormat.NativeJson) -> Path.ChangeExtension(r, ".json")
      | (".JSON", ReportFormat.OpenCover)
      | (".JSON", ReportFormat.NCover) -> Path.ChangeExtension(r, ".xml")
      | _ -> r

    if (portable.Value) then
      "./" + Path.GetFileName(path)
    else
      path

  let internal reportFormat0 () =
    let fmt = reportKind ()

    if
      fmt = ReportFormat.OpenCover
      && (trackingNames.Any() || interval () > 0)
    then
      ReportFormat.OpenCover
      ||| ReportFormat.WithTracking
    else if
      fmt = ReportFormat.NativeJson
      && (trackingNames.Any() || interval () > 0)
    then
      ReportFormat.NativeJson
      ||| ReportFormat.WithTracking
    else
      fmt

  let internal reportFormat () =
    let raw = reportFormat0 ()

    if zipReport.Value then
      raw ||| ReportFormat.Zipped
    else
      raw

  let internal isTracking () =
    int (reportFormat () &&& ReportFormat.WithTracking)
    <> 0

  let withBranches () = reportFormat () <> ReportFormat.NCover

  let mutable internal defaultStrongNameKey: option<StrongNameKeyData> =
    None

  let mutable internal recorderStrongNameKey: option<StrongNameKeyData> =
    None

  let internal keys =
    Dictionary<UInt64, KeyRecord>()

  let internal add (key: StrongNameKeyData) =
    let index = KeyStore.keyToIndex key
    keys.[index] <- KeyStore.keyToRecord key

  let mutable internal configurationHash: option<String> =
    None

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "UseStringEmptyRule",
                    Justification = "Probably in the 'string' inline")>]
  [<SuppressMessage("Gendarme.Rules.Globalization",
                    "PreferStringComparisonOverrideRule",
                    Justification = "Overload not in netstandard2.0")>]
  let private filterString (n: FilterClass) =
    (string n).Replace('\r', ';').Replace('\n', ';').Replace(";;", ";")

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "UseStringEmptyRule",
                    Justification = "Probably in the 'string' inline")>]
  [<SuppressMessage("Gendarme.Rules.Portability",
                    "NewLineLiteralRule",
                    Justification = "Constant string exactly for portability")>]
  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidLargeNumberOfLocalVariablesRule",
                    Justification = "Compiler change at dotnet 9.0.300")>]
  let internal makeConfiguration () =
    let components =
      [ "--methodpoint\t" + string methodPoint.Value
        "--callContext-A\t"
        + String.Join("\t", trackingNames)
        "--topLevels\t"
        + String.Join("\t", topLevel |> Seq.map filterString |> Seq.sort)
        "--filters\t"
        + String.Join("\t", nameFilters |> Seq.map filterString |> Seq.sort)
        "--showstatic\t" + string staticFilter
        "--showGenerated\t" + string showGenerated.Value
        "--visibleBranches\t"
        + string coalesceBranches.Value
        "--localSource\t" + string local.Value
        "--sourceLink\t" + string sourcelink.Value
        "--callContext-B\t" + (string <| interval ())
        "--line/branch-cover\t" + string coverstyle
        "--reportFormat\t" + (string <| reportFormat ())
        "--strongNameKey\t"
        + (defaultStrongNameKey
           |> Option.map KeyStore.keyToIndex
           |> string)
        "!!recorderStrongNameKey\t"
        + (recorderStrongNameKey
           |> Option.map KeyStore.keyToIndex
           |> string)
        "--key\t"
        + String.Join("\t", keys.Keys |> Seq.map string |> Seq.sort)
        if trivia.Value then
          "--trivia\t" + string trivia.Value
        else
          String.Empty ]

    configurationHash <-
      String.Join("\n", components).TrimEnd()
      |> System.Text.Encoding.ASCII.GetBytes
      |> hash.ComputeHash
      |> Convert.ToBase64String
      |> Some

[<AutoOpen>]
module internal Inspector =
  type Inspections with
    member self.IsInstrumented =
      (self &&& Inspections.Instrument) = Inspections.Instrument

  type System.Object with
    member nameProvider.IsIncluded =
      if
        (CoverageParameters.nameFilters
         |> Seq.exists (Filter.``match`` nameProvider))
        || nameProvider.LocalFilter
      then
        Inspections.Ignore
      else
        Inspections.Instrument

    member nameProvider.LocalFilter: bool =
      match nameProvider with
      | :? AssemblyDefinition as a ->
        (CoverageParameters.local.Value)
        && a.MainModule
           |> ProgramDatabase.getModuleDocuments
           |> Seq.map _.Url
           |> Seq.exists File.Exists
           |> not
      | _ -> false

[<RequireQualifiedAccess>]
module internal Visitor =
  let mutable internal moduleReport =
    String.Empty

  let private accumulator =
    HashSet<AssemblyDefinition>()

  let mutable private pointNumber: int = 0
  let mutable private branchNumber: int = 0
  let mutable private methodNumber: int = 0

  let mutable internal sourceLinkDocuments: Dictionary<string, string> option =
    None

  let internal zeroPoints () =
    pointNumber <- 0
    branchNumber <- 0
    sourceLinkDocuments <- None

  module I =
    let private specialCaseFilters =
      [ @"^CompareTo\$cont\@\d+\-?\d$"
        |> Regex
        |> FilterRegex.Exclude
        |> FilterClass.Build FilterScope.Method ]

    let internal mask =
      ~~~Inspections.Instrument

    let internal updateInspection before x =
      (before &&& mask)
      ||| (before &&& Inspections.Instrument &&& x.IsIncluded)

    let internal toSeq node = List.toSeq [ node ]

    let internal getRelativeDirectoryPath (relativeTo: string) path =
      let rebase = canonicalDirectory relativeTo
      let canon = canonicalDirectory path

      if canon == rebase then
        String.Empty
      else
        let uri = Uri(Uri("file://"), rebase)

        Uri
          .UnescapeDataString(uri.MakeRelativeUri(Uri(Uri("file://"), canon)).ToString())
          .Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar) // overkill

    let internal exists (url: Uri) =
      let request = createHttp (url)
      request.Method <- "HEAD"

      try
        use response = request.GetResponse()

        response.ContentLength > 0L
        && (response :?> System.Net.HttpWebResponse).StatusCode
           |> int < 400
      with :? WebException ->
        false

    let internal findClosestMatch file (dict: Dictionary<string, string>) =
      dict.Keys
      |> Seq.filter (fun x -> x |> Path.GetFileName == "*")
      |> Seq.map (fun x ->
        (x,
         getRelativeDirectoryPath
           (x |> Path.GetDirectoryName)
           (file |> Path.GetDirectoryName)))
      |> Seq.filter (fun (x, r) -> r.IndexOf("..", StringComparison.Ordinal) < 0)
      |> Seq.sortBy (fun (x, r) -> r.Length)
      |> Seq.tryHead

    [<SuppressMessage("Microsoft.Globalization",
                      "CA1307:SpecifyStringComparison",
                      Justification = "No suitable overload in netstandard2.0/net472")>]
    let internal locateMatch file dict =
      let find = findClosestMatch file dict

      match find with
      | Some(best, relative) ->
        let replacement =
          Path.Combine(relative, Path.GetFileName(file)).Replace('\\', '/')

        let url =
          dict.[best].Replace("*", replacement)

        let map =
          if Uri(url) |> exists then url else file

        dict.Add(file, map)
        map
      | _ -> file

    let internal isFSharpStaticClass (t: TypeDefinition) =
      (t.CustomAttributes
       |> Seq.fold
         (fun x a ->
           let fn = a.AttributeType.FullName

           if
             fn
             == "Microsoft.FSharp.Core.AbstractClassAttribute"
           then
             x ||| 1
           else if fn == "Microsoft.FSharp.Core.SealedAttribute" then
             x ||| 2
           else
             x)
         0) = 3

    let internal significant (m: MethodDefinition) =
      [ Filter.isFSharpInternal
        Filter.isCSharpAutoProperty
        (fun m ->
          specialCaseFilters
          |> Seq.exists (Filter.``match`` m))
        // Constructors of sealed abstract types otherwise pollute F# coverage
        (fun m ->
          let t = m.DeclaringType
          m.IsConstructor && (isFSharpStaticClass t))
        // Constructors of compiler generated types otherwise pollute F# coverage
        (fun m ->
          let t = m.DeclaringType

          m.IsConstructor
          && ((t.IsNested
               && t.CustomAttributes
                  |> Seq.exists (fun a ->
                    a.AttributeType.FullName
                    == "System.Runtime.CompilerServices.CompilerGeneratedAttribute"))
              || m.CustomAttributes
                 |> Seq.exists (fun a ->
                   a.AttributeType.FullName
                   == "System.Runtime.CompilerServices.CompilerGeneratedAttribute"))) ]
      |> Seq.exists (fun f -> f m)
      |> not

    let private startVisit
      (paths: seq<AssemblyDespatch>)
      (buildSequence: Node -> seq<Node>)
      =
      paths
      |> Seq.collect (fun path ->
        let makeInspection x =
          x |> accumulator.Add |> ignore

          // can't delay reading symbols any more
          ProgramDatabase.readSymbols x

          // Reject completely if filtered here
          let inspection = x.IsIncluded

          let included =
            inspection
            ||| if
                  inspection = Inspections.Instrument
                  && CoverageParameters.isTracking ()
                then
                  Inspections.Track
                else
                  Inspections.Ignore

          Assembly
            { Assembly = x
              Inspection = included
              Destinations = path.Destinations
              Identity = path.Identity }

        path.AssemblyPath
        |> (AssemblyResolver.ReadAssembly
            >> makeInspection
            >> buildSequence))

    let private visitAssembly
      (a: AssemblyDefinition)
      included
      (buildSequence: Node -> seq<Node>)
      =
      a.Modules
      |> Seq.cast
      |> Seq.collect (
        (fun x ->
          let interim = updateInspection included x

          Module
            { Module = x
              Inspection =
                if interim = Inspections.Track then
                  Inspections.TrackOnly
                else
                  interim })
        >> buildSequence
      )

    let internal selectAutomatic items exemption =
      if
        items
        |> Seq.exists (fun t' ->
          (CoverageParameters.generationFilter
           |> Seq.exists (Filter.``match`` t')))
      then
        Exemption.Automatic
      else
        exemption

    // actually all vestigial classes now the first line is commented out
    let internal stripInterfaces (t: TypeDefinition) =
      // t.BaseType.IsNotNull ||
      t.Methods |> Seq.exists (_.IsAbstract >> not)

    [<SuppressMessage("Gendarme.Rules.Maintainability",
                      "AvoidUnnecessarySpecializationRule",
                      Justification = "Avoid spurious generality")>]
    let internal stripAnonymous (t: TypeDefinition) =
      t.Name.StartsWith("<>f__AnonymousType", StringComparison.Ordinal)
      |> not
      || CoverageParameters.effectiveStaticFilter ()
         <> StaticFilter.Hidden

    let private visitModule (x: ModuleEntry) (buildSequence: Node -> seq<Node>) =
      zeroPoints ()

      sourceLinkDocuments <-
        Some x.Module
        |> Option.filter (fun _ -> CoverageParameters.sourcelink.Value)
        |> Option.bind (
          _.CustomDebugInformations
          >> Seq.tryFind (fun i -> i.Kind = CustomDebugInformationKind.SourceLink)
        )
        |> Option.map (fun i ->
          let c =
            (i :?> SourceLinkDebugInformation).Content

          JsonValue
            .Parse(c)
            .Object.["documents"].Object.ToDictionary(
              (fun kv -> kv.Key),
              (fun kv -> kv.Value.String)
            ))

      [ x ]
      |> Seq.takeWhile (fun _ -> x.Inspection <> Inspections.Ignore)
      |> Seq.collect (fun x ->
        x.Module.GetAllTypes()
        |> Seq.cast<TypeDefinition>
        |> Seq.filter stripInterfaces
        |> Seq.filter stripAnonymous)
      |> Seq.collect (
        (fun t ->
          let types =
            Seq.unfold
              (fun (state: TypeDefinition) ->
                if isNull state then
                  None
                else
                  Some(
                    state,
                    if
                      CoverageParameters.topLevel
                      |> Seq.exists (Filter.``match`` state)
                    then
                      null
                    else
                      state.DeclaringType
                  ))
              t
            |> Seq.toList

          let inclusion =
            Seq.fold updateInspection x.Inspection types

          let visitcount =
            if CoverageParameters.showGenerated.Value then
              selectAutomatic types Exemption.None
            else
              Exemption.None

          Type
            { Type = t
              VisibleType = (t :: types) |> List.last
              Inspection = inclusion
              DefaultVisitCount = visitcount })
        >> buildSequence
      )

    let internal track (m: MethodDefinition) =
      let name = m.Name

      let fullname =
        m.DeclaringType.FullName.Replace('/', '.')
        + "."
        + name

      CoverageParameters.trackingNames
      |> Seq.choose (fun n ->
        if n.Chars(0) = '[' then
          let stripped = n.Trim([| '['; ']' |])

          let full =
            if stripped.EndsWith("Attribute", StringComparison.Ordinal) then
              stripped
            else
              stripped + "Attribute"

          if
            m.HasCustomAttributes
            && m.CustomAttributes
               |> Seq.map _.AttributeType
               |> Seq.tryFind (fun a -> full == a.Name || full == a.FullName)
               |> Option.isSome
          then
            Some n
          else
            None
        else if n == name || n == fullname then
          Some n
        else
          None)
      |> Seq.tryFind (fun _ -> true)
      |> Option.map (fun n ->
        let id = methodNumber + 1
        methodNumber <- id
        (id, n))

    let private cSharpContainingMethod
      (name: string)
      (ct: TypeDefinition)
      index
      predicate
      =
      let stripped =
        name.Substring(1, index).Replace('-', '.')

      let methods = ct.Methods

      let candidates =
        (methods.Concat(
          ct.DeclaringType // Hope we don't have to generalise this
          |> Option.ofObj
          |> Option.filter (fun _ -> ct.Name.StartsWith("<", StringComparison.Ordinal))
          |> Option.map (_.Methods >> Seq.toList)
          |> Option.defaultValue []
        ))
        |> Seq.filter (fun mx -> (mx.Name == stripped) && mx.HasBody)
        |> Seq.toList

      match candidates with
      | [ x ] -> Some x
      | _ ->
        let tag = "<" + stripped + ">"

        let sibs =
          ct.DeclaringType // Hope we don't have to generalise this
          |> Option.ofObj
          |> Option.map (fun c ->
            c.NestedTypes
            |> Seq.filter (fun t -> t.Name.IndexOf(tag, StringComparison.Ordinal) >= 0)
            |> Seq.collect _.Methods
            |> Seq.filter _.HasBody)
          |> Option.defaultValue ([] |> Seq.ofList)

        let peers =
          methods
          |> Seq.filter (fun mx ->
            (mx.Name.IndexOf(tag, StringComparison.Ordinal)
             >= 0)
            && mx.HasBody)

        let children =
          ct.NestedTypes
          |> Seq.filter _.Name.StartsWith("<", StringComparison.Ordinal)
          |> Seq.collect _.Methods
          |> Seq.filter (fun mx ->
            mx.HasBody
            && (mx.Name.IndexOf(tag, StringComparison.Ordinal)
                >= 0
                || mx.DeclaringType.Name.IndexOf(tag, StringComparison.Ordinal)
                   >= 0))

        candidates.Concat(sibs).Concat(peers).Concat(children)
        |> Seq.filter predicate
        |> Seq.sortBy _.DeclaringType.FullName.Split('/').Length // strive upwards
        |> Seq.tryHead

    let internal sameType (target: TypeReference) (candidate: TypeReference) =
      if target = candidate then
        true
      else if target.HasGenericParameters then
        let cname = candidate.FullName
        let last = cname.LastIndexOf('<')

        if last < 0 then
          false
        else
          let stripped = cname.Substring(0, last)
          let tname = target.FullName
          stripped == tname
      else
        false

    let internal sameFunction (target: MethodReference) (candidate: MethodReference) =
      if target = candidate then
        true
      else if sameType target.DeclaringType candidate.DeclaringType then
        let cname = candidate.Name
        let tname = target.Name
        tname == cname
      else
        false

    let internal methodConstructsType (t: TypeReference) (m: MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Newobj)
      |> Seq.exists (fun i ->
        let tn =
          (i.Operand :?> MethodReference).DeclaringType

        sameType t tn)

    let internal methodLoadsType (t: TypeReference) (m: MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Ldsfld)
      |> Seq.exists (fun i ->
        let tn =
          (i.Operand :?> FieldReference).FieldType

        sameType t tn)

    [<SuppressMessage("Gendarme.Rules.Maintainability",
                      "AvoidUnnecessarySpecializationRule",
                      Justification = "AvoidSpeculativeGenerality too")>]
    let private fSharpContainingMethod (t: TypeDefinition) (tx: TypeReference) =
      let candidates =
        t.DeclaringType.Methods.Concat(
          t.DeclaringType.NestedTypes
          |> Seq.filter (fun t2 -> (t2 :> TypeReference) <> tx)
          |> Seq.collect _.Methods
        )
        |> Seq.filter _.HasBody

      candidates
      |> Seq.tryFind (fun c ->
        (methodConstructsType tx c)
        || (methodLoadsType tx c))

    let internal methodCallsMethod (t: MethodReference) (m: MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode.FlowControl = FlowControl.Call)
      |> Seq.exists (fun i ->
        let tn = (i.Operand :?> MethodReference)
        sameFunction t tn)

    let internal methodLoadsMethod (t: MethodReference) (m: MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Ldftn)
      |> Seq.exists (fun i ->
        let tn = (i.Operand :?> MethodReference)
        sameFunction t tn)

    let internal containingMethod (m: MethodDefinition) =
      let mname = m.Name
      let t = m.DeclaringType

      // like s.IndexOf('>') but need to match paired nested angle-brackets
      let indexOfMatchingClosingAngleBracket s =
        let mutable nesting = 0

        s
        |> Seq.takeWhile (fun c ->
          if c = '<' then
            nesting <- nesting + 1

          if c = '>' then
            nesting <- nesting - 1

          nesting > 0)
        |> Seq.length

      if
        mname.StartsWith("<", StringComparison.Ordinal)
        && charIndexOf mname '|' > 0
      then
        let index =
          (indexOfMatchingClosingAngleBracket mname) - 1

        cSharpContainingMethod mname t index (fun mx ->
          (mx.FullName != m.FullName)
          && (methodCallsMethod m mx))

      else
        let n = t.Name

        if t.IsNested |> not then
          None
        else if n.StartsWith("<", StringComparison.Ordinal) then
          let name =
            if n.StartsWith("<>", StringComparison.Ordinal) then
              mname
            else
              n

          // let index = name.IndexOf('>') - 1 // but need to match paired nested angle-brackets
          let index =
            (indexOfMatchingClosingAngleBracket name) - 1

          if (index < 1) then
            None
          else
            cSharpContainingMethod
              name
              t.DeclaringType
              index
              // Guard against simple recursion here (mutual will need more work!)
              (fun mx ->
                (mx.FullName != m.FullName)
                && (methodCallsMethod m mx
                    || methodConstructsType t mx
                    || methodLoadsMethod m mx))
        else if charIndexOf n '@' >= 0 then
          let tx =
            if n.EndsWith("T", StringComparison.Ordinal) then
              match
                t.Methods
                |> Seq.tryFind (fun m ->
                  m.IsConstructor
                  && m.HasParameters
                  && (m.Parameters.Count = 1))
                |> Option.map (_.Parameters >> Seq.head)
              with
              | None -> t :> TypeReference
              | Some other -> other.ParameterType
            else
              t :> TypeReference

          fSharpContainingMethod t tx
        else
          None

    let internal selectExemption k items exemption =
      if k = StaticFilter.AsCovered then
        Exemption.StaticAnalysis
      else if CoverageParameters.showGenerated.Value then
        selectAutomatic items exemption
      else
        exemption

    let internal containingMethods m =
      Seq.unfold
        (fun (state: MethodDefinition option) ->
          match state with
          | None -> None
          | Some x ->
            Some(
              x,
              if
                CoverageParameters.topLevel
                |> Seq.exists (Filter.``match`` x)
              then
                None
              else
                let next = containingMethod x
                next
            ))
        (Some m)
      |> Seq.toList

    let private visitType (t: TypeEntry) (buildSequence: Node -> seq<Node>) =
      t.Type.Methods
      |> Seq.cast
      |> Seq.filter (fun (m: MethodDefinition) ->
        not m.IsAbstract
        && not m.IsRuntime
        && not m.IsPInvokeImpl
        && m.HasBody)
      |> Seq.map (fun m ->
        let key =
          if significant m then
            StaticFilter.NoFilter
          else
            CoverageParameters.effectiveStaticFilter ()

        (m, key))
      |> Seq.filter (fun (m, k) -> k <> StaticFilter.Hidden)
      |> Seq.map (fun (m, k) ->
        let methods = containingMethods m
        let top = methods |> Seq.last

        let topped =
          methods
          |> List.takeWhile (fun x ->
            CoverageParameters.topLevel
            |> Seq.exists (Filter.``match`` x)
            |> not)

        (m, k, topped, top))
      // Skip nested methods when in method-point mode
      |> Seq.filter (fun (_, _, methods, _) ->
        CoverageParameters.methodPoint.Value |> not
        || methods |> List.tail |> List.isEmpty)
      |> Seq.collect (
        (fun (m, k, methods, top) ->
          let visitcount =
            selectExemption k methods t.DefaultVisitCount

          let inclusion =
            Seq.fold updateInspection t.Inspection methods

          Method
            { Method = m
              VisibleMethod = top
              Inspection = inclusion
              Track = track m
              DefaultVisitCount = visitcount })
        >> buildSequence
      )

    let internal isSequencePoint (s: SequencePoint) = s.IsNotNull && s.IsHidden |> not

    let internal fakeSequencePoint
      genuine
      (seq: SequencePoint)
      (instruction: Instruction)
      =
      match seq with
      | null ->
        if
          genuine = FakeAtReturn
          && instruction.IsNotNull
          && instruction.OpCode = OpCodes.Ret
        then
          SequencePoint(instruction, Document(null))
        else
          null
      | _ -> seq

    let getSequencePoint (dbg: IDictionary<int, SequencePoint>) (i: Instruction) =
      i.Offset |> dbg.TryGetValue |> snd

    let internal findEffectiveSequencePoint
      genuine
      (dbg: IDictionary<int, SequencePoint>)
      (instructions: Instruction seq)
      =
      instructions
      |> Seq.map (fun i ->
        let seq = getSequencePoint dbg i
        fakeSequencePoint genuine seq i)
      |> Seq.tryFind isSequencePoint

    let internal findSequencePoint
      (dbg: IDictionary<int, SequencePoint>)
      (instructions: Instruction seq)
      =
      findEffectiveSequencePoint Genuine dbg instructions

    let internal indexList l = l |> List.mapi (fun i x -> (i, x))

    let internal getJumpChain (terminal: Instruction) (i: Instruction) =
      // [<TailCall>]
      let rec accumulate (state: Instruction) l =
        let gendarme = l

        if
          state.OpCode = OpCodes.Br
          || state.OpCode = OpCodes.Br_S
        then
          let target = (state.Operand :?> Instruction)
          accumulate target (target :: l)
        else if
          (state.Offset > terminal.Offset
           || state.OpCode.FlowControl = FlowControl.Cond_Branch
           || state.OpCode.FlowControl = FlowControl.Branch // Leave or Leave_S
           || state.OpCode.FlowControl = FlowControl.Break
           || state.OpCode.FlowControl = FlowControl.Throw
           || state.OpCode.FlowControl = FlowControl.Return // includes state.Next = null
           || isNull state.Next)
        then
          (if
             CoverageParameters.coalesceBranches.Value
             && state <> l.Head
           then
             state :: l
           else
             l)
        else
          accumulate state.Next gendarme

      accumulate i [ i ]

    let private boundaryOfList
      (f: (Instruction -> int) -> Instruction list -> Instruction)
      (places: Instruction list)
      =
      places |> f _.Offset

    let internal includedSequencePoint dbg (toNext: Instruction list) toJump =
      let places = List.concat [ toNext; toJump ]

      let start =
        places |> (boundaryOfList List.minBy) // This line to suppress

      let finish =
        places |> (boundaryOfList List.maxBy) // This line to suppress

      let range =
        Seq.unfold
          (fun (state: Cil.Instruction) ->
            if isNull state || finish = state.Previous then
              None
            else
              Some(state, state.Next))
          start
        |> Seq.toList

      findEffectiveSequencePoint FakeAtReturn dbg range

    [<TailCall>]
    let rec internal lastOfSequencePoint
      (dbg: IDictionary<int, SequencePoint>)
      (i: Instruction)
      =
      let n = i.Next

      if
        n |> isNull
        || n |> (getSequencePoint dbg) |> isSequencePoint
      then
        i
      else
        lastOfSequencePoint dbg n

    [<TailCall>]
    let rec internal firstOfSequencePoint
      (dbg: IDictionary<int, SequencePoint>)
      (i: Instruction)
      =
      let p = i.Previous

      if
        p |> isNull // generated code e.g Fody won't have sequence point values
        || (i |> getSequencePoint dbg).IsNotNull
      then
        i
      else
        firstOfSequencePoint dbg p

    let internal getJumps (dbg: IDictionary<int, SequencePoint>) (i: Instruction) =
      let terminal = lastOfSequencePoint dbg i
      let next = i.Next

      if i.OpCode = OpCodes.Switch then
        (i, getJumpChain terminal next, next.Offset, -1)
        :: (i.Operand :?> Instruction[]
            |> Seq.mapi (fun k d -> i, getJumpChain terminal d, d.Offset, k)
            |> Seq.toList)
      else
        let jump = i.Operand :?> Instruction
        let toNext = getJumpChain terminal next
        let toJump = getJumpChain terminal jump
        // Eliminate the "all inside one SeqPnt" jumps
        // This covers a multitude of compiler generated branching cases
        // TODO can we simplify
        match
          (CoverageParameters.coalesceBranches.Value,
           includedSequencePoint dbg toNext toJump)
        with
        | (true, _)
        | (_, Some _) ->
          [ (i, toNext, next.Offset, -1)
            (i, toJump, jump.Offset, 0) ]
        | _ -> []

    let private coalesceBranchPoints dbg (bps: GoTo seq) =
      let selectRepresentatives (whatever, bs) =
        ignore whatever

        let last =
          lastOfSequencePoint dbg (bs |> Seq.head).Start

        let lastOffset = last.Offset
        let mutable path = 0

        bs
        |> Seq.map (fun b ->
          { b with
              Target =
                b.Target
                |> List.takeWhile (fun i ->
                  let o = i.Offset

                  o > lastOffset
                  || o < b.SequencePoint.Offset
                  || i.OpCode.FlowControl = FlowControl.Return
                  || i.OpCode.FlowControl = FlowControl.Break
                  || i.OpCode.FlowControl = FlowControl.Throw
                  || i.OpCode.FlowControl = FlowControl.Branch) }) // more??
        |> Seq.groupBy (_.Target >> Seq.tryHead)
        |> Seq.map (
          snd
          >> (fun bg ->
            bg
            |> Seq.mapi (fun i bx ->
              { bx with
                  Representative =
                    if i = 0 && bx.Target |> Seq.isEmpty |> not then
                      Reporting.Representative
                    else
                      Reporting.Contributing }))
        )
        |> Seq.sortBy (fun b -> (b |> Seq.head).Offset)
        |> Seq.mapi (fun i b ->
          if i = 0 then
            path <- 0

          if (b |> Seq.head).Representative = Reporting.Representative then
            let p = path
            path <- path + 1
            b |> Seq.map (fun bx -> { bx with Path = p })
          else
            b)
      //let demoteSingletons l = // TODO revisit
      //  let x = l |> Seq.length > 1
      //  l |> Seq.map (fun bs -> bs |> Seq.map (fun b -> { b with Representative = if x then b.Representative
      //                                                                            else Reporting.None }))

      let mutable uid = 0

      bps
      |> Seq.groupBy _.SequencePoint.Offset
      |> Seq.map selectRepresentatives // >> demoteSingletons)
      |> Seq.collect id
      |> Seq.map (fun bs ->
        bs
        |> if (bs |> Seq.head).Representative = Reporting.Representative then
             let i = uid
             uid <- uid + 1
             Seq.map (fun bx -> { bx with Uid = i + branchNumber })
           else
             Seq.map (fun bx ->
               { bx with
                   Representative = Reporting.None }))
      |> Seq.collect id
      |> Seq.sortBy _.Key // important! instrumentation assumes we work in the order we started with

    let private extractBranchPoints
      (v0t: TypeReference option)
      (dbg: IDictionary<int, SequencePoint>)
      rawInstructions
      interesting
      vc
      =
      let makeDefault i =
        if CoverageParameters.coalesceBranches.Value then
          -1
        else
          i

      let processBranches =
        if CoverageParameters.coalesceBranches.Value then
          coalesceBranchPoints dbg
        else
          id

      // possibly add MoveNext filtering
      let generated (i: Instruction) =
        let before = firstOfSequencePoint dbg i // This line to suppress
        let sp = getSequencePoint dbg before

        before.OpCode = OpCodes.Ldloc_0
        && sp.IsNotNull
        && sp.IsHidden
        && (v0t.IsSome
            && v0t.Value.MetadataType = MetadataType.Int32) // state machines do this

      [ rawInstructions |> Seq.cast ]
      |> Seq.filter (fun _ -> dbg.IsNotNull)
      |> Seq.concat
      |> Seq.filter (fun (i: Instruction) ->
        i.OpCode.FlowControl = FlowControl.Cond_Branch
        && (i |> generated |> not))
      |> Seq.map (fun (i: Instruction) ->
        getJumps dbg i // if two or more jumps go between the same two places, coalesce them
        |> List.groupBy (fun (_, _, o, _) -> o)
        |> List.map (fun (_, records) ->
          let (from, target, _, _) = Seq.head records

          (from,
           target,
           records
           |> List.map (fun (_, _, _, n) -> n)
           |> List.sort))
        |> List.sortBy (fun (_, _, l) -> l.Head)
        |> indexList)
      |> Seq.filter (fun l ->
        CoverageParameters.coalesceBranches.Value
        || l.Length > 1) // TODO revisit
      |> Seq.collect id
      |> Seq.mapi (fun i (path, (from, target, indexes)) ->
        Seq.unfold
          (fun (state: Cil.Instruction) ->
            state
            |> Option.ofObj
            |> Option.map (fun state' -> (state', state'.Previous)))
          from
        |> (findSequencePoint dbg)
        |> Option.map (fun context ->
          { Start = from
            SequencePoint = context
            Indexes = indexes
            Uid = makeDefault (i + branchNumber)
            Path = makeDefault path
            Offset = from.Offset
            Target = target
            Included = interesting
            VisitCount = vc
            Representative =
              if CoverageParameters.coalesceBranches.Value then
                Reporting.Contributing
              else
                Reporting.Representative
            Key = i }))
      |> Seq.choose id
      |> processBranches
      |> Seq.map BranchPoint // AltCover.Visitor/I/Pipe #2 stage #10
      |> Seq.toList

    let internal validateInstruction
      (dbg: IDictionary<int, SequencePoint>)
      (x: Instruction)
      =
      let (yes, s) = dbg.TryGetValue x.Offset
      yes && (s.IsHidden |> not)

    let internal trivial =
      HashSet(
        [ OpCodes.Ret
          OpCodes.Br
          OpCodes.Br_S
          OpCodes.Leave
          OpCodes.Leave_S
          OpCodes.Nop ]
      )

    let internal isNonTrivialSeqPnt
      (dbg: IDictionary<int, SequencePoint>)
      (x: Instruction)
      =
      if CoverageParameters.trivia.Value then
        let rest = // rest of the sequence point
          Seq.unfold
            (fun (i: Instruction) ->
              if i |> isNull || i.Offset |> dbg.ContainsKey then
                None
              else
                Some(i, i.Next))
            x.Next

        let nt =
          x :: (rest |> Seq.toList)
          |> List.filter (_.OpCode >> trivial.Contains >> not)
          |> List.tryHead

        Option.isSome nt
      else
        true

    let private visitMethod (m: MethodEntry) =
      let body = m.Method.Body

      let rawInstructions = body.Instructions

      let splut = Dictionary<int, SequencePoint>()

      do
        m.Method.DebugInformation
        |> Option.ofObj
        |> Option.filter _.HasSequencePoints
        |> Option.iter (fun dbg ->
          dbg.SequencePoints
          |> Seq.iter (fun s -> splut.Add(s.Offset, s)))

      let instructions =
        [ rawInstructions |> Seq.cast ]
        |> Seq.filter (fun _ -> splut.Any())
        |> Seq.concat
        |> Seq.filter (fun (x: Instruction) ->
          validateInstruction splut x
          && isNonTrivialSeqPnt splut x)
        |> Seq.toList

      let number = instructions.Length
      let point = pointNumber
      pointNumber <- point + number

      let interesting =
        m.Inspection.IsInstrumented

      let wanted i (s: SequencePoint) =
        i && (s.Document.Url.IsIncluded).IsInstrumented

      let methodPointOnly () =
        interesting
        && (((instructions |> Seq.isEmpty
              || CoverageParameters.coverstyle = CoverStyle.BranchOnly)
             && rawInstructions |> Seq.isEmpty |> not)
            || CoverageParameters.methodPoint.Value)

      let sp =
        if methodPointOnly () then
          rawInstructions
          |> Seq.take 1
          |> Seq.map (fun i ->
            MethodPoint
              { Instruction = i
                SeqPnt =
                  splut.TryGetValue(i.Offset)
                  |> snd
                  |> Option.ofObj
                  |> Option.filter (fun _ -> CoverageParameters.methodPoint.Value)
                  |> Option.map SeqPnt.Build
                Uid = m.Method.MetadataToken.ToInt32()
                Interesting = interesting
                DefaultVisitCount = m.DefaultVisitCount })
        else
          instructions.OrderByDescending(fun (x: Instruction) -> x.Offset)
          |> Seq.mapi (fun i x ->
            let s = x.Offset |> splut.TryGetValue |> snd

            MethodPoint
              { Instruction = x
                SeqPnt = s |> SeqPnt.Build |> Some // AltCover.Visitor/I/sp@1
                Uid = i + point
                Interesting = wanted interesting s
                DefaultVisitCount = m.DefaultVisitCount })

      let includeBranches () =
        instructions.Any()
        && CoverageParameters.withBranches ()
        && (CoverageParameters.coverstyle
            <> CoverStyle.LineOnly)
        && (CoverageParameters.methodPoint.Value |> not)

      let bp =
        if includeBranches () then
          let spnt =
            (instructions |> Seq.head).Offset
            |> splut.TryGetValue
            |> snd

          let branches = wanted interesting spnt

          let v0t =
            body.Variables
            |> Seq.tryHead
            |> Option.map _.VariableType

          extractBranchPoints v0t splut rawInstructions branches m.DefaultVisitCount
        else
          []

      branchNumber <- branchNumber + List.length bp
      Seq.append sp bp

    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "DoNotSwallowErrorsCatchingNonSpecificExceptionsRule",
                      Justification = "Wrap & rethrow")>]
    [<SuppressMessage("Gendarme.Rules.Globalization",
                      "PreferStringComparisonOverrideRule",
                      Justification = "No suitable overload in netstandard2.0/net472")>]
    let internal wrap op node =
      try
        op node
      with x ->
        let raw = sprintf "%A" node

        let where =
          match node with
          | MethodPoint p when Option.isSome p.SeqPnt ->
            raw.Replace("Mono.Cecil.Cil.Document", p.SeqPnt.Value.Document.Url)
          | BranchPoint b ->
            raw.Replace("Mono.Cecil.Cil.Document", b.SequencePoint.Document.Url)
          | _ -> raw

        let message =
          String.Format(
            CultureInfo.CurrentCulture,
            Output.resources.GetString "%s while visiting %A",
            x.Message,
            where
          )

        raise (InvalidOperationException(message, x))

    [<TailCall>]
    let rec internal deeper node =
      let visit n =
        // The pattern here is map x |> map y |> map x |> concat => collect (x >> y >> z)
        match n with
        | Start paths -> startVisit paths sequenceBuilder
        | Assembly a -> visitAssembly a.Assembly a.Inspection sequenceBuilder
        | Module m -> visitModule m sequenceBuilder
        | Type t -> visitType t sequenceBuilder
        | Method m -> visitMethod m
        | _ -> Seq.empty<Node>

      wrap visit node

    and internal sequenceBuilder node =
      Seq.concat
        [ toSeq node
          deeper node
          node.After() ]

    let internal invoke (node: Node) (visitor: Fix<Node>) = visitor.Invoke(node)

    let internal apply (visitors: list<Fix<Node>>) (node: Node) =
      visitors |> List.map (invoke node)

  // "Public" API
  let internal visit (visitors: seq<Fix<Node>>) (assemblies: seq<AssemblyDespatch>) =
    zeroPoints ()
    methodNumber <- 0

    try
      Start assemblies
      |> I.sequenceBuilder
      |> Seq.fold I.apply (visitors |> Seq.toList)
      |> ignore
    finally
      accumulator
      |> Seq.iter (fun a -> (a :> IDisposable).Dispose())

      accumulator.Clear()

  let internal encloseState (visitor: 'TState -> 'T -> 'TState) (current: 'TState) =
    // [<TailCall>]
    let rec stateful l =
      new Fix<'T>(fun (node: 'T) ->
        let next = visitor l node
        stateful next)

    stateful current

  let internal sourceLinkMapping file =
    match sourceLinkDocuments with
    | None -> file
    | Some dict ->
      match dict.TryGetValue file with
      | (true, url) -> url
      | _ -> I.locateMatch file dict

[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidMessageChainsRule",
                            Scope = "member",
                            Target =
                              "AltCover.Visitor/I/generated@1411::Invoke(Mono.Cecil.Cil.Instruction)",
                            Justification = "No direct call available")>]
[<assembly: SuppressMessage("Gendarme.Rules.Exceptions",
                            "InstantiateArgumentExceptionCorrectlyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Visitor/I/start@1250::Invoke(Microsoft.FSharp.Core.FSharpFunc`2<Mono.Cecil.Cil.Instruction,System.Int32>,Microsoft.FSharp.Collections.FSharpList`1<Mono.Cecil.Cil.Instruction>)",
                            Justification = "Inlined library code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Exceptions",
                            "InstantiateArgumentExceptionCorrectlyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Visitor/I/start@1253::Invoke(Microsoft.FSharp.Core.FSharpFunc`2<Mono.Cecil.Cil.Instruction,System.Int32>,Microsoft.FSharp.Collections.FSharpList`1<Mono.Cecil.Cil.Instruction>)",
                            Justification = "Inlined library code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Visitor/I/sp@1584-2::Invoke(AltCover.SeqPnt)",
                            Justification = "Inlined library code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Visitor/I/Pipe #2 stage #10 at line 1477@1477::Invoke(AltCover.GoTo)",
                            Justification = "Inlined library code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Exceptions",
                            "InstantiateArgumentExceptionCorrectlyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Visitor/I/finish@1256::Invoke(Microsoft.FSharp.Core.FSharpFunc`2<Mono.Cecil.Cil.Instruction,System.Int32>,Microsoft.FSharp.Collections.FSharpList`1<Mono.Cecil.Cil.Instruction>)",
                            Justification = "Inlined library code")>]
()