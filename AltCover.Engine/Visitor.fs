// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
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

[<Flags>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Design",
      "FlagsShouldNotDefineAZeroValueRule",
      Justification="Gives the unset state a name")>]
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
  { StartLine : int
    StartColumn : int
    EndLine : int
    EndColumn : int
    Document : string
    Offset : int }
  static member Build(codeSegment : Cil.SequencePoint) =
    { StartLine = codeSegment.StartLine
      StartColumn = codeSegment.StartColumn
      EndLine =
        if codeSegment.EndLine < 0 then codeSegment.StartLine else codeSegment.EndLine
      EndColumn =
        if codeSegment.EndLine < 0 then
          codeSegment.StartColumn + 1
        else
          codeSegment.EndColumn
      Document = codeSegment.Document.Url
      Offset = codeSegment.Offset }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal GoTo =
  { Start : Instruction
    SequencePoint : SequencePoint
    Indexes : int list
    Uid : int
    Path : int
    Offset : int
    [<NonSerialized>] Target : Instruction list
    Included : bool
    VisitCount : Exemption
    Representative : Reporting
    Key : int }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal Node =
  | Start of seq<string * string list>
  | Assembly of AssemblyDefinition * Inspections * string list
  | Module of ModuleDefinition * Inspections
  | Type of TypeDefinition * Inspections * Exemption
  | Method of MethodDefinition * Inspections * (int * string) option * Exemption
  | MethodPoint of Instruction * SeqPnt option * int * bool * Exemption
  | BranchPoint of GoTo
  | AfterMethod of MethodDefinition * Inspections * (int * string) option
  | AfterType
  | AfterModule
  | AfterAssembly of AssemblyDefinition * string list
  | Finish
  member this.After() =
    (match this with
     | Start _ -> [ Finish ]
     | Assembly(a, _, l) -> [ AfterAssembly(a, l) ]
     | Module _ -> [ AfterModule ]
     | Type _ -> [ AfterType ]
     | Method(m, included, track, _) -> [ AfterMethod(m, included, track) ]
     | _ -> [])
    |> List.toSeq

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal StrongNameKeyData =
  { Blob : byte list
    Parameters : System.Security.Cryptography.RSAParameters }

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
        this.Parameters.Modulus
        |> Seq.toList
        |> List.rev ]

  static member Make(data : byte array) =
    use csp = new System.Security.Cryptography.RSACryptoServiceProvider()
    csp.ImportCspBlob(data)
    let blob = csp.ExportCspBlob(true)
    { Blob = blob |> Array.toList
      Parameters = csp.ExportParameters(true) }

  static member Empty() =
    { Blob = []
      Parameters = System.Security.Cryptography.RSAParameters() }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal KeyRecord =
  { Pair : StrongNameKeyData
    Token : byte list }

[<ExcludeFromCodeCoverage; NoComparison>]
type internal SequenceType =
  | Genuine
  | FakeAfterReturn

[<RequireQualifiedAccess>]
module internal KeyStore =
  let private hash = new System.Security.Cryptography.SHA1CryptoServiceProvider()
  let private publicKeyOfKey (key : StrongNameKeyData) = key.PublicKey

  module internal I =
    let internal tokenOfArray(key : byte seq) =
      key
      |> Seq.toArray
      |> hash.ComputeHash
      |> Array.rev
      |> Array.take 8

  let internal tokenOfKey(key : StrongNameKeyData) =
    key
    |> publicKeyOfKey
    |> I.tokenOfArray
    |> Array.toList

  let internal tokenAsULong(token : byte array) = BitConverter.ToUInt64(token, 0)

  let internal keyToIndex(key : StrongNameKeyData) =
    key
    |> tokenOfKey
    |> List.toArray
    |> tokenAsULong

  let internal arrayToIndex(key : byte array) =
    key
    |> I.tokenOfArray
    |> tokenAsULong

  let internal keyToRecord(key : StrongNameKeyData) =
    { Pair = key
      Token = tokenOfKey key }

  let internal hashFile sPath =
    use stream = File.OpenRead sPath
    stream
    |> hash.ComputeHash
    |> BitConverter.ToString

[<ExcludeFromCodeCoverage; SuppressMessage("Gendarme.Rules.Design.Generic",
   "AvoidDeclaringCustomDelegatesRule",
    Justification="Recursive type definition can't be done with Fix<'T> = Func<'T, Fix<'T>>")>]
[<SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="Anonymous parameter")>]
type internal Fix<'T> = delegate of 'T -> Fix<'T>

[<RequireQualifiedAccess>]
module internal CoverageParameters =

  let internal methodPoint = ref false // ddFlag
  let internal collect = ref false // ddFlag
  let internal trackingNames = new List<String>()
  let internal topLevel = new List<FilterClass>()
  let internal nameFilters = new List<FilterClass>()

  let mutable internal staticFilter : StaticFilter option = None
  let internal showGenerated = ref false

  let generationFilter =
    [ "System.Runtime.CompilerServices.CompilerGeneratedAttribute"
      "System.CodeDom.Compiler.GeneratedCodeAttribute" ]
    |> List.map
         (Regex
          >> FilterRegex.Exclude
          >> (FilterClass.Build FilterScope.Attribute))

  let internal inplace = ref false // ddFlag
  let internal coalesceBranches = ref false // ddFlag
  let internal local = ref false // ddFlag

  let mutable internal single = false // more complicated

  let internal sampling() =
    (if single then Sampling.Single else Sampling.All)
    |> int

  let internal sourcelink = ref false // ddFlag
  let internal defer = ref false

  let internal deferOpCode() =
    if !defer then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0

  let internal theInputDirectories = List<string>()
  let private defaultInputDirectory = "."

  let internal inputDirectories() =
    if theInputDirectories.Any()
    then theInputDirectories |> Seq.toList
    else [ defaultInputDirectory ] |> List.map Path.GetFullPath

  let internal inplaceSelection a b =
    if !inplace then a else b

  let internal theOutputDirectories = List<string>()
  let private defaultOutputDirectory _ = inplaceSelection "__Saved" "__Instrumented"

  let internal outputDirectories() =
    let paired = inputDirectories()
    Seq.append (theOutputDirectories :> string seq)
      (Seq.initInfinite defaultOutputDirectory)
    |> Seq.zip paired
    |> Seq.map (fun (i, o) -> Path.Combine(i, o) |> Path.GetFullPath)
    |> Seq.toList

  let internal instrumentDirectories() = (inplaceSelection inputDirectories outputDirectories)()
  let internal sourceDirectories() = (inplaceSelection outputDirectories inputDirectories)()

  let mutable internal theReportPath : Option<string> = None
  let internal zipReport = ref false // ddFlag
  let internal defaultReportPath = "coverage.xml"
  let internal reportPath() = Path.GetFullPath(Option.defaultValue defaultReportPath theReportPath)

  let mutable internal theInterval : Option<int> = None
  let internal defaultInterval = 0
  let internal interval() = (Option.defaultValue defaultInterval theInterval)

  let mutable internal theReportFormat : Option<ReportFormat> = None
  let mutable internal coverstyle = CoverStyle.All

  let internal reportKind() = (Option.defaultValue ReportFormat.OpenCover theReportFormat)

  let internal reportFormat() =
    let fmt = reportKind()
    if fmt = ReportFormat.OpenCover && (trackingNames.Any() || interval() > 0)
    then ReportFormat.OpenCoverWithTracking
    else fmt

  let mutable internal defaultStrongNameKey : option<StrongNameKeyData> = None
  let mutable internal recorderStrongNameKey : option<StrongNameKeyData> = None
  let internal keys = new Dictionary<UInt64, KeyRecord>()
  let internal add(key : StrongNameKeyData) =
    let index = KeyStore.keyToIndex key
    keys.[index] <- KeyStore.keyToRecord key

[<AutoOpen>]
module internal Inspector =
  type Inspections with
    member self.IsInstrumented
      with get() =
        (self &&& Inspections.Instrument) = Inspections.Instrument

  type System.Object with
    member nameProvider.IsIncluded
      with get () =
        if (CoverageParameters.nameFilters |> Seq.exists (Filter.``match`` nameProvider))
            || nameProvider.LocalFilter then
          Inspections.Ignore
        else
          Inspections.Instrument

    member nameProvider.LocalFilter
      with get () : bool =
        let methodFile (m : MethodDefinition) =
          m.DebugInformation.SequencePoints
          |> Seq.tryHead // assume methods can only be in one file
          |> Option.map (fun sp -> sp.Document.Url)

        let typeFiles (t : TypeDefinition) =
          Option.ofObj t.Methods
          |> Option.map (fun ms ->
               ms
               |> Seq.map methodFile
               |> Seq.choose id
               |> Seq.distinct)

        let moduleFiles (m : ModuleDefinition) =
          m.GetAllTypes()
          |> Seq.map typeFiles
          |> Seq.choose id
          |> Seq.collect id
          |> Seq.distinct

        match nameProvider with
        | :? AssemblyDefinition as a ->
            (!CoverageParameters.local) &&
                    a.MainModule
                    |> moduleFiles
                    |> Seq.tryHead
                    |> Option.map File.Exists
                    |> Option.defaultValue false
                    |> not
        | _ -> false

[<RequireQualifiedAccess>]
module internal Visitor =
  let private accumulator = HashSet<AssemblyDefinition>()
  let mutable private pointNumber : int = 0
  let mutable private branchNumber : int = 0
  let mutable private methodNumber : int = 0
  let mutable internal sourceLinkDocuments : Dictionary<string, string> option = None

  let internal zeroPoints() =
    pointNumber <- 0
    branchNumber <- 0
    sourceLinkDocuments <- None

  [<SuppressMessage("Microsoft.Maintainability", "CA1506",
                    Justification = "partitioned into closures")>]
  module I =
    let private specialCaseFilters =
      [ @"^CompareTo\$cont\@\d+\-?\d$"
        |> Regex
        |> FilterRegex.Exclude
        |> FilterClass.Build FilterScope.Method ]

    let internal mask = ~~~Inspections.Instrument

    let internal updateInspection before x =
      (before &&& mask) ||| (before &&& Inspections.Instrument &&& x.IsIncluded)
    let internal toSeq node = List.toSeq [ node ]

    let internal ensureEndsWith c (s : string) =
      if s.EndsWith(c, StringComparison.Ordinal) then s else s + c

    let internal getRelativePath (relativeTo : string) path =
      if Path.GetFullPath path = Path.GetFullPath relativeTo then
        String.Empty
      else
        let ender = ensureEndsWith <| Path.DirectorySeparatorChar.ToString()
        let uri = new Uri(ender relativeTo)
        Uri.UnescapeDataString(uri.MakeRelativeUri(new Uri(path)).ToString())
           .Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar)

    let internal exists(url : Uri) =
      let request = System.Net.WebRequest.CreateHttp(url)
      request.Method <- "HEAD"
      try
        use response = request.GetResponse()
        response.ContentLength > 0L && (response :?> System.Net.HttpWebResponse).StatusCode
                                       |> int < 400
      with :? WebException -> false

    let internal findClosestMatch file (dict : Dictionary<string, string>) =
      dict.Keys
      |> Seq.filter (fun x ->
           x
           |> Path.GetFileName = "*")
      |> Seq.map (fun x ->
           (x,
            getRelativePath (x |> Path.GetDirectoryName) (file |> Path.GetDirectoryName)))
      |> Seq.filter (fun (x, r) -> r.IndexOf("..", StringComparison.Ordinal) < 0)
      |> Seq.sortBy (fun (x, r) -> r.Length)
      |> Seq.tryHead

    [<SuppressMessage("Microsoft.Usage", "CA2208:InstantiateArgumentExceptionsCorrectly",
                      Justification = "F# inlined code")>]
    let internal locateMatch file dict =
      let find = findClosestMatch file dict

      match find with
      | Some(best, relative) ->
          let replacement =
            Path.Combine(relative, Path.GetFileName(file)).Replace('\\', '/')
          let url = dict.[best].Replace("*", replacement)

          let map =
            if Uri(url) |> exists then url else file
          dict.Add(file, map)
          map
      | _ -> file

    let internal isFSharpStaticClass (t : TypeDefinition) =
      (t.CustomAttributes
       |> Seq.fold
          (fun x a -> let fn = a.AttributeType.FullName
                      if fn = "Microsoft.FSharp.Core.AbstractClassAttribute"
                      then x ||| 1
                      else if fn = "Microsoft.FSharp.Core.SealedAttribute"
                            then x ||| 2
                            else x ) 0) = 3

    let internal significant (m : MethodDefinition) =
      [ Filter.isFSharpInternal
        Filter.isCSharpAutoProperty
        (fun m -> specialCaseFilters |> Seq.exists (Filter.``match`` m))
        // Constructors of sealed abstract types otherwise pollute F# coverage
        (fun m ->
          let t = m.DeclaringType
          m.IsConstructor
          && (isFSharpStaticClass t))
        // Constructors of compiler generated types otherwise pollute F# coverage
        (fun m ->
          let t = m.DeclaringType
          m.IsConstructor
          && ((t.IsNested
              && t.CustomAttributes
                 |> Seq.exists
                      (fun a ->
                        a.AttributeType.FullName =
                          "System.Runtime.CompilerServices.CompilerGeneratedAttribute"))
          || m.CustomAttributes
             |> Seq.exists
                  (fun a ->
                    a.AttributeType.FullName =
                      "System.Runtime.CompilerServices.CompilerGeneratedAttribute"))) ]
      |> Seq.exists (fun f -> f m)
      |> not

    let private startVisit (paths : seq<string * string list>)
        (buildSequence : Node -> seq<Node>) =
      paths
      |> Seq.collect (fun (path, targets) ->
           let makeInspection x =
             x
             |> accumulator.Add
             |> ignore

             // can't delay reading symbols any more
             ProgramDatabase.readSymbols x

             // Reject completely if filtered here
             let inspection = x.IsIncluded

             let included =
               inspection ||| if inspection = Inspections.Instrument
                                 && CoverageParameters.reportFormat() = ReportFormat.OpenCoverWithTracking then
                                Inspections.Track
                              else
                                Inspections.Ignore
             Assembly(x, included, targets)

           path
           |> (AssemblyDefinition.ReadAssembly
               >> makeInspection
               >> buildSequence))

    let private visitAssembly (a : AssemblyDefinition) included (buildSequence : Node -> seq<Node>) =
      a.Modules
      |> Seq.cast
      |> Seq.collect
           ((fun x ->
             let interim = updateInspection included x
             Module
               (x,
                (if interim = Inspections.Track then Inspections.TrackOnly else interim)))
            >> buildSequence)

    let internal selectAutomatic items exemption =
      if items |> Seq.exists (fun t' -> (CoverageParameters.generationFilter |> Seq.exists (Filter.``match`` t')))
      then Exemption.Automatic
      else exemption

    // actually all vestigial classes now the first line is commented out
    let internal stripInterfaces (t:TypeDefinition) =
      // t.BaseType.IsNotNull ||
      t.Methods |> Seq.exists (fun m -> m.IsAbstract |> not)

    let private visitModule (x : ModuleDefinition) included (buildSequence : Node -> seq<Node>) =
      zeroPoints()
      sourceLinkDocuments <-
        Some x
        |> Option.filter (fun _ -> !CoverageParameters.sourcelink)
        |> Option.map (fun x ->
             x.CustomDebugInformations
             |> Seq.tryFind (fun i -> i.Kind = CustomDebugInformationKind.SourceLink))
        |> Option.bind id
        |> Option.map (fun i ->
             let c = (i :?> SourceLinkDebugInformation).Content
             JsonValue.Parse(c).Object.["documents"].Object.
                            ToDictionary((fun kv -> kv.Key),
                                         (fun kv -> kv.Value.String)))

      [ x ]
      |> Seq.takeWhile (fun _ -> included <> Inspections.Ignore)
      |> Seq.collect (fun x -> x.GetAllTypes()
                               |> Seq.cast<TypeDefinition>
                               |> Seq.filter stripInterfaces)
      |> Seq.collect
           ((fun t ->
             let types =
               Seq.unfold
                 (fun (state : TypeDefinition) ->
                   if isNull state then None
                   else
                    Some(state, if CoverageParameters.topLevel
                                   |> Seq.exists (Filter.``match`` state)
                                then null
                                else state.DeclaringType)) t
               |> Seq.toList

             let inclusion = Seq.fold updateInspection included types

             let visitcount =
               if !CoverageParameters.showGenerated then
                 selectAutomatic types Exemption.None
               else
                 Exemption.None
             Type(t, inclusion, visitcount))
            >> buildSequence)

    let internal track(m : MethodDefinition) =
      let name = m.Name
      let fullname = m.DeclaringType.FullName.Replace('/', '.') + "." + name
      CoverageParameters.trackingNames
      |> Seq.map (fun n ->
           if n.Chars(0) = '[' then
             let stripped = n.Trim([| '['; ']' |])

             let full =
               if stripped.EndsWith("Attribute", StringComparison.Ordinal)
               then stripped
               else stripped + "Attribute"
             if m.HasCustomAttributes && m.CustomAttributes
                                         |> Seq.map (fun a -> a.AttributeType)
                                         |> Seq.tryFind (fun a ->
                                              full = a.Name || full = a.FullName)
                                         |> Option.isSome then
               Some n
             else
               None
           else if n = name || n = fullname then
             Some n
           else
             None)
      |> Seq.choose id
      |> Seq.tryFind (fun _ -> true)
      |> Option.map (fun n ->
           let id = methodNumber + 1
           methodNumber <- id
           (id, n))

    let private cSharpContainingMethod (name : string) (ct : TypeDefinition) index predicate =
      let stripped = name.Substring(1, index).Replace('-', '.')
      let methods = ct.Methods

      let candidates =
        methods
        |> Seq.filter (fun mx -> (mx.Name = stripped) && mx.HasBody)
        |> Seq.toList
      match candidates with
      | [ x ] -> Some x
      | _ ->
          let tag = "<" + stripped + ">"
          candidates.Concat(methods
                            |> Seq.filter (fun mx ->
                                 (mx.Name.IndexOf(tag, StringComparison.Ordinal) >= 0)
                                 && mx.HasBody))
                    .Concat(ct.NestedTypes
                            |> Seq.filter
                                 (fun tx ->
                                   tx.Name.StartsWith("<", StringComparison.Ordinal))
                            |> Seq.collect (fun tx -> tx.Methods)
                            |> Seq.filter (fun mx ->
                                 mx.HasBody
                                 && (mx.Name.IndexOf(tag, StringComparison.Ordinal) >= 0
                                     || mx.DeclaringType.Name.IndexOf
                                          (tag, StringComparison.Ordinal) >= 0)))
          |> Seq.tryFind predicate

    let internal sameType (target : TypeReference) (candidate : TypeReference) =
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
          stripped.Equals(tname)
      else
        false

    let internal sameFunction (target : MethodReference) (candidate : MethodReference) =
      if target = candidate then
        true
      else if sameType target.DeclaringType candidate.DeclaringType then
        let cname = candidate.Name
        let tname = target.Name
        tname.Equals cname
      else
        false

    let internal methodConstructsType (t : TypeReference) (m : MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Newobj)
      |> Seq.exists (fun i ->
           let tn = (i.Operand :?> MethodReference).DeclaringType
           sameType t tn)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let private fSharpContainingMethod (t : TypeDefinition) (tx : TypeReference) =
      let candidates =
        t.DeclaringType.Methods.Concat
          (t.DeclaringType.NestedTypes
           |> Seq.filter (fun t2 -> (t2 :> TypeReference) <> tx)
           |> Seq.collect (fun t2 -> t2.Methods))
        |> Seq.filter (fun m -> m.HasBody)
      candidates |> Seq.tryFind (methodConstructsType tx)

    let internal methodCallsMethod (t : MethodReference) (m : MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Call)
      |> Seq.exists (fun i ->
           let tn = (i.Operand :?> MethodReference)
           sameFunction t tn)

    let internal methodLoadsMethod (t : MethodReference) (m : MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Ldftn)
      |> Seq.exists (fun i ->
           let tn = (i.Operand :?> MethodReference)
           sameFunction t tn)

    let internal containingMethod(m : MethodDefinition) =
      let mname = m.Name
      let t = m.DeclaringType

      // like s.IndexOf('>') but need to match paired nested angle-brackets
      let indexOfMatchingClosingAngleBracket s =
        let mutable nesting = 0
        s
        |> Seq.takeWhile (fun c ->
             if c = '<' then nesting <- nesting + 1
             if c = '>' then nesting <- nesting - 1
             nesting > 0)
        |> Seq.length

      if mname.StartsWith("<", StringComparison.Ordinal) && mname.IndexOf('|') > 0 then
        let index = (indexOfMatchingClosingAngleBracket mname) - 1
        cSharpContainingMethod mname t index (methodCallsMethod m)
      else
        let n = t.Name
        if t.IsNested |> not then
          None
        else if n.StartsWith("<", StringComparison.Ordinal) then
          let name =
            if n.StartsWith("<>", StringComparison.Ordinal)
            then mname
            else n

          // let index = name.IndexOf('>') - 1 // but need to match paired nested angle-brackets
          let index = (indexOfMatchingClosingAngleBracket name) - 1
          if (index < 1) then
            None
          else
            cSharpContainingMethod name t.DeclaringType index
              // Guard against simple recursion here (mutual will need more work!)
              (fun mx ->
                (mx.FullName <> m.FullName)
                && (methodCallsMethod m mx || methodConstructsType t mx
                    || methodLoadsMethod m mx))
        else if n.IndexOf('@') >= 0 then
          let tx =
            if n.EndsWith("T", StringComparison.Ordinal) then
              match t.Methods
                    |> Seq.tryFind (fun m ->
                         m.IsConstructor && m.HasParameters && (m.Parameters.Count = 1))
                    |> Option.map (fun m -> m.Parameters |> Seq.head) with
              | None -> t :> TypeReference
              | Some other -> other.ParameterType
            else
              t :> TypeReference
          fSharpContainingMethod t tx
        else
          None

    let internal selectExemption k items exemption =
      if k = StaticFilter.AsCovered then Exemption.StaticAnalysis
      else if !CoverageParameters.showGenerated then selectAutomatic items exemption
      else exemption

    let private visitType (t : TypeDefinition) included basevc (buildSequence : Node -> seq<Node>) =
      t.Methods
      |> Seq.cast
      |> Seq.filter (fun (m : MethodDefinition) ->
           not m.IsAbstract && not m.IsRuntime && not m.IsPInvokeImpl && m.HasBody)
      |> Seq.map (fun m ->
           let key =
             if significant m then
               StaticFilter.NoFilter
             else
               match CoverageParameters.staticFilter with
               | None -> StaticFilter.Hidden
               | Some f -> f
           (m, key))
      |> Seq.filter (fun (m, k) -> k <> StaticFilter.Hidden)
      |> Seq.map (fun (m, k) -> let methods =
                                 Seq.unfold (fun (state : MethodDefinition option) ->
                                   match state with
                                   | None -> None
                                   | Some x -> Some(x, if CoverageParameters.topLevel
                                                          |> Seq.exists (Filter.``match`` x)
                                                       then None
                                                       else containingMethod x)) (Some m)
                                 |> Seq.toList
                                (m, k, methods))
      // Skip nested methods when in method-point mode
      |> Seq.filter (fun (_,_,methods) -> !CoverageParameters.methodPoint |> not ||
                                          methods |> List.tail |> List.isEmpty)
      |> Seq.collect
           ((fun (m, k, methods) ->
             let visitcount = selectExemption k methods basevc

             let inclusion = Seq.fold updateInspection included methods
             Method(m, inclusion, track m, visitcount))
            >> buildSequence)

    let internal isSequencePoint(s : SequencePoint) =
      s.IsNotNull
      && s.IsHidden |> not

    let internal fakeSequencePoint genuine (seq : SequencePoint) (instruction : Instruction) =
      match seq with
      | null ->
          if genuine = FakeAfterReturn && instruction.IsNotNull
             && instruction.OpCode = OpCodes.Ret then
            SequencePoint(instruction, Document(null))
          else
            null
      | _ -> seq

    let internal findEffectiveSequencePoint genuine (dbg : MethodDebugInformation)
        (instructions : Instruction seq) =
      instructions
      |> Seq.map (fun i ->
           let seq = dbg.GetSequencePoint i
           fakeSequencePoint genuine seq i.Previous)
      |> Seq.tryFind isSequencePoint

    let internal findSequencePoint (dbg : MethodDebugInformation) (instructions : Instruction seq) =
      findEffectiveSequencePoint Genuine dbg instructions

    let internal indexList l = l |> List.mapi (fun i x -> (i, x))

    let internal getJumpChain (terminal : Instruction) (i : Instruction) =
      let rec accumulate (state : Instruction) l =
        let gendarme = l
        if state.OpCode = OpCodes.Br || state.OpCode = OpCodes.Br_S then
          let target = (state.Operand :?> Instruction)
          accumulate target (target :: l)
        else if (state.Offset > terminal.Offset
                 || state.OpCode.FlowControl = FlowControl.Cond_Branch
                 || state.OpCode.FlowControl = FlowControl.Branch // Leave or Leave_S
                 || state.OpCode.FlowControl = FlowControl.Break
                 || state.OpCode.FlowControl = FlowControl.Throw
                 || state.OpCode.FlowControl =
                      FlowControl.Return // includes state.Next = null
                 || isNull state.Next) then
          (if !CoverageParameters.coalesceBranches && state <> l.Head then state :: l else l)
        else
          accumulate state.Next gendarme
      accumulate i [ i ]

    let private boundaryOfList (f : (Instruction -> int) -> Instruction list -> Instruction)
        (places : Instruction list) = places |> f (fun i -> i.Offset)

    let internal includedSequencePoint dbg (toNext : Instruction list) toJump =
      let places = List.concat [ toNext; toJump ]
      let start = places |> (boundaryOfList List.minBy)
      let finish = places |> (boundaryOfList List.maxBy)

      let range =
        Seq.unfold (fun (state : Cil.Instruction) ->
          if isNull state || finish = state.Previous then None else Some(state, state.Next))
          start
        |> Seq.toList
      findEffectiveSequencePoint FakeAfterReturn dbg range

    let rec internal lastOfSequencePoint (dbg : MethodDebugInformation) (i : Instruction) =
      let n = i.Next
      if n
         |> isNull
         || n
            |> dbg.GetSequencePoint
            |> isSequencePoint then
        i
      else
        lastOfSequencePoint dbg n

    let internal getJumps (dbg : MethodDebugInformation) (i : Instruction) =
      let terminal = lastOfSequencePoint dbg i
      let next = i.Next
      if i.OpCode = OpCodes.Switch then
        (i, getJumpChain terminal next, next.Offset, -1) :: (i.Operand :?> Instruction []
                                                             |> Seq.mapi
                                                                  (fun k d ->
                                                                    i,
                                                                    getJumpChain terminal d,
                                                                    d.Offset, k)
                                                             |> Seq.toList)
      else
        let jump = i.Operand :?> Instruction
        let toNext = getJumpChain terminal next
        let toJump = getJumpChain terminal jump
        // Eliminate the "all inside one SeqPnt" jumps
        // This covers a multitude of compiler generated branching cases
        // TODO can we simplify
        match (!CoverageParameters.coalesceBranches, includedSequencePoint dbg toNext toJump) with
        | (true, _)
        | (_, Some _) ->
            [ (i, toNext, next.Offset, -1)
              (i, toJump, jump.Offset, 0) ]
        | _ -> []

    // cribbed from OpenCover's CecilSymbolManager -- internals of C# yield return iterators
    let private isMoveNext =
      Regex
        (@"\<[^\s>]+\>\w__\w(\w)?::MoveNext\(\)$",
         RegexOptions.Compiled ||| RegexOptions.ExplicitCapture)

    let private coalesceBranchPoints dbg (bps : GoTo seq) =
      let selectRepresentatives (_, bs) =
        let last = lastOfSequencePoint dbg (bs |> Seq.head).Start
        let lastOffset = last.Offset
        let mutable path = 0
        bs
        |> Seq.map (fun b ->
             { b with
                 Target =
                   b.Target
                   |> List.takeWhile (fun i ->
                        let o = i.Offset
                        o > lastOffset || o < b.SequencePoint.Offset
                        || i.OpCode.FlowControl = FlowControl.Return
                        || i.OpCode.FlowControl = FlowControl.Break
                        || i.OpCode.FlowControl = FlowControl.Throw
                        || i.OpCode.FlowControl = FlowControl.Branch) }) // more??
        |> Seq.groupBy (fun b -> b.Target |> Seq.tryHead)
        |> Seq.map
             (snd
              >> (fun bg ->
              bg
              |> Seq.mapi (fun i bx ->
                   { bx with
                       Representative =
                         if i = 0 && bx.Target
                                     |> Seq.isEmpty
                                     |> not then
                           Reporting.Representative
                         else
                           Reporting.Contributing })))
        |> Seq.sortBy (fun b -> (b |> Seq.head).Offset)
        |> Seq.mapi (fun i b ->
             if i = 0 then path <- 0
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
      |> Seq.groupBy (fun b -> b.SequencePoint.Offset)
      |> Seq.map selectRepresentatives // >> demoteSingletons)
      |> Seq.collect id
      |> Seq.map (fun bs ->
           bs
           |> if (bs |> Seq.head).Representative = Reporting.Representative then
                let i = uid
                uid <- uid + 1
                Seq.map (fun bx -> { bx with Uid = i + branchNumber })
              else
                Seq.map (fun bx -> { bx with Representative = Reporting.None }))
      |> Seq.collect id
      |> Seq.sortBy
           (fun b -> b.Key) // important! instrumentation assumes we work in the order we started with

    [<SuppressMessage("Microsoft.Maintainability", "CA1506",
                      Justification = "partitioned into closures")>]
    let private extractBranchPoints dbg methodFullName rawInstructions interesting vc =
      let makeDefault i =
        if !CoverageParameters.coalesceBranches then -1 else i

      let processBranches =
        if !CoverageParameters.coalesceBranches then coalesceBranchPoints dbg else id

      // Generated MoveNext => skip one branch
      let skip = (isMoveNext.IsMatch methodFullName).ToInt32
      (Seq.map
        (snd
         >> (fun (i : Instruction) ->
         getJumps dbg
           i // if two or more jumps go between the same two places, coalesce them
         |> List.groupBy (fun (_, _, o, _) -> o)
         |> List.map (fun (_, records) ->
              let (from, target, _, _) = Seq.head records
              (from, target,
               records
               |> List.map (fun (_, _, _, n) -> n)
               |> List.sort))
         |> List.sortBy (fun (_, _, l) -> l.Head)
         |> indexList))
         ([ rawInstructions |> Seq.cast ]
          |> Seq.filter (fun _ ->
               dbg.IsNotNull)
          |> Seq.concat
          |> Seq.filter
               (fun (i : Instruction) -> i.OpCode.FlowControl = FlowControl.Cond_Branch)
          |> Seq.mapi (fun n i -> (n, i)) //
          |> Seq.filter (fun (n, _) -> n >= skip)))
      |> Seq.filter (fun l -> !CoverageParameters.coalesceBranches || l.Length > 1) // TODO revisit
      |> Seq.collect id
      |> Seq.mapi (fun i (path, (from, target, indexes)) ->
           Seq.unfold (fun (state : Cil.Instruction) ->
             state
             |> Option.ofObj
             |> Option.map (fun state' -> (state', state'.Previous))) from
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
                    if !CoverageParameters.coalesceBranches then
                      Reporting.Contributing
                    else
                      Reporting.Representative
                  Key = i }))
      |> Seq.choose id
      |> processBranches
      |> Seq.map BranchPoint
      |> Seq.toList

    let private visitMethod (m : MethodDefinition) (included : Inspections) vc =
      let rawInstructions = m.Body.Instructions
      let dbg = m.DebugInformation

      let instructions =
        [ rawInstructions |> Seq.cast ]
        |> Seq.filter (fun _ ->
             dbg.IsNotNull)
        |> Seq.concat
        |> Seq.filter (fun (x : Instruction) ->
             if dbg.HasSequencePoints then
               let s = dbg.GetSequencePoint x
               (not << isNull) s && (s.IsHidden |> not)
             else
               false)
        |> Seq.toList

      let number = instructions.Length
      let point = pointNumber
      pointNumber <- point + number
      let interesting = included.IsInstrumented

      let wanted i (s : SequencePoint) =
        i && (s.Document.Url.IsIncluded).IsInstrumented

      let methodPointOnly() =
        interesting && (((instructions
                           |> Seq.isEmpty
                           || CoverageParameters.coverstyle = CoverStyle.BranchOnly) && rawInstructions
                                                                                        |> Seq.isEmpty
                                                                                        |> not)
                           || !CoverageParameters.methodPoint)

      let sp =
        if methodPointOnly() then
          rawInstructions
          |> Seq.take 1
          |> Seq.map
               (fun i -> MethodPoint(i, dbg.GetSequencePoint(i)
                                        |> Option.ofObj
                                        |> Option.filter (fun _ -> !CoverageParameters.methodPoint)
                                        |> Option.map SeqPnt.Build,
                                        m.MetadataToken.ToInt32(), interesting, vc))
        else
          instructions.OrderByDescending(fun (x : Instruction) -> x.Offset)
          |> Seq.mapi (fun i x ->
               let s = dbg.GetSequencePoint(x)
               MethodPoint
                 (x,
                  s
                  |> SeqPnt.Build
                  |> Some, i + point, wanted interesting s, vc))

      let includeBranches() =
        instructions.Any() && CoverageParameters.reportKind() = ReportFormat.OpenCover
        && (CoverageParameters.coverstyle <> CoverStyle.LineOnly)
        && (!CoverageParameters.methodPoint |> not)

      let bp =
        if includeBranches() then
          let spnt =
            instructions
            |> Seq.head
            |> dbg.GetSequencePoint

          let branches = wanted interesting spnt
          extractBranchPoints dbg m.FullName rawInstructions branches vc
        else
          []
      branchNumber <- branchNumber + List.length bp
      Seq.append sp bp

    let rec internal deeper node =
      // The pattern here is map x |> map y |> map x |> concat => collect (x >> y >> z)
      match node with
      | Start paths -> startVisit paths sequenceBuilder
      | Assembly(a, included, _) -> visitAssembly a included sequenceBuilder
      | Module(x, included) -> visitModule x included sequenceBuilder
      | Type(t, included, vc) -> visitType t included vc sequenceBuilder
      | Method(m, included, _, vc) -> visitMethod m included vc
      | _ -> Seq.empty<Node>

    and internal sequenceBuilder node =
      Seq.concat
        [ toSeq node
          deeper node
          node.After() ]

    let internal invoke (node : Node) (visitor : Fix<Node>) = visitor.Invoke(node)
    let internal apply (visitors : list<Fix<Node>>) (node : Node) =
      visitors |> List.map (invoke node)

  // "Public" API
  let internal visit (visitors : seq<Fix<Node>>) (assemblies : seq<string * string list>) =
    zeroPoints()
    methodNumber <- 0
    try
      Start assemblies
      |> I.sequenceBuilder
      |> Seq.fold I.apply (visitors |> Seq.toList)
      |> ignore
    finally
      accumulator |> Seq.iter (fun a -> (a :> IDisposable).Dispose())
      accumulator.Clear()

  let internal encloseState (visitor : 'TState -> 'T -> 'TState) (current : 'TState) =
    let rec stateful l =
      new Fix<'T>(fun (node : 'T) ->
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