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

open AltCover.Augment
open AltCover.Base
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Net

[<Flags>]
type internal Inspect =
  | Ignore = 0
  | Instrument = 1
  | Track = 2
  | TrackOnly = 4

type internal CoverStyle =
  | All = 0
  | LineOnly = 1
  | BranchOnly = 2

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
        if codeSegment.EndLine < 0 then codeSegment.StartLine
        else codeSegment.EndLine
      EndColumn =
        if codeSegment.EndLine < 0 then codeSegment.StartColumn + 1
        else codeSegment.EndColumn
      Document = codeSegment.Document.Url
      Offset = codeSegment.Offset }

[<ExcludeFromCodeCoverage; NoComparison>]
type internal GoTo =
  { Start : Instruction
    Indexes : int list
    Uid : int
    Path : int
    StartLine : int
    Offset : int
    Target : int list
    Document : string
    Included : bool }

[<ExcludeFromCodeCoverage; NoComparison>]
type internal Node =
  | Start of seq<string * string list>
  | Assembly of AssemblyDefinition * Inspect * string list
  | Module of ModuleDefinition * Inspect
  | Type of TypeDefinition * Inspect
  | Method of MethodDefinition * Inspect * (int * string) option
  | MethodPoint of Instruction * SeqPnt option * int * bool
  | BranchPoint of GoTo
  | AfterMethod of MethodDefinition * Inspect * (int * string) option
  | AfterType
  | AfterModule
  | AfterAssembly of AssemblyDefinition * string list
  | Finish
  member this.After() =
    (match this with
     | Start _ -> [ Finish ]
     | Assembly(a, _, l) -> [ AfterAssembly (a, l) ]
     | Module _ -> [ AfterModule ]
     | Type _ -> [ AfterType ]
     | Method(m, included, track) -> [ AfterMethod(m, included, track) ]
     | _ -> [])
    |> List.toSeq

[<ExcludeFromCodeCoverage; NoComparison>]
type internal StrongNameKeyData =
    {
      Blob : byte list
      Parameters : System.Security.Cryptography.RSAParameters
    }
    member this.PublicKey
      with get() =
        let lead = [
          0uy  // RSA
          36uy
          0uy
          0uy
          4uy  // SHA
          128uy
          0uy
          0uy
          148uy  // modulus length + 20
          0uy
          0uy
          0uy
          6uy // public key
          2uy // version
          0uy // zero
          0uy
          0uy  // RSA
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
          0uy
          ]
          // possibly reverse exponent too?
        let exponent = Seq.append this.Parameters.Exponent (Seq.initInfinite (fun _ -> 0uy))
                       |> Seq.take 4
                       |> Seq.toList
        Seq.concat [lead
                    exponent
                    this.Parameters.Modulus |> Seq.toList |> List.rev ]
        |> Seq.toArray

    static member Make (data : byte array) =
      use csp = new System.Security.Cryptography.RSACryptoServiceProvider()
      csp.ImportCspBlob(data)
      let blob = csp.ExportCspBlob(true)
      {
        Blob = blob |> Array.toList
        Parameters = csp.ExportParameters(true)
      }
    static member Empty () =
     {
        Blob = []
        Parameters = System.Security.Cryptography.RSAParameters()
     }

[<ExcludeFromCodeCoverage; NoComparison>]
type internal KeyRecord =
  { Pair : StrongNameKeyData
    Token : byte list }

[<ExcludeFromCodeCoverage; NoComparison>]
type internal SequenceType =
  | Genuine
  | FakeAfterReturn

module internal KeyStore =
  let private hash = new System.Security.Cryptography.SHA1CryptoServiceProvider()
  let private publicKeyOfKey (key : StrongNameKeyData) =
    key.PublicKey

  let internal TokenOfArray(key : byte array) =
    hash.ComputeHash(key)
    |> Array.rev
    |> Array.take 8

  let internal TokenOfKey(key : StrongNameKeyData) =
    key
    |> publicKeyOfKey
    |> TokenOfArray
    |> Array.toList

  let internal TokenAsULong(token : byte array) = BitConverter.ToUInt64(token, 0)

  let internal KeyToIndex(key : StrongNameKeyData) =
    key
    |> TokenOfKey
    |> List.toArray
    |> TokenAsULong

  let internal ArrayToIndex(key : byte array) =
    key
    |> TokenOfArray
    |> TokenAsULong

  let internal KeyToRecord(key : StrongNameKeyData) =
    { Pair = key
      Token = TokenOfKey key }

  let internal HashFile sPath =
    use stream = File.OpenRead sPath
    stream
    |> hash.ComputeHash
    |> BitConverter.ToString

[<ExcludeFromCodeCoverage>]
type Fix<'T> = delegate of 'T -> Fix<'T>

module internal Visitor =
  let internal collect = ref false
  let internal TrackingNames = new List<String>()

  let internal NameFilters = new List<FilterClass>()

  let private specialCaseFilters =
    [ @"^CompareTo\$cont\@\d+\-?\d$"
      |> Regex
      |> FilterClass.Method ]

  let internal inplace = ref false
  let mutable internal single = false
  let Sampling() =
    (if single then Base.Sampling.Single
               else Base.Sampling.All) |> int
  let internal sourcelink = ref false
  let internal defer = ref (Some false)
  let internal deferOpCode () =
    if Option.getOrElse false !defer
                         then OpCodes.Ldc_I4_1
                         else OpCodes.Ldc_I4_0

  let internal inputDirectories = List<string>()
  let private defaultInputDirectory = "."
  let InputDirectories() = if inputDirectories.Any()
                           then inputDirectories :> string seq
                           else [ defaultInputDirectory ] |> List.toSeq
                           |> Seq.map Path.GetFullPath
                           |> Seq.toList

  let inplaceSelection a b =
    if !inplace then a
    else b

  let internal outputDirectories = List<string>()
  let private defaultOutputDirectory _ = inplaceSelection "__Saved" "__Instrumented"
  let OutputDirectories() = let paired = InputDirectories()
                            Seq.append (outputDirectories :> string seq) (Seq.initInfinite defaultOutputDirectory)
                            |> Seq.zip paired
                            |> Seq.map (fun (i, o) -> Path.Combine(i, o) |> Path.GetFullPath)
                            |> Seq.toList

  let InstrumentDirectories() = (inplaceSelection InputDirectories OutputDirectories)()
  let SourceDirectories() = (inplaceSelection OutputDirectories InputDirectories)()

  let mutable internal reportPath : Option<string> = None
  let defaultReportPath = "coverage.xml"
  let ReportPath() = Path.GetFullPath(Option.getOrElse defaultReportPath reportPath)

  let mutable internal interval : Option<int> = None
  let defaultInterval = 0
  let Interval() = (Option.getOrElse defaultInterval interval)

  let mutable internal reportFormat : Option<ReportFormat> = None
  let mutable internal coverstyle = CoverStyle.All

  let defaultReportFormat() =
    if coverstyle = CoverStyle.All then ReportFormat.NCover
    else ReportFormat.OpenCover

  let ReportKind() = (Option.getOrElse (defaultReportFormat()) reportFormat)

  let ReportFormat() =
    let fmt = ReportKind()
    if fmt = ReportFormat.OpenCover && (TrackingNames.Any() || Interval() > 0) then
      ReportFormat.OpenCoverWithTracking
    else fmt

  let mutable internal defaultStrongNameKey : option<StrongNameKeyData> = None
  let mutable internal recorderStrongNameKey : option<StrongNameKeyData> = None
  let internal keys = new Dictionary<UInt64, KeyRecord>()

  let internal Add(key : StrongNameKeyData) =
    let index = KeyStore.KeyToIndex key
    keys.[index] <- KeyStore.KeyToRecord key

  let IsIncluded(nameProvider : Object) =
    if (NameFilters |> Seq.exists (Filter.Match nameProvider)) then Inspect.Ignore
    else Inspect.Instrument

  let Mask = ~~~Inspect.Instrument

  let UpdateInspection before x =
    (before &&& Mask) ||| (before &&& Inspect.Instrument &&& IsIncluded x)
  let IsInstrumented x = (x &&& Inspect.Instrument) = Inspect.Instrument
  let ToSeq node = List.toSeq [ node ]

  let mutable private PointNumber : int = 0
  let mutable private BranchNumber : int = 0
  let mutable private MethodNumber : int = 0
  let mutable internal SourceLinkDocuments : Dictionary<string, string> option = None

  let internal EnsureEndsWith c (s:string) =
    if s.EndsWith (c, StringComparison.Ordinal)
    then s
    else s + c

  let internal GetRelativePath (relativeTo:string) path =
    if Path.GetFullPath path = Path.GetFullPath relativeTo
    then String.Empty
    else
      let ender = EnsureEndsWith <| Path.DirectorySeparatorChar.ToString()
      let uri = new Uri(ender relativeTo)
      Uri.UnescapeDataString(uri.MakeRelativeUri(new Uri(path)).ToString()).
            Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar)

  let internal Exists (url:Uri) =
    let request = System.Net.WebRequest.CreateHttp(url)
    request.Method <- "HEAD"
    try
      use response = request.GetResponse()
      response.ContentLength > 0L &&
      (response :?> System.Net.HttpWebResponse).StatusCode |> int < 400
    with
    | :? WebException -> false

  let internal FindClosestMatch  file (dict : Dictionary<string, string>) =
    dict.Keys
    |> Seq.filter (fun x -> x |> Path.GetFileName = "*")
    |> Seq.map (fun x -> (x, GetRelativePath (x |> Path.GetDirectoryName) (file |> Path.GetDirectoryName)))
    |> Seq.filter (fun (x, r) -> r.IndexOf("..") < 0)
    |> Seq.sortBy (fun (x, r) -> r.Length)
    |> Seq.tryHead

  [<SuppressMessage("Microsoft.Usage",
                    "CA2208:InstantiateArgumentExceptionsCorrectly",
                    Justification = "F# inlined code")>]
  let internal LocateMatch file dict =
    let find = FindClosestMatch file dict

    match find with
    | Some (best, relative) ->
      let replacement = Path.Combine(relative, Path.GetFileName(file)).Replace('\\', '/')
      let url = dict.[best].Replace("*", replacement)
      let map = if Uri(url) |> Exists then url else file
      dict.Add(file, map)
      map
    | _ -> file

  let internal SourceLinkMapping file =
    match SourceLinkDocuments with
    | None -> file
    | Some dict -> match dict.TryGetValue file with
                   | (true, url) -> url
                   | _ -> LocateMatch file dict

  let significant (m : MethodDefinition) =
    [ (fun _ -> m.HasBody |> not)
      Filter.IsFSharpInternal
      Filter.IsCSharpAutoProperty
      (fun m -> specialCaseFilters |> Seq.exists (Filter.Match m))

      // Constructors of compiler generated types otherwise pollute F# coverage
      (fun m ->
      let t = m.DeclaringType
      m.IsConstructor
      && (t.IsNested
          && t.CustomAttributes
             |> Seq.exists
                  (fun a ->
                  a.AttributeType.FullName = "System.Runtime.CompilerServices.CompilerGeneratedAttribute"))
      || m.CustomAttributes
         |> Seq.exists
              (fun a ->
              a.AttributeType.FullName = "System.Runtime.CompilerServices.CompilerGeneratedAttribute")) ]
    |> Seq.exists (fun f -> f m)
    |> not

  let private accumulator = HashSet<AssemblyDefinition>()

  let private StartVisit (paths : seq<string * string list>) buildSequence =
    paths
    |> Seq.collect (fun (path, targets) -> path
                                           |> (AssemblyDefinition.ReadAssembly
                                                >> (fun x ->
                                                x
                                                |> accumulator.Add
                                                |> ignore
                                                // Reject completely if filtered here
                                                let inspection = IsIncluded x

                                                let included =
                                                  inspection ||| if inspection = Inspect.Instrument
                                                                    && ReportFormat() = Base.ReportFormat.OpenCoverWithTracking then
                                                                   Inspect.Track
                                                                 else Inspect.Ignore
                                                ProgramDatabase.ReadSymbols(x)
                                                Assembly(x, included, targets)))
                    >> buildSequence)

  let private VisitAssembly (a : AssemblyDefinition) included buildSequence =
    a.Modules
    |> Seq.cast
    |> Seq.collect ((fun x ->
                    let interim = UpdateInspection included x
                    Module(x,
                           if interim = Inspect.Track then Inspect.TrackOnly
                           else interim))
                    >> buildSequence)

  let private ZeroPoints() =
    PointNumber <- 0
    BranchNumber <- 0
    SourceLinkDocuments <- None

  let private VisitModule (x : ModuleDefinition) included buildSequence =
    ZeroPoints()
    SourceLinkDocuments <- Some x
        |> Option.filter (fun _ -> !sourcelink)
        |> Option.map (fun x -> x.CustomDebugInformations
                                |> Seq.tryFind (fun i -> i.Kind = CustomDebugInformationKind.SourceLink))
        |> Option.bind id
        |> Option.map (fun i -> let c = (i :?> SourceLinkDebugInformation).Content
                                let j = JObject.Parse(c).["documents"]
                                JsonConvert.DeserializeObject<Dictionary<string, string>>(j.ToString()))

    [ x ]
    |> Seq.takeWhile (fun _ -> included <> Inspect.Ignore)
    |> Seq.collect (fun x -> x.GetAllTypes() |> Seq.cast)
    |> Seq.collect ((fun t ->
                    let types =
                      Seq.unfold (fun (state : TypeDefinition) ->
                        if isNull state then None
                        else Some(state, state.DeclaringType)) t

                    let inclusion = Seq.fold UpdateInspection included types
                    Type(t, inclusion))
                    >> buildSequence)

  let internal Track(m : MethodDefinition) =
    let name = m.Name
    let fullname = m.DeclaringType.FullName.Replace('/', '.') + "." + name
    TrackingNames
    |> Seq.map (fun n ->
         if n.Chars(0) = '[' then
           let stripped = n.Trim([| '['; ']' |])

           let full =
             if stripped.EndsWith("Attribute", StringComparison.Ordinal) then stripped
             else stripped + "Attribute"
           if m.HasCustomAttributes && m.CustomAttributes
                                       |> Seq.map (fun a -> a.AttributeType)
                                       |> Seq.tryFind
                                            (fun a -> full = a.Name || full = a.FullName)
                                       |> Option.isSome
           then Some n
           else None
         else if n = name || n = fullname then Some n
         else None)
    |> Seq.choose id
    |> Seq.tryFind (fun _ -> true)
    |> Option.map (fun n ->
         let id = MethodNumber + 1
         MethodNumber <- id
         (id, n))

  let private CSharpContainingMethod (name : string) (ct : TypeDefinition) index predicate =
    let stripped = name.Substring(1, index).Replace('-','.')
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
                        |> Seq.filter
                             (fun mx ->
                             (mx.Name.IndexOf(tag, StringComparison.Ordinal) >= 0)
                             && mx.HasBody))
                .Concat(ct.NestedTypes
                        |> Seq.filter
                             (fun tx -> tx.Name.StartsWith("<", StringComparison.Ordinal))
                        |> Seq.collect (fun tx -> tx.Methods)
                        |> Seq.filter
                             (fun mx ->
                             mx.HasBody
                             && (mx.Name.IndexOf(tag, StringComparison.Ordinal) >= 0
                                 || mx.DeclaringType.Name.IndexOf
                                      (tag, StringComparison.Ordinal) >= 0)))
      |> Seq.tryFind predicate

  let SameType (target : TypeReference) (candidate : TypeReference) =
    if target = candidate then true
    else if target.HasGenericParameters then
      let cname = candidate.FullName
      let last = cname.LastIndexOf('<')
      if last < 0 then false
      else
        let stripped = cname.Substring(0, last)
        let tname = target.FullName
        stripped.Equals(tname)
    else false

  let SameFunction (target : MethodReference) (candidate : MethodReference) =
    if target = candidate then true
    else if SameType target.DeclaringType candidate.DeclaringType then
      let cname = candidate.Name
      let tname = target.Name
      tname.Equals cname
    else false

  let MethodConstructsType (t : TypeReference) (m : MethodDefinition) =
    m.Body.Instructions
    |> Seq.filter (fun i -> i.OpCode = OpCodes.Newobj)
    |> Seq.exists (fun i ->
         let tn = (i.Operand :?> MethodReference).DeclaringType
         SameType t tn)

  let private FSharpContainingMethod (t : TypeDefinition) (tx : TypeReference) =
    let candidates =
      t.DeclaringType.Methods.Concat(t.DeclaringType.NestedTypes
                                     |> Seq.filter (fun t2 -> (t2 :> TypeReference) <> tx)
                                     |> Seq.collect (fun t2 -> t2.Methods))
      |> Seq.filter (fun m -> m.HasBody)
    candidates |> Seq.tryFind (MethodConstructsType tx)

  let MethodCallsMethod (t : MethodReference) (m : MethodDefinition) =
    m.Body.Instructions
    |> Seq.filter (fun i -> i.OpCode = OpCodes.Call)
    |> Seq.exists (fun i ->
         let tn = (i.Operand :?> MethodReference)
         SameFunction t tn)

  let MethodLoadsMethod (t : MethodReference) (m : MethodDefinition) =
    m.Body.Instructions
    |> Seq.filter (fun i -> i.OpCode = OpCodes.Ldftn)
    |> Seq.exists (fun i ->
         let tn = (i.Operand :?> MethodReference)
         SameFunction t tn)

  let internal ContainingMethod(m : MethodDefinition) =
    let mname = m.Name
    let t = m.DeclaringType

    // like s.IndexOf('>') but need to match paired nested angle-brackets
    let IndexOfMatchingClosingAngleBracket s =
      let mutable nesting = 0
      s
      |> Seq.takeWhile (fun c ->
           if c = '<' then nesting <- nesting + 1
           if c = '>' then nesting <- nesting - 1
           nesting > 0)
      |> Seq.length

    if mname.StartsWith("<", StringComparison.Ordinal) && mname.IndexOf('|') > 0 then
      let index = (IndexOfMatchingClosingAngleBracket mname) - 1
      CSharpContainingMethod mname t index (MethodCallsMethod m)
    else
      let n = t.Name
      if t.IsNested |> not then None
      else if n.StartsWith("<", StringComparison.Ordinal) then
        let name =
          if n.StartsWith("<>", StringComparison.Ordinal) then mname
          else n

        // let index = name.IndexOf('>') - 1 // but need to match paired nested angle-brackets
        let index = (IndexOfMatchingClosingAngleBracket name) - 1
        if (index < 1) then None
        else
          CSharpContainingMethod name t.DeclaringType index
            // Guard against simple recursion here (mutual will need more work!)
            (fun mx ->
            (mx.FullName <> m.FullName)
            && (MethodCallsMethod m mx || MethodConstructsType t mx
                || MethodLoadsMethod m mx))
      else if n.IndexOf('@') >= 0 then
        let tx =
          if n.EndsWith("T", StringComparison.Ordinal) then
            match t.Methods
                  |> Seq.tryFind
                       (fun m ->
                       m.IsConstructor && m.HasParameters && (m.Parameters.Count = 1))
                  |> Option.map (fun m -> m.Parameters |> Seq.head) with
            | None -> t :> TypeReference
            | Some other -> other.ParameterType
          else t :> TypeReference
        FSharpContainingMethod t tx
      else None

  let private VisitType (t : TypeDefinition) included buildSequence =
    t.Methods
    |> Seq.cast
    |> Seq.filter
         (fun (m : MethodDefinition) ->
         not m.IsAbstract && not m.IsRuntime && not m.IsPInvokeImpl && significant m)
    |> Seq.collect ((fun m ->
                    let methods =
                      Seq.unfold (fun (state : MethodDefinition option) ->
                        match state with
                        | None -> None
                        | Some x -> Some(x, ContainingMethod x)) (Some m)

                    let inclusion = Seq.fold UpdateInspection included methods
                    Method(m, inclusion, Track m))
                    >> buildSequence)

  let IsSequencePoint(s : SequencePoint) =
    (s
     |> isNull
     |> not)
    && s.IsHidden |> not

  let fakeSequencePoint genuine (seq : SequencePoint) (instruction : Instruction) =
    match seq with
    | null ->
      if genuine = FakeAfterReturn && instruction
                                      |> isNull
                                      |> not
         && instruction.OpCode = OpCodes.Ret
      then SequencePoint(instruction, Document(null))
      else null
    | _ -> seq

  let findEffectiveSequencePoint genuine (dbg : MethodDebugInformation)
      (instructions : Instruction seq) =
    instructions
    |> Seq.map (fun i ->
         let seq = dbg.GetSequencePoint i
         fakeSequencePoint genuine seq i.Previous)
    |> Seq.tryFind IsSequencePoint

  let findSequencePoint (dbg : MethodDebugInformation) (instructions : Instruction seq) =
    findEffectiveSequencePoint Genuine dbg instructions

  let indexList l = l |> List.mapi (fun i x -> (i, x))

  let getJumpChain (terminal : Instruction) (i : Instruction) =
    let rec accumulate (state : Instruction) l =
      let gendarme = l
      if state.OpCode = OpCodes.Br || state.OpCode = OpCodes.Br_S then
        let target = (state.Operand :?> Instruction)
        accumulate target (target :: l)
      else if (state.Offset > terminal.Offset
               // depart current context, especially important if inlined
               || state.OpCode.FlowControl = FlowControl.Cond_Branch
               || state.OpCode.FlowControl = FlowControl.Branch // Leave or Leave_S
               || state.OpCode.FlowControl = FlowControl.Break
               || state.OpCode.FlowControl = FlowControl.Throw
               || state.OpCode.FlowControl = FlowControl.Return // includes state.Next = null
               || isNull state.Next) then l
      else accumulate state.Next gendarme
    accumulate i [ i ]

  let private boundaryOfList (f : (Instruction -> int) -> Instruction list -> Instruction)
      (places : Instruction list) = places |> f (fun i -> i.Offset)

  let includedSequencePoint dbg (toNext : Instruction list) toJump =
    let places = List.concat [ toNext; toJump ]
    let start = places |> (boundaryOfList List.minBy)
    let finish = places |> (boundaryOfList List.maxBy)

    let range =
      Seq.unfold (fun (state : Cil.Instruction) ->
        if isNull state || finish = state.Previous then None
        else Some(state, state.Next)) start
      |> Seq.toList
    findEffectiveSequencePoint FakeAfterReturn dbg range

  let rec lastOfSequencePoint (dbg : MethodDebugInformation) (i : Instruction) =
    let n = i.Next
    if n
       |> isNull
       || n
          |> dbg.GetSequencePoint
          |> IsSequencePoint
    then i
    else lastOfSequencePoint dbg n

  let getJumps (dbg : MethodDebugInformation) (i : Instruction) =
    let terminal = lastOfSequencePoint dbg i
    let next = i.Next
    if i.OpCode = OpCodes.Switch then
      (i, getJumpChain terminal next, next.Offset, -1) :: (i.Operand :?> Instruction []
                                                           |> Seq.mapi
                                                                (fun k d ->
                                                                i, getJumpChain terminal d,
                                                                d.Offset, k)
                                                           |> Seq.toList)
    else
      let jump = i.Operand :?> Instruction
      let toNext = getJumpChain terminal next
      let toJump = getJumpChain terminal jump
      // Eliminate the "all inside one SeqPnt" jumps
      // This covers a multitude of compiler generated branching cases
      match includedSequencePoint dbg toNext toJump with
      | Some _ ->
        [ (i, toNext, next.Offset, -1)
          (i, toJump, jump.Offset, 0) ]
      | None -> []

  // cribbed from OpenCover's CecilSymbolManager -- internals of C# yield return iterators
  let private IsMoveNext =
    Regex
      (@"\<[^\s>]+\>\w__\w(\w)?::MoveNext\(\)$",
       RegexOptions.Compiled ||| RegexOptions.ExplicitCapture)

  let private ExtractBranchPoints dbg methodFullName rawInstructions interesting =
    // Generated MoveNext => skip one branch
    let skip = IsMoveNext.IsMatch methodFullName |> Augment.Increment
    (Seq.map (snd >> (fun (i : Instruction) ->
         getJumps dbg i // if two or more jumps go between the same two places, coalesce them
         |> List.groupBy (fun (_, _, o, _) -> o)
         |> List.map (fun (_, records) ->
              let (from, target, _, _) = Seq.head records
              (from, target,
               records
               |> List.map (fun (_, _, _, n) -> n)
               |> List.sort))
         |> List.sortBy (fun (_, _, l) -> l.Head)
         |> indexList)) ([ rawInstructions |> Seq.cast ]
    |> Seq.filter (fun _ ->
         dbg
         |> isNull
         |> not)
    |> Seq.concat
    |> Seq.filter
         (fun (i : Instruction) -> i.OpCode.FlowControl = FlowControl.Cond_Branch)
    |> Seq.mapi (fun n i -> (n, i)) //
    |> Seq.filter (fun (n, _) -> n >= skip)))
    |> Seq.filter (fun l -> l.Length > 1)
    |> Seq.collect id
    |> Seq.mapi (fun i (path, (from, target, indexes)) ->
         Seq.unfold (fun (state : Cil.Instruction) ->
           state
           |> Option.nullable
           |> Option.map (fun state' -> (state', state'.Previous))) from
         |> (findSequencePoint dbg)
         |> Option.map (fun context ->
              BranchPoint { Path = path
                            Indexes = indexes
                            Uid = i + BranchNumber
                            Start = from
                            StartLine = context.StartLine
                            Offset = from.Offset
                            Target = target |> List.map (fun i -> i.Offset)
                            Document = context.Document.Url
                            Included = interesting }))
    |> Seq.choose id
    |> Seq.toList

  let private VisitMethod (m : MethodDefinition) (included : Inspect) =
    let rawInstructions = m.Body.Instructions
    let dbg = m.DebugInformation

    let instructions =
      [ rawInstructions |> Seq.cast ]
      |> Seq.filter (fun _ ->
           dbg
           |> isNull
           |> not)
      |> Seq.concat
      |> Seq.filter (fun (x : Instruction) ->
           if dbg.HasSequencePoints then
             let s = dbg.GetSequencePoint x
             (not << isNull) s && (s.IsHidden |> not)
           else false)
      |> Seq.toList

    let number = instructions.Length
    let point = PointNumber
    PointNumber <- point + number
    let interesting = IsInstrumented included

    let wanted i (s : SequencePoint) =
      i && (s.Document.Url
            |> IsIncluded
            |> IsInstrumented)

    let MethodPointOnly() =
      interesting && (instructions
                      |> Seq.isEmpty
                      || coverstyle = CoverStyle.BranchOnly) && rawInstructions
                                                                |> Seq.isEmpty
                                                                |> not

    let sp =
      if MethodPointOnly() then
        rawInstructions
        |> Seq.take 1
        |> Seq.map (fun i -> MethodPoint(i, None, m.MetadataToken.ToInt32(), interesting))
      else
        instructions.OrderByDescending(fun (x : Instruction) -> x.Offset)
        |> Seq.mapi (fun i x ->
             let s = dbg.GetSequencePoint(x)
             MethodPoint(x,
                         s
                         |> SeqPnt.Build
                         |> Some, i + point, wanted interesting s))

    let IncludeBranches() =
      instructions.Any() && ReportKind() = Base.ReportFormat.OpenCover
      && (coverstyle <> CoverStyle.LineOnly)

    let bp =
      if IncludeBranches() then
        let spnt =
          instructions
          |> Seq.head
          |> dbg.GetSequencePoint

        let branches = wanted interesting spnt
        ExtractBranchPoints dbg m.FullName rawInstructions branches
      else []
    BranchNumber <- BranchNumber + List.length bp
    Seq.append sp bp

  let rec internal Deeper node =
    // The pattern here is map x |> map y |> map x |> concat => collect (x >> y >> z)
    match node with
    | Start paths -> StartVisit paths BuildSequence
    | Assembly(a, included, _) -> VisitAssembly a included BuildSequence
    | Module(x, included) -> VisitModule x included BuildSequence
    | Type(t, included) -> VisitType t included BuildSequence
    | Method(m, included, _) -> VisitMethod m included
    | _ -> Seq.empty<Node>

  and internal BuildSequence node =
    Seq.concat [ ToSeq node
                 Deeper node
                 node.After() ]

  let internal invoke (node : Node) (visitor : Fix<Node>) = visitor.Invoke(node)
  let internal apply (visitors : list<Fix<Node>>) (node : Node) =
    visitors |> List.map (invoke node)

  let internal Visit (visitors : seq<Fix<Node>>) (assemblies : seq<string * string list>) =
    ZeroPoints()
    MethodNumber <- 0
    try
      Start assemblies
      |> BuildSequence
      |> Seq.fold apply (visitors |> Seq.toList)
      |> ignore
    finally
      accumulator |> Seq.iter (fun a -> (a :> IDisposable).Dispose())
      accumulator.Clear()

  let EncloseState (visitor : 'State -> 'T -> 'State) (current : 'State) =
    let rec stateful l =
      new Fix<'T>(fun (node : 'T) ->
      let next = visitor l node
      stateful next)
    stateful current