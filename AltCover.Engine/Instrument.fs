// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/CounterAssemblyBuilder.cs
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/InstrumentorVisitor.cs

namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection

open Manatee.Json
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

[<assembly: SuppressMessage("Microsoft.Globalization",
                            "CA1307:SpecifyStringComparison",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Engine>.$Instrument.#.cctor()",
                            MessageId = "System.String.Replace(System.String,System.String)",
                            Justification = "No suitable overload in netstandard2.0/net472")>]
()

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal RecorderRefs =
  { Visit: MethodReference
    Push: MethodReference
    Pop: MethodReference }
  static member Build() =
    { Visit = null
      Push = null
      Pop = null }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal AsyncSupport =
  { TaskAssembly: AssemblyDefinition // kept for context
    AsyncAssembly: AssemblyDefinition // kept for context
    Wait: MethodDefinition
    LocalWait: MethodReference
    RunSynch: MethodDefinition }
  static member private DisposeAssemblyDefinition(def: IDisposable) = def.Dispose()

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  member self.RunSynchronously (m: MethodDefinition) (asyncOf: TypeReference) =
    let r1 =
      self.RunSynch
      |> m.DeclaringType.Module.ImportReference

    let gm = GenericInstanceMethod(r1)
    let ga = gm.GenericArguments
    ga.Clear()
    ga.Add asyncOf
    gm

  member self.Close() =
    [ self.TaskAssembly
      self.AsyncAssembly ]
    |> List.iter (
      Option.ofObj
      >> (Option.iter AsyncSupport.DisposeAssemblyDefinition)
    )

  [<SuppressMessage("Gendarme.Rules.Correctness",
                    "EnsureLocalDisposalRule",
                    Justification = "Disposed on exit")>]
  static member Update(m: IMemberDefinition) =
    // Maybe get version of assembly being used by m?  Probably not important
    let def =
      typeof<System.Threading.Tasks.Task>
        .Assembly
        .Location
      |> AssemblyDefinition.ReadAssembly

    let task =
      def.MainModule.GetType("System.Threading.Tasks.Task")

    let wait =
      task.Methods
      |> Seq.filter
           (fun f ->
             f.FullName = "System.Boolean System.Threading.Tasks.Task::Wait(System.Int32)")
      |> Seq.head

    let def2 =
      typeof<Microsoft.FSharp.Control.AsyncReturn>
        .Assembly
        .Location
      |> AssemblyDefinition.ReadAssembly

    let fsasync =
      def2.MainModule.GetType("Microsoft.FSharp.Control.FSharpAsync")

    let runsynch =
      fsasync.Methods
      |> Seq.filter (fun f -> f.Name = "RunSynchronously")
      |> Seq.head

    { TaskAssembly = def
      AsyncAssembly = def2
      Wait = wait
      LocalWait = wait |> m.DeclaringType.Module.ImportReference
      RunSynch = runsynch }

// State object passed from visit to visit
[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal InstrumentContext =
  { InstrumentedAssemblies: string list
    ModuleId: String
    RecordingAssembly: AssemblyDefinition
    RecorderSource : Stream
    RecordingMethod: MethodDefinition list // initialised once
    RecordingMethodRef: RecorderRefs // updated each module
    MethodBody: MethodBody
    MethodWorker: ILProcessor
    AsyncSupport: AsyncSupport option }
  static member Build assemblies =
    { InstrumentedAssemblies = assemblies
      ModuleId = String.Empty
      RecordingAssembly = null
      RecorderSource = null
      RecordingMethod = []
      RecordingMethodRef = RecorderRefs.Build()
      MethodBody = null
      MethodWorker = null
      AsyncSupport = None }

// Module to handle instrumentation visitor
module internal Instrument =
  let recorderVersion() =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.AltCover.Recorder.net20.dll")
    use def = AssemblyDefinition.ReadAssembly stream
    def.Name.Version.ToString()
  let version = recorderVersion()

  let internal resolutionTable = Dictionary<string, AssemblyDefinition>()

  module internal I =

    // Locate the method that must be called to register a code point for coverage visit.
    // param name="assembly">The assembly containing the recorder method</param>
    // returns>A representation of the method to call to signal a coverage visit.</returns>
    let internal recordingMethod (recordingAssembly: AssemblyDefinition) =
      recordingAssembly.MainModule.GetAllTypes()
      |> Seq.filter (fun t -> t.FullName = "AltCover.Recorder.Instance")
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.map (fun t -> (t.Name, t))
      |> Seq.filter (fun (n, _) -> n = "Visit" || n = "Push" || n = "Pop")
      |> Seq.sortBy fst
      |> Seq.map snd
      |> Seq.toList
      |> List.rev

    // Applies a new key to an assembly name
    // param name="assemblyName">The name to update</param>
    // param name="key">The possibly empty key to use</param>
    let internal updateStrongNaming
      (assembly: AssemblyDefinition)
      (key: StrongNameKeyData option)
      =
      let assemblyName = assembly.Name

      match key with
      | None ->
          assembly.MainModule.Attributes <-
            assembly.MainModule.Attributes
            &&& (~~~ModuleAttributes.StrongNameSigned)

          assemblyName.HasPublicKey <- false
          assemblyName.PublicKey <- null
          assemblyName.PublicKeyToken <- null
      | Some key' ->
          assemblyName.HasPublicKey <- true
          assemblyName.PublicKey <- key'.PublicKey |> Seq.toArray // sets token implicitly

    // Locate the key, if any, which was used to name this assembly.
    // param name="name">The name of the assembly</param>
    // returns>A key, if we have a match.</returns>
    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification = "AvoidSpeculativeGenerality too")>]
    let internal knownKey (name: AssemblyNameDefinition) =
      if not name.HasPublicKey then
        None
      else
        let index = KeyStore.arrayToIndex name.PublicKey

        match CoverageParameters.keys.TryGetValue(index) with
        | (false, _) -> None
        | (_, record) -> Some record.Pair

    // Locate the key, if any, which was used to name this assembly.
    // param name="name">The name of the assembly</param>
    // returns>A key, if we have a match.</returns>
    let internal knownToken (name: AssemblyNameReference) =
      let pktoken = name.PublicKeyToken

      if pktoken.Length <> 8 then
        None
      else
        let index = KeyStore.tokenAsULong pktoken

        match CoverageParameters.keys.TryGetValue(index) with
        | (false, _) -> None
        | (_, record) -> Some record

    // This trivial extraction appeases Gendarme
    let private extractName (assembly: AssemblyDefinition) = assembly.Name.Name

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification = "AvoidSpeculativeGenerality too")>]
    let guard (assembly: AssemblyDefinition) (f: unit -> unit) =
      try
        f ()
        assembly
      with _ ->
        (assembly :> IDisposable).Dispose()
        reraise ()

    // Create the new assembly that will record visits, based on the prototype.
    // returns>A representation of the assembly used to record all coverage visits.</returns>
    let internal prepareAssemblyDefinition (definition: AssemblyDefinition) =
      guard
        definition
        (fun () -> // set the timer interval in ticks

          //if monoRuntime |> not then
          ProgramDatabase.readSymbols definition

          definition.Name.Name <- (extractName definition) + ".g"

          let pair = CoverageParameters.recorderStrongNameKey
          updateStrongNaming definition pair

          [ // set the coverage file path and unique token
            ("get_ReportFile",
             (fun (w: ILProcessor) ->
               w.Create(OpCodes.Ldstr, CoverageParameters.reportPath ())))
            ("get_Token",
             (fun (w: ILProcessor) ->
               w.Create(OpCodes.Ldstr, "Altcover-" + Guid.NewGuid().ToString())))
            ("get_CoverageFormat",
             (fun (w: ILProcessor) ->
               w.Create(OpCodes.Ldc_I4, CoverageParameters.reportFormat () |> int)))
            ("get_Sample",
             (fun (w: ILProcessor) ->
               w.Create(OpCodes.Ldc_I4, CoverageParameters.sampling ())))
            ("get_Defer",
             (fun (w: ILProcessor) -> w.Create(CoverageParameters.deferOpCode ()))) ]
          |> List.iter
               (fun (property, value) ->
                 let pathGetterDef =
                   definition.MainModule.GetTypes()
                   |> Seq.collect (fun t -> t.Methods)
                   |> Seq.filter (fun m -> m.Name = property)
                   |> Seq.head

                 let body = pathGetterDef.Body
                 let worker = body.GetILProcessor()
                 let initialBody = body.Instructions |> Seq.toList
                 let head = initialBody |> Seq.head
                 worker.InsertBefore(head, value (worker))
                 worker.InsertBefore(head, worker.Create(OpCodes.Ret))
                 initialBody |> Seq.iter worker.Remove
                 pruneLocalScopes pathGetterDef)

          [ ("get_Timer",  // set the timer interval in ticks
             CoverageParameters.interval ()) ]
          |> List.iter
               (fun (property, value) ->
                 let pathGetterDef =
                   definition.MainModule.GetTypes()
                   |> Seq.collect (fun t -> t.Methods)
                   |> Seq.filter (fun m -> m.Name = property)
                   |> Seq.head

                 let body = pathGetterDef.Body
                 let worker = body.GetILProcessor()
                 let initialBody = body.Instructions |> Seq.toList
                 let head = initialBody |> Seq.head
                 worker.InsertBefore(head, worker.Create(OpCodes.Ldc_I4, value))
                 worker.InsertBefore(head, worker.Create(OpCodes.Conv_I8))
                 worker.InsertBefore(head, worker.Create(OpCodes.Ret))
                 initialBody |> Seq.iter worker.Remove
                 pruneLocalScopes pathGetterDef))

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
                                                      "EnsureLocalDisposalRule",
                                                      Justification = "Return confusing Gendarme -- TODO")>]
    let internal prepareAssembly (assembly: Stream) =
      let definition =
        AssemblyDefinition.ReadAssembly(assembly)

      prepareAssemblyDefinition definition

    let private nugetCache =
      Path.Combine(
        Path.Combine(
          Environment.GetFolderPath Environment.SpecialFolder.UserProfile,
          ".nuget"
        ),
        "packages"
      )

    [<SuppressMessage("Gendarme.Rules.Performance",
                      "AvoidUnusedParametersRule",
                      Justification = "meets an interface")>]
    [<SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters",
                      Justification = "meets an interface")>]
    let internal resolveFromNugetCache _ (y: AssemblyNameReference) =
      let name = y.ToString()

      if resolutionTable.ContainsKey name then
        resolutionTable.[name]
      else
        // Placate Gendarme here
        let share =
          "|usr|share"
            .Replace('|', Path.DirectorySeparatorChar)

        let shared =
          "dotnet|shared"
            .Replace('|', Path.DirectorySeparatorChar)

        let sources =
          [ Environment.GetEnvironmentVariable "NUGET_PACKAGES"
            Path.Combine(
              Environment.GetEnvironmentVariable "ProgramFiles"
              |> Option.ofObj
              |> (Option.defaultValue share),
              shared
            )
            Path.Combine(share, shared)
            nugetCache ]

        let candidate source =
          source
          |> List.filter (String.IsNullOrWhiteSpace >> not)
          |> List.filter Directory.Exists
          |> Seq.distinct
          |> Seq.collect
               (fun dir ->
                 Directory.GetFiles(dir, y.Name + ".*", SearchOption.AllDirectories))
          |> Seq.sortDescending
          |> Seq.filter
               (fun f ->
                 let x = Path.GetExtension f

                 x.Equals(".exe", StringComparison.OrdinalIgnoreCase)
                 || x.Equals(".dll", StringComparison.OrdinalIgnoreCase))
          |> Seq.filter
               (fun f ->
                 y
                   .ToString()
                   .Equals(CommandLine.findAssemblyName f, StringComparison.Ordinal))
          |> Seq.tryHead

        match candidate sources with
        | None -> null
        | Some x ->
            String.Format(
              System.Globalization.CultureInfo.CurrentCulture,
              CommandLine.resources.GetString "resolved",
              y.ToString(),
              x
            )
            |> (Output.warnOn true)

            let a = AssemblyDefinition.ReadAssembly x
            resolutionTable.[name] <- a
            a

    let internal hookResolveHandler =
      new AssemblyResolveEventHandler(resolveFromNugetCache)

    let internal hookResolver (resolver: IAssemblyResolver) =
      if resolver.IsNotNull then
        let hook =
          resolver.GetType().GetMethod("add_ResolveFailure")

        hook.Invoke(resolver, [| hookResolveHandler :> obj |])
        |> ignore

    let internal findProvider pdb write =
      match (pdb, write) with
      | (".pdb", true) -> Mono.Cecil.Pdb.PdbWriterProvider() :> ISymbolWriterProvider
      | (_, true) -> Mono.Cecil.Mdb.MdbWriterProvider() :> ISymbolWriterProvider
      | _ -> null

    // Commit an instrumented assembly to disk
    // param name="assembly">The instrumented assembly object</param>
    // param name="path">The full path of the output file</param>
    // remark>Can raise "System.Security.Cryptography.CryptographicException: Keyset does not exist" at random
    // when asked to strongname.  This writes a new .pdb/.mdb alongside the instrumented assembly</remark>
    let internal writeAssembly (assembly: AssemblyDefinition) (path: string) =
      let pkey = Mono.Cecil.WriterParameters()

      let isWindows =
        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"

      let pdb =
        ProgramDatabase.getPdbWithFallback assembly
        |> Option.defaultValue "x.pdb"
        |> Path.GetExtension

      let separatePdb =
        ProgramDatabase.getPdbFromImage assembly
        |> Option.filter (fun s -> s <> (assembly.Name.Name + ".pdb"))
        |> Option.isSome

      //Non-windows embedded symbols => do not write, else
      //Unhandled exception. System.Runtime.InteropServices.MarshalDirectiveException: Cannot marshal 'parameter #2': Invalid managed/unmanaged type combination (Marshaling to and from COM interface pointers isn't supported).
      //   at Mono.Cecil.Pdb.SymWriter.CoCreateInstance(Guid& rclsid, Object pUnkOuter, UInt32 dwClsContext, Guid& riid, Object& ppv)
      //   at Mono.Cecil.Pdb.SymWriter..ctor() in C:/sources/cecil/symbols/pdb/Mono.Cecil.Pdb/SymWriter.cs:line 39
      //   at Mono.Cecil.Pdb.NativePdbWriterProvider.CreateWriter(ModuleDefinition module, String pdb) in C:/sources/cecil/symbols/pdb/Mono.Cecil.Pdb/PdbHelper.cs:line 81

      let shouldWrite =
        assembly.MainModule.HasSymbols
        && (isWindows || separatePdb)

      pkey.SymbolWriterProvider <- findProvider pdb shouldWrite
      pkey.WriteSymbols <- pkey.SymbolWriterProvider.IsNotNull

      knownKey assembly.Name
      |> Option.iter (fun key -> pkey.StrongNameKeyBlob <- key.Blob |> List.toArray)

      let here = Directory.GetCurrentDirectory()

      try
        Directory.SetCurrentDirectory(Path.GetDirectoryName(path))

        let write (a: AssemblyDefinition) p pk =
          use sink =
            File.Open(p, FileMode.Create, FileAccess.ReadWrite)

          a.Write(sink, pk)

        let resolver = assembly.MainModule.AssemblyResolver
        hookResolver resolver
        write assembly path pkey
      finally
        Directory.SetCurrentDirectory(here)

    let internal insertVisit
      (instruction: Instruction)
      (methodWorker: ILProcessor)
      (recordingMethodRef: MethodReference)
      (moduleId: string)
      (point: int)
      =
      bulkInsertBefore
        methodWorker
        instruction
        [ methodWorker.Create(OpCodes.Ldstr, moduleId)
          methodWorker.Create(OpCodes.Ldc_I4, point)
          methodWorker.Create(OpCodes.Call, recordingMethodRef) ]
        true

    // Determine new names for input strong-named assemblies; if we have a key and
    // the assembly was already strong-named then give it the new key token, otherwise
    // set that there is no strongname.
    // param name="assembly">The assembly object being operated upon</param>
    let internal updateStrongReferences
      (assembly: AssemblyDefinition)
      (assemblies: string list)
      =
      let effectiveKey =
        if assembly.Name.HasPublicKey then
          CoverageParameters.defaultStrongNameKey
        else
          None

      updateStrongNaming assembly effectiveKey

      let interestingReferences =
        assembly.MainModule.AssemblyReferences
        |> Seq.cast<AssemblyNameReference>
        |> Seq.filter
             (fun x ->
               assemblies
               |> List.exists (fun y -> y.Equals(x.Name)))
        |> Seq.toList

      // The return value is for unit testing purposes, only
      // The side-effects are what is important.
      let assemblyReferenceSubstitutions = new Dictionary<String, String>()

      interestingReferences
      |> Seq.iter
           (fun r ->
             let original = r.ToString()
             let token = knownToken r

             let effectiveKey =
               match token with
               | None ->
                   CoverageParameters.defaultStrongNameKey
                   |> Option.map KeyStore.keyToRecord
               | Some _ -> token

             match effectiveKey with
             | None ->
                 r.HasPublicKey <- false
                 r.PublicKeyToken <- null
                 r.PublicKey <- null
             | Some key ->
                 r.HasPublicKey <- true
                 r.PublicKey <- key.Pair.PublicKey |> Seq.toArray // implicitly sets token

             let updated = r.ToString()

             if
               not
               <| updated.Equals
                 (
                   original,
                   StringComparison.Ordinal
                 )
             then
               assemblyReferenceSubstitutions.[original] <- updated)

      assemblyReferenceSubstitutions

    [<SuppressMessage("Microsoft.Globalization",
                      "CA1307:SpecifyStringComparison",
                      Justification = "No suitable overload in netstandard2.0/net472")>]
    let internal injectJSON (json: String) =
      let o = JsonValue.Parse json
      let oo = o.Object
      let x = StringComparison.Ordinal

      let target =
        oo.["runtimeTarget"].Object.["name"].String

      let targets =
        (oo |> Seq.find (fun kv -> kv.Key = "targets"))
          .Value
          .Object

      let targeted =
        (targets |> Seq.find (fun p -> p.Key = target))
          .Value
          .Object

      let app = (targeted.Values |> Seq.head).Object

      let existingDependencies =
        app
        |> Seq.tryFind (fun p -> p.Key = "dependencies")

      let prior =
        match existingDependencies with
        | None -> Set.empty<string>
        | Some p ->
            p.Value.Object
            |> Seq.map (fun p -> p.Key)
            |> Set.ofSeq

      let addFirst
        (properties: KeyValuePair<string, JsonValue> seq)
        (jsonObject: JsonObject)
        =
        let existing = jsonObject |> Seq.toList
        jsonObject.Clear()

        [ properties; existing |> List.toSeq ]
        |> Seq.concat
        |> Seq.iter (fun l -> jsonObject.Add(l.Key, l.Value))

      do

        let dependencies =
          version
          |> sprintf """{"dependencies": {"AltCover.Recorder.g": "%s"}}"""

        let updateDependencies () =
          let rawDependencies =
            (JsonValue.Parse dependencies).Object
            |> Seq.find (fun p -> p.Key = "dependencies")

          match app
                |> Seq.tryFind (fun p -> p.Key = "dependencies") with
          | None -> app |> addFirst [ rawDependencies ]
          | Some p ->
              let recorder = rawDependencies.Value.Object |> Seq.head
              p.Value.Object.[recorder.Key] <- recorder.Value

        updateDependencies ()

      let stripRecorderRefs (j: JsonObject) =
        j.Keys
        |> Seq.filter
             (fun k -> k.StartsWith("AltCover.Recorder.g/", StringComparison.Ordinal))
        |> Seq.toList
        |> List.iter (j.Remove >> ignore)

      do
        let runtime =
          version
          |> sprintf
               """{"AltCover.Recorder.g/%s": {"runtime": { "AltCover.Recorder.g.dll": {}}}}"""

        let updateRuntime () =
          let runtimeObject = (JsonValue.Parse runtime).Object
          stripRecorderRefs targeted
          let recorder = runtimeObject |> Seq.head
          targeted.[recorder.Key] <- recorder.Value

        updateRuntime ()

      let libraries =
        (oo |> Seq.find (fun p -> p.Key = "libraries"))
          .Value
          .Object

      do
        let newLibraries =
          version
          |> sprintf
               """{"AltCover.Recorder.g/%s": {"type": "project", "serviceable": false, "sha512": "" }}"""

        let updateLibraries () =
          let newlibs = (JsonValue.Parse newLibraries).Object
          stripRecorderRefs libraries
          libraries |> addFirst newlibs

        updateLibraries ()

      o
        .GetIndentedString()
        .Replace("		", "  ")
        .Replace("	", "  ")
        .Replace(" :", ":")

    let private visitModule (state: InstrumentContext) (m: ModuleEntry) =
      let restate =
        match m.Inspection <> Inspections.Ignore with
        | true ->
            let recordingMethod =
              match state.RecordingMethod with
              | [] -> recordingMethod state.RecordingAssembly
              | _ -> state.RecordingMethod

            let refs =
              recordingMethod
              |> List.map m.Module.ImportReference

            { state with
                RecordingMethodRef =
                  { Visit = refs.[0]
                    Push = refs.[1]
                    Pop = refs.[2] }
                RecordingMethod = recordingMethod
                AsyncSupport =
                  state.AsyncSupport
                  |> Option.map
                       (fun a ->
                         { a with
                             LocalWait = a.Wait |> m.Module.ImportReference }) }
        | _ -> state

      { restate with
          ModuleId =
            match CoverageParameters.reportKind () with
            | ReportFormat.OpenCover -> KeyStore.hashFile m.Module.FileName
            | ReportFormat.NativeJson -> m.Module.FileName |> Path.GetFileName
            | _ -> m.Module.Mvid.ToString() }

    let private visitMethod (state: InstrumentContext) m =
      match m.Inspection.IsInstrumented with
      | true ->
          let body = m.Method.Body

          { state with
              MethodBody = body
              MethodWorker = body.GetILProcessor() }
      | _ -> state

    let private updateBranchReferences (body: MethodBody) instruction injected =
      // Change references in operands from "instruction" to first counter invocation instruction (instrLoadModuleId)
      body.Instructions
      |> Seq.iter (substituteInstructionOperand instruction injected)

      body.ExceptionHandlers
      |> Seq.iter (substituteExceptionBoundary instruction injected)

    let private visitMethodPoint (state: InstrumentContext) e =
      if e.Interesting then
        let instrLoadModuleId =
          insertVisit
            e.Instruction
            state.MethodWorker
            state.RecordingMethodRef.Visit
            state.ModuleId
            e.Uid

        updateBranchReferences state.MethodBody e.Instruction instrLoadModuleId

      state

    let internal visitBranchPoint (state: InstrumentContext) branch =
      if branch.Included && state.MethodWorker.IsNotNull then
        let point = (branch.Uid ||| Counter.branchFlag)

        let instrument instruction =
          if branch.Representative <> Reporting.None then
            insertVisit
              instruction
              state.MethodWorker
              state.RecordingMethodRef.Visit
              state.ModuleId
              point
          else
            instruction // maybe have to insert NOPs?

        let updateSwitch update =
          let operands = branch.Start.Operand :?> Instruction []

          branch.Indexes
          |> Seq.filter (fun i -> i >= 0)
          // See SubstituteInstructionOperand for why we do it this way
          |> Seq.iter (fun i -> Array.set operands i update)

        match branch.Indexes |> Seq.tryFind (fun i -> i = -1) with
        | Some _ ->
            // before
            // Cond_Branch xxx
            // Next
            //
            // after
            // Cond_Branch xxx
            // jump instrument#-1
            // instrument#-1
            // Next
            let target = branch.Start.Next
            let preamble = instrument target

            let jump =
              state.MethodWorker.Create(OpCodes.Br, preamble)

            state.MethodWorker.InsertAfter(branch.Start, jump)

            if branch.Start.OpCode = OpCodes.Switch then
              updateSwitch jump
        | None ->
            // before
            // Cond_Branch #n
            // jump instrument#-1
            // ...
            // instrument#-1
            // Next
            //
            // after
            // Cond_Branch instrument#n
            // jump instrument#-1
            // instrument#n
            // jump #n
            // ...
            // instrument#-1
            // Next
            let target =
              if branch.Start.OpCode = OpCodes.Switch then
                branch.Start.Operand :?> Instruction []
                |> Seq.skip (branch.Indexes.Head)
                |> Seq.head
              else
                branch.Start.Operand :?> Instruction

            let jump =
              state.MethodWorker.Create(OpCodes.Br, target)

            state.MethodWorker.InsertAfter(branch.Start.Next, jump)
            let preamble = instrument jump

            if branch.Start.OpCode = OpCodes.Switch then
              updateSwitch preamble
            else
              branch.Start.Operand <- preamble

      state

    let writeAssemblies definition file targets sink =
      let first = Path.Combine(targets |> Seq.head, file)

      String.Format(
        System.Globalization.CultureInfo.CurrentCulture,
        CommandLine.resources.GetString "instrumented",
        definition,
        first
      )
      |> sink

      writeAssembly definition first

      targets
      |> Seq.tail
      |> Seq.iter
           (fun p ->
             let pathn = Path.Combine(p, file)

             String.Format(
               System.Globalization.CultureInfo.CurrentCulture,
               CommandLine.resources.GetString "instrumented",
               definition,
               pathn
             )
             |> sink

             File.Copy(first, pathn, true))

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.BadRecursiveInvocationRule",
                                                      "BadRecursiveInvocationRule",
                                                      Justification = "False positive")>]
    let private invokePredicate (f: unit -> bool) = f ()

    let internal doTrack state (m: MethodEntry) =
      m.Track
      |> Option.fold
           (fun (s: InstrumentContext) (n, _) ->
             let body =
               [ m.Method.Body; state.MethodBody ].[(m.Inspection.IsInstrumented).ToInt32]

             let methodWorker = body.GetILProcessor()
             removeTailInstructions methodWorker
             let (endFinally, rtype, leave) = encapsulateWithTryFinally methodWorker

             bulkInsertBefore
               methodWorker
               endFinally
               [ methodWorker.Create(OpCodes.Call, state.RecordingMethodRef.Pop) ]
               true
             |> ignore

             bulkInsertBefore
               methodWorker
               (methodWorker.Body.Instructions |> Seq.head)
               [ methodWorker.Create(OpCodes.Ldc_I4, n)
                 methodWorker.Create(OpCodes.Call, state.RecordingMethodRef.Push) ]
               true
             |> ignore

             let e = rtype.GetElementType().FullName

             let isTaskType () =
               [ "System.Threading.Tasks.Task"
                 "System.Threading.Tasks.Task`1" ]
               |> Seq.exists (fun n -> n = e)

             let isStateMachine () =
               m.Method.CustomAttributes // could improve this
               |> Seq.exists
                    (fun a ->
                      a.AttributeType.FullName = "System.Runtime.CompilerServices.AsyncStateMachineAttribute")

             let asyncChecks = [ isTaskType; isStateMachine ]

             let processAsyncAwait (s: InstrumentContext) =

               if asyncChecks |> Seq.forall invokePredicate then
                 // the instruction list is
                 // IL_0040: call System.Threading.Tasks.Task`1<!0> System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Int32>::get_Task()
                 // IL_0000: stloc V_1 <<== This one
                 // IL_0045: leave IL_0000

                 // Want to insert
                 //+IL_0045: ldloc V_xx <<== whatever
                 //  and either
                 //+IL_0046: callvirt instance void [System.Runtime]System.Threading.Tasks.Task::Wait()
                 //  or
                 //+IL_0046: ldc.i4 65535
                 //+IL_004b: callvirt instance bool [System.Runtime]System.Threading.Tasks.Task::Wait(int32)
                 //+IL_0050: pop                    // = discard the return value
                 // ahead of the leave opcode

                 let newstate =
                   { s with
                       AsyncSupport =
                         Some(
                           Option.defaultWith
                             (fun () -> AsyncSupport.Update m.Method)
                             state.AsyncSupport
                         ) }

                 let injectWait ilp (i: Instruction) =
                   bulkInsertBefore
                     ilp
                     i.Next
                     [ ilp.Create(OpCodes.Ldloc, i.Operand :?> VariableDefinition)
                       ilp.Create(OpCodes.Ldc_I4, 65535)
                       ilp.Create(OpCodes.Callvirt, newstate.AsyncSupport.Value.LocalWait)
                       ilp.Create(OpCodes.Pop) ]
                     true

                 leave
                 |> Seq.iter ((injectWait methodWorker) >> ignore)

                 newstate
               else
                 s

             let isAsyncType () =
               [ "Microsoft.FSharp.Control.FSharpAsync`1" ]
               |> Seq.exists (fun n -> n = e)

             let processFSAsync (s: InstrumentContext) =

               if isAsyncType () then
                 let asyncOf =
                   (rtype :?> GenericInstanceType).GenericArguments
                   |> Seq.head // only one

                 // the instruction list is
                 // IL_0023: callvirt instance class [FSharp.Core]Microsoft.FSharp.Control.FSharpAsync`1<!!0> [FSharp.Core]Microsoft.FSharp.Control.FSharpAsyncBuilder::Delay<class [FSharp.Core]Microsoft.FSharp.Core.Unit>(class [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<class [FSharp.Core]Microsoft.FSharp.Core.Unit, class [FSharp.Core]Microsoft.FSharp.Control.FSharpAsync`1<!!0>>)
                 // IL_0000: stloc V_1 <<== This one
                 // IL_0045: leave IL_0000

                 // Want to insert
                 //+IL_0045: ldloc V_xx <<== whatever
                 //IL_0014: ldnull
                 //IL_0015: ldnull
                 //IL_0016: call !!0 [FSharp.Core]Microsoft.FSharp.Control.FSharpAsync::RunSynchronously<class [FSharp.Core]Microsoft.FSharp.Core.Unit>(class [FSharp.Core]Microsoft.FSharp.Control.FSharpAsync`1<!!0>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<valuetype [System.Runtime]System.Threading.CancellationToken>)
                 //IL_001b: pop
                 // ahead of the leave opcode

                 let newstate =
                   { s with
                       AsyncSupport =
                         Some(
                           Option.defaultWith
                             (fun () -> AsyncSupport.Update m.Method) // TODO
                             state.AsyncSupport
                         ) }

                 let injectWait ilp (i: Instruction) =
                   bulkInsertBefore
                     ilp
                     i.Next
                     [ ilp.Create(OpCodes.Ldloc, i.Operand :?> VariableDefinition)
                       ilp.Create(OpCodes.Ldnull)
                       ilp.Create(OpCodes.Ldnull)
                       ilp.Create(
                         OpCodes.Call,
                         newstate.AsyncSupport.Value.RunSynchronously m.Method asyncOf
                       )
                       ilp.Create(OpCodes.Pop) ]
                     true

                 leave
                 |> Seq.iter ((injectWait methodWorker) >> ignore)

                 newstate
               else
                 s

             state |> processAsyncAwait |> processFSAsync

             )
           state

    let private visitAfterMethod state (m: MethodEntry) =
      if m.Inspection.IsInstrumented then
        let body = state.MethodBody
        // changes conditional (br.s, brtrue.s ...) operators to corresponding "long" ones (br, brtrue)
        body.SimplifyMacros()
        // changes "long" conditional operators to their short representation where possible
        body.OptimizeMacros()
        // purge dodgy scope data
        state.MethodBody.Method |> pruneLocalScopes

      doTrack state m

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification = "AvoidSpeculativeGenerality too")>]
    let private visitAfterAssembly (state: InstrumentContext) (assembly: AssemblyEntry) =
      let originalFileName =
        Path.GetFileName assembly.Assembly.MainModule.FileName

      writeAssemblies assembly.Assembly originalFileName assembly.Destinations Output.info
      state

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
                                                      "EnsureLocalDisposalRule",
                                                      Justification = "Record return confusing Gendarme -- TODO")>]
    let private visitStart state =
      let stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream("AltCover.AltCover.Recorder.net20.dll")
      let recordingAssembly =
        prepareAssembly (stream)

      { state with
          RecordingAssembly = recordingAssembly
          RecorderSource = stream }

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
                                                      "EnsureLocalDisposalRule",
                                                      Justification = "Return confusing Gendarme -- TODO")>]
    let private loadClr4AssemblyFromResources (stream: Stream) =
      AssemblyDefinition.ReadAssembly(stream)
      |> prepareAssemblyDefinition

    let private finishVisit (state: InstrumentContext) =
      try
        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream("AltCover.AltCover.Recorder.net46.dll")

        let clr4 =
          state.AsyncSupport
          |> Option.map (fun _ -> loadClr4AssemblyFromResources stream)

        let recorder =
          Option.defaultValue state.RecordingAssembly clr4

        let recorderFileName =
          (extractName state.RecordingAssembly) + ".dll"

        writeAssemblies
          recorder
          recorderFileName
          (CoverageParameters.instrumentDirectories ())
          ignore

        CoverageParameters.instrumentDirectories ()
        |> Seq.iter
             (fun instrument ->

               Directory.GetFiles(
                 instrument,
                 "*.deps.json",
                 SearchOption.TopDirectoryOnly
               )
               |> Seq.iter
                    (fun f ->

                      File.WriteAllText(f, (f |> File.ReadAllText |> injectJSON))))
      finally
        (state.RecordingAssembly :> IDisposable).Dispose()
        state.RecorderSource
        |> Option.ofObj
        |> Option.iter (fun a -> a.Close())
        state.AsyncSupport
        |> Option.iter (fun a -> a.Close())

      { state with
          RecordingAssembly = null
          RecorderSource = null
          AsyncSupport = None }

    // Perform visitor operations
    // param name="state">Contextual information for the visit</param>
    // param name="node">The node being visited</param>
    // returns>Updated state</returns>
    let internal instrumentationVisitorCore (state: InstrumentContext) (node: Node) =
      match node with
      | Start _ -> visitStart state
      | Assembly assembly ->
          updateStrongReferences assembly.Assembly state.InstrumentedAssemblies
          |> ignore

          if assembly.Inspection <> Inspections.Ignore then
            assembly.Assembly.MainModule.AssemblyReferences.Add(
              state.RecordingAssembly.Name
            )

          state
      | Module m -> visitModule state m
      | Type _ -> state
      | Method m -> visitMethod state m
      | MethodPoint m -> visitMethodPoint state m
      | BranchPoint branch -> visitBranchPoint state branch
      | AfterMethod m -> visitAfterMethod state m
      | AfterType -> state
      | AfterModule -> state
      | AfterAssembly assembly -> visitAfterAssembly state assembly
      | Finish -> finishVisit state

    let internal instrumentationVisitorWrapper
      (core: InstrumentContext -> Node -> InstrumentContext)
      (state: InstrumentContext)
      (node: Node)
      =
      try
        core state node
      with _ ->
        match node with
        | Finish -> ()
        | _ ->
            if state.RecordingAssembly.IsNotNull then
              (state.RecordingAssembly :> IDisposable).Dispose()
            state.RecorderSource
            |> Option.ofObj
            |> Option.iter (fun a -> a.Close())
            state.AsyncSupport
            |> Option.iter (fun a -> a.Close())

        reraise ()

    let internal instrumentationVisitor (state: InstrumentContext) (node: Node) =
      instrumentationVisitorWrapper instrumentationVisitorCore state node

  // "Public" API
  // Higher-order function that returns a visitor
  // param name="assemblies">List of assembly paths to visit</param>
  // returns>Stateful visitor function</returns>
  let internal instrumentGenerator (assemblies: string list) =
    Visitor.encloseState I.instrumentationVisitor (InstrumentContext.Build assemblies)