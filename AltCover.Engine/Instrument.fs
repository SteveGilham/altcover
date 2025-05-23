﻿// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
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

open AltCover.Shared

[<assembly: SuppressMessage("Microsoft.Globalization",
                            "CA1307:SpecifyStringComparison",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Engine>.$Instrument.#.cctor()",
                            MessageId =
                              "System.String.Replace(System.String,System.String)",
                            Justification =
                              "No suitable overload in netstandard2.0/net472")>]
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
      typeof<System.Threading.Tasks.Task>.Assembly.Location
      |> AssemblyResolver.ReadAssembly

    let task =
      def.MainModule.GetType("System.Threading.Tasks.Task")

    let wait =
      task.Methods
      |> Seq.filter
        _.FullName.Equals(
          "System.Boolean System.Threading.Tasks.Task::Wait(System.Int32)",
          StringComparison.Ordinal
        )
      |> Seq.head

    let def2 =
      typeof<Microsoft.FSharp.Control.AsyncReturn>.Assembly.Location
      |> AssemblyResolver.ReadAssembly

    let fsasync =
      def2.MainModule.GetType("Microsoft.FSharp.Control.FSharpAsync")

    let runsynch =
      fsasync.Methods
      |> Seq.filter _.Name.Equals("RunSynchronously", StringComparison.Ordinal)
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
    RecorderSource: Stream
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
  let recorderVersion () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.AltCover.Recorder.net20.dll")

    use def =
      AssemblyResolver.ReadAssembly stream

    def.Name.Version.ToString()

  let version = recorderVersion ()

  let internal modules = List<string>()

  module internal I =

    let prelude =
      [ 0x01uy
        0x00uy
        0x02uy
        0x00uy
        0x54uy
        0x0euy
        0x08uy
        0x41uy
        0x73uy
        0x73uy
        0x65uy
        0x6duy
        0x62uy
        0x6cuy
        0x79uy ]

    let interlude =
      [ 0x54uy
        0x0euy
        0x0duy
        0x43uy
        0x6fuy
        0x6euy
        0x66uy
        0x69uy
        0x67uy
        0x75uy
        0x72uy
        0x61uy
        0x74uy
        0x69uy
        0x6fuy
        0x6euy ]

    let internal injectInstrumentation
      (recorder: AssemblyDefinition)
      (assembly: AssemblyEntry)
      =
      let ascii = System.Text.Encoding.ASCII

      let a =
        ascii.GetBytes(assembly.Identity.Assembly)

      let c =
        ascii.GetBytes(assembly.Identity.Configuration)

      let blob =
        [| prelude |> List.toArray
           [| byte a.Length |]
           a
           interlude |> List.toArray
           [| byte c.Length |]
           c |] // slight inefficiency
        |> Array.concat

      let attribute =
        recorder.MainModule.GetType("AltCover.Recorder.InstrumentationAttribute")

      let constructor =
        attribute.GetConstructors()
        |> Seq.head
        |> assembly.Assembly.MainModule.ImportReference

      let inject =
        CustomAttribute(constructor, blob)

      assembly.Assembly.CustomAttributes.Add inject

    // Locate the method that must be called to register a code point for coverage visit.
    // param name="assembly">The assembly containing the recorder method</param>
    // returns>A representation of the method to call to signal a coverage visit.</returns>
    let internal recordingMethod (recordingAssembly: AssemblyDefinition) =
      recordingAssembly.MainModule.GetAllTypes()
      |> Seq.filter (fun t -> t.FullName == "AltCover.Recorder.Instance")
      |> Seq.collect _.Methods
      |> Seq.map (fun t -> (t.Name, t))
      |> Seq.filter (fun (n, _) -> n == "Visit" || n == "Push" || n == "Pop")
      |> Seq.sortBy fst
      |> Seq.map snd
      |> Seq.toList
      |> List.rev

    let internal updateVisibleTo (assembly: AssemblyDefinition) =
      let va =
        assembly.CustomAttributes
        |> Seq.filter (fun a ->
          a.AttributeType.FullName.Equals(
            "System.Runtime.CompilerServices.InternalsVisibleToAttribute",
            StringComparison.Ordinal
          ))
        |> Seq.toList

      let tag a =
        match CoverageParameters.defaultStrongNameKey with
        | None -> a
        | Some key ->
          a
          + ", PublicKey="
          + (key.PublicKey
             |> Seq.toArray
             |> BitConverter.ToString)
            .Replace("-", String.Empty)

      let attrtype = va |> Seq.tryHead

      let injectRef (ref: string) =
        let constructor = attrtype.Value.Constructor

        let blob =
          System.Collections.Generic.List<Byte>(System.Text.Encoding.ASCII.GetBytes(ref))

        blob.Insert(0, 0uy)
        blob.Insert(0, 1uy)
        blob.AddRange [ 0uy; 0uy ]

        let inject =
          CustomAttribute(constructor, blob |> Seq.toArray)

        inject.ConstructorArguments.Add(
          CustomAttributeArgument(constructor.Parameters[0].ParameterType, ref)
        )

        assembly.CustomAttributes.Add inject

      va
      |> List.map (_.ConstructorArguments >> Seq.head)
      |> List.map (_.Value.ToString())
      |> List.map (_.Split(',') >> Seq.head >> tag)
      |> List.iter injectRef

      assembly

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
                                                      Justification =
                                                        "AvoidSpeculativeGenerality too")>]
    let internal knownKey (name: AssemblyNameDefinition) =
      if not name.HasPublicKey then
        None
      else
        let index =
          KeyStore.arrayToIndex name.PublicKey

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
                                                      Justification =
                                                        "AvoidSpeculativeGenerality too")>]
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
      guard definition (fun () ->

        //if monoRuntime |> not then
        ProgramDatabase.readSymbols definition

        definition.Name.Name <- (extractName definition) + ".g"

        let pair =
          CoverageParameters.recorderStrongNameKey

        updateStrongNaming definition pair

        //definition.MainModule.GetTypes()
        //|> Seq.iter (fun t ->
        //  if
        //    t.IsPublic
        //    && (not
        //        <| t.FullName.StartsWith("AltCover", StringComparison.Ordinal))
        //  then
        //    t.IsPublic <- false)

        injectInstrumentation
          definition
          { Assembly = definition
            Inspection = Inspections.Ignore
            Destinations = []
            Identity =
              { Assembly = "AltCover+Recorder+g+"
                Configuration = "Uninstrumented++" } }

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
          ("get_Eager",
           (fun (w: ILProcessor) -> w.Create(CoverageParameters.eagerOpCode ()))) ]
        |> List.iter (fun (property, value) ->
          let pathGetterDef =
            definition.MainModule.GetTypes()
            |> Seq.collect _.Methods
            |> Seq.filter (fun m -> m.Name == property)
            |> Seq.head

          let body = pathGetterDef.Body
          let worker = body.GetILProcessor()

          let initialBody =
            body.Instructions |> Seq.toList

          let head = initialBody |> Seq.head
          worker.InsertBefore(head, value (worker))
          worker.InsertBefore(head, worker.Create(OpCodes.Ret))
          initialBody |> Seq.iter worker.Remove)

        [ ("get_Timer", // set the timer interval in ticks
           CoverageParameters.interval ()) ]
        |> List.iter (fun (property, value) ->
          let pathGetterDef =
            definition.MainModule.GetTypes()
            |> Seq.collect _.Methods
            |> Seq.filter (fun m -> m.Name == property)
            |> Seq.head

          let body = pathGetterDef.Body
          let worker = body.GetILProcessor()

          let initialBody =
            body.Instructions |> Seq.toList

          let head = initialBody |> Seq.head
          worker.InsertBefore(head, worker.Create(OpCodes.Ldc_I4, value))
          worker.InsertBefore(head, worker.Create(OpCodes.Conv_I8))
          worker.InsertBefore(head, worker.Create(OpCodes.Ret))
          initialBody |> Seq.iter worker.Remove))

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
                                                      "EnsureLocalDisposalRule",
                                                      Justification =
                                                        "Return confusing Gendarme -- TODO")>]
    let internal prepareAssembly (assembly: Stream) =
      let definition =
        AssemblyResolver.ReadAssembly(assembly)

      prepareAssemblyDefinition definition

    // #if IDEMPOTENT_INSTRUMENT
    //     let internal safeWait (mutex: System.Threading.WaitHandle) =
    //       try
    //         mutex.WaitOne() |> ignore
    //       with
    //       | :? System.Threading.AbandonedMutexException -> ()

    //     let internal withFileMutex (p: string) f =
    //       let key =
    //         p
    //         |> System.Text.Encoding.UTF8.GetBytes
    //         |> CoverageParameters.hash.ComputeHash
    //         |> Convert.ToBase64String

    //       use mutex =
    //         new System.Threading.Mutex(false, "AltCover-" + key.Replace('/', '.') + ".mutex")

    //       safeWait mutex

    //       try
    //         f ()
    //       finally
    //         mutex.ReleaseMutex()
    // #endif

    // Commit an instrumented assembly to disk
    // param name="assembly">The instrumented assembly object</param>
    // param name="path">The full path of the output file</param>
    // remark>Can raise "System.Security.Cryptography.CryptographicException: Keyset does not exist" at random
    // when asked to strongname.  This writes a new .pdb/.mdb alongside the instrumented assembly</remark>
    let internal writeAssembly (assembly: AssemblyDefinition) (path: string) =
      let pkey = Mono.Cecil.WriterParameters()

      pkey.SymbolWriterProvider <-
        Mono.Cecil.Cil.EmbeddedPortablePdbWriterProvider() :> ISymbolWriterProvider

      pkey.WriteSymbols <- true

      knownKey assembly.Name
      |> Option.iter (fun key -> pkey.StrongNameKeyBlob <- key.Blob |> List.toArray)

      let here = Directory.GetCurrentDirectory()

      try
        Directory.SetCurrentDirectory(Path.GetDirectoryName(path))

        // #if IDEMPOTENT_INSTRUMENT
        //         let write (a: AssemblyDefinition) (p: string) pk =
        //           withFileMutex
        //             p
        //             (fun () ->
        //               if p |> File.Exists |> not
        //                  || DateTime.Now.Year > 2000 // TODO -- check hashes
        //               then
        //                 use sink =
        //                   File.Open(p, FileMode.Create, FileAccess.ReadWrite)

        //                 a.Write(sink, pk))
        // #else
        let write (a: AssemblyDefinition) p pk =
          use sink =
            File.Open(p, FileMode.Create, FileAccess.ReadWrite)

          a.Write(sink, pk)
        // #endif

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
        |> Seq.filter (fun x -> assemblies |> List.exists (fun y -> y == x.Name))
        |> Seq.toList

      // The return value is for unit testing purposes, only
      // The side-effects are what is important.
      let assemblyReferenceSubstitutions =
        Dictionary<String, String>()

      interestingReferences
      |> Seq.iter (fun r ->
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
          <| updated.Equals(original, StringComparison.Ordinal)
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
        (oo |> Seq.find (fun kv -> kv.Key == "targets")).Value.Object

      let targeted =
        (targets |> Seq.find (fun p -> p.Key == target)).Value.Object

      let app =
        (targeted.Values |> Seq.head).Object

      let existingDependencies =
        app
        |> Seq.tryFind (fun p -> p.Key == "dependencies")

      let prior =
        match existingDependencies with
        | None -> Set.empty<string>
        | Some p -> p.Value.Object |> Seq.map _.Key |> Set.ofSeq

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
            |> Seq.find (fun p -> p.Key == "dependencies")

          match
            app
            |> Seq.tryFind (fun p -> p.Key == "dependencies")
          with
          | None -> app |> addFirst [ rawDependencies ]
          | Some p ->
            let recorder =
              rawDependencies.Value.Object |> Seq.head

            p.Value.Object.[recorder.Key] <- recorder.Value

        updateDependencies ()

      let stripRecorderRefs (j: JsonObject) =
        j.Keys
        |> Seq.filter (fun k ->
          k.StartsWith("AltCover.Recorder.g/", StringComparison.Ordinal))
        |> Seq.toList
        |> List.iter (j.Remove >> ignore)

      do
        let runtime =
          version
          |> sprintf
            """{"AltCover.Recorder.g/%s": {"runtime": { "AltCover.Recorder.g.dll": {}}}}"""

        let updateRuntime () =
          let runtimeObject =
            (JsonValue.Parse runtime).Object

          stripRecorderRefs targeted
          let recorder = runtimeObject |> Seq.head
          targeted.[recorder.Key] <- recorder.Value

        updateRuntime ()

      let libraries =
        (oo |> Seq.find (fun p -> p.Key == "libraries")).Value.Object

      do
        let newLibraries =
          version
          |> sprintf
            """{"AltCover.Recorder.g/%s": {"type": "project", "serviceable": false, "sha512": "" }}"""

        let updateLibraries () =
          let newlibs =
            (JsonValue.Parse newLibraries).Object

          stripRecorderRefs libraries
          libraries |> addFirst newlibs

        updateLibraries ()

      o.GetIndentedString().Replace("\t\t", "  ").Replace("\t", "  ").Replace(" :", ":")

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
                |> Option.map (fun a ->
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
        let mt = m.Method

        let body = mt.Body

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
        let point =
          (branch.Uid ||| Counter.branchFlag)

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
          let operands =
            branch.Start.Operand :?> Instruction[]

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
              branch.Start.Operand :?> Instruction[]
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
      let first =
        Path.Combine(targets |> Seq.head, file)

      String.Format(
        System.Globalization.CultureInfo.CurrentCulture,
        Output.resources.GetString "instrumented",
        definition,
        first
      )
      |> sink

      writeAssembly (updateVisibleTo definition) first

      targets
      |> Seq.tail
      |> Seq.iter (fun p ->
        let pathn = Path.Combine(p, file)

        String.Format(
          System.Globalization.CultureInfo.CurrentCulture,
          Output.resources.GetString "instrumented",
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

          let (endFinally, rtype, leave) =
            encapsulateWithTryFinally methodWorker

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
            |> Seq.exists (fun n -> n == e)

          let isStateMachine () =
            m.Method.CustomAttributes // could improve this
            |> Seq.exists (fun a ->
              a.AttributeType.FullName
              == "System.Runtime.CompilerServices.AsyncStateMachineAttribute")

          let asyncChecks =
            [ isTaskType; isStateMachine ]

          let processAsyncAwait (s: InstrumentContext, unhandled: bool) =

            if
              unhandled
              && asyncChecks |> Seq.forall invokePredicate
            then
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

              (newstate, false)
            else
              (s, unhandled)

          let isAsyncType () =
            [ "Microsoft.FSharp.Control.FSharpAsync`1" ]
            |> Seq.exists (fun n -> n == e)

          let processFSAsync (s: InstrumentContext, unhandled: bool) =

            if unhandled && isAsyncType () then
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

              (newstate, false)
            else
              (s, unhandled)

          let processTaskReturns (s: InstrumentContext, unhandled: bool) =
            if unhandled && isTaskType () then
              let newstate =
                { s with
                    AsyncSupport =
                      Some(
                        Option.defaultWith
                          (fun () -> AsyncSupport.Update m.Method) // TODO
                          state.AsyncSupport
                      ) }

              let injectWait ilp (i: Instruction) =
                // before
                // IL_003f: stloc V1
                // IL_0040: leave.s IL_0054

                // after
                // IL_003d: stloc V1
                //+IL_003e: ldloc V1
                //+IL_003f: callvirt instance void [System.Runtime]System.Threading.Tasks.Task::Wait()
                //+IL_0044: ldloc V1
                //+IL_0045: stloc.0
                // IL_0046: leave.s IL_005a

                bulkInsertBefore
                  ilp
                  i.Next
                  [ ilp.Create(OpCodes.Ldloc, i.Operand :?> VariableDefinition)
                    ilp.Create(OpCodes.Ldc_I4, 65535)
                    ilp.Create(OpCodes.Callvirt, newstate.AsyncSupport.Value.LocalWait)
                    ilp.Create(OpCodes.Pop)
                    ilp.Create(OpCodes.Ldloc, i.Operand :?> VariableDefinition)
                    ilp.Create(OpCodes.Stloc_0) ]
                  true

              leave
              |> Seq.iter ((injectWait methodWorker) >> ignore)

              (newstate, false)
            else
              (s, unhandled)

          (state, true)
          |> processAsyncAwait
          |> processFSAsync
          |> processTaskReturns
          |> fst)
        state

    let private visitAfterMethod state (m: MethodEntry) =
      if m.Inspection.IsInstrumented then
        let body = state.MethodBody
        // changes conditional (br.s, brtrue.s ...) operators to corresponding "long" ones (br, brtrue)
        body.SimplifyMacros()
        // changes "long" conditional operators to their short representation where possible
        body.OptimizeMacros()

      doTrack state m

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification =
                                                        "AvoidSpeculativeGenerality too")>]
    let private visitAfterAssembly (state: InstrumentContext) (assembly: AssemblyEntry) =
      let ``module`` =
        assembly.Assembly.MainModule

      let originalFileName =
        Path.GetFileName ``module``.FileName

      use mem = new System.IO.MemoryStream()

      use crush =
        new System.IO.Compression.DeflateStream(
          mem,
          System.IO.Compression.CompressionMode.Compress
        )

      let data =
        System.Text.Encoding.UTF8.GetBytes(Visitor.moduleReport)

      crush.Write(data, 0, data.Length)
      crush.Flush()
      mem.Position <- 0L

      // Cyrillic capitals in "АltСover"
      let extra =
        EmbeddedResource(
          ("\u0410lt\u0421over."
           + (CoverageParameters.reportKind ()).ToString()),
          ManifestResourceAttributes.Private,
          mem
        )

      ``module``.Resources.Add extra

      writeAssemblies assembly.Assembly originalFileName assembly.Destinations Output.info
      state

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
                                                      "EnsureLocalDisposalRule",
                                                      Justification =
                                                        "Record return confusing Gendarme -- TODO")>]
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

    [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
    type CallTracker =
      { CallTrack: TypeDefinition
        Value: PropertyDefinition
        GetValue: MethodDefinition
        Instance: MethodDefinition
        Field: FieldDefinition
        FieldType: GenericInstanceType
        Maker: MethodDefinition }

    // make the assembly net46 targeted
    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "InstantiateArgumentExceptionCorrectlyRule",
                      Justification = "Library method inlined")>]
    let framework46 (make: MethodDefinition) (r: AssemblyDefinition) =
      let constructor =
        make.Body.Instructions
        |> Seq.filter (fun i -> i.OpCode = OpCodes.Newobj)
        |> Seq.map (fun i -> i.Operand :?> MethodReference)
        |> Seq.head
        |> r.MainModule.ImportReference

      let blob =
        "01 00 1a 2e 4e 45 54 46 72 61 6d 65 77 6f 72 6b 2c 56 65 72 73 69 6f 6e 3d 76 34 2e 36 01 00 54 0e 14 46 72 61 6d 65 77 6f 72 6b 44 69 73 70 6c 61 79 4e 61 6d 65 12 2e 4e 45 54 20 46 72 61 6d 65 77 6f 72 6b 20 34 2e 36"
          .Split(' ')
        |> Array.map (fun x -> Convert.ToByte(x, 16))

      let inject =
        CustomAttribute(constructor, blob)

      r.CustomAttributes.Add inject

    let internal uprateRecorder (recorder: AssemblyDefinition) =
      let m = recorder.MainModule

      // make it net4+
      let uprateReferences () =
        m.AssemblyReferences
        |> Seq.filter (fun a -> a.Version = Version(2, 0, 0, 0))
        |> Seq.iter (fun a -> a.Version <- Version(4, 0, 0, 0))

        m.RuntimeVersion <- "v4.0.30319"
        m.Runtime <- TargetRuntime.Net_4_0

      uprateReferences ()

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream("AltCover.AltCover.Async.net46.dll")

      use delta =
        AssemblyResolver.ReadAssembly(stream)

      // get a handle on the property
      let readCallTrackType (m: ModuleDefinition) =
        let calltrack =
          m.GetType("AltCover.Recorder.Instance/I/CallTrack")

        let value =
          calltrack.Properties
          |> Seq.find (fun m -> m.Name == "Value")

        let getValue = value.GetMethod

        let field =
          ((getValue.Body.Instructions |> Seq.head).Operand :?> FieldReference).Resolve()

        { CallTrack = calltrack
          Value = value
          GetValue = getValue
          Instance =
            calltrack.Methods
            |> Seq.find (fun m -> m.Name == "Instance")
          Field = field
          FieldType = field.FieldType :?> GenericInstanceType
          Maker = field.Resolve().DeclaringType.GetStaticConstructor() }

      let net20 = readCallTrackType m

      let asy46 =
        readCallTrackType delta.MainModule

      let field =
        ((net20.GetValue.Body.Instructions |> Seq.head).Operand :?> FieldReference)
          .Resolve()

      let localAsync = field.FieldType.Resolve()

      let oldtype =
        field.FieldType :?> GenericInstanceType

      // replace the type references in the recorder
      let generics =
        asy46.FieldType.GenericArguments

      generics.Clear()

      net20.FieldType.GenericArguments
      |> Seq.iter (m.ImportReference >> generics.Add)

      let async2 =
        (asy46.FieldType |> m.ImportReference)

      let maker =
        asy46.Maker.Body.Instructions
        |> Seq.filter (fun i -> i.OpCode = OpCodes.Newobj)
        |> Seq.map (fun i -> i.Operand :?> MethodReference)
        |> Seq.skip 1
        |> Seq.head
        |> m.ImportReference

      let build =
        net20.Maker.Body.Instructions
        |> Seq.filter (fun i -> i.OpCode = OpCodes.Newobj)
        |> Seq.find (fun i ->
          (i.Operand :?> MethodReference).DeclaringType.Name
          == oldtype.Name)

      net20.Field.FieldType <- async2
      net20.Value.PropertyType <- async2
      net20.GetValue.ReturnType <- async2
      build.Operand <- maker

      let copyInstance (old: CallTracker) (updated: MethodDefinition) =
        let body = old.Instance.Body
        let worker = body.GetILProcessor()

        let initialBody =
          body.Instructions |> Seq.toList

        let head = initialBody |> Seq.head

        bulkInsertBefore
          worker
          head
          (updated.Body.Instructions
           |> Seq.map (fun i ->
             match i.OpCode.FlowControl with
             | FlowControl.Call ->
               i.Operand <-
                 let mr = (i.Operand :?> MethodReference)

                 if
                   mr.DeclaringType.FullName
                   == old.CallTrack.FullName
                 then
                   old.GetValue :> MethodReference
                 else
                   mr |> m.ImportReference

               i
             | _ -> i))
          true
        |> ignore

        initialBody |> Seq.iter worker.Remove

      copyInstance net20 asy46.Instance

      // delete the placeholder type
      m.GetType("AltCover.Recorder.Instance/I").NestedTypes.Remove(localAsync)
      |> ignore

      framework46 asy46.Maker recorder

    let private finishVisit (state: InstrumentContext) =
      try
        let recorder = state.RecordingAssembly

        state.AsyncSupport
        |> Option.iter (fun _ -> uprateRecorder recorder)

        // write the module ID values
        let keys =
          modules |> Seq.distinct |> Seq.toList

        let getterDef =
          recorder.MainModule.GetTypes()
          |> Seq.collect _.Methods
          |> Seq.filter (fun m -> m.Name == "get_Modules")
          |> Seq.head

        let body = getterDef.Body
        let worker = body.GetILProcessor()

        let initialBody =
          body.Instructions |> Seq.toList

        let head = initialBody |> Seq.head

        let stringtype =
          getterDef.MethodReturnType.ReturnType.GetElementType()

        let makeArray =
          [ worker.Create(OpCodes.Ldc_I4, keys |> List.length)
            worker.Create(OpCodes.Newarr, stringtype) ]

        bulkInsertBefore worker head makeArray true
        |> ignore

        keys
        |> Seq.iteri (fun i k ->
          let addElement =
            [ worker.Create(OpCodes.Dup)
              worker.Create(OpCodes.Ldc_I4, i)
              worker.Create(OpCodes.Ldstr, k)
              worker.Create(OpCodes.Stelem_Ref) ]

          bulkInsertBefore worker head addElement true
          |> ignore)

        let ret = [ worker.Create OpCodes.Ret ]
        bulkInsertBefore worker head ret true |> ignore

        let recorderFileName =
          (extractName state.RecordingAssembly) + ".dll"

        writeAssemblies
          recorder
          recorderFileName
          (CoverageParameters.instrumentDirectories ())
          ignore

        CoverageParameters.instrumentDirectories ()
        |> Seq.iter (fun instrument ->

          Directory.GetFiles(instrument, "*.deps.json", SearchOption.TopDirectoryOnly)
          |> Seq.iter (fun f ->

            File.WriteAllText(f, (f |> File.ReadAllText |> injectJSON))))
      finally
        (state.RecordingAssembly :> IDisposable).Dispose()

        state.RecorderSource
        |> Option.ofObj
        |> Option.iter _.Close()

        state.AsyncSupport |> Option.iter _.Close()

      { state with
          RecordingAssembly = null
          RecorderSource = null
          AsyncSupport = None }

    let visitAssembly state assembly =
      updateStrongReferences assembly.Assembly state.InstrumentedAssemblies
      |> ignore

      if assembly.Inspection <> Inspections.Ignore then
        assembly.Assembly.MainModule.AssemblyReferences.Add(state.RecordingAssembly.Name)

        // TODO -- react to source or destination being stamped
        injectInstrumentation state.RecordingAssembly assembly

      state

    // Perform visitor operations
    // param name="state">Contextual information for the visit</param>
    // param name="node">The node being visited</param>
    // returns>Updated state</returns>
    let internal instrumentationVisitorCore (state: InstrumentContext) (node: Node) =
      match node with
      | Start _ -> visitStart state
      | Assembly assembly -> visitAssembly state assembly
      //updateStrongReferences assembly.Assembly state.InstrumentedAssemblies
      //|> ignore
      //
      //if assembly.Inspection <> Inspections.Ignore then
      //  assembly.Assembly.MainModule.AssemblyReferences.Add(
      //    state.RecordingAssembly.Name
      //  )
      //
      //state
      | Module m -> visitModule state m
      | Type _ -> state
      | Method m -> visitMethod state m
      | MethodPoint m -> visitMethodPoint state m
      | BranchPoint branch -> visitBranchPoint state branch
      | AfterMethod m -> visitAfterMethod state m
      | AfterType -> state
      | AfterModule ->
        modules.Add state.ModuleId
        state
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
          |> Option.iter _.Close()

          state.AsyncSupport |> Option.iter _.Close()

        reraise ()

    let internal instrumentationVisitor (state: InstrumentContext) (node: Node) =
      instrumentationVisitorWrapper instrumentationVisitorCore state node

  // "Public" API
  // Higher-order function that returns a visitor
  // param name="assemblies">List of assembly paths to visit</param>
  // <returns>Stateful visitor function</returns>
  let internal instrumentGenerator (assemblies: string list) =
    Visitor.encloseState I.instrumentationVisitor (InstrumentContext.Build assemblies)

[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Instrument/I/tag@246::Invoke(System.String)",
                            Justification = "Replace override not available")>]
()