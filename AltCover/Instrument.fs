// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/CounterAssemblyBuilder.cs
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/InstrumentorVisitor.cs

namespace AltCover

open Mono.Cecil

module Instrument =

  let DefineRecordingAssembly () =
    let recorder = typeof<AltCover.Recorder.Tracer>
    let definition = AssemblyDefinition.ReadAssembly(recorder.Assembly.Location)
    definition.Name.Name <- definition.Name.Name + ".g"
    match Visitor.strongNameKey with
    | None -> definition.Name.HasPublicKey <- false
              definition.Name.PublicKey <- null
              definition.Name.PublicKeyToken <- null
    | Some key -> definition.Name.HasPublicKey <- true
                  definition.Name.PublicKey <- key.PublicKey