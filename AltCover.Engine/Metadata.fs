namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.IO.Compression

open Mono.Cecil.Cil

module internal Metadata =
  let private squash (source:byte[]) =
    use squashed = new MemoryStream()
    // Get deflation working with this one weird trick
    // Dispose the deflate stream w/o closing the one it points at!
    do
      use compress = new DeflateStream(squashed, CompressionMode.Compress, true)
      compress.Write(source, 0, source.Length)
    let crushed = Array.create<byte> (int squashed.Length) 0uy
    squashed.Seek (0L, SeekOrigin.Begin) |> ignore
    squashed.Read (crushed, 0, crushed.Length) |> ignore
    crushed

  let internal getCompressedSource (embed:EmbeddedSourceDebugInformation) =
    let datamake = Maybe embed.Compress squash id
    let data = datamake embed.Content
    Convert.ToBase64String data

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal getSource (record:Document) =
    record.CustomDebugInformations
    |> Seq.tryFind (fun c -> c.Kind = CustomDebugInformationKind.EmbeddedSource)
    |> Option.map (fun c -> getCompressedSource (c :?> EmbeddedSourceDebugInformation))
    |> Option.filter (String.IsNullOrEmpty >> not)