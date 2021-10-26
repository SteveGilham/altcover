namespace AltCover

open System
open System.Globalization
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
    String.Join(String.Empty,
                data |> Seq.map (fun d -> d.ToString("X2", CultureInfo.InvariantCulture)))