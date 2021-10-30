open System
open System.IO
open System.IO.Compression
open System.Text

// fsi --exec .\Build\SquashFile.fsx ".\Samples\Sample1\Program.cs"

for arg in Environment.GetCommandLineArgs() |> Seq.skip 3 do
  printfn "Read value %A" arg
  let src = File.ReadAllText arg
  let binary = Encoding.UTF8.GetBytes src
  use squashed = new MemoryStream()
  do
    use compress = new DeflateStream(squashed, CompressionMode.Compress, true)
    compress.Write(binary, 0, binary.Length)
  let crushed = Array.create<byte> (int squashed.Length) 0uy
  squashed.Seek (0L, SeekOrigin.Begin) |> ignore
  squashed.Read (crushed, 0, crushed.Length) |> ignore
  crushed
  |> Convert.ToBase64String
  |> printfn "%s"