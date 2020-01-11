open System
open System.IO
open System.Reflection
open System.Security.Cryptography

let key = fsi.CommandLineArgs.[1]
let stream = new FileStream(key, System.IO.FileMode.Open, System.IO.FileAccess.Read)
let pair = new StrongNameKeyPair(stream)
// get the public key token as 8 bytes from the end of a SHA1 hash of the key material
let hash = new SHA1CryptoServiceProvider()

let token =
  hash.ComputeHash(pair.PublicKey)
  |> Array.rev
  |> Seq.take 8

let patchArray s =
  s
  |> Seq.map (fun (x : byte) -> x.ToString("X2").ToLower())
  |> Seq.toList

let rec chunkArray (s : string list) =
  let chunk = 39
  s
  |> Seq.truncate chunk
  |> Seq.iter Console.Write
  Console.WriteLine(String.Empty)
  if chunk > (List.length s) then
    ()
  else
    s
    |> Seq.skip chunk
    |> Seq.toList
    |> chunkArray

let dumpArray s =
  s
  |> patchArray
  |> chunkArray

Console.WriteLine("Public key is")
pair.PublicKey |> dumpArray
Console.WriteLine(String.Empty)
Console.Write("Public key token is ")
token |> dumpArray
Console.WriteLine(String.Empty)