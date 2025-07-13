namespace Tests

open System
open System.IO
open System.Reflection

open AltCover

#nowarn "25"

module ProgramDatabase =

  let isAssemblyType (file: string) =
#if !NET472
    [ ".dll" ]
#else
    [ ".dll"; ".exe" ]
#endif
    |> Seq.exists (fun x -> file.EndsWith(x, StringComparison.OrdinalIgnoreCase))

  [<Test>]
  let ShouldTrapIndexOutOfRangeException () =
    let a = [| "a"; "b" |]

    Assert.Throws<SymbolReadException>(fun () ->
      AltCover.ProgramDatabase.I.raiseSymbolError (fun () -> printfn "%s" a[2]))
    |> ignore

    a.[1] <- null

    Assert.Throws<NullReferenceException>(fun () ->
      AltCover.ProgramDatabase.I.raiseSymbolError (fun () -> printfn "%d" a[1].Length))
    |> ignore

  [<Test>]
  let ShouldGetPdbFromImage () =
    let files =
      [ Directory.GetFiles(dir, "AltCover*")
        Directory.GetFiles(dir, "Sample*") ]
      |> Seq.concat
      |> Seq.filter isAssemblyType
      |> Seq.filter (fun f -> f |> Path.GetFileName <> "AltCover.Recorder.g.dll")
      |> Seq.map (fun x -> (x, AssemblyResolver.ReadAssembly x))
      |> Seq.filter (fun x -> (fst x) + ".mdb" |> File.Exists |> not)
      |> Seq.toList

    test <@ files <> [] @>

    let pdbs =
      files
      |> List.map (fun (f, s) -> f, AltCover.ProgramDatabase.getPdbFromImage s)

    pdbs
    |> Seq.iter (fun x ->
      let f = fst x
      let tokens, pdb = snd x

      test' <@ pdb.IsSome @> f

      let name = pdb.Value

      printfn "%s => %s %s" f name (name |> Path.GetDirectoryName)

      let seperatePdb =
        name
        |> Path.GetDirectoryName
        |> String.IsNullOrEmpty
        |> not

      Assert.That(
        AltCover.ProgramDatabase.I.symbolMatch tokens name,
        Is.EqualTo seperatePdb,
        f |> Path.GetFileName
      )

      //Assert.That(f |> Path.GetFileName, Is.Not.EqualTo "Sample2.dll", f)
      let probe = Path.ChangeExtension(f, ".pdb")
      let file = FileInfo(probe)
      let filename = file.Name.Replace("\\", "/")

      Assert.That(
        "/" + name.Replace("\\", "/"),
        Does.EndWith("/" + filename),
        f + " -> " + name
      ))

  [<Test>]
  let ShouldGetGUIDfromNativePdb () =
    let here =
      System.Reflection.Assembly.GetExecutingAssembly()

    let nativeName =
      here.GetManifestResourceNames()
      |> Seq.find _.EndsWith("native.pdb", StringComparison.Ordinal)

    let native =
      here.GetManifestResourceStream(nativeName)

    use b = new BinaryReader(native)

    let checkFormat =
      ProgramDatabase.I.checkPdb b

    Assert.That(checkFormat, Is.True, "bad format")

    let buffer = b.ReadBytes(16)
    let g = Guid buffer
    test <@ g = Guid("36fc1f5a-f829-41d9-b0f5-e0a935db09f4") @> //for native.pdb

  [<Test>]
  let ShouldGetEmbeddedPdbFromImage () =
    let target = sample8path
    maybeIgnore (fun () -> target |> File.Exists |> not)

    use image =
      AssemblyResolver.ReadAssembly target

    let (tokens, pdb) =
      AltCover.ProgramDatabase.getPdbFromImage image

    match pdb with // embedded pdb
    | Some name -> Assert.That(name, Is.EqualTo "Sample8.pdb", target + " -> " + name)

  [<Test>]
  let ShouldGetNoMdbFromMonoImage () =
    let path =
      Path.GetDirectoryName monoSample1path

    maybeIgnore (fun () -> path |> Directory.Exists |> not)

    let files =
      Directory.GetFiles(path)
      |> Seq.filter (fun x ->
        x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
        || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
      |> Seq.map (fun x -> (x, AssemblyResolver.ReadAssembly x))
      |> Seq.toList

    Assert.That(files, Is.Not.Empty)

    files
    |> Seq.iter (fun x ->
      let probe = (fst x) + ".mdb"

      let pdb =
        AltCover.ProgramDatabase.getPdbFromImage (snd x)

      match pdb with
      | (_, None) -> Assert.That(File.Exists probe, probe + " not found"))

  [<Test>]
  let ShouldGetPdbWithFallback () =
    Directory.GetFiles(dir)
    |> Seq.filter isAssemblyType
    |> Seq.filter (fun x -> (x + ".mdb") |> File.Exists |> not)
    |> Seq.filter (fun f ->
      f |> Path.GetFileNameWithoutExtension
      <> "testhost")
    |> Seq.filter (fun f -> f |> Path.GetFileName <> "AltCover.Tests.exe")
    |> Seq.iter (fun x ->
      use def = AssemblyResolver.ReadAssembly x

      let pdb =
        AltCover.ProgramDatabase.getPdbWithFallback (def)

      match pdb with
      | None ->
        Assert.That(
          File.Exists(Path.ChangeExtension(x, ".pdb")),
          Is.Not.True,
          "No .pdb for " + x
        )
      | Some name ->
        let probe = Path.ChangeExtension(x, ".pdb")
        let file = FileInfo(probe)
        let filename = file.Name.Replace("\\", "/")

        Assert.That(
          "/" + name.Replace("\\", "/"),
          Does.EndWith("/" + filename),
          x + " -> " + name
        ))

  [<Test>]
  let ShouldGetForeignPdbWithFallback () =
    let path =
      Path.Combine(SolutionDir(), "packages")

    let exclude = Path.Combine(path, "altcover")

    // Looking for the Mono.Options symbols
    let files =
      Directory.GetFiles(path, "*.pdb", SearchOption.AllDirectories)

    files
    |> Seq.filter (
      _.StartsWith(exclude, StringComparison.OrdinalIgnoreCase)
      >> not
    )
    |> Seq.filter (fun p -> Path.ChangeExtension(p, ".dll") |> File.Exists)
    |> Seq.iter (fun p ->
      let dll = Path.ChangeExtension(p, ".dll")

      try
        use def = AssemblyResolver.ReadAssembly dll

        let pdb =
          AltCover.ProgramDatabase.getPdbWithFallback (def)

        let normalized =
          Path.Combine(Path.GetDirectoryName p, Path.GetFileName p)

        match pdb with
        | Some name -> Assert.That(name, Is.EqualTo normalized)
      //| _ ->
      //  raise <| InvalidOperationException((sprintf "%A for %A" dll normalized))
      with :? BadImageFormatException ->
        ())

  [<Test>]
  let ShouldGetForeignPdbWithFallbackWhenNotColocated () =
    try
      let where =
        Assembly.GetExecutingAssembly().Location

      let path =
        Path.Combine(SolutionDir(), "packages")

      let exclude = Path.Combine(path, "altcover")

      // Looking for the Mono.Options symbols
      let files =
        Directory.GetFiles(path, "*.pdb", SearchOption.AllDirectories)

      files
      |> Seq.filter (
        _.StartsWith(exclude, StringComparison.OrdinalIgnoreCase)
        >> not
      )
      |> Seq.filter (fun p -> Path.ChangeExtension(p, ".dll") |> File.Exists)
      |> Seq.iter (fun p ->
        let dll0 = Path.ChangeExtension(p, ".dll")
        let unique = Guid.NewGuid().ToString()

        let output =
          Path.Combine(Path.GetDirectoryName(where), unique)

        Directory.CreateDirectory(output) |> ignore

        let dll =
          Path.Combine(output, Path.GetFileName dll0)

        System.IO.File.Copy(dll0, dll)
        ProgramDatabase.symbolFolders.Clear()

        p
        |> Path.GetDirectoryName
        |> ProgramDatabase.symbolFolders.Add

        try
          use def = AssemblyResolver.ReadAssembly dll

          let pdb =
            AltCover.ProgramDatabase.getPdbWithFallback (def)

          let normalized =
            Path.Combine(Path.GetDirectoryName p, Path.GetFileName p)

          match pdb with
          | Some name ->
            Assert.That(name, Is.EqualTo normalized)
            AltCover.ProgramDatabase.readSymbols def
            Assert.That(def.MainModule.HasSymbols, def.MainModule.FileName)
        //| _ ->
        //  raise <| InvalidOperationException((sprintf "%A for %A" dll normalized))
        with :? BadImageFormatException ->
          ())
    finally
      ProgramDatabase.symbolFolders.Clear()

  [<Test>]
  let ShouldGetMdbWithFallback () =
    let path =
      Path.GetDirectoryName monoSample1path

    maybeIgnore (fun () -> path |> Directory.Exists |> not)
    let files = Directory.GetFiles(path)

    files
    |> Seq.filter (fun x ->
      x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
      || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter (fun x ->
      use def = AssemblyResolver.ReadAssembly x

      let mdb =
        AltCover.ProgramDatabase.getPdbWithFallback (def)

      match mdb with
      //           | None -> Assert.That(File.Exists(x + ".mdb"), Is.Not.True, "No .mdb for " + x)
      | Some name ->
        let probe = x + ".mdb"
        let file = FileInfo(probe)
        let filename = file.Name.Replace("\\", "/")

        Assert.That(
          name.Replace("\\", "/") + ".mdb",
          Does.EndWith("/" + filename),
          x + " -> " + name
        ))

  [<Test>]
  let ShouldGetSymbolsFromPdb () =
    Directory.GetFiles(dir)
    |> Seq.filter isAssemblyType
    |> Seq.filter (fun f ->
      f |> Path.GetFileNameWithoutExtension
      <> "testhost")
    |> Seq.filter (fun f -> f |> Path.GetFileName <> "AltCover.Tests.exe")
    |> Seq.map AssemblyResolver.ReadAssembly
    |> Seq.filter (fun x ->
      not
      <| x.FullName.StartsWith("altcode.", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x ->
      not
      <| x.FullName.StartsWith("AltCover,", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x ->
      not
      <| x.FullName.StartsWith("AltCover.Recorder", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x ->
      x.FullName.EndsWith(
        "PublicKeyToken=c02b1a9f5b7cade8",
        StringComparison.OrdinalIgnoreCase
      ))
    |> Seq.iter (fun def ->
      AltCover.ProgramDatabase.readSymbols def
      Assert.That(def.MainModule.HasSymbols, def.MainModule.FileName))

  [<Test>]
  let ShouldGetSymbolsFromEmbeddedPdb () =
    let target = sample8path
    maybeIgnore (fun () -> target |> File.Exists |> not)

    use image =
      AssemblyResolver.ReadAssembly target

    AltCover.ProgramDatabase.readSymbols image
    Assert.That(image.MainModule.HasSymbols, image.MainModule.FileName)

  [<Test>]
  let ShouldNotGetSymbolsWhenNoPdb () =
    Directory.GetFiles(dir)
    |> Seq.filter (fun x ->
      x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
      || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x ->
      Path.GetFileName(x).StartsWith("BlackFox", StringComparison.OrdinalIgnoreCase))
    |> Seq.map AssemblyResolver.ReadAssembly
    |> Seq.filter (fun x ->
      not
      <| x.FullName.EndsWith(
        "PublicKeyToken=c02b1a9f5b7cade8",
        StringComparison.OrdinalIgnoreCase
      ))
    |> Seq.iter (fun def ->
      AltCover.ProgramDatabase.readSymbols def
      Assert.That(not def.MainModule.HasSymbols, def.MainModule.FileName))

  [<Test>]
  let ShouldGetSymbolsFromMdb () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let pdb =
      Path.ChangeExtension(where, ".pdb")

    maybeIgnore (fun () -> monoSample1path |> File.Exists |> not)

    let path =
      Path.GetDirectoryName monoSample1path

    let files = Directory.GetFiles(path)

    files
    |> Seq.filter (fun x ->
      x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
      || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.iter (fun x ->
      use def = AssemblyResolver.ReadAssembly x

      AltCover.ProgramDatabase.readSymbols def
      Assert.That(def.MainModule.HasSymbols, def.MainModule.FileName))