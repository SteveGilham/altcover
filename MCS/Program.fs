open System.Reflection

[<EntryPoint>]
let main argv =
  let settings = Mono.CSharp.CompilerSettings()
  let assembly = settings.GetType().Assembly

  let driver =
    assembly.GetTypes()
    |> Seq.filter (fun t -> t.FullName = "Mono.CSharp.Driver")
    |> Seq.head

  let main = driver.GetMethod("Main", BindingFlags.Public ||| BindingFlags.Static)
  main.Invoke(null, [| argv |]) :?> int