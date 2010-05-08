namespace Tests

open System
open System.IO
open System.Reflection
 
open NUnit.Framework

[<TestFixture>]
type ProgramDatabaseTests() = class
  [<Test>]
  member self.CheckPdbMapping() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location;
    let dir = match AltCover.ProgramDatabase.PdbPath(where) with
              | None -> "\\.."
              | _ -> String.Empty
  
    let files = Directory.GetFiles(Path.GetDirectoryName(where) + dir)
    files 
    |> Seq.filter (fun x -> x.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) 
                            || x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
    |> Seq.filter (fun x -> not (x.EndsWith("nunit.framework.dll", StringComparison.OrdinalIgnoreCase))
                         && not (x.EndsWith("Microsoft.Cci.dll", StringComparison.OrdinalIgnoreCase)))
    |> Seq.iter( fun x ->
      let pdb = AltCover.ProgramDatabase.PdbPath(x)
      match pdb with
      | None -> Assert.Fail("No .pdb for " + x)
      | Some name -> 
         let probe = Path.ChangeExtension(x, ".pdb")
         let file = new FileInfo(probe)
         let filename = file.Name
         Assert.That(name, Does.EndWith("\\" + filename), x + " -> " + name)
    )

  [<Test; Ignore("awaits refactoring")>]
  member self.CheckPdbLoading() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location;
    let dir = match AltCover.ProgramDatabase.PdbPath(where) with
              | None -> "\\.."
              | _ -> String.Empty
    let path = Path.Combine(Path.GetDirectoryName(where) + dir, "Sample1.exe")
    let symbols = AltCover.ProgramDatabase.LoadAssembly(path)
    Assert.That(symbols.Symbols, Is.Not.Empty, "should be some symbols")
    Assert.That(symbols.Symbols.Keys.Count, Is.EqualTo(1), "the number we expect from NCoverExplorer")
    
end    
