﻿namespace Tests

open System
open System.IO
open System.Reflection

#if NETCOREAPP3_0
open AltCover
open Expecto
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
#endif

type Assert = NUnit.Framework.Assert
type Does = NUnit.Framework.Does
type Is = NUnit.Framework.Is

#if NETCOREAPP3_0
[<System.AttributeUsage(AttributeTargets.Method)>]
type TestAttribute() = class
    inherit Attribute()
end
#else
type TestAttribute = NUnit.Framework.TestAttribute
#endif

[<AutoOpen>]
module TestCommon =
  let SolutionDir() =
    AltCover.SolutionRoot.location

  let maybeIOException f =
    try
      f()
    with 
    | :? System.UnauthorizedAccessException
    | :? IOException -> ()

  let maybeDeleteFile f =
    if File.Exists f
    then File.Delete f

  let maybeReraise f g =
    try
      f()
    with _ ->
      g()
      reraise()

  let maybe a b c =
    if a
    then b
    else c

  let test x = Swensen.Unquote.Assertions.test x

  let test' x message =
    try
      test x
    with
    | fail -> Swensen.Unquote.AssertionFailedException(message + Environment.NewLine + fail.Message, fail) |> raise

module TestCommonTests =
    [<Test>]
    let ExerciseItAll() =
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(Path.Combine(where, Guid.NewGuid().ToString()), "nonesuch.txt")
      let realDir = Path.Combine(where, Guid.NewGuid().ToString())
      realDir
      |> Directory.CreateDirectory
      |> ignore
      let another = Path.Combine(realDir, "another.txt")
      do use _dummy = File.Create another
         ()
      maybeDeleteFile another
      maybeDeleteFile unique
      maybeIOException (fun () ->
        maybeReraise (fun () -> File.Delete unique) ignore
      )
      maybeIOException (fun () ->
        maybeReraise (fun () -> IOException() |> raise) ignore
      )
      maybeIOException (fun () -> System.UnauthorizedAccessException() |> raise )

      test <@ (maybe true 1 2) = 1 @>
      test <@ (maybe false 1 2) = 2 @>
      test <@ SolutionDir() |> String.IsNullOrEmpty |> not @>
      test <@ SolutionDir() = AltCover.SolutionRoot.location @>
      test <@ where.StartsWith(AltCover.SolutionRoot.location, StringComparison.Ordinal) @>

    [<Test>]
    let SelfTest() =
      test <@ true @>
#if NETCOREAPP3_0
      Assert.Throws<Expecto.AssertException>(
#else
#if (ValidateGendarmeEmulation || GUI)
      Assert.Throws<NUnit.Framework.AssertionException>(
#else
      Assert.Throws<Xunit.Sdk.TrueException>(
#endif
#endif
        fun  () -> test <@ false @> ) |> ignore
      Assert.Throws<Swensen.Unquote.AssertionFailedException>(
        fun () -> test' <@ false @> "junk") |> ignore

#if NETCOREAPP3_0
module ExpectoTestCommon =
  let sync = System.Object()

  let consistencyCheck (regular:((unit -> unit)*string) list) expected =
    let here = System.Reflection.Assembly.GetExecutingAssembly().Location
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly(here)

    let testMethods = def.MainModule.GetTypes()
                      |> Seq.collect (fun t -> t.Methods)
                      |> Seq.filter (fun m -> m.CustomAttributes.IsNotNull)
                      |> Seq.filter (fun m -> m.CustomAttributes |> Seq.exists (fun a -> a.AttributeType.Name = "TestAttribute"))
                      |> Seq.map (fun m -> m.DeclaringType.FullName + "::" + m.Name)

    let lookup = def.MainModule.GetAllTypes()
                 |> Seq.filter (fun t -> t.Methods |> Seq.exists(fun m -> m.Name = "Invoke"))
                 |> Seq.map (fun t -> (t.FullName.Replace("/","+"), t.Methods |> Seq.find(fun m -> m.Name = "Invoke")))
                 |> Map.ofSeq

    let calls = regular
                |> List.map (fst
                            >> (fun f -> f.GetType().FullName.Replace("/","+"))
                            >> (fun f -> Map.find f lookup)
                            >> (fun f -> f.Body.Instructions
    // Where the test assembly is itself instrumented
    // we have to allow for calls to AltCover.Recorder.Instance::Visit
    // or the coverlet equivalent
    // having been injected into the local function reference

                                         |> Seq.find (fun i -> i.OpCode = OpCodes.Call
                                                               && i.Operand.GetType().Name.Equals("MethodDefinition", StringComparison.Ordinal))
                            )
                            >> (fun i -> let m = (i.Operand :?> MethodDefinition)
                                         m.DeclaringType.FullName + "::" + m.Name))
                |> Set.ofList

    let omitted = testMethods
                  |> Seq.filter (fun t -> (Set.contains t calls) |> not)
                  |> Seq.toList

    // cover all but the special cases
    TestCommon.test <@ omitted = expected @>

  let makeTests name (check:unit -> unit) (regular:((unit -> unit)*string) list) specials pretest =
    testList name
    <| (((check, "ConsistencyCheck") :: regular
        |> List.map (fun (f,name) -> testCase name (fun () -> lock sync (fun () ->
                                                              pretest()
                                                              f())))) @ specials)
#endif