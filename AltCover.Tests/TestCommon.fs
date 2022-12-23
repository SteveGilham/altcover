namespace Tests

open System
open System.IO
open System.Reflection

open AltCover

#if !NET472
open Expecto
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
#endif

type Assert = NUnit.Framework.Assert
type Does = NUnit.Framework.Does
type Is = NUnit.Framework.Is

#if NET472
type TestAttribute = NUnit.Framework.TestAttribute
#else
[<System.AttributeUsage(AttributeTargets.Method)>]
type TestAttribute() =
  class
    inherit Attribute()
  end

#endif

[<AutoOpen>]
module TestCommon =
  let SolutionDir () = AltCover.SolutionRoot.location

  let maybeIgnore f =
    if f () then
      Assert.Ignore()

  let maybeIOException f =
    try
      f ()
    with
    | :? System.UnauthorizedAccessException
    | :? IOException -> ()

  let maybeDeleteFile f =
    if File.Exists f then
      File.Delete f

  let maybeReraise f g =
    try
      f ()
    with _ ->
      g ()
      reraise ()

  let private test0 x = Swensen.Unquote.Assertions.test x

  let test x =
    try
      test0 x
    with fail ->
      NUnit.Framework.Assert.Fail(fail.Message)

  let test' x message =
    try
      test0 x
    with fail ->
      NUnit.Framework.Assert.Fail(message + Environment.NewLine + fail.Message)

  let testWithFallback<'a>
    x
    (value: 'a)
    (constraining: NUnit.Framework.Constraints.IResolveConstraint)
    =
    try
      test0 x
    with fail ->
      Assert.That<'a>(value, constraining, fail.Message.Trim())

module TestCommonTests =
  [<Test>]
  let TestIgnoredTests () =
    let allOK = (fun () -> true)

    Assert.Throws<NUnit.Framework.IgnoreException>(fun () -> maybeIgnore allOK)
    |> ignore
#if !NET472
    maybeIgnore allOK
#endif

  [<Test>]
  let ExerciseItAll () =
#if !NET472
    let attr = TestAttribute()
    test <@ attr.IsNotNull @>
#endif
    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(Path.Combine(where, Guid.NewGuid().ToString()), "nonesuch.txt")

    let realDir =
      Path.Combine(where, Guid.NewGuid().ToString())

    realDir |> Directory.CreateDirectory |> ignore

    let another =
      Path.Combine(realDir, "another.txt")

    do
      use _dummy = File.Create another
      ()

    maybeDeleteFile another
    maybeDeleteFile unique
    maybeIOException (fun () -> maybeReraise (fun () -> File.Delete unique) ignore)
    maybeIOException (fun () -> maybeReraise (fun () -> IOException() |> raise) ignore)
    maybeIOException (fun () -> System.UnauthorizedAccessException() |> raise)

    test <@ (Maybe true 1 2) = 1 @>
    test <@ (Maybe false 1 2) = 2 @>
    test <@ SolutionDir() |> String.IsNullOrEmpty |> not @>
    test <@ SolutionDir() = AltCover.SolutionRoot.location @>
    test <@ where.StartsWith(AltCover.SolutionRoot.location, StringComparison.Ordinal) @>

  [<Test>]
  let SelfTest () =
    test <@ true @>

    Assert.Throws<NUnit.Framework.AssertionException>(fun () -> test <@ false @>)
    |> ignore

    Assert.Throws<NUnit.Framework.AssertionException>(fun () -> test' <@ false @> "junk")
    |> ignore

  [<Test>]
  let TestMultiple () =
    let exp1 = 4
    let exp2 = "no"

    let fallback =
      Assert
        .Throws<NUnit.Framework.AssertionException>(fun () ->
          testWithFallback <@ "yes" = exp2 @> "yes" (Is.EqualTo exp2))
        .Message
        .Replace(
          """Expected: True
Actual:   False
""",
          String.Empty
        )
        .Replace(
          """

""",
          """
"""
        )

    // printfn "*********************************************"

    // printfn
    //   "%s"
    //   (System
    //     .Reflection
    //     .Assembly
    //     .GetExecutingAssembly()
    //     .FullName)

    // printfn "%s" fallback
    // printfn "*********************************************"

    Assert.That(
      fallback,
      Does.EndWith
        """  "yes" = exp2
"yes" = "no"
false
  Expected string length 2 but was 3. Strings differ at index 0.
  Expected: "no"
  But was:  "yes"
  -----------^
"""
    )

    let m =
      Assert.Throws<NUnit.Framework.MultipleAssertException>(fun () ->
        Assert.Multiple(fun () ->
          Assert.That(3, Is.EqualTo exp1)
          test <@ 3 = exp1 @>
          Assert.That("yes", Is.EqualTo exp2)
          test <@ "yes" = exp2 @>))
        .Message

    //printfn "%s" m

    let m2 =
      m.Replace(
        """Expected: True
Actual:   False
""",
        String.Empty
      )

    //printfn "%s" m2

    Assert.That(
      m2,
      Is.EqualTo
        """Multiple failures or warnings in test:
  1)   Expected: 4
  But was:  3

  2) 

3 = exp1
3 = 4
false

  3)   Expected string length 2 but was 3. Strings differ at index 0.
  Expected: "no"
  But was:  "yes"
  -----------^

  4) 

"yes" = exp2
"yes" = "no"
false

"""
    )

#if !NET472
module ExpectoTestCommon =
  let sync = System.Object()

  let consistencyCheck (regular: ((unit -> unit) * string) list) expected =
    let here =
      System
        .Reflection
        .Assembly
        .GetExecutingAssembly()
        .Location

    let def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly(here)

    let testMethods =
      def.MainModule.GetTypes()
      |> Seq.collect (fun t -> t.Methods)
      |> Seq.filter (fun m -> m.CustomAttributes.IsNotNull)
      |> Seq.filter (fun m ->
        m.CustomAttributes
        |> Seq.exists (fun a -> a.AttributeType.Name = "TestAttribute"))
      |> Seq.map (fun m -> m.DeclaringType.FullName + "::" + m.Name)

    let lookup =
      def.MainModule.GetAllTypes()
      |> Seq.filter (fun t ->
        t.Methods
        |> Seq.exists (fun m -> m.Name = "Invoke"))
      |> Seq.map (fun t ->
        (t.FullName.Replace("/", "+"), t.Methods |> Seq.find (fun m -> m.Name = "Invoke")))
      |> Map.ofSeq

    let calls =
      regular
      |> List.map (
        fst
        >> (fun f -> f.GetType().FullName.Replace("/", "+"))
        >> (fun f -> Map.find f lookup)
        >> (fun f ->
          f.Body.Instructions
          // Where the test assembly is itself instrumented
          // we have to allow for calls to AltCover.Recorder.Instance::Visit
          // or the coverlet equivalent
          // having been injected into the local function reference

          |> Seq.find (fun i ->
            i.OpCode = OpCodes.Call
            && i
              .Operand
              .GetType()
              .Name.Equals("MethodDefinition", StringComparison.Ordinal)))
        >> (fun i ->
          let m = (i.Operand :?> MethodDefinition)
          m.DeclaringType.FullName + "::" + m.Name)
      )
      |> Set.ofList

    let omitted =
      testMethods
      |> Seq.filter (fun t -> (Set.contains t calls) |> not)
      |> Seq.toList

    // cover all but the special cases
    TestCommon.test <@ omitted = expected @>

  let makeTests
    name
    (check: unit -> unit)
    (regular: ((unit -> unit) * string) list)
    specials
    pretest
    =
    testList name
    <| (((check, "TestCommonTests.ConsistencyCheck")
         :: regular
         |> List.map (fun (f, name) ->
           testCase name (fun () ->
             lock sync (fun () ->
               pretest ()

               try
                 f ()
               with :? NUnit.Framework.IgnoreException ->
                 ()))))
        @ specials)
#endif