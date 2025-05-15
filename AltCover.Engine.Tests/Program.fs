namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular =
    [ Tests.TestCommonTests.TestMultiple, "Tests.TestCommonTests.TestMultiple"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommonTests.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
      // Augment.fs
      Tests.Augment.ZeroIsNotVisited, "Tests.ZeroIsNotVisited"
      Tests.Augment.PositiveIsVisited, "Tests.PositiveIsVisited"
      Tests.Augment.NegativesSpray, "Tests.NegativesSpray"
      // BaseTests
      Tests.BaseTests.ExerciseBoth, "BaseTests.ExerciseBoth"
      Tests.BaseTests.ExerciseTime, "BaseTests.ExerciseTime"
      Tests.BaseTests.ExerciseCall, "BaseTests.ExerciseCall"
      Tests.BaseTests.ExerciseNull, "BaseTests.ExerciseNull"
      Tests.BaseTests.ExercisePointVisit, "BaseTests.ExercisePointVisit"
      // ProgramDatabase.fs
      Tests.ProgramDatabase.ShouldGetPdbFromImage, "Tests.ShouldGetPdbFromImage"
      Tests.ProgramDatabase.ShouldGetEmbeddedPdbFromImage,
      "Tests.ShouldGetEmbeddedPdbFromImage"
      Tests.ProgramDatabase.ShouldGetNoMdbFromMonoImage,
      "Tests.ShouldGetNoMdbFromMonoImage"
      Tests.ProgramDatabase.ShouldGetGUIDfromNativePdb, "Tests.ShouldGetGUIDfromNativePdb"
      Tests.ProgramDatabase.ShouldGetPdbWithFallback, "Tests.ShouldGetPdbWithFallback"
      Tests.ProgramDatabase.ShouldGetForeignPdbWithFallback,
      "Tests.ShouldGetForeignPdbWithFallback"
      Tests.ProgramDatabase.ShouldGetForeignPdbWithFallbackWhenNotColocated,
      "Tests.ShouldGetForeignPdbWithFallbackWhenNotColocated"
      Tests.ProgramDatabase.ShouldGetMdbWithFallback, "Tests.ShouldGetMdbWithFallback"
      Tests.ProgramDatabase.ShouldGetSymbolsFromPdb, "Tests.ShouldGetSymbolsFromPdb"
      Tests.ProgramDatabase.ShouldGetSymbolsFromEmbeddedPdb,
      "Tests.ShouldGetSymbolsFromEmbeddedPdb"
      Tests.ProgramDatabase.ShouldNotGetSymbolsWhenNoPdb,
      "Tests.ShouldNotGetSymbolsWhenNoPdb"
      Tests.ProgramDatabase.ShouldGetSymbolsFromMdb, "Tests.ShouldGetSymbolsFromMdb"
      ]

  let specials = []

  let consistencyCheck () =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCoverEngineTests"
      consistencyCheck
      regular
      specials
      ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =

    runTestsWithCLIArgs Seq.empty<CLIArguments> argv ExpectoMain.tests
#endif