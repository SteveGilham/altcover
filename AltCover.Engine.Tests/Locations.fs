namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection

open AltCover

#nowarn "25"

[<AutoOpen>]
module Locations =

#if !NET472
  let dir =
    Path.Combine(SolutionDir(), "_Binaries/AltCover.Engine.Tests/Debug+AnyCPU/net10.0")
#else
  let dir =
    Path.Combine(SolutionDir(), "_Binaries/AltCover.Engine.Tests/Debug+AnyCPU/net472")
#endif

#if !NET472
  let sample2path =
    Path.Combine(SolutionDir(), "_Binaries/Sample2/Debug+AnyCPU/net10.0/Sample2.dll")

  let sample4path =
    Path.Combine(SolutionDir(), "_Binaries/Sample4/Debug+AnyCPU/net10.0/Sample4.dll")

  let sample8path =
    Path.Combine(SolutionDir(), "_Binaries/Sample8/Debug+AnyCPU/net10.0/Sample8.dll")

  let sample24path =
    Path.Combine(SolutionDir(), "_Binaries/Sample24/Debug+AnyCPU/net10.0/Sample24.dll")

  let sample27path =
    Path.Combine(SolutionDir(), "_Binaries/Sample27/Debug+AnyCPU/net10.0/Sample27.dll")

  let sample30path =
    Path.Combine(SolutionDir(), "_Binaries/Sample30/Debug+AnyCPU/net10.0/Sample30.dll")

  let sample32path =
    Path.Combine(SolutionDir(), "_Binaries/Sample32/Debug+AnyCPU/net10.0/Sample32.dll")

#else
  let sample2path =
    Path.Combine(SolutionDir(), "_Binaries/Sample2/Debug+AnyCPU/net472/Sample2.dll")

  let sample4path =
    Path.Combine(SolutionDir(), "_Binaries/Sample4/Debug+AnyCPU/net472/Sample4.dll")

  let sample8path =
    Path.Combine(SolutionDir(), "_Binaries/Sample8/Debug+AnyCPU/net20/Sample8.exe")

  let sample24path =
    Path.Combine(SolutionDir(), "_Binaries/Sample24/Debug+AnyCPU/net472/Sample24.dll")

  let sample27path =
    Path.Combine(SolutionDir(), "_Binaries/Sample27/Debug+AnyCPU/net472/Sample27.dll")

  let sample30path =
    Path.Combine(SolutionDir(), "_Binaries/Sample30/Debug+AnyCPU/net472/Sample30.dll")

  let sample32path =
    Path.Combine(SolutionDir(), "_Binaries/Sample32/Debug+AnyCPU/net472/Sample32.exe")
#endif

  let monoSample1path =
    Path.Combine(SolutionDir(), "_Mono/Sample1/Sample1.exe")

  let sample1path =
    Path.Combine(SolutionDir(), "_Binaries/Sample1/Debug+AnyCPU/net20/Sample1.exe")