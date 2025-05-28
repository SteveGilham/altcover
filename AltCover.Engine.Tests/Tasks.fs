namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover
open Microsoft.FSharp.Reflection
open Mono.Options
open Mono.Cecil.Cil

#nowarn "25"

module Tasks =
  type Logging() =
    member val Info: Action<String> = null with get, set
    member val Warn: Action<String> = null with get, set
    member val Failure: Action<String> = null with get, set
    member val Echo: Action<String> = null with get, set
    member val Verbose: Action<String> = null with get, set

    interface Abstract.ILoggingOptions with
      member self.Info = self.Info
      member self.Warn = self.Warn
      member self.Failure = self.Failure
      member self.Echo = self.Echo
      member self.Verbose = self.Verbose

  [<Test>]
  let LoggingCanBeExercised () =
    Main.init ()
    Assert.That(AltCover.LoggingOptions.ActionAdapter null, Is.Not.Null)
    (AltCover.LoggingOptions.ActionAdapter null) "23"

    let ignoreAction =
      new Action<String>(ignore)

    ignoreAction.Invoke("ignoreAction")
    Assert.That(AltCover.LoggingOptions.ActionAdapter(ignoreAction), Is.Not.Null)
    let mutable x = String.Empty
    let f = (fun s -> x <- s)
    (AltCover.LoggingOptions.ActionAdapter(new Action<String>(f))) "42"
    Assert.That(x, Is.EqualTo "42")
    AltCover.LoggingOptions.Create().Info "32"
    AltCover.LoggingOptions.Create().Warn "32"
    AltCover.LoggingOptions.Create().Error "32"
    AltCover.LoggingOptions.Create().Echo "32"
    AltCover.LoggingOptions.Create().Verbose "32"

    let o = Logging()
    o.Info <- null
    o.Warn <- null
    o.Failure <- null
    o.Echo <- null
    o.Verbose <- null

    Assert.That(o.Info, Is.Null)
    Assert.That(o.Warn, Is.Null)
    Assert.That(o.Failure, Is.Null)
    Assert.That(o.Echo, Is.Null)
    Assert.That(o.Verbose, Is.Null)

    let p = AltCover.LoggingOptions.Translate o
    Assert.That(p.Warn, Is.Not.Null)
    let p2 = AltCover.LoggingOptions.Abstract o
    p2.Info "32"
    p2.Warn "32"
    p2.Error "32"
    p2.Echo "32"
    p2.Verbose "32"