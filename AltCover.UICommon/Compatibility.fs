namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Net
open System.Security.Cryptography

[<AutoOpen>]
module Compatibility =
#if !GUI
  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "RemoveDependenceOnObsoleteCodeRule",
                    Justification = "netstandard 2.0/net472")>]
  let internal sha1Hash () : SHA1  =
    new SHA1CryptoServiceProvider() :> SHA1
#endif

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "RemoveDependenceOnObsoleteCodeRule",
                    Justification = "netstandard 2.0/net472")>]
  let internal createHttp (path:Uri) =
    WebRequest.CreateHttp(path) :> WebRequest