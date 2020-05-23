# AltCover.exe/.dll

A common API for in-process coverage instrumentation and collection

* [C# style documentation](AltCover-apidoc)
* F# style documentation
  * [`module Abstract`](Abstract-fsapidoc) -- This holds the weakly ("stringly") typed equivalent of the command line options in a C# friendly manner as interfaces with the values expressed as read-only properties.
  * [`module AltCover`](AltCover-fsapidoc) -- This represents the  command line options and its validation.
  * [`module Command`](Command-fsapidoc) -- This represents the various operations available.
  * [`module Primitive`](Primitive-fsapidoc) -- This holds the weakly ("stringly") typed equivalent of the command line options.
  * [`module Tasks`](Tasks-fsapidoc) -- Published MSBuild tasks.
  * [`module TypeSafe`](TypeSafe-fsapidoc) -- This holds the strongly-typed equivalent of the command line options.
  * [WhatIf extension modules](WhatIfExtension-fsapidoc) -- These provide C#-compatible extension methods to perform a WhatIf style command line validation.