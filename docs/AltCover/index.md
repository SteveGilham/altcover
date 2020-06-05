# AltCover.exe/.dll

A common API for in-process coverage instrumentation and collection

* [C# style documentation](AltCover-apidoc)
* F# style documentation - this document

## Composing the AltCover command line

### Basic elements

Start with one of these

  * [`module Abstract`](Abstract-fsapidoc) -- This holds the weakly ("stringly") typed equivalent of the command line options in a C# friendly manner as interfaces with the values expressed as read-only properties.
  * [`module Primitive`](Primitive-fsapidoc) -- This holds the weakly ("stringly") typed equivalent of the command line options.
  * [`module TypeSafe`](TypeSafe-fsapidoc) -- This holds the strongly-typed equivalent of the command line options.

### The common expression of the command line elements

Check with these
  * [WhatIf extension modules](WhatIfExtension-fsapidoc) -- These provide C#-compatible extension methods to perform a WhatIf style command line validation.  

Make one of these

  * [`module AltCover`](AltCover-fsapidoc) -- This represents the  command line options and its validation.

### Running the command

  * [`module Command`](Command-fsapidoc) -- This represents the various operations available.
  * [`module Tasks`](Tasks-fsapidoc) -- Published MSBuild tasks.
