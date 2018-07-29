namespace AltCover.Commands

#if MONO
module OpenCover =
    let hello name =
        printfn "Hello %s" name
#else

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Management.Automation
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

open AltCover.PowerShell
#endif