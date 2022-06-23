namespace AltCover

open System
open System.IO

[<AutoOpen>]
module Abstraction =

  // abstract away selection based on OS dependencies and other environment constants
  let Maybe predicate (result: 'TResult) (alternate: 'TResult) =
    if predicate then result else alternate

  let DoPathOperation (f: unit -> 'a) (handle: exn -> 'a) =
    try
      f ()
    with
    | x when
      (x :? ArgumentException)
      || (x :? NotSupportedException)
      || (x :? IOException)
      || (x :? System.Security.SecurityException)
      || (x :? UnauthorizedAccessException)
      ->
      handle (x)