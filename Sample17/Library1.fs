namespace Sample17

type Close =
  | Neither
  | Pause
  | Resume

type Message =
  | AsyncItem of int
  | Item of int
  | Finish of Close * int

module Carrier =
  let Function msg =
      match msg with
      | AsyncItem i -> 3 * i
      | Item i -> 5 * i
      | Finish (Pause, i) -> 7 * i
      | Finish (Resume, i) -> 11 * i
      | Finish _ -> 0