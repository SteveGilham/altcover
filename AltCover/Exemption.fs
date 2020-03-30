namespace AltCover
open System.Diagnostics.CodeAnalysis

[<SuppressMessage("Gendarme.Rules.Design", "EnumsShouldUseInt32Rule",
  Justification="Data size is important")>]
[<SuppressMessage("Gendarme.Rules.Design", "EnumsShouldDefineAZeroValueRule",
  Justification="What is 'None', then?")>]
type internal Exemption =
  | NonCode = 2y // Grey (comments etc.) or
  | Visited = 1y // White on Green (covered) or
  | None = 0y // Red (uncovered) on White
  | Declared = -1y // Dark Orange on White (other user-defined exemptions TBD)
  | Automatic = -2y // Grey on Yellow (other compiler generated code)
  | StaticAnalysis = -3y // White Smoke on Grey (e.g. auto-properties)
  | Excluded = -4y // Deep Sky Blue on white (filtered)