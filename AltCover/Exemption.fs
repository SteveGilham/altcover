namespace AltCover

type Exemption =
  | NonCode = 2y // Grey (comments etc.) or
  | Visited = 1y // Green (covered) or
  | None = 0y // Red (uncovered) on White
  | Declared = -1y // Orange on White (other user-defined exemptions TBD)
  | Automatic = -2y // Grey on Yellow (other compiler generated code)
  | StaticAnalysis = -3y // Grey on White Smoke (e.g. auto-properties)
  | Excluded = -4y // Sky Blue on white (filtered)