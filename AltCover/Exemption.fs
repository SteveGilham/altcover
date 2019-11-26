namespace AltCover

type internal Exemption =
  | None = 0             // Red (uncovered) or Green (covered) on White
  | Declared = -1        // Orange on White (other user-defined exemptions TBD)
  | Automatic = -2       // Grey on Yellow (other compiler generated code)
  | StaticAnalysis = -3  // Grey on White Smoke (e.g. auto-properties)
  | Excluded = -4        // Sky Blue on white (filtered)