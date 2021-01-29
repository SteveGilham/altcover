namespace AltCover

open System.Collections.Generic

module NativeJson =
  // Coverlet compatible -- src/coverlet.core/CoverageResult.cs
  // also round-trippable
  type BranchInfo =
      {
          Line:int
          Offset:int
          EndOffset:int
          Path:int
          Ordinal:uint
          Hits:int
      }

  type Lines = SortedDictionary<int, int>

  type Branches = List<BranchInfo>

  type Method =
    {
      Lines:Lines
      Branches:Branches
    }

  type Methods = Dictionary<string, Method>
  type Classes = Dictionary<string, Methods>
  type Documents = Dictionary<string, Classes>
  type Modules = Dictionary<string, Documents>
  type CoverageResult =
    {
      Modules:Modules
    }