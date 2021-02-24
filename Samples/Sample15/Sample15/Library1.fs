namespace Sample15

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization

 [<ExcludeFromCodeCoverage; NoComparison>]
 type TeamCityFormat =
   | Default
   | R
   | B
   | RPlus
   | BPlus

module Class1 =
  let TCtotal = "##teamcity[buildStatisticValue key='CodeCoverageAbs{0}Total' value='{1}']"

  let WriteTC template what value =
    let line = String.Format(CultureInfo.InvariantCulture,
                              template, what, value)
    printfn "%s" line

  let OpenCoverSummary summaryFormat nb =
      let tag = match summaryFormat with
                | R
                | RPlus -> "R"
                | _ -> "B"
      WriteTC TCtotal tag nb