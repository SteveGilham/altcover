namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.Reflection

[<AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Smells",
                  "AvoidSpeculativeGeneralityRule",
                  Justification = "It's not speculative")>]
[<SuppressMessage("Gendarme.Rules.Maintainability",
                  "VariableNamesShouldNotMatchFieldNamesRule",
                  Justification = "Compiler generated")>]
type Icons<'TIcon>(toIcon: System.IO.Stream -> 'TIcon) =
  let makeIcon name =
    lazy
      //try
      (toIcon (
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream("AltCover.UICommon." + name + ".png")
      ))
  //with
  //| x -> System.InvalidOperationException(name, x) |> raise

  member self.Report = makeIcon "Report"

  member self.ReportDated =
    makeIcon "ReportStale"

  member self.ReportWarning =
    makeIcon "ReportWarning"

  member self.Assembly = makeIcon "Assembly"
  member self.Event = makeIcon "Event"

  member self.Namespace =
    makeIcon "Namespace"

  member self.Module = makeIcon "Module"
  member self.Effect = makeIcon "Effect"
  member self.Class = makeIcon "Class"
  member self.Property = makeIcon "Property"
  member self.Method = makeIcon "Method"

  member self.MethodDated =
    makeIcon "MethodStale"

  member self.MethodMissingSource =
    makeIcon "MethodWarning"

  member self.MethodNoSource =
    makeIcon "MissingMethod"

  member self.Branched =
    makeIcon "BranchFork_grn"

  member self.Branch =
    makeIcon "BranchFork_ylw"

  member self.RedBranch =
    makeIcon "BranchFork_red"

  member self.Blank = makeIcon "Blank"

  member self.TreeExpand =
    makeIcon "ExpandRight"

  member self.TreeCollapse =
    makeIcon "ExpandDown"

  member self.MRU = makeIcon "ExpandDown"
  member self.Source = makeIcon "TextFile"

  member self.SourceDated =
    makeIcon "TextFileStale"

  member self.SourceLink =
    makeIcon "TextFileWeb"

  member self.NoSource =
    makeIcon "MissingFile"

  member self.MRUInactive =
    makeIcon "ExpandDown_lightGrey"

  member self.RefreshActive =
    makeIcon "Refresh"

  member self.Refresh =
    makeIcon "Refresh_greyThin"
  // Use https://www.iloveimg.com/resize-image/resize-svg and https://svgtopng.com/
  member self.Info =
    makeIcon "StatusInformation_32"

  member self.Warn = makeIcon "StatusWarning_32"

  member self.Error =
    makeIcon "StatusError_32"

  member self.Open = makeIcon "OpenFile"
  member self.Font = makeIcon "Font"

  member self.ShowAbout =
    makeIcon "VisualStudioToolsAboutBox"

  member self.Exit = makeIcon "Exit"
  member self.Logo = makeIcon "logo"

  member self.VIcon =
    let makeIco name =
      toIcon (
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream("AltCover.UICommon." + name + ".ico")
      )

    makeIco "VIcon"