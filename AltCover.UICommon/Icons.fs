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
    makeIcon "ReportDated_16x"

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
    makeIcon "MethodDated_16x"

  member self.MethodMissingSource =
    makeIcon "MethodWarning"

  member self.MethodNoSource =
    makeIcon "SourcelessMethod_16x"
  // actually 16x16
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
    makeIcon "TextFileDated_16x"

  member self.SourceLink =
    makeIcon "TextFileWeb"

  member self.NoSource =
    makeIcon "TextFileMissing_16x"

  member self.MRUInactive =
    makeIcon "ExpandDown_lightGrey"

  member self.RefreshActive =
    makeIcon "Refresh"

  member self.Refresh =
    makeIcon "Refresh_greyThin"
  // Use https://www.iloveimg.com/resize-image/resize-svg and https://svgtopng.com/
  member self.Info =
    makeIcon "StatusInformation_32x" // 16x Unchanged @ 2019

  member self.Warn = makeIcon "StatusWarning_32x" // 16x Unchanged @ 2019

  member self.Error =
    makeIcon "StatusCriticalError_32x"

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