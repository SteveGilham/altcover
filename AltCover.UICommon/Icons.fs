namespace AltCover.Visualizer

open System.Diagnostics.CodeAnalysis
open System.Reflection

[<AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Smells", "AvoidSpeculativeGeneralityRule",
  Justification = "It's not speculative")>]
[<SuppressMessage("Gendarme.Rules.Maintainability", "VariableNamesShouldNotMatchFieldNamesRule",
  Justification = "Compiler generated")>]
type Icons<'TIcon> (toIcon: System.IO.Stream -> 'TIcon) =
  let makeIcon name =
    lazy
      (toIcon(Assembly.GetExecutingAssembly()
                          .GetManifestResourceStream("AltCover.UICommon." + name + ".png")))

  member self.Xml = makeIcon "XMLFile_16x"
  member self.Assembly = makeIcon "Assembly_6212"
  member self.Event = makeIcon "Event_16x"
  member self.Namespace = makeIcon "Namespace_16x"
  member self.Module = makeIcon "Module_16x"
  member self.Effect = makeIcon "Effects_16x"
  member self.Class = makeIcon "class_16xLG"
  member self.Property = makeIcon "Property_16x"
  member self.Method = makeIcon "method_16xLG"
  member self.Branched = makeIcon "Branch_12x_16x_grn"
  member self.Branch = makeIcon "Branch_12x_16x_ylw"
  member self.RedBranch = makeIcon "Branch_12x_16x_red"
  member self.Blank = makeIcon "Blank_12x_16x"
  member self.TreeExpand = makeIcon "ExpandChevronRight_16x"
  member self.TreeCollapse = makeIcon "CollapseChevronDown_16x"
  member self.MRU = makeIcon "ExpandChevronDown_16x"
  member self.MRUInactive = makeIcon "ExpandChevronDown_lightGray_16x"
  member self.RefreshActive = makeIcon "Refresh_16x"
  member self.Refresh = makeIcon "Refresh_greyThin_16x"
  member self.Info = makeIcon "StatusInformation_32x"
  member self.Warn = makeIcon "StatusWarning_32x"
  member self.Error = makeIcon "StatusCriticalError_32x"
  member self.Open = makeIcon "Open_6529"
  member self.Font = makeIcon "Font_16x"
  member self.ShowAbout = makeIcon "VSTAAbout_16x"
  member self.Exit = makeIcon "Exit_16x"
  member self.Logo = makeIcon "logo"
  member self.VIcon =
    let makeIco name =
      toIcon(Assembly.GetExecutingAssembly()
                            .GetManifestResourceStream("AltCover.UICommon." + name + ".ico"))
    makeIco "VIcon"