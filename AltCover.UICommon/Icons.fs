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
  member self.Blank = makeIcon "Blank_12x_16x.png"
  member self.MRU = makeIcon "ExpandChevronDown_16x"
  member self.MRUInactive = makeIcon "ExpandChevronDown_lightGray_16x"
  member self.Refresh = makeIcon "Refresh_16x"
  member self.RefreshInactive = makeIcon "Refresh_greyThin_16x"
  member self.Info = makeIcon "dialog-information"
  member self.Warn = makeIcon "dialog-warning"
  member self.Error = makeIcon "dialog-error"
  member self.Logo = makeIcon "logo"
  member self.VIcon =
    let makeIco name =
      toIcon(Assembly.GetExecutingAssembly()
                            .GetManifestResourceStream("AltCover.UICommon." + name + ".ico"))
    makeIco "VIcon"