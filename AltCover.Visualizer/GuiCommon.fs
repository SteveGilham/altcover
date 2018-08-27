namespace AltCover.Visualizer

open System
open System.Xml.XPath

module GuiCommon =
  // Binds class name and XML
  type internal MethodKey =
    { m : XPathNavigator
      spacename : string
      classname : string
      name : string }

  // Range colouring information
  type internal CodeTag =
    { visitcount : int
      line : int
      column : int
      endline : int
      endcolumn : int }

  // -------------------------- Method Name Handling ---------------------------
  let internal MethodNameCompare (leftKey : MethodKey) (rightKey : MethodKey) =
    let HandleSpecialName(name : string) =
      if name.StartsWith("get_", StringComparison.Ordinal) || name.StartsWith("set_", StringComparison.Ordinal) then
        (name.Substring(4), true)
      else (name, false)

    let x = leftKey.name
    let y = rightKey.name
    let (left, specialLeft) = HandleSpecialName x
    let (right, specialRight) = HandleSpecialName y
    let sort = String.CompareOrdinal(left, right)
    let specialCase = (0 = sort) && specialLeft && specialRight
    if 0 = sort then
      if specialCase then String.CompareOrdinal(x, y)
      else
        let l1 = leftKey.m.GetAttribute("fullname", String.Empty)
        let r1 = rightKey.m.GetAttribute("fullname", String.Empty)
        String.CompareOrdinal(l1, r1)
    else sort