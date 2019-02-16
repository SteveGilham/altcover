namespace AltCover.Visualizer

open System
open System.IO
open System.Xml.XPath
open System.Net

module GuiCommon =
  // Binds class name and XML
  [<NoComparison>]
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
      if name.StartsWith("get_", StringComparison.Ordinal)
         || name.StartsWith("set_", StringComparison.Ordinal) then
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

  // -------------------------- Source file Handling ---------------------------
  [<NoComparison>]
  type internal Source =
    | File of FileInfo
    | Url of Uri
    member self.Exists
      with get() =
        match self with
        | File info -> info.Exists
        | Url u -> let request = WebRequest.CreateHttp(u)
                   request.Method <- "HEAD"
                   try
                     use response = request.GetResponse()
                     response.ContentLength > 0L &&
                     (response :?> HttpWebResponse).StatusCode |> int < 400
                   with
                   | :? WebException -> false

    member self.FullName
      with get() =
        match self with
        | File info -> info.FullName
        | Url u -> u.AbsoluteUri
    member self.Outdated epoch =
      match self with
      | File info -> (info.LastWriteTimeUtc > epoch)
      | _ -> false // Sensible SourceLink assumed
    member self.ReadAllText () =
      match self with
      | File info -> info.FullName |> File.ReadAllText
      | Url u -> use client = new System.Net.WebClient()
                 client.DownloadString(u)

  let internal GetSource (document : string) =
    if document.StartsWith("http://", StringComparison.Ordinal) ||
       document.StartsWith("https://", StringComparison.Ordinal)
    then System.Uri(document) |> Url
    else FileInfo(document) |> File