namespace AltCover.Visualizer

open System
open System.IO
open System.Xml.XPath
open System.Net

module internal GuiCommon =
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

    let DisplayName (name:string) =
      let offset =
        match name.LastIndexOf("::", StringComparison.Ordinal) with
        | -1 -> 0
        | o -> o + 2

      name.Substring(offset)

    let x = DisplayName leftKey.name
    let y = DisplayName rightKey.name
    let (left, specialLeft) = HandleSpecialName x
    let (right, specialRight) = HandleSpecialName y
    let sort = String.Compare(left, right, StringComparison.OrdinalIgnoreCase)
    let tiebreaker = String.Compare(left, right, StringComparison.Ordinal)
    match (sort, specialLeft, specialRight) with
    | (0, true, false) -> if tiebreaker = 0 then -1 else tiebreaker
    | (0, false, true) -> if tiebreaker = 0 then 1 else tiebreaker
    | (0, false, false)
    | (0, true, true) -> String.Compare(x, y, StringComparison.Ordinal)
    | _ -> sort

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