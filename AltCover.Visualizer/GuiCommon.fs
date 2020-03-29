namespace AltCover.Visualizer

open System
open System.IO
open System.Xml.XPath
open System.Net

module internal GuiCommon =
  // Binds class name and XML
  [<NoComparison; AutoSerializable(false)>]
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

  type internal MethodType =
    | Normal = 0
    | Property = -1
    | Event = -2

  // -------------------------- Method Name Handling ---------------------------
  let displayName(name : string) =
    let offset =
      match name.LastIndexOf("::", StringComparison.Ordinal) with
      | -1 -> 0
      | o -> o + 2

    name.Substring(offset).Split('(') |> Seq.head

  let handleSpecialName(name : string) =
    if name.StartsWith("get_", StringComparison.Ordinal)
       || name.StartsWith("set_", StringComparison.Ordinal) then
      (name.Substring(4), MethodType.Property)
    else if name.StartsWith("add_", StringComparison.Ordinal) then
      (name.Substring(4), MethodType.Event)
    else if name.StartsWith("remove_", StringComparison.Ordinal) then
      (name.Substring(7), MethodType.Event)
    else
      (name, MethodType.Normal)

  // -------------------------- Source file Handling ---------------------------
  [<NoComparison; AutoSerializable(false)>]
  type internal Source =
    | File of FileInfo
    | Url of Uri

    member self.Exists =
      match self with
      | File info -> info.Exists
      | Url u ->
          let request = WebRequest.CreateHttp(u)
          request.Method <- "HEAD"
          try
            use response = request.GetResponse()
            response.ContentLength > 0L && (response :?> HttpWebResponse).StatusCode
                                           |> int < 400
          with :? WebException -> false

    member self.FullName =
      match self with
      | File info -> info.FullName
      | Url u -> u.AbsoluteUri

    member self.Outdated epoch =
      match self with
      | File info -> (info.LastWriteTimeUtc > epoch)
      | _ -> false // Sensible SourceLink assumed

    member self.ReadAllText() =
      match self with
      | File info -> info.FullName |> File.ReadAllText
      | Url u ->
          use client = new System.Net.WebClient()
          client.DownloadString(u)

  let internal getSource(document : string) =
    if document.StartsWith("http://", StringComparison.Ordinal)
       || document.StartsWith("https://", StringComparison.Ordinal) then
      System.Uri(document) |> Url
    else
      FileInfo(document) |> File

  type internal AltCover.Exemption with
    static member internal OfInt i =
     if i > 0 then AltCover.Exemption.Visited
     else if i < -4
          then AltCover.Exemption.None
          else i |> sbyte |> Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<sbyte, AltCover.Exemption>