namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.IO.Compression
open System.Xml.XPath
open System.Net

module GuiCommon =
  // Binds class name and XML
  [<NoComparison; AutoSerializable(false)>]
  type MethodKey =
    { Navigator: XPathNavigator
      NameSpace: string
      ClassName: string
      Name: string }

  // Range colouring information
  [<NoComparison>]
  type CodeTag =
    { VisitCount: int
      Line: int
      Column: int
      EndLine: int
      EndColumn: int
      LineOnly: bool
      Style: Exemption }

  type MethodType =
    | Normal = 0
    | Property = -1
    | Event = -2

  // -------------------------- Method Name Handling ---------------------------
  let DisplayName (name: string) =
    let offset =
      match name.LastIndexOf("::", StringComparison.Ordinal) with
      | -1 -> 0
      | o -> o + 2

    name.Substring(offset).Split('(') |> Seq.head

  let HandleSpecialName (name: string) =
    if
      name.StartsWith("get_", StringComparison.Ordinal)
      || name.StartsWith
        (
          "set_",
          StringComparison.Ordinal
        )
    then
      (name.Substring(4), MethodType.Property)
    else if name.StartsWith("add_", StringComparison.Ordinal) then
      (name.Substring(4), MethodType.Event)
    else if name.StartsWith("remove_", StringComparison.Ordinal) then
      (name.Substring(7), MethodType.Event)
    else
      (name, MethodType.Normal)

  // -------------------------- Source file Handling ---------------------------
  let Embed (node:XPathNavigator) (document: string) =
    node.SelectAncestors("module", String.Empty, false)
    |> Seq.cast<XPathNavigator>
    |> Seq.collect(fun n -> n.SelectDescendants("altcover.file", String.Empty, false)
                            |> Seq.cast<XPathNavigator>)
    |> Seq.filter (fun n -> n.GetAttribute("document", String.Empty) = document)
    |> Seq.map (fun n -> n.GetAttribute("embed", String.Empty))
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.tryHead

  [<NoComparison; AutoSerializable(false)>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
                                                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                                    Justification = "match expressions are still too much the same for it")>]
  type Source =
    | File of FileInfo
    | Url of Uri
    | Embed of (string * string)

    member internal self.Exists =
      match self with
      | File info -> info.Exists
      | Embed (_, s) -> s |> String.IsNullOrWhiteSpace |> not
      | Url u ->
          let request = createHttp(u)
          request.Method <- "HEAD"

          try
            use response = request.GetResponse()

            response.ContentLength > 0L
            && (response :?> HttpWebResponse).StatusCode |> int < 400
          with :? WebException -> false

    member internal self.FullName =
      match self with
      | Embed (name, _) -> name
      | File info ->
          if info |> isNull then
            String.Empty
          else
            info.FullName
      | Url u -> u.AbsoluteUri

    member internal self.Outdated epoch =
      match self with
      | File info -> if info.Exists
                     then info.LastWriteTimeUtc > epoch
                     else false // can't tell; should show as non-existing
      | _ -> false // Embed or ensible SourceLink assumed

    member self.ReadAllText() =
      match self with
      | File info -> info.FullName |> File.ReadAllText
      | Url u -> readAllText u
      | Embed (_,source) -> let data = Convert.FromBase64String source
                            use raw = new MemoryStream(data)
                            use expanded = new MemoryStream()
                            // Get deflation working with this one weird trick
                            // Dispose the deflate stream w/o closing the one it points at!
                            do
                              use expand = new DeflateStream(raw, CompressionMode.Decompress, true)
                              expand.CopyTo expanded
                            System.Text.Encoding.UTF8.GetString(expanded.GetBuffer(),
                                                                0, int expanded.Length)

    member internal self.MakeEmbedded (filename: string) (source: string option) =
      match (self, source) with
      | (File info, Some _) -> Embed (filename, source.Value)
      | _ -> self

  let GetSource (document: string) =
    if
      document.StartsWith("http://", StringComparison.Ordinal)
      || document.StartsWith
        (
          "https://",
          StringComparison.Ordinal
        )
    then
      System.Uri(document) |> Url
    else
      FileInfo(document) |> File

  type AltCover.Exemption with
    static member OfInt i =
      if i > 0 then
        AltCover.Exemption.Visited
      else if i < -4 then
        AltCover.Exemption.None
      else
        i
        |> sbyte
        |> Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<sbyte, AltCover.Exemption>

[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "type",
                            Target = "AltCover.GuiCommon",
                            MessageId = "Gui",
                            Justification = "We all know what it means")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.GuiCommon.#Exemption.OfInt.Static(System.Int32)",
                            MessageId = "i",
                            Justification = "Another obvious name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1703:ResourceStringsShouldBeSpelledCorrectly",
                            Scope = "resource",
                            Target = "AltCover.UICommon.Resource.resources",
                            MessageId = "freedesktop",
                            Justification = "A proper name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1703:ResourceStringsShouldBeSpelledCorrectly",
                            Scope = "resource",
                            Target = "AltCover.UICommon.Resource.resources",
                            MessageId = "ownload",
                            Justification = "A proper name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1703:ResourceStringsShouldBeSpelledCorrectly",
                            Scope = "resource",
                            Target = "AltCover.UICommon.Resource.resources",
                            MessageId = "visualstudio",
                            Justification = "A proper name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1702:CompoundWordsShouldBeCasedCorrectly",
                            Scope = "member",
                            Target = "AltCover.GuiCommon+MethodKey.#.ctor(System.Xml.XPath.XPathNavigator,System.String,System.String,System.String)",
                            MessageId = "nameSpace",
                            Justification = "Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1702:CompoundWordsShouldBeCasedCorrectly",
                            Scope = "member",
                            Target = "AltCover.GuiCommon+MethodKey.#NameSpace",
                            MessageId = "NameSpace",
                            Justification = "Compound of two words")>]
()