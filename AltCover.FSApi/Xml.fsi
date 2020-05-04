// # namespace `AltCover.FSApi`
// ```
namespace AltCover.FSApi
// ```
// ## module `Xml`
// ```

/// <summary>
/// <para type="description">Methods for inter-converting XML types</para>
/// </summary>
[<RequireQualifiedAccess>]
  module Xml = begin
    /// <summary>
    /// <para type="description">Takes an `XDocument` in and returns an equivalent `XmlDocument`.</para>
    /// </summary>
    /// <param name="document">The input document</param>
    /// <returns>The equivalent `XmlDocument`</returns>
    val ToXmlDocument :
      document:System.Xml.Linq.XDocument -> System.Xml.XmlDocument
    /// <summary>
    /// <para type="description">Takes an `XmlDocument` in and returns an equivalent `XDocument`.</para>
    /// </summary>
    /// <param name="document">The input document</param>
    /// <returns>The equivalent `XDocument`</returns>
    val ToXDocument :
      xmlDocument:System.Xml.XmlDocument -> System.Xml.Linq.XDocument
    end
// ```
// These functions do just what's said on the tin.