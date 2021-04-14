﻿// # namespace `AltCover`
// ```
namespace AltCover
// ```
// ## module `XmlTypes`
// ```

/// <summary>
/// <para type="description">Methods for inter-converting XML types</para>
/// </summary>
[<RequireQualifiedAccess>]
  module XmlTypes = begin
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
    /// <param name="xmlDocument">The input document</param>
    /// <returns>The equivalent `XDocument`</returns>
    val ToXDocument :
      xmlDocument:System.Xml.XmlDocument -> System.Xml.Linq.XDocument
    end
// ```
// These functions do just what's said on the tin.