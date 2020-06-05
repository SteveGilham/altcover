// # namespace `AltCover`
// ```
namespace AltCover
// ```
// ## module `RenderToHtml`
// ```

  /// <summary>
  /// <para type="description">Conversions of NCover and OpenCover format data to HTML.</para>
  /// </summary>
  module RenderToHtml = begin
    /// <summary>
    /// <para type="description">Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument and creates a set of XML/HTML documents mappiong coverage to source.</para>
    /// </summary>
    /// <param name="xmlDocument">The report to convert.</param>
    /// <returns>The converted document</returns>
    val Action :
      xmlDocument:System.Xml.Linq.XDocument ->
        seq<string * System.Xml.XmlDocument>
  end