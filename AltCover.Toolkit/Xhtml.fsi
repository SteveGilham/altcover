// # namespace `AltCover`
// ```
namespace AltCover
// ```
// ## module `Xhtml`
// ```

  /// <summary>
  /// <para>Functions for building HTML-based reports</para>
  /// </summary>
  module Xhtml = begin
    /// <summary>
    /// <para type="synopsis">Generates a simple HTML report from coverage data.</para>
    /// <para type="description">The report produced is based on the old NCover 1.5.8 XSLT, for both NCover and OpenCover coverage format data.  The input is as a file name or an `XDocument` from the pipeline, the output is to the pipeline as an `XDocument`, and, optionally, to a file. </para>
    /// </summary>
    /// <param name="document">The input report</param>
    /// <returns>The HTML summary</returns>
    val ConvertToBarChart :
      document:System.Xml.Linq.XDocument -> System.Xml.Linq.XDocument
  end
  // ```
  // The input is in either NCover or OpenCover format; the report produced is based on the old NCover 1.5.8 XSLT coverage summary.