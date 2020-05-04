// # namespace `AltCover`
// ```
namespace AltCover
// ```
// ## module `CoverageFormats`
// ```

  /// <summary>
  /// <para type="description">Conversions of NCover and OpenCover format data to other report formats.</para>
  /// </summary>
  [<RequireQualifiedAccess>]
  module CoverageFormats = begin
    /// <summary>
    /// <para type="description">Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object pipeline. Writes the Lcov report to a file.</para>
    /// </summary>
    /// <param name="document">The report to convert.</param>
    /// <param name="stream">The output is written here.</param>
    val ConvertToLcov :
      document:System.Xml.Linq.XDocument -> stream:System.IO.Stream -> unit

    /// <summary>
    /// <para type="synopsis">Creates a Cobertura format report from other report formats.</para>
    /// <para type="description">Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object pipeline.</para>
    /// <para type="description">Writes the Cobertura report to the object pipeline as an `XDocument`, and optionally to a file.</para>
    /// </summary>
    /// <param name="document">The report to convert.</param>
    /// <returns>The converted document</returns>
    val ConvertToCobertura :
      document:System.Xml.Linq.XDocument -> System.Xml.Linq.XDocument
// ```
// The input is either in NCover for OpenCover format; Cobertura, being XML, is returned as a document, the lcov format output is to a stream.
// ```

    /// <summary>
    /// <para type="synopsis">Converts classic NCover format and returns OpenCover format.</para>
    /// <para type="description">The classic NCover format input either may be as an `XDocument` from the object pipeline or from a file.</para>
    /// <para type="description">Writes the OpenCover format report to the pipeline as an `XDocument`, and, optionally, to a file.  The report will contain data for the assemblies listed as the `-Assembly` argument and that are in the NCover input.</para>
    /// </summary>
    /// <param name="document">The report to convert.</param>
    /// <param name="assemblies">The assemblies contributing to the report.</param>
    /// <returns>The converted document</returns>
    val ConvertFromNCover :
      document:System.Xml.Linq.XDocument ->
        assemblies:seq<string> -> System.Xml.Linq.XDocument

    /// <summary>
    /// <para type="synopsis">Converts OpenCover format to NCover format.</para>
    /// <para type="description">Takes the OpenCover input either as an ``XDocument`` from the object pipeline or from a file.</para>
    /// <para type="description">Writes the classic NCover report to the pipeline as an ``XDocument``, and, optionally, to a file.</para>
    /// </summary>
    /// <param name="document">The report to convert.</param>
    /// <returns>The converted document</returns>
    val ConvertToNCover :
      document:System.Xml.Linq.XDocument -> System.Xml.Linq.XDocument
// ```
// Interconvert OpenCover and NCover formats.  Converting from NCover also needs the assemblies used to generate the report in order to fill in information missing from the NCover format.  Conversion in the opposite direction is, of course, lossy.
// ```
  end
// ```