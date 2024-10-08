<!-- DO NOT EDIT: generated by ./Build/prepareDocumentation.ps1 for .\AltCover.Toolkit\CoverageFormats.fsi -->

# namespace `AltCover`
```
namespace AltCover
```
## module `CoverageFormats`
```

  [<RequireQualifiedAccess>]
  module CoverageFormats = begin
    val ConvertToLcov :
      document:System.Xml.Linq.XDocument -> stream:System.IO.Stream -> unit

    val ConvertToCobertura :
      document:System.Xml.Linq.XDocument -> packages : string seq -> System.Xml.Linq.XDocument

    val ConvertToJson :
      document:System.Xml.Linq.XDocument -> string

```
The input is either in NCover for OpenCover format; Cobertura, being XML, is returned as a document, the lcov format output is to a stream, and JSON as a string.
```

    val ConvertFromNCover :
      document:System.Xml.Linq.XDocument ->
        assemblies:seq<string> -> System.Xml.Linq.XDocument

    val ConvertToNCover :
      document:System.Xml.Linq.XDocument -> System.Xml.Linq.XDocument
```
Interconvert OpenCover and NCover formats.  Converting from NCover also needs the assemblies used to generate the report in order to fill in information missing from the NCover format.  Conversion in the opposite direction is, of course, lossy.
