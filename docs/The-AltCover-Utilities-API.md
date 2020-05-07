# F# Utilities API v7.x

The API is extended in `AltCover.FSApi.dll`, which includes APIs used by the PowerShell cmdlets.  The C#-style API documentation is [here](AltCover.FSApi/AltCover.FSApi-apidoc).  This functionality is present in all NuGet packages except the `altcover.visualizer` global tool package.

* [module `AltCover.CoverageFormats`](AltCover.FSApi/CoverageFormats-apidoc) for coverage report format interconversion
* [module `AltCover.OpenCover`](AltCover.FSApi/OpenCover-apidoc) for post-processis different dialcts of the OpenCover format
* [module `AltCover.Xhtml`](AltCover.FSApi/Xhtml-apidoc) for generating HTML reports from raw coverage
* [module `AltCover.XmlTypes`](AltCover.FSApi/Xml-apidoc) for general purpose XML document manipulation