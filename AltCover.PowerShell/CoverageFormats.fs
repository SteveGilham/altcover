namespace AltCover.Commands

#if MONO
module CoverageFormats
    let hello name =
        printfn "Hello %s" name
#else

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml
open System.Xml.Linq

open AltCover.PowerShell

[<Cmdlet(VerbsData.ConvertTo, "Lcov")>]
[<OutputType("System.Void")>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Lcov is OK")>]
type ConvertToLcovCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = ConvertToLcovCommand(String.Empty)

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:XmlDocument = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  override self.ProcessRecord() =
    let format = XmlUtilities.DiscoverFormat self.XmlDocument
    let xdoc = XmlUtilities.ToXDocument self.XmlDocument
    use stream = File.Open(self.OutputFile, FileMode.OpenOrCreate, FileAccess.Write)
    // TODO detect formats
#if NETCOREAPP2_0
    AltCover.LCov.ConvertReport xdoc format stream
#else
#if DEBUG
    AltCover.LCov.ConvertReport xdoc format stream
#else
    AltCover.LCov.ConvertReport (xdoc, format, stream)
#endif
#endif

[<Cmdlet(VerbsData.ConvertTo, "Cobertura")>]
[<OutputType(typeof<XDocument>)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToCoberturaCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = ConvertToCoberturaCommand(String.Empty)

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:XmlDocument = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  override self.ProcessRecord() =
    let format = XmlUtilities.DiscoverFormat self.XmlDocument
    let xdoc = XmlUtilities.ToXDocument self.XmlDocument
#if NETCOREAPP2_0
    let rewrite = AltCover.Cobertura.ConvertReport xdoc format
#else
#if DEBUG
    let rewrite = AltCover.Cobertura.ConvertReport xdoc format
#else
    let rewrite = AltCover.Cobertura.ConvertReport (xdoc, format)
#endif
#endif
    if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        rewrite.Save(self.OutputFile)

    self.WriteObject rewrite

#endif