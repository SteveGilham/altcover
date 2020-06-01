namespace AltCover.Commands

open System
open System.IO
open System.Management.Automation
open System.Xml.Linq

/// <summary>
/// <para type="synopsis">Removes compiler-generated hidden branches from OpenCover.</para>
/// <para type="description">Takes output from the OpenCover program, and adjust from OpenCover's liberal idea of significant branches towards AltCover's more restricted approach -- chose either or both of `-SameSpan` to unify branches that go from the same start, and take the same trajectory to the same end (OpenCover issue #786 being one instance of this) and `-WithinSequencePoint` to remove branches interior to a statement (compiler generated things like stashing of lambdas, the hidden conditional `Dispose()` after a `using`, or inside F# inlines -- OpenCover issues #657, #807 being instances of this).</para>
/// <para type="description">Either takes an `XDocument` from the pipeline or from a file; emits the result as an `XDocument` to the pipeline and optionally to a file.</para>
/// <example>
///   <code>    $xml = Compress-Branching -WithinSequencePoint -InputFile "./Tests/Compressible.xml" -OutputFile "./_Packaging/CompressInterior.xml"</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.Compress, "Branching")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type CompressBranchingCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `XDocument` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  /// <summary>
  /// <para type="description">Input as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  /// <summary>
  /// <para type="description">Output as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = String.Empty with get, set

  /// <summary>
  /// <para type="description">Merge branches when start and end at the same place</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val SameSpan : SwitchParameter = SwitchParameter(false) with get, set

  /// <summary>
  /// <para type="description">Discard branches within a sequence point</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val WithinSequencePoint : SwitchParameter = SwitchParameter(false) with get, set

  /// <summary>
  /// <para type="description">Create transformed document</para>
  /// </summary>
  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName.StartsWith("FromFile", StringComparison.Ordinal) then
        self.XDocument <- XDocument.Load self.InputFile

      let xmlDocument =
        AltCover.OpenCover.CompressBranching self.XDocument
          self.WithinSequencePoint.IsPresent self.SameSpan.IsPresent

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then xmlDocument.Save(self.OutputFile)

      self.WriteObject xmlDocument
    finally
      Directory.SetCurrentDirectory here