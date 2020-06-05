// # namespace `AltCover`
// ```
namespace AltCover
// ```
// ## module `OpenCover`
// ```

  /// <summary>
  /// <para>Functions manipulating OpenCover format reports from various generators</para>
  /// </summary>
  [<RequireQualifiedAccess>]
  module OpenCover = begin
    /// <summary>
    /// <para type="synopsis">Updates summary and related coverage-derived data based on visit counts.</para>
    /// </summary>
    /// <param name="document">The input report</param>
    /// <param name="ordinal">How branches are indexed.  If offset values are available, those are preferred.</param>
    val PostProcess :
      document:System.Xml.Linq.XDocument -> ordinal:BranchOrdinal -> unit
// ```
// Updates summary and related coverage-derived data based on visit counts.  Argument `ordinal` indicates which XML attribute should be used to associate branches with sequence points.
// ```

    /// <summary>
    /// <para type="synopsis">Fills in gaps in `coverlet`'s OpenCover dialect.</para>
    /// <para type="description">Adds summary data and other items to report in `coverlet`'s OpenCover dialect, particularly giving somewhat meaningful start and end column values for its line-based paradigm, as well as npath coverage and branch exits.</para>
    /// </summary>
    /// <param name="document">The input report</param>
    /// <returns>The filled-in report</returns>
    val FormatFromCoverlet :
      report:System.Xml.Linq.XDocument ->
        files:string array -> System.Xml.Linq.XDocument
// ```
// Adds summary data and other items to report in `coverlet`'s OpenCover dialect, particularly giving somewhat meaningful start and end column values for its line-based paradigm, as well as npath coverage and branch exits.  Arguments `files` indicates the assemblies used in the report, which are used to provide sopme of the missing information.
// ```

    /// <summary>
    /// <para type="synopsis">Removes compiler-generated hidden branches from OpenCover.</para>
    /// <para type="description">Takes output from the OpenCover program, and adjust from OpenCover's liberal idea of significant branches towards AltCover's more restricted approach -- chose either or both of `sameSpan` to unify branches that go from the same start, and take the same trajectory to the same end (OpenCover issue #786 being one instance of this) and `withinSequencePoint` to remove branches interior to a statement (compiler generated things like stashing of lambdas, the hidden conditional `Dispose()` after a `using`, or inside F# inlines -- OpenCover issues #657, #807 being instances of this).</para>
    /// <para type="description">Either takes an `XDocument` from the pipeline or from a file; emits the result as an `XDocument` to the pipeline and optionally to a file.</para>
    /// </summary>
    /// <param name="document">The input report</param>
    /// <param name="withinSequencePoint">Whether to hide branches that terminate inside the same sequence point as they begin</param>
    /// <param name="sameSpan">Whether to treat branches between the same points as being the same branch</param>
    /// <returns>The filled-in report</returns>
    val CompressBranching :
      document:System.Xml.Linq.XDocument ->
        withinSequencePoint:bool -> sameSpan:bool -> System.Xml.Linq.XDocument
// ```
// Takes output from the OpenCover program, and adjust from OpenCover's liberal idea of significant branches towards AltCover's more restricted approach -- chose either or both of `sameSpan` to unify branches that go from the same start, and take the same trajectory to the same end (OpenCover issue #786 being one instance of this) and `withinSequencePoint` to remove branches interior to a statement (compiler generated things like stashing of lambdas, the hidden conditional `Dispose()` after a `using`, or inside F# inlines -- OpenCover issues #657, #807 being instances of this).
  end //// no doc