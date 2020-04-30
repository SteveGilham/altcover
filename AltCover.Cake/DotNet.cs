using Cake.Common.Tools.DotNetCore;
using Cake.Common.Tools.DotNetCore.Test;
using Cake.Core;
using Cake.Core.Annotations;
using Cake.Core.IO;
using System;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

[assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope = "member", Target = "AltCover.Cake.DotNet.#DotNetCoreTest(Cake.Core.ICakeContext,Cake.Core.IO.FilePath,Cake.Common.Tools.DotNetCore.Test.DotNetCoreTestSettings,AltCover.Cake.AltCoverSettings)",
  MessageId = "altcover", Justification = "It's the product name.")]

namespace AltCover.Cake
{
  /// <summary>
  /// Combines the coverage process arguments into one object for use with `dotnet test`
  /// </summary>
  public class AltCoverSettings
  {
    /// <summary>
    /// Gets or sets the parameters for the preparation phase
    /// </summary>
    public CSApi.IPrepareParameters PreparationPhase { get; set; }

    /// <summary>
    /// Gets or sets the parameters for the collection phase
    /// </summary>
    public CSApi.ICollectParameters CollectionPhase { get; set; }

    /// <summary>
    ///  Gets or sets the other command line options for the operation
    /// </summary>
    public CSApi.ICLIOptions Control { get; set; }

    /// <summary>
    /// <para>For applying these settings in a pipeline; returns a delegate to transform a `ProcessArgumentBuilder` based on the current settings</para>
    /// </summary>
    /// <returns>`ProcessArgumentBuilder` to `ProcessArgumentBuilder` transform</returns>
    public Func<ProcessArgumentBuilder, ProcessArgumentBuilder> Customize()
    {
      return pabIn =>
      {
        var pabOut = new ProcessArgumentBuilder();
        if (pabIn != null)
        {
          pabIn.CopyTo(pabOut);
        }
        var args = CSApi.ToTestArgumentList(
                    this.PreparationPhase,
                    this.CollectionPhase,
            this.Control).ToArray();
        Array.Reverse(args);
        Array.ForEach(
            args,
            t => pabOut.Prepend(t));
        return pabOut;
      };
    }

    /// <summary>
    /// <para>For applying these settings in a pipeline; returns a delegate to transform a `ProcessArgumentBuilder` based on the current settings,
    /// combined with the input value</para>
    /// </summary>
    /// <param name="customIn">Another `ProcessArgumentBuilder` to `ProcessArgumentBuilder` transform to apply before the `AltCover` one.</param>
    /// <returns>A concatenated `ProcessArgumentBuilder` to `ProcessArgumentBuilder` transform</returns>
    public Func<ProcessArgumentBuilder, ProcessArgumentBuilder> Concatenate(Func<ProcessArgumentBuilder, ProcessArgumentBuilder> customIn)
    {
      var altcover = Customize();
      if (customIn == null)
      {
        return altcover;
      }
      else
      {
        return args => altcover(customIn(args));
      }
    }
  }

  /// <summary>
  /// Extensions for `dotnet`.  This class is a `[CakeAliasCategory("DotNetCore")]`
  /// </summary>
  [CakeAliasCategory("DotNetCore")]
  public static class DotNet
  {
    /// <summary>
    /// <para>Hooks into the Cake wrapper for `dotnet test` and injects the AltCover command line arguments as specified.</para>
    /// <para>Equivalent to</para>
    /// <code>
    /// settings.ArgumentCustomization = altcover.Concatenate(settings.ArgumentCustomization);
    /// context.DotNetCoreTest(project.FullPath, settings);
    /// </code>
    /// <para>This method is a `[CakeMethodAlias]` extension method on `ICakeContext`, and `[CakeAliasCategory("Test")]`.</para>
    /// </summary>
    /// <param name="context">The Cake build script `ICakeContext`; a `this` parameter</param>
    /// <param name="project">The project to test as a `FilePath`</param>
    /// <param name="settings">The `DotNetCoreTestSettings` for the test</param>
    /// <param name="altcover">The `AltCoverSettings` for the test instrumentation</param>
    [CakeMethodAlias]
    [CakeAliasCategory("Test")]
    [System.Diagnostics.CodeAnalysis.SuppressMessage(
"Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
Justification = "AvoidSpeculativeGenerality too")]
    public static void DotNetCoreTest(
                this ICakeContext context,
                FilePath project,
                DotNetCoreTestSettings settings,
                AltCoverSettings altcover)
    {
      if (project == null) throw new ArgumentNullException(nameof(project));
      if (settings == null) throw new ArgumentNullException(nameof(settings));
      if (altcover == null) throw new ArgumentNullException(nameof(altcover));

      settings.ArgumentCustomization = altcover.Concatenate(settings.ArgumentCustomization);
      context.DotNetCoreTest(project.GetFilename().FullPath, settings);
    }
  }
}