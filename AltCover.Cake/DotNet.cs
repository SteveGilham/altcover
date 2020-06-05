using Cake.Common.Tools.DotNetCore;
using Cake.Common.Tools.DotNetCore.Test;
using Cake.Core;
using Cake.Core.Annotations;
using Cake.Core.IO;
using Microsoft.FSharp.Collections;
using System;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

using FSDotNet = AltCover.DotNet;

namespace AltCover.Cake
{
  /// <summary>
  /// Combines the coverage process arguments into one object for use with `dotnet test`
  /// </summary>
  public class CoverageSettings
  {
    /// <summary>
    /// Gets or sets the parameters for the preparation phase
    /// </summary>
    public Abstract.IPrepareOptions PreparationPhase { get; set; }

    /// <summary>
    /// Gets or sets the parameters for the collection phase
    /// </summary>
    public Abstract.ICollectOptions CollectionPhase { get; set; }

    /// <summary>
    ///  Gets or sets the other command line options for the operation
    /// </summary>
    public FSDotNet.ICLIOptions Options { get; set; }

    /// <summary>
    /// Provides simple validation support for the options; of necessity, it runs in the "before preparation" state.
    /// </summary>
    /// <returns>A validated command line containing any errors, or an empty one if all is ok</returns>
    public AltCover.ValidatedCommandLine WhatIf()
    {
      var prep = PreparationPhase.WhatIf();
      if (prep.Errors.Any())
        return prep;
      else
      {
        var collect = CollectionPhase.WhatIf(false);
        if (collect.Errors.Any())
          return collect;
      }

      return new AltCover.ValidatedCommandLine(FSharpList<string>.Empty, Enumerable.Empty<string>());
    }

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
        var args = FSDotNet.ToTestArgumentList(
                    this.PreparationPhase,
                    this.CollectionPhase,
                    this.Options).ToArray();
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
    /// <param name="testSettings">The `DotNetCoreTestSettings` for the test</param>
    /// <param name="coverageSettings">The `CoverageSettings` for the test instrumentation</param>
    [CakeMethodAlias]
    [CakeAliasCategory("Test")]
    [SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")]
    public static void DotNetCoreTest(
                this ICakeContext context,
                FilePath project,
                DotNetCoreTestSettings testSettings,
                CoverageSettings coverageSettings)
    {
      if (project == null) throw new ArgumentNullException(nameof(project));
      if (testSettings == null) throw new ArgumentNullException(nameof(testSettings));
      if (coverageSettings == null) throw new ArgumentNullException(nameof(coverageSettings));

      testSettings.ArgumentCustomization = coverageSettings.Concatenate(testSettings.ArgumentCustomization);
      context.DotNetCoreTest(project.GetFilename().FullPath, testSettings);
    }
  }
}