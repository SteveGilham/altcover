using Cake.Common.Tools.DotNetCore;
using Cake.Common.Tools.DotNetCore.Test;
using Cake.Core;
using Cake.Core.Annotations;
using Cake.Core.IO;
using System;
using System.Linq;
using AltCover.Parameters;

namespace AltCover.Cake
{
  public class AltCoverSettings
  {
    public IPrepareArgs PreparationPhase { get; set; }
    public ICollectArgs CollectionPhase { get; set; }
    public ICLIArg Force { get; set; }

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
                    this.Force);
        Array.Reverse(args);
        Array.ForEach(
            args,
            t => pabOut.Prepend(t));
        return pabOut;
      };
    }

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

  [CakeAliasCategory("DotNetCore")]
  public static class DotNet
  {
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