using Cake.Common.Tools.DotNetCore;
using Cake.Common.Tools.DotNetCore.Test;
using Cake.Core;
using Cake.Core.Annotations;
using Cake.Core.IO;
using System;
using System.Linq;

namespace AltCover.Cake
{
    public class AltCoverSettings
    {
        public PrepareArgs PreparationPhase { get; set; }
        public CollectArgs CollectionPhase { get; set; }
    }

    [CakeAliasCategory("DotNetCore")]
    public static class DotNet
    {
        [CakeMethodAlias]
        [CakeAliasCategory("Test")]
        public static void DotNetCoreTest(
                    this ICakeContext context,
                    FilePath project,
                    DotNetCoreTestSettings settings,
                    AltCoverSettings altcover)
        {
            var currentCustomization = settings.ArgumentCustomization;
            settings.ArgumentCustomization = (args) => ProcessArguments(context, currentCustomization?.Invoke(args) ?? args, project, altcover);
            context.DotNetCoreTest(project.FullPath, settings);
        }

        private static ProcessArgumentBuilder ProcessArguments(
                        ICakeContext cakeContext,
                        ProcessArgumentBuilder builder,
                        FilePath project,
                        AltCoverSettings altcover)
        {
            Array.ForEach(
                CSApi.ToTestArgumentList(altcover.PreparationPhase, altcover.CollectionPhase),
                t => builder.Append(t));
            return builder;
        }
    }
}