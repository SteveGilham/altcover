using System.Diagnostics.CodeAnalysis;
using Cake.Core;
using Cake.Core.Annotations;
using LogLevel = Cake.Core.Diagnostics.LogLevel;
using Verbosity = Cake.Core.Diagnostics.Verbosity;
using AltCover.Parameters;
using AltCover.Parameters.Primitive;

namespace AltCover.Cake
{
  [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "It's the API for the system")]
  public static class Api
  {
    private static ILogArgs MakeLog(ICakeContext context, ILogArgs log)
    {
      if (log != null)
        return log;

      if (context != null)
        return new LogArgs()
        {
          Info = x => context.Log.Write(Verbosity.Normal, LogLevel.Information, x),
          Warn = x => context.Log.Write(Verbosity.Normal, LogLevel.Warning, x),
          Echo = x => context.Log.Write(Verbosity.Normal, LogLevel.Error, x),
          StandardError = x => context.Log.Write(Verbosity.Verbose, LogLevel.Information, x),
        };

      return new LogArgs()
      {
        Info = x => { },
        Warn = x => { },
        Echo = x => { },
        StandardError = x => { }
      };
    }

    [CakeMethodAlias]
    [SuppressMessage("Microsoft.Design", "CA1026:DefaultParametersShouldNotBeUsed",
                     Justification = "WTF is this rule saying?")]
    public static int Prepare(this ICakeContext context, IPrepareArgs prepareArgs, ILogArgs log = null)
    {
      return CSApi.Prepare(prepareArgs, MakeLog(context, log));
    }

    [CakeMethodAlias]
    [SuppressMessage("Microsoft.Design", "CA1026:DefaultParametersShouldNotBeUsed",
                     Justification = "WTF is this rule saying?")]
    public static int Collect(this ICakeContext context, ICollectArgs collectArgs, ILogArgs log = null)
    {
      return CSApi.Collect(collectArgs, MakeLog(context, log));
    }

    [CakeMethodAlias]
    public static string ImportModule(this ICakeContext context)
    {
      if (context == null) throw new System.ArgumentNullException(nameof(context));
      return CSApi.ImportModule();
    }

    [CakeMethodAlias]
    public static string Version(this ICakeContext context)
    {
      if (context == null) throw new System.ArgumentNullException(nameof(context));
      return CSApi.Version();
    }
  }
}