using System.Diagnostics.CodeAnalysis;
using Cake.Core;
using Cake.Core.Annotations;
using LogLevel = Cake.Core.Diagnostics.LogLevel;
using Verbosity = Cake.Core.Diagnostics.Verbosity;

namespace AltCover.Cake
{
  /// <summary>
  /// A C#-friendly expression of the core API to drive the instrumentation and collection process.
  /// </summary>
  [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "It's the API for the system")]
  public static class Api
  {
    private static CSApi.ILogging MakeLog(ICakeContext context, CSApi.ILogging log)
    {
      if (log != null)
        return log;

      if (context != null)
        return new CSApi.Primitive.LoggingParameters()
        {
          Info = x => context.Log.Write(Verbosity.Normal, LogLevel.Information, x),
          Warn = x => context.Log.Write(Verbosity.Normal, LogLevel.Warning, x),
          Echo = x => context.Log.Write(Verbosity.Normal, LogLevel.Error, x),
          StandardError = x => context.Log.Write(Verbosity.Verbose, LogLevel.Information, x),
        };

      return new CSApi.Primitive.LoggingParameters()
      {
        Info = x => { },
        Warn = x => { },
        Echo = x => { },
        StandardError = x => { }
      };
    }

    /// <summary>
    /// <para>Performs the instrumentation phase of the coverage operation.</para>
    /// <para>This method is a `[CakeMethodAlias]` extension method on the Cake build script context.</para>
    /// </summary>
    /// <param name="context">The Cake build script context; a `this` parameter</param>
    /// <param name="prepareArgs">C# definition of the instrumentation (and optional execution) process.</param>
    /// <param name="log">[Optional]C# definition of the process logging; if no logging support is supplied, then Cake logging at an appropriate severity is used.</param>
    /// <returns>The integer return code for the instrumentation process.</returns>
    [CakeMethodAlias]
    [SuppressMessage("Microsoft.Design", "CA1026:DefaultParametersShouldNotBeUsed",
                     Justification = "WTF is this rule saying?")]
    public static int Prepare(this ICakeContext context, CSApi.IPrepareParameters prepareArgs, CSApi.ILogging log = null)
    {
      return CSApi.Prepare(prepareArgs, MakeLog(context, log));
    }

    /// <summary>
    /// <para>Performs the collection phase of the coverage operation.</para>
    /// <para>This method is a `[CakeMethodAlias]` extension method on the Cake build script context.</para>
    /// </summary>
    /// <param name="context">The Cake build script context; a `this` parameter</param>
    /// <param name="collectArgs">C# definition of the collection (and optional execution) process.</param>
    /// <param name="log">[Optional]C# efinition of the process logging; if no logging support is supplied, then Cake logging at an appropriate severity is used.</param>
    /// <returns>The integer return code for the collection process.</returns>
    [CakeMethodAlias]
    [SuppressMessage("Microsoft.Design", "CA1026:DefaultParametersShouldNotBeUsed",
                     Justification = "WTF is this rule saying?")]
    public static int Collect(this ICakeContext context, CSApi.ICollectParameters collectArgs, CSApi.ILogging log = null)
    {
      return CSApi.Collect(collectArgs, MakeLog(context, log));
    }

    /// <summary>
    /// <para>Performs the `AltCover ImportModule` operation.</para>
    /// <para>This method is a `[CakeMethodAlias]` extension method on the Cake build script context.</para>
    /// </summary>
    /// <param name="context">The Cake build script context; a `this` parameter</param>
    /// <returns>The PowerShell Import-Module command to access the AltCover cmdlets</returns>
    [CakeMethodAlias]
    public static string ImportModule(this ICakeContext context)
    {
      if (context == null) throw new System.ArgumentNullException(nameof(context));
      return CSApi.ImportModule();
    }

    /// <summary>
    /// <para>Performs the `AltCover Version` operation.</para>
    /// <para>This method is a `[CakeMethodAlias]` extension method on the Cake build script context.</para>
    /// </summary>
    /// <param name="context">The Cake build script context; a `this` parameter</param>
    /// <returns>The AltCover built version</returns>
    [CakeMethodAlias]
    public static System.Version Version(this ICakeContext context)
    {
      if (context == null) throw new System.ArgumentNullException(nameof(context));
      return CSApi.Version();
    }
  }
}