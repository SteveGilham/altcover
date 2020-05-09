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
  public static class Command
  {
    private static Abstract.ILoggingOptions MakeLog(ICakeContext context, Abstract.ILoggingOptions log)
    {
      if (log != null)
        return log;

      var result = new Options.LoggingOptions();

      if (context != null)
      {
        result.Info = x => context.Log.Write(Verbosity.Normal, LogLevel.Information, x);
        result.Warn = x => context.Log.Write(Verbosity.Normal, LogLevel.Warning, x);
        result.Failure = x => context.Log.Write(Verbosity.Normal, LogLevel.Error, x);
        result.Echo = x => context.Log.Write(Verbosity.Verbose, LogLevel.Information, x);
      }

      return result;
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
    public static int Prepare(this ICakeContext context, Abstract.IPrepareOptions prepareArgs, Abstract.ILoggingOptions log = null)
    {
      return CSharp.Command.Prepare(prepareArgs, MakeLog(context, log));
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
    public static int Collect(this ICakeContext context, Abstract.ICollectOptions collectArgs, Abstract.ILoggingOptions log = null)
    {
      return CSharp.Command.Collect(collectArgs, MakeLog(context, log));
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
      return CSharp.Command.ImportModule();
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
      return CSharp.Command.Version();
    }
  }
}