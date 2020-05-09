using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using AltCover;

using FSharpCLIOptions = AltCover.DotNet.CLIOptions;
using FSharpDotnet = AltCover.DotNet;
using FSharpCommand = AltCover.Command;

namespace AltCover.CSharp
{
  /// <summary>
  ///  Represents  other `altcover`-related command line arguments for the `dotnet test` operation
  /// </summary>
  [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
    Justification = "CLI is a standard TLA")]
  public interface ICLIOptions
  {
    /// <summary>
    /// Indicates whether a left-over `__Saved` directory from a previous run should be forcibly deleted
    /// </summary>
    bool Force { get; }

    /// <summary>
    /// Indicates whether to exit if the tests fail, without processing coverage data
    /// </summary>
    bool FailFast { get; }

    /// <summary>
    /// Indicates the colour to use to display the coverage summary to stdout; a non-colour will use system default, empty will echo nothing
    /// </summary>
    string ShowSummary { get; }

    /// <summary>
    /// Returns the F#-defined equivalent type
    /// </summary>
    /// <returns>An immutable representation of the same structure</returns>
    FSharpCLIOptions ToOptions();
  }

  /// <summary>
  /// This class is the API for driving `AltCover` as an in-process operation
  /// </summary>
  [SuppressMessage("Gendarme.Rules.Smells", "AvoidSpeculativeGeneralityRule",
    Justification = "This assembles the significant public surface for the assembly")]
  public static class Command
  {
    /// <summary>
    /// Performs the instrumentation phase of `altcover`
    /// </summary>
    /// <param name="prepareArgs"></param>
    /// <param name="log"></param>
    /// <returns></returns>
    public static int Prepare(Abstract.IPrepareOptions prepareArgs, Abstract.ILoggingOptions log)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (log == null) throw new ArgumentNullException(nameof(log));
      return FSharpCommand.Prepare(
        AltCover.PrepareOptions.NewAbstract(prepareArgs),
        AltCover.LoggingOptions.NewAbstract(log));
    }

    /// <summary>
    /// Performs the collection/reporting phase of `altcover`
    /// </summary>
    /// <param name="collectArgs">Description of the operation to perform</param>
    /// <param name="log">Where to route logging output</param>
    /// <returns>The return code of the operation</returns>
    public static int Collect(Abstract.ICollectOptions collectArgs, Abstract.ILoggingOptions log)
    {
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      if (log == null) throw new ArgumentNullException(nameof(log));
      return FSharpCommand.Collect(
        AltCover.CollectOptions.NewAbstract(collectArgs),
        AltCover.LoggingOptions.NewAbstract(log));
    }

    /// <summary>
    /// Get the PowerShell command needed to import `altcover` as a PowerShell module
    /// </summary>
    /// <returns>The PowerShell command</returns>
    public static string ImportModule()
    {
      return FSharpCommand.ImportModule();
    }

    /// <summary>
    /// Get the last computed coverage summary, if any
    /// </summary>
    /// <returns>The summary string</returns>
    public static string Summary()
    {
      return FSharpCommand.Summary();
    }

    /// <summary>
    /// Get the version of `altcover` being used
    /// </summary>
    /// <returns>The version object</returns>
    public static Version Version()
    {
      return FSharpCommand.Version();
    }
  }

  /// <summary>
  ///  Represents  other `altcover`-related command line arguments for the `dotnet test` operation
  /// </summary>
  [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
    Justification = "CLI is a standard TLA")]
  public class CLIOptions : ICLIOptions
  {
    /// <summary>
    /// Indicates whether a left-over `__Saved` directory from a previous run should be forcibly deleted
    /// </summary>
    public bool Force { get; set; }

    /// <summary>
    /// Indicates whether to exit if the tests fail, without processing coverage data
    /// </summary>
    public bool FailFast { get; set; }

    /// <summary>
    /// Indicates the colour to use to display the coverage summary to stdout; a non-colour will use system default, empty will echo nothing
    /// </summary>
    public string ShowSummary { get; set; }

    /// <summary>
    /// Returns the F#-defined equivalent type
    /// </summary>
    /// <returns>An immutable representation of the same structure</returns>
    public FSharpCLIOptions ToOptions()
    {
      var force = FSharpCLIOptions.NewForce(Force);
      var failfast = FSharpCLIOptions.NewFailFast(FailFast);
      var showsummary = FSharpCLIOptions.NewShowSummary(ShowSummary);
      return FSharpCLIOptions.NewMany(new[] { force, failfast, showsummary });
    }
  }

  /// <summary>
  /// This class deals with the interface to`dotnet test`
  /// </summary>
  public static class DotNet
  {
    /// <summary>
    /// Converts the input into the command line for `dotnet test`
    /// </summary>
    /// <param name="prepareArgs">Description of the instrumentation operation to perform</param>
    /// <param name="collectArgs">Description of the collection operation to perform</param>
    /// <param name="options">All other `altcover` related command line arguments</param>
    /// <returns>The composed command line</returns>
    public static string ToTestArguments(Abstract.IPrepareOptions prepareArgs,
                                         Abstract.ICollectOptions collectArgs,
                                          ICLIOptions options)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      if (options == null) throw new ArgumentNullException(nameof(options));
      return FSharpDotnet.ToTestArguments(AltCover.PrepareOptions.NewAbstract(prepareArgs),
                                          AltCover.CollectOptions.NewAbstract(collectArgs),
                                          options.ToOptions());
    }

    /// <summary>
    /// Converts the input into the command line for `dotnet test`
    /// </summary>
    /// <param name="prepareArgs">Description of the instrumentation operation to perform</param>
    /// <param name="collectArgs">Description of the collection operation to perform</param>
    /// <param name="options">All other `altcover` related command line arguments</param>
    /// <returns>The command line as a sequence of individual items</returns>
    public static IEnumerable<string> ToTestArgumentList(Abstract.IPrepareOptions prepareArgs,
                                              Abstract.ICollectOptions collectArgs,
                                              ICLIOptions options)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      if (options == null) throw new ArgumentNullException(nameof(options));
      return FSharpDotnet.ToTestArgumentList(AltCover.PrepareOptions.NewAbstract(prepareArgs),
                                             AltCover.CollectOptions.NewAbstract(collectArgs),
                                             options.ToOptions()).
      ToArray();
    }
  }
}