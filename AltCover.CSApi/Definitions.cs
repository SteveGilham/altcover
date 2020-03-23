using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

[assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  MessageId = "Api", Justification = "It's the API for the system")]

namespace AltCover.Parameters
{
  public interface ICollectArgs
  {
    string RecorderDirectory { get; }
    string WorkingDirectory { get; }
    string Executable { get; }

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Lcov is a name")]
    string LcovReport { get; }

    string Threshold { get; }

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Cobertura is a name")]
    string Cobertura { get; }

    string OutputFile { get; }
    bool ExposeReturnCode { get; }
    string SummaryFormat { get; }
    IEnumerable<string> CommandLine { get; }

    FSApi.CollectParameters ToParameters();

    IEnumerable<string> Validate(bool afterPreparation);

    FSApi.ValidatedCommandLine WhatIf(bool afterPreparation);
  }

  public interface IPrepareArgs
  {
    IEnumerable<string> InputDirectories { get; }
    IEnumerable<string> OutputDirectories { get; }
    IEnumerable<string> SymbolDirectories { get; }
    IEnumerable<string> Dependencies { get; }
    IEnumerable<string> Keys { get; }
    string StrongNameKey { get; }
    string XmlReport { get; }
    IEnumerable<string> FileFilter { get; }
    IEnumerable<string> AssemblyFilter { get; }
    IEnumerable<string> AssemblyExcludeFilter { get; }
    IEnumerable<string> TypeFilter { get; }
    IEnumerable<string> MethodFilter { get; }
    IEnumerable<string> AttributeFilter { get; }
    IEnumerable<string> PathFilter { get; }
    IEnumerable<string> CallContext { get; }

    bool OpenCover { get; }
    bool InPlace { get; }
    bool Save { get; }
    bool SingleVisit { get; }
    bool LineCover { get; }
    bool BranchCover { get; }
    bool ExposeReturnCode { get; }
    bool SourceLink { get; }
    bool Defer { get; }
    bool LocalSource { get; }
    bool VisibleBranches { get; }
    string ShowStatic { get; }
    bool ShowGenerated { get; }

    IEnumerable<string> CommandLine { get; }

    FSApi.PrepareParameters ToParameters();

    IEnumerable<string> Validate();

    FSApi.ValidatedCommandLine WhatIf();
  }

  public interface ILogArgs
  {
    Action<String> Info { get; }
    Action<String> Warn { get; }
    Action<String> StandardError { get; }
    Action<String> Echo { get; }

    FSApi.Logging ToParameters();
  }

  [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
    Justification = "CLI is a standard TLA")]
  public interface ICLIArg
  {
    bool Force { get; }
    bool FailFast { get; }
    string ShowSummary { get; }
  }
}

namespace AltCover.Parameters.Primitive
{
  [System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidLackOfCohesionOfMethodsRule",
      Justification = "What's there not to get?")]
  public class CollectArgs : ICollectArgs
  {
    private IEnumerable<string> commandLine;

    public string RecorderDirectory { get; set; }
    public string WorkingDirectory { get; set; }
    public string Executable { get; set; }
    public string LcovReport { get; set; }
    public string Threshold { get; set; }
    public string Cobertura { get; set; }
    public string OutputFile { get; set; }
    public string SummaryFormat { get; set; }

    public IEnumerable<string> CommandLine
    {
      get
      {
        return commandLine.ToArray();
      }
      set
      {
        commandLine = (value ?? Enumerable.Empty<string>()).ToArray();
      }
    }

    public bool ExposeReturnCode { get; set; }

    public FSApi.CollectParameters ToParameters()
    {
      var primitive = new AltCover.Primitive.CollectParameters(
          RecorderDirectory,
          WorkingDirectory,
          Executable,
          LcovReport,
          Threshold,
          Cobertura,
          OutputFile,
          CommandLine,
          ExposeReturnCode,
          SummaryFormat
                                                          );
      return FSApi.CollectParameters.NewPrimitive(primitive);
    }

    public static CollectArgs Create()
    {
      return new CollectArgs
      {
        RecorderDirectory = string.Empty,
        WorkingDirectory = string.Empty,
        Executable = string.Empty,
        LcovReport = string.Empty,
        Threshold = string.Empty,
        Cobertura = string.Empty,
        OutputFile = string.Empty,
        CommandLine = Enumerable.Empty<string>(),
        ExposeReturnCode = true,
        SummaryFormat = string.Empty
      };
    }

    public IEnumerable<string> Validate(bool afterPreparation)
    {
      return ToParameters().Validate(afterPreparation);
    }

    public FSApi.ValidatedCommandLine WhatIf(bool afterPreparation)
    {
      return ToParameters().WhatIf(afterPreparation);
    }
  }

  public class PrepareArgs : IPrepareArgs
  {
    public IEnumerable<string> InputDirectories { get; set; }
    public IEnumerable<string> OutputDirectories { get; set; }
    public IEnumerable<string> SymbolDirectories { get; set; }
    public IEnumerable<string> Dependencies { get; set; }
    public IEnumerable<string> Keys { get; set; }
    public string StrongNameKey { get; set; }
    public string XmlReport { get; set; }
    public IEnumerable<string> FileFilter { get; set; }
    public IEnumerable<string> AssemblyFilter { get; set; }
    public IEnumerable<string> AssemblyExcludeFilter { get; set; }
    public IEnumerable<string> TypeFilter { get; set; }
    public IEnumerable<string> MethodFilter { get; set; }
    public IEnumerable<string> AttributeFilter { get; set; }
    public IEnumerable<string> PathFilter { get; set; }
    public IEnumerable<string> CallContext { get; set; }

    public bool OpenCover { get; set; }
    public bool InPlace { get; set; }
    public bool Save { get; set; }
    public bool SingleVisit { get; set; }
    public bool LineCover { get; set; }
    public bool BranchCover { get; set; }
    public bool ExposeReturnCode { get; set; }
    public bool SourceLink { get; set; }
    public bool Defer { get; set; }
    public bool LocalSource { get; set; }
    public bool VisibleBranches { get; set; }
    public string ShowStatic { get; set; }
    public bool ShowGenerated { get; set; }

    public IEnumerable<string> CommandLine { get; set; }

    public FSApi.PrepareParameters ToParameters()
    {
      var primitive = new AltCover.Primitive.PrepareParameters(
                      InputDirectories,
                      OutputDirectories,
                      SymbolDirectories,
                      Dependencies,
                      Keys,
                      StrongNameKey,
                      XmlReport,
                      FileFilter,
                      AssemblyFilter,
                      AssemblyExcludeFilter,
                      TypeFilter,
                      MethodFilter,
                      AttributeFilter,
                      PathFilter,
                      CallContext,

                      OpenCover,
                      InPlace,
                      Save,
                      SingleVisit,
                      LineCover,
                      BranchCover,
                      CommandLine,
                      ExposeReturnCode,
                      SourceLink,
                      Defer,
                      LocalSource,
                      VisibleBranches,
                      ShowStatic,
                      ShowGenerated
                                                          );
      return FSApi.PrepareParameters.NewPrimitive(primitive);
    }

    public static PrepareArgs Create()
    {
      return new PrepareArgs
      {
        InputDirectories = new string[0],
        OutputDirectories = new string[0],
        SymbolDirectories = new string[0],
        Dependencies = new string[0],
        Keys = new string[0],
        StrongNameKey = string.Empty,
        XmlReport = string.Empty,
        FileFilter = new string[0],
        AssemblyFilter = new string[0],
        AssemblyExcludeFilter = new string[0],
        TypeFilter = new string[0],
        MethodFilter = new string[0],
        AttributeFilter = new string[0],
        PathFilter = new string[0],
        CallContext = new string[0],

        OpenCover = true,
        InPlace = true,
        Save = true,
        SingleVisit = false,
        LineCover = false,
        BranchCover = false,
        CommandLine = { },

        ExposeReturnCode = true,
        SourceLink = false,
        Defer = false
      };
    }

    public IEnumerable<string> Validate()
    {
      return ToParameters().Validate();
    }

    public FSApi.ValidatedCommandLine WhatIf()
    {
      return ToParameters().WhatIf();
    }
  }

  public class LogArgs : ILogArgs
  {
    public Action<String> Info { get; set; }
    public Action<String> Warn { get; set; }
    public Action<String> StandardError { get; set; }
    public Action<String> Echo { get; set; }

    public FSApi.Logging ToParameters()
    {
      var primitive = new AltCover.Primitive.Logging(
          FSApi.Logging.ActionAdapter(Info),
          FSApi.Logging.ActionAdapter(Warn),
          FSApi.Logging.ActionAdapter(StandardError),
          FSApi.Logging.ActionAdapter(Echo));
      return FSApi.Logging.NewPrimitive(primitive);
    }

    public static LogArgs Create()
    {
      return new LogArgs
      {
        Info = x => { },
        Warn = x => { },
        StandardError = x => { },
        Echo = x => { }
      };
    }
  }

  [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
    Justification = "CLI is a standard TLA")]
  public class CLIArgs : ICLIArg
  {
    public bool Force { get; set; }
    public bool FailFast { get; set; }
    public string ShowSummary { get; set; }
  }
}

namespace AltCover
{
  using AltCover.Parameters;

  [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "It's the API for the system")]
  public static class CSApi
  {
    public static int Prepare(IPrepareArgs prepareArgs, ILogArgs log)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (log == null) throw new ArgumentNullException(nameof(log));
      return Api.Prepare(prepareArgs.ToParameters(), log.ToParameters());
    }

    public static int Collect(ICollectArgs collectArgs, ILogArgs log)
    {
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      if (log == null) throw new ArgumentNullException(nameof(log));
      return Api.Collect(collectArgs.ToParameters(), log.ToParameters());
    }

    public static string ImportModule()
    {
      return Api.ImportModule();
    }

    public static string Version()
    {
      return Api.Version();
    }

    private static DotNet.CLIArgs ToCLIArgs(ICLIArg args)
    {
      var force = DotNet.CLIArgs.NewForce(args.Force);
      var failfast = DotNet.CLIArgs.NewFailFast(args.FailFast);
      var showsummary = DotNet.CLIArgs.NewShowSummary(args.ShowSummary);
      return DotNet.CLIArgs.NewMany(new[] { force, failfast, showsummary });
    }

    public static string ToTestArguments(IPrepareArgs prepareArgs,
                                         ICollectArgs collectArgs,
                                         ICLIArg control)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      return DotNet.ToTestArguments(prepareArgs.ToParameters(),
                                    collectArgs.ToParameters(),
                                    ToCLIArgs(control));
    }

    public static IEnumerable<string> ToTestArgumentList(IPrepareArgs prepareArgs,
                                              ICollectArgs collectArgs,
                                              ICLIArg control)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      return DotNet.ToTestArgumentList(prepareArgs.ToParameters(),
                                       collectArgs.ToParameters(),
                                       ToCLIArgs(control)).
      ToArray();
    }
  }
}