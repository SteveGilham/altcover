using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

[assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  MessageId = "Api", Justification = "It's the API for the system")]

namespace AltCover
{
  /// <summary>
  ///  This class is the C#-friendly invocation API
  /// </summary>
  [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "It's the API for the system")]
  public static class CSApi
  {
    /// <summary>
    ///  This type defines the Collect (runner) behaviour.
    /// </summary>
    [SuppressMessage(
        "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
        Justification = "Design decision.")]
    [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
    public interface ICollectParameters
    {
      /// <summary>
      ///
      /// </summary>
      string RecorderDirectory { get; }

      /// <summary>
      ///
      /// </summary>
      string WorkingDirectory { get; }

      /// <summary>
      ///
      /// </summary>
      string Executable { get; }

      /// <summary>
      ///
      /// </summary>

      [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
        Justification = "Lcov is a name")]
      string LcovReport { get; }

      /// <summary>
      ///
      /// </summary>

      string Threshold { get; }
      /// <summary>
      ///
      /// </summary>

      [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
        Justification = "Cobertura is a name")]
      string Cobertura { get; }

      /// <summary>
      ///
      /// </summary>

      string OutputFile { get; }

      /// <summary>
      ///
      /// </summary>
      bool ExposeReturnCode { get; }

      /// <summary>
      ///
      /// </summary>
      string SummaryFormat { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> CommandLine { get; }

      /// <summary>
      ///
      /// </summary>
      /// <returns></returns>
      FSApi.CollectParameters ToParameters();

      /// <summary>
      ///
      /// </summary>
      /// <param name="afterPreparation"></param>
      /// <returns></returns>
      IEnumerable<string> Validate(bool afterPreparation);

      /// <summary>
      ///
      /// </summary>
      /// <param name="afterPreparation"></param>
      /// <returns></returns>
      FSApi.ValidatedCommandLine WhatIf(bool afterPreparation);
    }

    /// <summary>
    ///
    /// </summary>
    [SuppressMessage(
        "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
        Justification = "Design decision.")]
    [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
    public interface IPrepareParameters
    {
      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> InputDirectories { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> OutputDirectories { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> SymbolDirectories { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> Dependencies { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> Keys { get; }

      /// <summary>
      ///
      /// </summary>
      string StrongNameKey { get; }

      /// <summary>
      ///
      /// </summary>
      string XmlReport { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> FileFilter { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> AssemblyFilter { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> AssemblyExcludeFilter { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> TypeFilter { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> MethodFilter { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> AttributeFilter { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> PathFilter { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> CallContext { get; }

      /// <summary>
      ///
      /// </summary>
      string ReportFormat { get; }

      /// <summary>
      ///
      /// </summary>
      bool InPlace { get; }

      /// <summary>
      ///
      /// </summary>
      bool Save { get; }

      /// <summary>
      ///
      /// </summary>
      bool ZipFile { get; }

      /// <summary>
      ///
      /// </summary>
      bool MethodPoint { get; }

      /// <summary>
      ///
      /// </summary>
      bool SingleVisit { get; }

      /// <summary>
      ///
      /// </summary>
      bool LineCover { get; }

      /// <summary>
      ///
      /// </summary>
      bool BranchCover { get; }

      /// <summary>
      ///
      /// </summary>
      bool ExposeReturnCode { get; }

      /// <summary>
      ///
      /// </summary>
      bool SourceLink { get; }

      /// <summary>
      ///
      /// </summary>
      bool Defer { get; }

      /// <summary>
      ///
      /// </summary>
      bool LocalSource { get; }

      /// <summary>
      ///
      /// </summary>
      bool VisibleBranches { get; }

      /// <summary>
      ///
      /// </summary>
      string ShowStatic { get; }

      /// <summary>
      ///
      /// </summary>
      bool ShowGenerated { get; }

      /// <summary>
      ///
      /// </summary>
      IEnumerable<string> CommandLine { get; }

      /// <summary>
      ///
      /// </summary>
      /// <returns></returns>
      FSApi.PrepareParameters ToParameters();

      /// <summary>
      ///
      /// </summary>
      /// <returns></returns>
      IEnumerable<string> Validate();

      /// <summary>
      ///
      /// </summary>
      /// <returns></returns>
      FSApi.ValidatedCommandLine WhatIf();
    }

    /// <summary>
    ///
    /// </summary>
    [SuppressMessage(
        "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
        Justification = "Design decision.")]
    [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
    public interface ILogging
    {
      /// <summary>
      ///
      /// </summary>
      Action<String> Info { get; }

      /// <summary>
      ///
      /// </summary>
      Action<String> Warn { get; }

      /// <summary>
      ///
      /// </summary>
      Action<String> StandardError { get; }

      /// <summary>
      ///
      /// </summary>
      Action<String> Echo { get; }

      /// <summary>
      ///
      /// </summary>
      /// <returns></returns>
      FSApi.Logging ToParameters();
    }

    /// <summary>
    ///
    /// </summary>
    [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
      Justification = "CLI is a standard TLA")]
    [SuppressMessage(
          "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
          Justification = "Design decision.")]
    [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
    public interface ICLIOptions
    {
      /// <summary>
      ///
      /// </summary>
      bool Force { get; }

      /// <summary>
      ///
      /// </summary>
      bool FailFast { get; }

      /// <summary>
      ///
      /// </summary>
      string ShowSummary { get; }
    }

    /// <summary>
    ///
    /// </summary>
    [SuppressMessage(
        "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
        Justification = "Design decision.")]
    [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
    public static class Primitive
    {
      /// <summary>
      ///
      /// </summary>
      [SuppressMessage(
          "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
          Justification = "Design decision.")]
      [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
      [SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidLackOfCohesionOfMethodsRule",
      Justification = "What's there not to get?")]
      public class CollectParameters : ICollectParameters
      {
        /// <summary>
        ///
        /// </summary>
        private IEnumerable<string> commandLine;

        /// <summary>
        ///
        /// </summary>
        public string RecorderDirectory { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string WorkingDirectory { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string Executable { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string LcovReport { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string Threshold { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string Cobertura { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string OutputFile { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string SummaryFormat { get; set; }

        /// <summary>
        ///
        /// </summary>
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

        /// <summary>
        ///
        /// </summary>
        public bool ExposeReturnCode { get; set; }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
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

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public static CollectParameters Create()
        {
          return new CollectParameters
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

        /// <summary>
        ///
        /// </summary>
        /// <param name="afterPreparation"></param>
        /// <returns></returns>
        public IEnumerable<string> Validate(bool afterPreparation)
        {
          return ToParameters().Validate(afterPreparation);
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="afterPreparation"></param>
        /// <returns></returns>
        public FSApi.ValidatedCommandLine WhatIf(bool afterPreparation)
        {
          return ToParameters().WhatIf(afterPreparation);
        }
      }

      /// <summary>
      ///
      /// </summary>
      [SuppressMessage(
          "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
          Justification = "Design decision.")]
      [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
      public class PrepareParameters : IPrepareParameters
      {
        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> InputDirectories { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> OutputDirectories { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> SymbolDirectories { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> Dependencies { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> Keys { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string StrongNameKey { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string XmlReport { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> FileFilter { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> AssemblyFilter { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> AssemblyExcludeFilter { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> TypeFilter { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> MethodFilter { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> AttributeFilter { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> PathFilter { get; set; }

        /// <summary>
        ///
        /// </summary>
        public IEnumerable<string> CallContext { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string ReportFormat { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool InPlace { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool Save { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool ZipFile { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool MethodPoint { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool SingleVisit { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool LineCover { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool BranchCover { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool ExposeReturnCode { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool SourceLink { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool Defer { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool LocalSource { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool VisibleBranches { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string ShowStatic { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool ShowGenerated { get; set; }

        /// <summary>
        ///
        /// </summary>

        public IEnumerable<string> CommandLine { get; set; }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
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

                          ReportFormat,
                          InPlace,
                          Save,
                          ZipFile,
                          MethodPoint,
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

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public static PrepareParameters Create()
        {
          return new PrepareParameters
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

            ReportFormat = "OpenCover",
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

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public IEnumerable<string> Validate()
        {
          return ToParameters().Validate();
        }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public FSApi.ValidatedCommandLine WhatIf()
        {
          return ToParameters().WhatIf();
        }
      }

      /// <summary>
      ///
      /// </summary>
      [SuppressMessage(
          "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
          Justification = "Design decision.")]
      [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
      public class LoggingParameters : ILogging
      {
        /// <summary>
        ///
        /// </summary>
        public Action<String> Info { get; set; }

        /// <summary>
        ///
        /// </summary>
        public Action<String> Warn { get; set; }

        /// <summary>
        ///
        /// </summary>
        public Action<String> StandardError { get; set; }

        /// <summary>
        ///
        /// </summary>
        public Action<String> Echo { get; set; }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public FSApi.Logging ToParameters()
        {
          var primitive = new AltCover.Primitive.Logging(
              FSApi.Logging.ActionAdapter(Info),
              FSApi.Logging.ActionAdapter(Warn),
              FSApi.Logging.ActionAdapter(StandardError),
              FSApi.Logging.ActionAdapter(Echo));
          return FSApi.Logging.NewPrimitive(primitive);
        }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public static LoggingParameters Create()
        {
          return new LoggingParameters
          {
            Info = x => { },
            Warn = x => { },
            StandardError = x => { },
            Echo = x => { }
          };
        }
      }

      /// <summary>
      ///
      /// </summary>
      [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
        Justification = "CLI is a standard TLA")]
      [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
      [SuppressMessage(
          "Gendarme.Rules.Design", "AvoidVisibleNestedTypesRule",
          Justification = "Design decision.")]
      [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
        Justification = "Design decision.")]
      public class CLIOptions : ICLIOptions
      {
        /// <summary>
        ///
        /// </summary>
        public bool Force { get; set; }

        /// <summary>
        ///
        /// </summary>
        public bool FailFast { get; set; }

        /// <summary>
        ///
        /// </summary>
        public string ShowSummary { get; set; }
      }
    }

    /// <summary>
    ///
    /// </summary>
    /// <param name="prepareArgs"></param>
    /// <param name="log"></param>
    /// <returns></returns>
    public static int Prepare(IPrepareParameters prepareArgs, ILogging log)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (log == null) throw new ArgumentNullException(nameof(log));
      return Api.Prepare(prepareArgs.ToParameters(), log.ToParameters());
    }

    /// <summary>
    ///
    /// </summary>
    /// <param name="collectArgs"></param>
    /// <param name="log"></param>
    /// <returns></returns>
    public static int Collect(ICollectParameters collectArgs, ILogging log)
    {
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      if (log == null) throw new ArgumentNullException(nameof(log));
      return Api.Collect(collectArgs.ToParameters(), log.ToParameters());
    }

    /// <summary>
    ///
    /// </summary>
    /// <returns></returns>
    public static string ImportModule()
    {
      return Api.ImportModule();
    }

    /// <summary>
    ///
    /// </summary>
    /// <returns></returns>
    public static Version Version()
    {
      return Api.Version();
    }

    /// <summary>
    ///
    /// </summary>
    /// <param name="args"></param>
    /// <returns></returns>
    private static DotNet.CLIOptions ToCLIOptions(ICLIOptions args)
    {
      var force = DotNet.CLIOptions.NewForce(args.Force);
      var failfast = DotNet.CLIOptions.NewFailFast(args.FailFast);
      var showsummary = DotNet.CLIOptions.NewShowSummary(args.ShowSummary);
      return DotNet.CLIOptions.NewMany(new[] { force, failfast, showsummary });
    }

    /// <summary>
    ///
    /// </summary>
    /// <param name="prepareArgs"></param>
    /// <param name="collectArgs"></param>
    /// <param name="control"></param>
    /// <returns></returns>
    public static string ToTestArguments(IPrepareParameters prepareArgs,
                                         ICollectParameters collectArgs,
                                         ICLIOptions control)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      return DotNet.ToTestArguments(prepareArgs.ToParameters(),
                                    collectArgs.ToParameters(),
                                    ToCLIOptions(control));
    }

    /// <summary>
    ///
    /// </summary>
    /// <param name="prepareArgs"></param>
    /// <param name="collectArgs"></param>
    /// <param name="control"></param>
    /// <returns></returns>
    public static IEnumerable<string> ToTestArgumentList(IPrepareParameters prepareArgs,
                                              ICollectParameters collectArgs,
                                              ICLIOptions control)
    {
      if (prepareArgs == null) throw new ArgumentNullException(nameof(prepareArgs));
      if (collectArgs == null) throw new ArgumentNullException(nameof(collectArgs));
      return DotNet.ToTestArgumentList(prepareArgs.ToParameters(),
                                       collectArgs.ToParameters(),
                                       ToCLIOptions(control)).
      ToArray();
    }
  }
}