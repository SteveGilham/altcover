using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Text;

using FSDotNet = AltCover.DotNet;

namespace AltCover.Cake
{
  /// <summary>
  /// Overall test run options; override defaults as required
  /// </summary>
  public class TestOptions : FSDotNet.ICLIOptions
  {
    /// <summary>
    /// Gets whether to force delete previous runs
    /// </summary>
    public virtual bool ForceDelete => false;

    /// <summary>
    /// Gets whether to fail without computing coverage on test failure
    /// </summary>
    public virtual bool FailFast => false;

    /// <summary>
    /// Gets the type of summary to show
    /// </summary>
    public virtual string ShowSummary => String.Empty;
  }

  /// <summary>
  /// Preparation stage options; override defaults as required
  /// </summary>
  public class PrepareOptions : Abstract.IPrepareOptions
  {
    /// <summary>
    /// Gets the input directories (overridden by the MSBuild integration)
    /// </summary>
#pragma warning disable IDE0079 // Remove unnecessary suppression

    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public IEnumerable<string> InputDirectories => throw new NotImplementedException("InputDirectories not used");

    /// <summary>
    /// Gets the output directories (overridden by the MSBuild integration)
    /// </summary>

    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public IEnumerable<string> OutputDirectories => throw new NotImplementedException("OutputDirectories not used");

    ///<summary>
    /// Corresponds to command line option `-y, --symbolDirectory=VALUE`
    ///</summary>
    public virtual IEnumerable<string> SymbolDirectories => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-d, --dependency=VALUE`
    ///</summary>
    public virtual IEnumerable<string> Dependencies => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-k, --key=VALUE`
    ///</summary>
    public virtual IEnumerable<string> Keys => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `--sn, --strongNameKey=VALUE`
    ///</summary>
    public virtual string StrongNameKey => String.Empty;

    ///<summary>
    /// Gets or sets the value that corresponds to command line option `-r, --report=VALUE`
    ///</summary>
    public virtual string Report => String.Empty;

    ///<summary>
    /// Corresponds to command line option `-f, --fileFilter=VALUE`
    ///</summary>
    public virtual IEnumerable<string> FileFilter => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-s, --assemblyFilter=VALUE`
    ///</summary>
    public virtual IEnumerable<string> AssemblyFilter => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE`
    ///</summary>
    public virtual IEnumerable<string> AssemblyExcludeFilter => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-t, --typeFilter=VALUE`
    ///</summary>
    public virtual IEnumerable<string> TypeFilter => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-m, --methodFilter=VALUE`
    ///</summary>
    public virtual IEnumerable<string> MethodFilter => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-a, --attributeFilter=VALUE`
    ///</summary>
    public virtual IEnumerable<string> AttributeFilter => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-p, --pathFilter=VALUE`
    ///</summary>
    public virtual IEnumerable<string> PathFilter => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `--attributetoplevel=VALUE`
    ///</summary>
    public virtual IEnumerable<string> AttributeTopLevel => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `--typetoplevel=VALUE`
    ///</summary>
    public virtual IEnumerable<string> TypeTopLevel => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `--methodtoplevel=VALUE`
    ///</summary>
    public virtual IEnumerable<string> MethodTopLevel => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `-c, --callContext=VALUE`
    ///</summary>
    public virtual IEnumerable<string> CallContext => Array.Empty<string>();

    ///<summary>
    /// Corresponds to command line option `--reportFormat=VALUE`
    ///</summary>
    public virtual string ReportFormat => String.Empty;

    ///<summary>
    /// Corresponds to command line option `--inplace`
    ///</summary>
    public virtual bool InPlace => false;

    ///<summary>
    /// Corresponds to command line option `--save`
    ///</summary>
    public virtual bool Save => false;

    ///<summary>
    /// Corresponds to command line option `--zipfile`
    ///</summary>
    public virtual bool ZipFile => false;

    ///<summary>
    /// Corresponds to command line option `--methodpoint`
    ///</summary>
    public virtual bool MethodPoint => false;

    ///<summary>
    /// Corresponds to command line option `--single`
    ///</summary>
    public virtual bool SingleVisit => false;

    ///<summary>
    /// Corresponds to command line option `--linecover`
    ///</summary>
    public virtual bool LineCover => false;

    ///<summary>
    /// Corresponds to command line option `--branchcover`
    ///</summary>
    public virtual bool BranchCover => false;

    /// <summary>
    /// Gets the command line (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public IEnumerable<string> CommandLine => throw new NotImplementedException("CommandLine not used");

    /// <summary>
    /// Gets whether to expose a program return code (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public bool ExposeReturnCode => throw new NotImplementedException("ExposeReturnCode not used");

    ///<summary>
    /// Corresponds to command line option `--sourcelink`
    ///</summary>
    public virtual bool SourceLink => true;

    /// <summary>
    /// Gets whether to defer coverage reporting (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    public bool Defer => throw new NotImplementedException("Defer not used");

    ///<summary>
    /// Corresponds to command line option `-l, --localSource`
    ///</summary>
    public virtual bool LocalSource => true;

    ///<summary>
    /// Corresponds to command line option `-v, --visibleBranches`
    ///</summary>
    public virtual bool VisibleBranches => false;

    ///<summary>
    /// Corresponds to command line option `--showstatic[=VALUE]`
    ///</summary>
    public virtual string ShowStatic => String.Empty;

    ///<summary>
    /// Corresponds to command line option `--showGenerated`
    ///</summary>
    public virtual bool ShowGenerated => false;

    ///<summary>
    /// Corresponds to command line option `-q`
    ///</summary>
    public virtual System.Diagnostics.TraceLevel Verbosity => System.Diagnostics.TraceLevel.Info;

    ///<summary>
    /// Corresponds to command line option `--trivia`
    ///</summary>
    public virtual bool Trivia => false;

    ///<summary>
    /// Corresponds to dotnet test option `/p:AltCoverOutputRoot`
    ///</summary>
    public virtual string OutputRoot => String.Empty;
  }

  /// <summary>
  /// Collection stage options; override defaults as required
  /// </summary>
  public class CollectOptions : Abstract.ICollectOptions
  {
    /// <summary>
    /// Gets the recorder directory (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public string RecorderDirectory => throw new NotImplementedException("RecorderDirectory not used");

    /// <summary>
    /// Gets the working directory (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public string WorkingDirectory => throw new NotImplementedException("WorkingDirectory not used");

    /// <summary>
    /// Gets the programe to run (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    public string Executable => throw new NotImplementedException("Executable not used");

    ///<summary>
    /// Corresponds to command line option `-l, --lcovReport=VALUE`
    ///</summary>
    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Lcov is a name")]
    public virtual string LcovReport => String.Empty;

    ///<summary>
    /// Corresponds to command line option `-t, --threshold=VALUE`
    ///</summary>
    public virtual string Threshold => String.Empty;

    ///<summary>
    /// Corresponds to command line option `-c, --cobertura=VALUE`
    ///</summary>
    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Lcov is a name")]
    public virtual string Cobertura => String.Empty;

    ///<summary>
    /// Corresponds to command line option `-o, --outputFile=VALUE`
    ///</summary>
    public virtual string OutputFile => String.Empty;

    /// <summary>
    /// Gets the command line (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public IEnumerable<string> CommandLine => throw new NotImplementedException("CommandLine not used");

    /// <summary>
    /// Gets whether to expose a program return code (overridden by the MSBuild integration)
    /// </summary>
    [SuppressMessage("Gendarme.Rules.BadPractice",
                     "DoNotForgetNotImplementedMethodsRule",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Design",
                     "CA1065:DoNotRaiseExceptionsInUnexpectedLocations",
                     Justification = "Means 'do not use'")]
    [SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly",
                  Justification = "Property name")]
    public bool ExposeReturnCode => throw new NotImplementedException("ExposeReturnCode not used");

    ///<summary>
    /// Corresponds to command line option `--teamcity[=VALUE]`
    ///</summary>
    public virtual string SummaryFormat => String.Empty;

    ///<summary>
    /// Corresponds to command line option `-q`
    ///</summary>
    public virtual System.Diagnostics.TraceLevel Verbosity => System.Diagnostics.TraceLevel.Info;
  }
}