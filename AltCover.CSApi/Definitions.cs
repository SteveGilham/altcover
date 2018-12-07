using System;
using System.Linq;

namespace AltCover
{
    public class CollectArgs
    {
        public string RecorderDirectory { get; set; }
        public string WorkingDirectory { get; set; }
        public string Executable { get; set; }
        public string LcovReport { get; set; }
        public string Threshold { get; set; }
        public string Cobertura { get; set; }
        public string OutputFile { get; set; }

        [Obsolete("Please use AltCover.CollectArgs.Command instead instead.")]
        public string CommandLine { get; set; }

        public string[] Command { get; set; }

        internal CollectParams ToParameters()
        {
            return new CollectParams(
                RecorderDirectory,
                WorkingDirectory,
                Executable,
                LcovReport,
                Threshold,
                Cobertura,
                OutputFile,
#pragma warning disable 0618
                CommandLine,
#pragma warning restore 0618
                Command
                );
        }

        public static CollectArgs Default
        {
            get
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
#pragma warning disable 0618
                    CommandLine = string.Empty,
#pragma warning restore 0618
                    Command = new string[] { }
                };
            }
        }

        public string[] Validate(bool afterPreparation)
        {
            return ToParameters().Validate(afterPreparation);
        }
    }

    public class PrepareArgs
    {
        public string InputDirectory { get; set; }
        public string OutputDirectory { get; set; }
        public string[] SymbolDirectories { get; set; }
        public string[] Dependencies { get; set; }
        public string[] Keys { get; set; }
        public string StrongNameKey { get; set; }
        public string XmlReport { get; set; }
        public string[] FileFilter { get; set; }
        public string[] AssemblyFilter { get; set; }
        public string[] AssemblyExcludeFilter { get; set; }
        public string[] TypeFilter { get; set; }
        public string[] MethodFilter { get; set; }
        public string[] AttributeFilter { get; set; }
        public string[] PathFilter { get; set; }
        public string[] CallContext { get; set; }

        public bool OpenCover { get; set; }
        public bool InPlace { get; set; }
        public bool Save { get; set; }
        public bool Single { get; set; }
        public bool LineCover { get; set; }
        public bool BranchCover { get; set; }

        [Obsolete("Please use AltCover.PrepareArgs.Command instead instead.")]
        public string CommandLine { get; set; }

        public string[] Command { get; set; }

        internal PrepareParams ToParameters()
        {
            return new PrepareParams(
                        InputDirectory,
                        OutputDirectory,
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
                        Single,
                        LineCover,
                        BranchCover,
#pragma warning disable 0618
                        CommandLine,
#pragma warning restore 0618
                        Command
                );
        }

        public static PrepareArgs Default
        {
            get
            {
                return new PrepareArgs
                {
                    InputDirectory = string.Empty,
                    OutputDirectory = string.Empty,
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
                    Single = false,
                    LineCover = false,
                    BranchCover = false,
#pragma warning disable 0618
                    CommandLine = string.Empty,
#pragma warning restore 0618
                    Command = new string[] { }
                };
            }
        }

        public string[] Validate()
        {
            return ToParameters().Validate();
        }
    }

    public class LogArgs
    {
        public Action<String> Info { get; set; }
        public Action<String> Warn { get; set; }
        public Action<String> Error { get; set; }
        public Action<String> Echo { get; set; }

        internal Logging ToParameters()
        {
            return new Logging(
                Logging.ActionAdapter(Info),
                Logging.ActionAdapter(Warn),
                Logging.ActionAdapter(Error),
                Logging.ActionAdapter(Echo));
        }

        public static LogArgs Default
        {
            get
            {
                return new LogArgs
                {
                    Info = x => { },
                    Warn = x => { },
                    Error = x => { },
                    Echo = x => { }
                };
            }
        }
    }

    public static class CSApi
    {
        public static int Prepare(PrepareArgs p, LogArgs l)
        {
            return Api.Prepare(p.ToParameters(), l.ToParameters());
        }

        public static int Collect(CollectArgs c, LogArgs l)
        {
            return Api.Collect(c.ToParameters(), l.ToParameters());
        }

        public static string Ipmo()
        {
            return Api.Ipmo();
        }

        public static string Version()
        {
            return Api.Version();
        }

        public static string ToTestArguments(PrepareArgs p,
                                             CollectArgs c)
        {
            return DotNet.ToTestArguments(p.ToParameters(), c.ToParameters());
        }

        public static string[] ToTestArgumentList(PrepareArgs p,
                                             CollectArgs c)
        {
            return DotNet.ToTestArgumentList(p.ToParameters(), c.ToParameters()).ToArray();
        }
    }
}