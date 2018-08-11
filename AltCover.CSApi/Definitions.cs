using System;

namespace AltCover
{
    public static class DotNetApi
    {
        public static string ToTestArguments(PrepareArgs p, 
                                             CollectArgs c,
                                             bool version = false,
                                             bool ipmo = false)
        {
            return DotNet.CSToTestArguments(p.ToParameters(), c.ToParameters(), version, ipmo);
        }
    }

    public class CollectArgs
    {
        public string RecorderDirectory { get; set; }
        public string WorkingDirectory { get; set; }
        public string Executable { get; set; }
        public string LcovReport { get; set; }
        public string Threshold { get; set; }
        public string Cobertura { get; set; }
        public string OutputFile { get; set; }

        public string CommandLine { get; set; }

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
                CommandLine
                );
        }
    }

    public class PrepareArgs
    {
        public string InputDirectory { get; set; }
        public string OutputDirectory { get; set; }
        public string[] SymbolDirectories { get; set; }
#if NETSTANDARD2_0
        public string[] Dependencies { get; set; }
#else
        public string[] Keys { get; set; }
        public string StrongNameKey { get; set; }
#endif
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
        public string CommandLine { get; set; }

        internal PrepareParams ToParameters()
        {
            return new PrepareParams(
                         InputDirectory,
                         OutputDirectory,
                         SymbolDirectories,
#if NETSTANDARD2_0
                         Dependencies,
#else
                         Keys,
                         StrongNameKey,
#endif
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
                        CommandLine
                );
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

        public static int Ipmo(LogArgs l)
        {
            return Api.Ipmo(l.ToParameters());
        }

        public static int Version(LogArgs l)
        {
            return Api.Version(l.ToParameters());
        }
    }
}