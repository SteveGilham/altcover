using System;
using System.Linq;

namespace AltCover.Parameters
{
    public interface ICollectArgs
    {
        string RecorderDirectory { get; }
        string WorkingDirectory { get; }
        string Executable { get; }
        string LcovReport { get; }
        string Threshold { get; }
        string Cobertura { get; }
        string OutputFile { get; }
        bool ExposeReturnCode { get; }
        string[] CommandLine { get; }

        FSApi.CollectParams ToParameters();

        string[] Validate(bool afterPreparation);
    }

    public interface IPrepareArgs
    {
        string InputDirectory { get; }
        string OutputDirectory { get; }
        string[] SymbolDirectories { get; }
        string[] Dependencies { get; }
        string[] Keys { get; }
        string StrongNameKey { get; }
        string XmlReport { get; }
        string[] FileFilter { get; }
        string[] AssemblyFilter { get; }
        string[] AssemblyExcludeFilter { get; }
        string[] TypeFilter { get; }
        string[] MethodFilter { get; }
        string[] AttributeFilter { get; }
        string[] PathFilter { get; }
        string[] CallContext { get; }

        bool OpenCover { get; }
        bool InPlace { get; }
        bool Save { get; }
        bool Single { get; }
        bool LineCover { get; }
        bool BranchCover { get; }
        bool ExposeReturnCode { get; }
        bool SourceLink { get; }

        string[] CommandLine { get; }

        FSApi.PrepareParams ToParameters();

        string[] Validate();
    }

    public interface ILogArgs
    {
        Action<String> Info { get; }
        Action<String> Warn { get; }
        Action<String> Error { get; }
        Action<String> Echo { get; }

        FSApi.Logging ToParameters();
    }

    public interface ICLIArg
    {
        bool Force { get; }
    }

    public interface ICLIArg2 : ICLIArg
    {
        bool FailFast { get; }
    }
}

namespace AltCover.Parameters.Primitive
{
    public class CollectArgs : ICollectArgs
    {
        public string RecorderDirectory { get; set; }
        public string WorkingDirectory { get; set; }
        public string Executable { get; set; }
        public string LcovReport { get; set; }
        public string Threshold { get; set; }
        public string Cobertura { get; set; }
        public string OutputFile { get; set; }
        public string[] CommandLine { get; set; }
        public bool ExposeReturnCode { get; set; }

        public FSApi.CollectParams ToParameters()
        {
            var primitive = new AltCover.Primitive.CollectParams(
                RecorderDirectory,
                WorkingDirectory,
                Executable,
                LcovReport,
                Threshold,
                Cobertura,
                OutputFile,
                CommandLine,
                ExposeReturnCode
                );
            return FSApi.CollectParams.NewPrimitive(primitive);
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
                CommandLine = new string[] { },
                ExposeReturnCode = true
            };
        }

        public string[] Validate(bool afterPreparation)
        {
            return ToParameters().Validate(afterPreparation);
        }
    }

    public class PrepareArgs : IPrepareArgs
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
        public bool ExposeReturnCode { get; set; }
        public bool SourceLink { get; set; }

        public string[] CommandLine { get; set; }

        public FSApi.PrepareParams ToParameters()
        {
            var primitive = new AltCover.Primitive.PrepareParams(
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
                        CommandLine,
                        ExposeReturnCode,
                        SourceLink
                );
            return FSApi.PrepareParams.NewPrimitive(primitive);
        }

        public static PrepareArgs Create()
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
                CommandLine = { },

                ExposeReturnCode = true,
                SourceLink = false
            };
        }

        public string[] Validate()
        {
            return ToParameters().Validate();
        }
    }

    public class LogArgs : ILogArgs
    {
        public Action<String> Info { get; set; }
        public Action<String> Warn { get; set; }
        public Action<String> Error { get; set; }
        public Action<String> Echo { get; set; }

        public FSApi.Logging ToParameters()
        {
            var primitive = new AltCover.Primitive.Logging(
                FSApi.Logging.ActionAdapter(Info),
                FSApi.Logging.ActionAdapter(Warn),
                FSApi.Logging.ActionAdapter(Error),
                FSApi.Logging.ActionAdapter(Echo));
            return FSApi.Logging.NewPrimitive(primitive);
        }

        public static LogArgs Create()
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

    public class CLIArgs : ICLIArg2
    {
        public bool Force { get; set; }
        public bool FailFast { get; set; }
    }
}

namespace AltCover
{
    using AltCover.Parameters;

    public static class CSApi
    {
        public static int Prepare(IPrepareArgs p, ILogArgs l)
        {
            return Api.Prepare(p.ToParameters(), l.ToParameters());
        }

        public static int Collect(ICollectArgs c, ILogArgs l)
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

        private static DotNet.CLIArgs ToCLIArgs(ICLIArg args)
        {
            var force = DotNet.CLIArgs.NewForce(args.Force);
            switch (args)
            {
                case ICLIArg2 args2:
                    var failfast = DotNet.CLIArgs.NewFailFast(args2.FailFast);
                    return DotNet.CLIArgs.NewMany(new[] { force, failfast });

                default: return force;
            }
        }

        public static string ToTestArguments(IPrepareArgs p,
                                             ICollectArgs c,
                                             ICLIArg force)
        {
            return DotNet.ToTestArguments(p.ToParameters(),
                                          c.ToParameters(),
                                          ToCLIArgs(force));
        }

        public static string[] ToTestArgumentList(IPrepareArgs p,
                                                  ICollectArgs c,
                                                  ICLIArg force)
        {
            return DotNet.ToTestArgumentList(p.ToParameters(),
                                             c.ToParameters(),
                                             ToCLIArgs(force)).
            ToArray();
        }
    }
}