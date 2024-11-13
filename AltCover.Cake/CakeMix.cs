using System;
using System.ComponentModel;
using System.Collections.Generic;

#if NETSTANDARD2_1

#pragma warning disable CS1591 // Missing XML comment for publicly visible type or member

namespace Cake.Core
{
  [EditorBrowsable(EditorBrowsableState.Never)]
  public interface ICakeContext
  {
    Cake.Core.Diagnostics.ICakeLog Log { get; }
  }
}

namespace Cake.Core.Diagnostics
{
  [EditorBrowsable(EditorBrowsableState.Never)]
  public interface ICakeLog
  {
    void Write(Verbosity v, LogLevel l, string format);
  }

  [EditorBrowsable(EditorBrowsableState.Never)]
  public enum Verbosity
  {
    Normal,
    Verbose
  }

  [EditorBrowsable(EditorBrowsableState.Never)]
  public enum LogLevel
  {
    Information,
    Error,
    Warning,
    Verbose
  }
}

namespace Cake.Core.IO
{
  [EditorBrowsable(EditorBrowsableState.Never)]
  public class ProcessArgumentBuilder
  {
    public void Append(string s)
    { }

    public void Append(IProcessArgument s)
    { }

    public IEnumerable<IProcessArgument> Skip(int n)
    { return null; }

    public IEnumerable<IProcessArgument> Take(int n)
    { return null; }
  }

  [EditorBrowsable(EditorBrowsableState.Never)]
  public interface IProcessArgument
  { }

  [EditorBrowsable(EditorBrowsableState.Never)]
  public class FilePath
  {
    public FilePath GetFilename()
    { return null; }

    public string FullPath
    { get { return String.Empty; } }
  }
}

namespace Cake.Core.Annotations
{
  [EditorBrowsable(EditorBrowsableState.Never)]
  [AttributeUsage(AttributeTargets.Method)]
  public class CakeMethodAliasAttribute : Attribute
  { }

  [EditorBrowsable(EditorBrowsableState.Never)]
  [AttributeUsage(AttributeTargets.All)]
  public class CakeAliasCategoryAttribute : Attribute
  {
    public CakeAliasCategoryAttribute(string s)
    { }
  }
}

namespace Cake.Common.Tools.DotNet
{
  [EditorBrowsable(EditorBrowsableState.Never)]
  public static class DotNetAliases
  {
    public static void DotNetTest(
      Cake.Core.ICakeContext context,
      string fullPath,
      Cake.Common.Tools.DotNet.Test.DotNetTestSettings testSettings)
    { }
  }
}

namespace Cake.Common.Tools.DotNet.Test
{
  [EditorBrowsable(EditorBrowsableState.Never)]
  public class DotNetTestSettings
  {
    public Func<Cake.Core.IO.ProcessArgumentBuilder, Cake.Core.IO.ProcessArgumentBuilder> ArgumentCustomization { get; set; }
  }
}

#endif