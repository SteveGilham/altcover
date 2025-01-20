using System;
using System.ComponentModel;
using System.Collections.Generic;

#if NETSTANDARD2_1

#pragma warning disable CS1591 // Missing XML comment for publicly visible type or member

#pragma warning disable IDE0130 // Namespace does not match folder structure
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
#pragma warning disable CA1822 // Mark members as static
    public void Append(string _)
    { }

    public void Append(IProcessArgument _)
    { }

    public IEnumerable<IProcessArgument> Skip(int _)
    { return null; }

    public IEnumerable<IProcessArgument> Take(int _)
    { return null; }
#pragma warning restore CA1822 // Mark members as static
  }

  [EditorBrowsable(EditorBrowsableState.Never)]
  public interface IProcessArgument
  { }

  [EditorBrowsable(EditorBrowsableState.Never)]
  public class FilePath
  {
#pragma warning disable CA1822 // Mark members as static
    public FilePath GetFilename()
    { return null; }

    public string FullPath
    { get { return String.Empty; } }
#pragma warning restore CA1822 // Mark members as static
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
    public CakeAliasCategoryAttribute(string _)
    { }
  }
}

namespace Cake.Common.Tools.DotNet
{
  [EditorBrowsable(EditorBrowsableState.Never)]
  public static class DotNetAliases
  {
    public static void DotNetTest(
#pragma warning disable IDE0060 // Remove unused parameter
      Cake.Core.ICakeContext context,
      string fullPath,
      Cake.Common.Tools.DotNet.Test.DotNetTestSettings testSettings)
#pragma warning restore IDE0060 // Remove unused parameter
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