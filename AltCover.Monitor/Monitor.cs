﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Reflection;
using System.Xml;

#if !DEBUG
namespace AltCover
#else

namespace AltCover.Local
#endif
{
  /// <summary>
  /// A simple data type to hold sequence and branch point counts.
  /// </summary>
#pragma warning disable IDE0079 // Remove unnecessary suppression

  [SuppressMessage("Gendarme.Rules.Performance",
                   "OverrideValueTypeDefaultsRule",
                   Justification = "There is no use case")]
  [SuppressMessage("Microsoft.Performance",
                   "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes",
                   Justification = "There is no use case")]
  public struct PointCount
  {
    /// <summary>
    /// Count of sequence/method points.
    /// </summary>
#pragma warning disable IDE0079 // Remove unnecessary suppression

    [SuppressMessage("Gendarme.Rules.Design",
                     "AvoidVisibleFieldsRule",
                     Justification = "It's a struct, dude")]
    [SuppressMessage("Microsoft.Design",
                     "CA1051:DoNotDeclareVisibleInstanceFields",
                     Justification = "It's a struct, dude")]
    public int Code;

    /// <summary>
    /// Count of branch points.
    /// </summary>
#pragma warning disable IDE0079 // Remove unnecessary suppression

    [SuppressMessage("Gendarme.Rules.Design",
                 "AvoidVisibleFieldsRule",
                 Justification = "It's a struct, dude")]
    [SuppressMessage("Microsoft.Design",
                 "CA1051:DoNotDeclareVisibleInstanceFields",
                 Justification = "It's a struct, dude")]
    public int Branch;
  }

  internal sealed class Carrier : IDisposable
  {
    private readonly ArrayList data = new ArrayList();
    private bool disposedValue;

#pragma warning disable IDE0079 // Remove unnecessary suppression

    [SuppressMessage("Gendarme.Rules.Exceptions",
                     "UseObjectDisposedExceptionRule",
                     Justification = "There is no use case")]
    public T Add<T>(T item)
    {
      data.Add(item);
      return item;
    }

#pragma warning disable IDE0079 // Remove unnecessary suppression

    [SuppressMessage("Gendarme.Rules.Interoperability",
                     "DelegatesPassedToNativeCodeMustIncludeExceptionHandlingRule",
                     Justification = "Gendarme bug here")]
    private void Dispose(bool disposing)
    {
      if (!disposedValue)
      {
        if (disposing)
        {
          // dispose managed state (managed objects)
          foreach (var item in data)
          {
            (item as IDisposable)?.Dispose();
          }

          data.Clear();
        }

        // TODO: free unmanaged resources (unmanaged objects) and override finalizer
        // TODO: set large fields to null
        disposedValue = true;
      }
    }

    // // TODO: override finalizer only if 'Dispose(bool disposing)' has code to free unmanaged resources
    // ~Carrier()
    // {
    //     // Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
    //     Dispose(disposing: false);
    // }

    public void Dispose()
    {
      // Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
      Dispose(disposing: true);
      GC.SuppressFinalize(this);
    }
  }

  /// <summary>
  /// Provides real-time insights into the recording process.
  /// </summary>
  public static class Monitor
  {
    internal static string assembly = "AltCover.Recorder.g";

    // Use the Null Object pattern here
    private static IEnumerable<Type> TypeInstance(string name)
    {
      {
        Assembly rec = null;
        foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
        {
          if (a.GetName().Name == assembly)
          {
            rec = a;
            break;
          }
        }

        if (rec != null)
        {
          foreach (var i in rec.GetTypes())
          {
            if (i.Name == name)
            {
              yield return i;
              break;
            }
          }
        }
        yield break;
      }
    }

    /// <summary>
    /// Try to get the total visitable points
    /// </summary>
    /// <param name="totals">The total point counts if running under AltCover coverage</param>
    /// <returns>True if running under AltCover coverage</returns>
    public static bool TryGetPointTotals(out PointCount totals)
    {
      var instance = TypeInstance("Instance");
      totals = new PointCount();
      var found = false;
      foreach (var i in instance) // only 0 or 1 expected
      {
        var xml = i.GetProperty("ReportFile").GetValue(null, Type.EmptyTypes).ToString();
        using (var file = File.OpenRead(xml))
        {
          var doc = new XmlDocument();
          doc.Load(file);
          totals = CountVisitPoints(doc);
          found = true;
        }
      }

      return found;
    }

    /// <summary>
    /// Counts the visitable points in an XmlDocument
    /// </summary>
    /// <param name="doc">The XML document to scan</param>
    /// <returns>The value from the document</returns>
#pragma warning disable IDE0079 // Remove unnecessary suppression

    [SuppressMessage("Gendarme.Rules.Correctness",
                     "EnsureLocalDisposalRule",
                     Justification = "Carrier type does it")]
    [SuppressMessage("Gendarme.Rules.Performance",
                     "AvoidRepetitiveCallsToPropertiesRule",
                     Justification = "Once per instance")]
    [SuppressMessage("Gendarme.Rules.Design",
                     "PreferXmlAbstractionsRule",
                     Justification = "There is no use case")]
    internal static PointCount CountVisitPoints(XmlDocument doc)
    {
      var totals = new PointCount();
      using (var scans = new Carrier())
      {
        var seqpnt = scans.Add(doc.DocumentElement.SelectNodes("//seqpnt")).Count;
        var sp2 = scans.Add(doc.DocumentElement.SelectNodes("//SequencePoint")).Count;

        foreach (XmlElement m in scans.Add(doc.DocumentElement.SelectNodes("//Method")))
        {
          if (scans.Add(m.SelectNodes(".//SequencePoint")).Count == 0)
          {
            sp2++;
          }
        }

        totals.Branch = scans.Add(doc.DocumentElement).SelectNodes("//BranchPoint").Count;
        totals.Code = Math.Max(seqpnt, sp2);
        return totals;
      }
    }

    /// <summary>
    /// Try to get the total visited points
    /// </summary>
    /// <param name="totals">The visited point counts if running under AltCover coverage</param>
    /// <returns>True if running under AltCover coverage</returns>
    /// <remarks>Current implementation requires `dotnet test`, or other command-line testing with `--eager` NOT set, in which the cumulative visit numbers are available, rather than everything having been dumped to file instead.</remarks>
    public static bool TryGetVisitTotals(out PointCount totals)
    {
      var counter = TypeInstance("Counter");
      totals = new PointCount();
      var found = false;
      foreach (var t in counter)
      {
        var temp = (Int64)t.GetField("branchVisits",
           System.Reflection.BindingFlags.NonPublic |
           System.Reflection.BindingFlags.Public |
           System.Reflection.BindingFlags.Static).GetValue(null);
        totals.Branch = (int)temp;

        temp = (Int64)t.GetField("totalVisits",
           System.Reflection.BindingFlags.NonPublic |
           System.Reflection.BindingFlags.Public |
           System.Reflection.BindingFlags.Static).GetValue(null);
        totals.Code = (int)temp - totals.Branch;

        found = true;
      }

      return found;
    }
  }
}