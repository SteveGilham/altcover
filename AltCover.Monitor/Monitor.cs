using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Reflection;
using System.Xml;

namespace AltCover
{
  /// <summary>
  /// A simple data type tp hold sequence and branch point counts.
  /// </summary>
#pragma warning disable IDE0079 // Remove unnecessary suppression

  [SuppressMessage("Gendarme.Rules.Performance",
                   "OverrideValueTypeDefaultsRule",
                   Justification = "There is no use case")]
  [SuppressMessage("Microsoft.Performance",
                   "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes",
                   Justification = "There is no use case")]
#pragma warning restore IDE0079 // Remove unnecessary suppression
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
#pragma warning restore IDE0079 // Remove unnecessary suppression
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
#pragma warning restore IDE0079 // Remove unnecessary suppression
    public int Branch;
  }

  /// <summary>
  /// Provides real-time insights into the recording process.
  /// </summary>
  public static class Monitor
  {
    internal static string assembly = "AltCover.Recorder.g";

    // Use the Null Object pattern here
    private static IEnumerable<Type> RecorderInstance
    {
      get
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
            if (i.Name == "Instance")
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
      var instance = RecorderInstance;
      totals = new PointCount();
      var found = false;
      foreach (var i in instance)
      {
        var xml = i.GetProperty("ReportFile").GetValue(null, Type.EmptyTypes).ToString();
        using (var file = File.OpenRead(xml))
        {
          var doc = new XmlDocument();
          doc.Load(file);
          var seqpnt = doc.DocumentElement.SelectNodes("//seqpnt").Count;
          var sp2 = doc.DocumentElement.SelectNodes("//SequencePoint").Count;

          foreach (XmlNode m in doc.DocumentElement.SelectNodes("//Method"))
          {
            if (m.SelectSingleNode("//SequencePoint") is null)
              sp2++;
          }

          totals.Branch = doc.DocumentElement.SelectNodes("//BranchPoint").Count;
          totals.Code = Math.Max(seqpnt, sp2);
          found = true;
        }
      }

      return found;
    }

    /// <summary>
    /// Try to get the total visited points
    /// </summary>
    /// <param name="totals">The visited point counts if running under AltCover coverage</param>
    /// <returns>True if running under AltCover coverage</returns>
    /// <remarks>Current implementation requires `dotnet test`, or other command-line testing with `--defer` set, in which the cumulative visit numbers are available, rather than everything having been dumped to file instead.</remarks>
    public static bool TryGetVisitTotals(out PointCount totals)
    {
      var instance = RecorderInstance;
      totals = new PointCount();
      var found = false;
      foreach (var i in instance)
      {
        found = true;
        var inner = i.GetNestedType("I", System.Reflection.BindingFlags.NonPublic);
        var visits = inner.GetProperty("visits",
           System.Reflection.BindingFlags.NonPublic |
           System.Reflection.BindingFlags.Public |
           System.Reflection.BindingFlags.Static).GetValue(null, Type.EmptyTypes)
         as IDictionary;

        lock (visits)
        {
          foreach (var v in visits.Values)
          {
            var innerV = v as IDictionary;
            foreach (var k in innerV.Keys)
            {
              if ((int)k < 0)
                totals.Branch++;
              else
                totals.Code++;
            }
          }
        }
      }
      return found;
    }
  }
}