using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Xml;
using System.Xml.Linq;

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
        var rec =
        AppDomain.CurrentDomain.GetAssemblies()
            .Where(a => a.GetName().Name == assembly)
            .FirstOrDefault();
        if (rec != null)
        {
          var i = rec.GetTypes()
              .Where(t => t.Name == "Instance")
              .FirstOrDefault();
          if (i != null)
          {
            yield return i;
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
      totals = instance.Aggregate(new PointCount(), (t, i) =>
         {
           var xml = i.GetProperty("ReportFile").GetValue(null).ToString();
           var doc = XDocument.Load(xml);
           var seqpnt = doc.Descendants("seqpnt").Count();
           var sp2 = doc.Descendants("SequencePoint").Count()
                   + doc.Descendants("MethodPoint").Count();
           t.Branch = doc.Descendants("BranchPoint").Count();
           t.Code = Math.Max(seqpnt, sp2);
           return t;
         });

      return instance.Any();
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
      totals = instance
        .Select(t => t.GetNestedType("I", System.Reflection.BindingFlags.NonPublic))
        .Where(t => t is object)
        .Aggregate(new PointCount(), (t, i) =>
      {
        var visits = i.GetProperty("visits", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static).GetValue(null)
         as IDictionary;

        lock (visits)
        {
          var indexes = visits.Values.Cast<IDictionary>()
                .SelectMany(d => d.Keys.Cast<int>());
          var total = indexes.Count();
          t.Branch = indexes.Count(index => index < 0);
          t.Code = total - t.Branch;
          return t;
        }
      });

      return instance.Any();
    }
  }
}