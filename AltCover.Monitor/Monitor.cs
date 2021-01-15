﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using System.Xml.Linq;

namespace AltCover
{
  /// <summary>
  /// A simple data type tp hold sequence and branch point counts.
  /// </summary>
  public struct PointCount
  {
    public int Code;
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
        if (rec == null)
        {
          yield break;
        }
        else
        {
          var i = rec.GetTypes()
              .Where(t => t.Name == "Instance")
              .FirstOrDefault();
          if (i == null)
          {
          }
          else
          {
            yield return i;
          }
        }
      }
    }

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

    public static bool TryGetVisitTotals(out PointCount totals)
    {
      var instance = RecorderInstance;
      totals = instance.Select(t => t.Assembly.GetTypes()
                                    .Where(t2 => t2.FullName == "AltCover.Recorder.Instance.I")
                                    .FirstOrDefault())
               .Where(t => !ReferenceEquals(t, null))
        .Aggregate(new PointCount(), (t, i) =>
      {
        var visits = i.GetProperty("visits").GetValue(null)
         as IDictionary;
        var indexes = visits.Values.Cast<IDictionary>()
              .SelectMany(d => d.Keys.Cast<int>());
        var total = indexes.Count();
        t.Branch = indexes.Count(index => index < 0);
        t.Code = total - t.Branch;
        return t;
      });

      return instance.Any();
    }
  }
}