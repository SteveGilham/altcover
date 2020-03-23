using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Reflection;

using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollection;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollector.InProcDataCollector;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.InProcDataCollector;

namespace AltCover
{
  public class DataCollector : InProcDataCollection
  {
    // Use the Null Object pattern here
    private static IEnumerable<Type> RecorderInstance
    {
      get
      {
        var rec =
        AppDomain.CurrentDomain.GetAssemblies()
            .Where(a => a.GetName().Name == "AltCover.Recorder.g")
            .FirstOrDefault();
        if (rec == null)
        {
          Debug.WriteLine("Recorder not found");
          yield break;
        }
        else
        {
          var i = rec.GetTypes()
              .Where(t => t.Name == "Instance")
              .FirstOrDefault();
          if (i == null)
          {
            Debug.WriteLine("Instance not found");
            yield break;
          }
          else
          {
            yield return i;
          }
        }
      }
    }

    private static void Supervise()
    {
      RecorderInstance.ToList().ForEach(
        i =>
        {
          var supervision = i.GetProperty("supervision", 
                                  BindingFlags.Static | BindingFlags.NonPublic);
          if (supervision == null)
          {
            Debug.WriteLine("Supervision not found");
          }
          else
          {
            supervision.SetValue(null, true);
          }
        }
      );
    }

    public void Initialize(IDataCollectionSink dataCollectionSink)
    {
      Debug.WriteLine("Initialize {0}", dataCollectionSink);
      Supervise();
    }

    public void TestCaseEnd(TestCaseEndArgs testCaseEndArgs)
    {
      Debug.WriteLine("TestCaseEnd {0}", testCaseEndArgs);
    }

    public void TestCaseStart(TestCaseStartArgs testCaseStartArgs)
    {
      Debug.WriteLine("TestCaseStart {0}", testCaseStartArgs);
    }

    public void TestSessionEnd(TestSessionEndArgs testSessionEndArgs)
    {
      Debug.WriteLine("TestSessionEnd {0}", testSessionEndArgs);
      RecorderInstance.ToList().ForEach(
        i =>
        {
          var flush = i.GetMethod("FlushFinish", BindingFlags.Static | BindingFlags.Public);
          if (flush == null)
          {
            Debug.WriteLine("Flush not found");
          }
          else
          {
            flush.Invoke(null, null);
          }
        }
      );
    }

    public void TestSessionStart(TestSessionStartArgs testSessionStartArgs)
    {
      Debug.WriteLine("TestSessionStart : {0}", testSessionStartArgs);
      Supervise();
    }
  }
}