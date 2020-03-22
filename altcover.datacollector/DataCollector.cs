using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Reflection;

using Microsoft.VisualStudio.TestPlatform.ObjectModel;
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
          if (EqtTrace.IsErrorEnabled)
          {
            // resgen /PublicClass /compile .\Resources\Resources.resx
            EqtTrace.Warning(Resources.Resources.RecorderNotFound);
          }
          yield break;
        }
        else
        {
          var i = rec.GetTypes()
              .Where(t => t.Name == "Instance")
              .FirstOrDefault();
          if (i == null)
          {
            if (EqtTrace.IsErrorEnabled)
            {
              EqtTrace.Warning(Resources.Resources.InstanceNotFound);
            }
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
          var supervision = i.GetProperty("Supervision", BindingFlags.Static | BindingFlags.Public);
          if (supervision == null)
          {
            if (EqtTrace.IsErrorEnabled)
            {
              EqtTrace.Warning(Resources.Resources.SupervisionNotFound);
            }
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
            if (EqtTrace.IsErrorEnabled)
            {
              EqtTrace.Warning(Resources.Resources.FlushNotFound);
            }
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