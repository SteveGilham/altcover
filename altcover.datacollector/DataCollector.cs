using System;
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
    [SuppressMessage("Gendarme.Rules.Correctness", "MethodCanBeMadeStaticRule"),
     SuppressMessage("Gendarme.Rules.Smells", "RelaxedAvoidCodeDuplicatedInSameClassRule"),
     SuppressMessage("Microsoft.Performance", "CA1822")]
        private void Supervise()
        {
            var rec =
            AppDomain.CurrentDomain.GetAssemblies()
                .Where(a => a.GetName().Name == "AltCover.Recorder.g")
                .FirstOrDefault();
            if (rec == null)
            {
                Debug.WriteLine("Recorder not found");
            }
            else
            {
                var i = rec.GetTypes()
                    .Where(t => t.Name == "Instance")
                    .FirstOrDefault();
                if (i == null)
                {
                    Debug.WriteLine("Instance not found");
                }
                else
                {
                    var supervision = i.GetProperty("supervision", BindingFlags.Static | BindingFlags.Public);
                    if (supervision == null)
                    {
                        Debug.WriteLine("Supervision not found");
                    }
                    else
                    {
                        supervision.SetValue(null, true);
                    }
                }
            }
        }

    public void Initialize(IDataCollectionSink dataCollectionSink)
        {
      Debug.WriteLine("Initialize {0}", dataCollectionSink);
            Supervise();
        }

    public void TestCaseEnd(TestCaseEndArgs testCaseEndArgs)
    {
#if DEBUG
      Debug.WriteLine("TestCaseEnd {0}", testCaseEndArgs);
#endif
    }

    public void TestCaseStart(TestCaseStartArgs testCaseStartArgs)
        {
#if DEBUG
      Debug.WriteLine("TestCaseStart {0}", testCaseStartArgs);
#endif
        }

    public void TestSessionEnd(TestSessionEndArgs testSessionEndArgs)
        {
      Debug.WriteLine("TestSessionEnd {0}", testSessionEndArgs);
            var rec =
            AppDomain.CurrentDomain.GetAssemblies()
                .Where(a => a.GetName().Name == "AltCover.Recorder.g")
                .FirstOrDefault();
            if (rec == null)
            {
                Debug.WriteLine("Recorder not found");
            }
            else
            {
                var i = rec.GetTypes()
                    .Where(t => t.Name == "Instance")
                    .FirstOrDefault();
                if (i == null)
                {
                    Debug.WriteLine("Instance not found");
                }
                else
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
            }
        }

    public void TestSessionStart(TestSessionStartArgs testSessionStartArgs)
        {
      Debug.WriteLine("TestSessionStart : {0}", testSessionStartArgs);
            Supervise();
        }
    }
}