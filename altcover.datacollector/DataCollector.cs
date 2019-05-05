using System;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollection;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollector.InProcDataCollector;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.InProcDataCollector;

namespace AltCover
{
    public class DataCollector : InProcDataCollection
    {
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
                    var supervision = i.GetProperty("Supervision", BindingFlags.Static | BindingFlags.Public);
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

        public void Initialize(IDataCollectionSink _dataCollectionSink)
        {
            Debug.WriteLine("Initialize");
            Supervise();
        }

        public void TestCaseEnd(TestCaseEndArgs _testCaseEndArgs)
        { }

        public void TestCaseStart(TestCaseStartArgs _testCaseStartArgs)
        {
        }

        public void TestSessionEnd(TestSessionEndArgs _testSessionEndArgs)
        {
            Debug.WriteLine("TestSessionEnd");
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

        public void TestSessionStart(TestSessionStartArgs _testSessionStartArgs)
        {
            Debug.WriteLine("TestSessionStart");
            Supervise();
        }
    }
}