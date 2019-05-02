using System;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollection;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollector.InProcDataCollector;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.InProcDataCollector;

namespace AltCover.DataCollector
{
    public class Recorder : InProcDataCollection
    {
        public void Initialize(IDataCollectionSink _dataCollectionSink)
        {
        }

        public void TestCaseEnd(TestCaseEndArgs _testCaseEndArgs)
        { }

        public void TestCaseStart(TestCaseStartArgs _testCaseStartArgs)
        {
            Debug.WriteLine("Test Case Start");
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
                    var prop = i.GetProperty("Supervision", BindingFlags.Static | BindingFlags.NonPublic);
                    if (prop == null)
                    {
                        Debug.WriteLine("Supervision not found");
                    }
                    else
                    {
                        var value = prop.GetValue(null);
                        Debug.WriteLine("Supervision = " + value.ToString());
                    }
                }
            }
        }

        public void TestSessionEnd(TestSessionEndArgs _testSessionEndArgs)
        {
            //    sprintf "Test Session Initialize" |> Debug.WriteLine
            //    AppDomain.CurrentDomain.GetAssemblies()
            //|> Seq.iter(sprintf "    %A" >> Debug.WriteLine)
        }

        public void TestSessionStart(TestSessionStartArgs _testSessionStartArgs)
        {
            Debug.WriteLine("Test Session Start");
            AppDomain.CurrentDomain.GetAssemblies().ToList().ForEach(a => Debug.WriteLine(a));
        }
    }
}