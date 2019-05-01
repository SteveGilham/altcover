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
            //System.IO.File.WriteAllText(@"C:\Users\steve\Documents\GitHub\altcover\Initialize.txt",
            //                            "Initialize!");
            //Debug.WriteLine("Test Session Initialize");
            //AppDomain.CurrentDomain.GetAssemblies().ToList().ForEach(a => Debug.WriteLine(a));

            //sprintf "Test Session Initialize" |> Debug.WriteLine
            //
            //|> Seq.iter(sprintf "    %A" >> Debug.WriteLine)
        }

        public void TestCaseEnd(TestCaseEndArgs _testCaseEndArgs)
        { }

        public void TestCaseStart(TestCaseStartArgs _testCaseStartArgs)
        {
            Debug.WriteLine("Test Case Start");
            AppDomain.CurrentDomain.GetAssemblies().ToList().ForEach(a => Debug.WriteLine(a));
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