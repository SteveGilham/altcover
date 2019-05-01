using System;
using System.Diagnostics;
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
            System.IO.File.WriteAllText(@"C:\Users\steve\Documents\GitHub\altcover\Initialize.txt",
                                        "Initialize!");
            //sprintf "Test Session Initialize" |> Debug.WriteLine
            //    AppDomain.CurrentDomain.GetAssemblies()
            //|> Seq.iter(sprintf "    %A" >> Debug.WriteLine)
        }

        public void TestCaseEnd(TestCaseEndArgs _testCaseEndArgs)
        { }

        public void TestCaseStart(TestCaseStartArgs _testCaseStartArgs)
        { }

        public void TestSessionEnd(TestSessionEndArgs _testSessionEndArgs)
        {
            //    sprintf "Test Session Initialize" |> Debug.WriteLine
            //    AppDomain.CurrentDomain.GetAssemblies()
            //|> Seq.iter(sprintf "    %A" >> Debug.WriteLine)
        }

        public void TestSessionStart(TestSessionStartArgs _testSessionStartArgs)
        {
            //System.IO.File.WriteAllText(@"C:\Users\steve\Documents\GitHub\altcover\datacollector.txt", "TestSessionStart!")
            //sprintf "Test Session Initialize" |> Debug.WriteLine
            //AppDomain.CurrentDomain.GetAssemblies()
            //|> Seq.iter(sprintf "    %A" >> Debug.WriteLine)
        }
    }
}