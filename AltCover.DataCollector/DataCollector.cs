﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Reflection;

using Microsoft.VisualStudio.TestPlatform.ObjectModel;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollection;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.DataCollector.InProcDataCollector;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.InProcDataCollector;
using Microsoft.VisualStudio.TestPlatform.Utilities;

[assembly: System.Resources.NeutralResourcesLanguageAttribute("en-GB")]
[assembly: SuppressMessage("AltCode.Rules.General",
                           "JustifySuppressionRule",
                           Scope = "member", // MethodDefinition
                           Target = "AltCover.Resources.Resources::.ctor()",
                           Justification = "Compiler generated")]

namespace AltCover
{
  public class DataCollector : InProcDataCollection
  {
    private bool supervising;

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
          if (EqtTrace.IsWarningEnabled)
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
            if (EqtTrace.IsWarningEnabled)
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

    private void Supervise()
    {
      RecorderInstance.ToList().ForEach(
        i =>
        {
          var supervision = i.GetField("supervision",
                                  BindingFlags.Static | BindingFlags.NonPublic);
          if (supervision == null)
          {
            if (EqtTrace.IsWarningEnabled)
            {
              EqtTrace.Warning(Resources.Resources.SupervisionNotFound);
            }
          }
          else
          {
            supervision.SetValue(null, true);
            supervising = true;
          }
        }
      );
    }

    public void Initialize(IDataCollectionSink dataCollectionSink)
    {
      supervising = false;
      Debug.WriteLine("Initialize {0}", dataCollectionSink);
      Supervise();
    }

    public void TestCaseEnd(TestCaseEndArgs testCaseEndArgs)
    {
      Debug.WriteLine("Debug TestCaseEnd {0} => {1}",
        testCaseEndArgs?.DataCollectionContext?.TestCase?.FullyQualifiedName,
        testCaseEndArgs?.TestOutcome);

      if (EqtTrace.IsInfoEnabled)
      {
        EqtTrace.Info("TestCaseEnd {0}", testCaseEndArgs);
      }
    }

    public void TestCaseStart(TestCaseStartArgs testCaseStartArgs)
    {
      if (!supervising) Supervise();
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
            if (EqtTrace.IsWarningEnabled)
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