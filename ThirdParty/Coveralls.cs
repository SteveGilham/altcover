using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Coveralls
{
  internal enum ServiceType
  {
    AppVeyor,
    Jenkins,
    Unknown
  }

  public class CoverallsBootstrap
  {
    private ServiceType _service;

    public string ServiceJobId
    {
      get
      {
        switch (_service)
        {
          case ServiceType.AppVeyor:
            return Environment.GetEnvironmentVariable("APPVEYOR_JOB_ID");

          case ServiceType.Jenkins:
            return Environment.GetEnvironmentVariable("BUILD_NUMBER");

          default:
            return DateTime.UtcNow.ToString("yyMMdd-HHmmss");
        }
      }
    }
  }
}