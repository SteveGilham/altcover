using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Test
{
    [TestClass]
    public class UnitTest1
    {
        [AssemblyInitialize]
        public static void AssemblyInit(TestContext context)
        {
            var workingDir = Path.Combine(SolutionRoot.Location, "Echo/bin/Debug/netcoreapp2.2");
            var targetDll = "Echo.dll";
            var proc = new Process
            {
                StartInfo =
                    {
                        FileName = "dotnet",
                        Arguments = Path.Combine(workingDir, targetDll),
                        WorkingDirectory = workingDir,
                        CreateNoWindow = true,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true
                    }
            };

            proc.Start();
            Thread.Sleep(1000);
        }

        [TestMethod]
        public void TestMethod1()
        {
            var token = Guid.NewGuid().ToString();
            TcpClient client = new TcpClient("127.0.0.1", 18000);
            NetworkStream stream = client.GetStream();
            using (StreamReader reader = new StreamReader(stream))
            using (StreamWriter writer = new StreamWriter(stream))
            {
                writer.WriteLine(token);
                writer.Flush();

                String response = reader.ReadLine();
                Assert.AreEqual(token, response);
            }
        }
    }
}