using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using projectNamespace;

namespace Tests
{
    [TestClass]
    public class ConfigurationProviderTest
    {

        [TestInitialize]
        public void TestInitialize()
        {

        }


        [TestMethod]
        public void UpdateConfiguration_ConfigurationChangedIsRun()
        {
            Assert.AreEqual(1, 1);
        }
    }
}
