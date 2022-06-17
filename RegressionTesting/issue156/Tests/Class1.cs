using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using projectNamespace;

namespace Tests
{
    /// <summary>
    /// Test fixrure
    /// </summary>
    [TestClass]
    public class ConfigurationProviderTest
    {

        /// <summary>
        /// Test setup
        /// </summary>
        [TestInitialize]
        public void TestInitialize()
        {

        }

        /// <summary>
        /// Test.
        /// </summary>
        [TestMethod]
        public void UpdateConfiguration_ConfigurationChangedIsRun()
        {
            Assert.AreEqual(1, 1);
        }
    }
}
