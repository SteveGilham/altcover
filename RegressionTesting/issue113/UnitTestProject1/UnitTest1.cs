using ConsoleApp14;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTestProject1
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void AddingTwoPositiveNumbersReturnsCorrectResult()
        {
            Assert.AreEqual(4, Program.Add(1, 3));
        }

        [TestMethod]
        public void AddingTwoNegativeNumbersReturnsCorrectResult()
        {
            Assert.AreEqual(-4, Program.Add(-1, -3));
        }
    }
}
