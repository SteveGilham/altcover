using NUnit.Framework;

namespace Tests
{
    public class Tests
    {
        [SetUp]
        public void Setup()
        {
        }

        public int Ternary (bool select, int left, int right)
        {
            return select ? left : right;
        }

        [Test]
        public void Test1()
        {
            Assert.That (Ternary(true, 1, 6), Is.EqualTo(1));
        }
    }
}