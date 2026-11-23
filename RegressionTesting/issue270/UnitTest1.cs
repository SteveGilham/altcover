using System.IO;
using NUnit.Framework;

namespace Tests
{
    public class Tests
    {
        public int Ternary (bool select, int left, int right)
        {
            return select ? left : right;
        }

        public int Uncovered (int value)
        {
            var doubled = value * 2;
            var tripled = value * 3;
            return doubled + tripled;
        }

        [Test]
        public void Test1()
        {
            Assert.That (Ternary(true, 1, 6), Is.EqualTo(1));
        }

        [Test]
        public void DataFileIsBesideTheTests()
        {
            var data = Path.Combine(TestContext.CurrentContext.TestDirectory, "Data", "Foo.txt");
            Assert.That (File.ReadAllText(data).Trim(), Is.EqualTo("issue270 data"));
        }
    }
}
