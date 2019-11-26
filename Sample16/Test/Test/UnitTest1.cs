using System.Collections.Generic;
using Sample16;
using Xunit;

namespace Test
{
    public class UnitTest1
    {
        private string foobar(string t)
        {
            var into = new List<string>() { t };
            var outof = Foo.Bar(into);
            if (t == null)
            {
                Assert.Empty(outof);
                return "d";
            }
            Assert.Single(outof);
            return outof[0];
        }

        [Fact]
        public void AtoA()
        {
            Assert.Equal("a", foobar("a"));
        }

        [Fact]
        public void BtoB()
        {
            Assert.Equal("b", foobar("b"));
        }

        [Fact]
        public void CtoC()
        {
            Assert.Equal("c", foobar("c"));
        }

        [Fact]
        public void NulltoD()
        {
            Assert.Equal("d", foobar(null));
        }

        [Fact]
        public void AnytoD()
        {
            Assert.Equal("d", foobar("any"));
        }
    }
}