using Sample16;
using Xunit;

namespace Test
{
    public class UnitTest1
    {
        [Fact]
        public void AtoA()
        {
            Assert.Equal("a", Foo.Bar("a"));
        }

        [Fact]
        public void BtoB()
        {
            Assert.Equal("b", Foo.Bar("b"));
        }

        [Fact]
        public void CtoC()
        {
            Assert.Equal("c", Foo.Bar("c"));
        }

        [Fact]
        public void NulltoD()
        {
            Assert.Equal("d", Foo.Bar(null));
        }

        [Fact]
        public void AnytoD()
        {
            Assert.Equal("d", Foo.Bar("any"));
        }
    }
}