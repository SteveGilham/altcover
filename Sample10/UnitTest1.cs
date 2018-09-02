using System;
using Xunit;

namespace sample10.core
{
    public class UnitTest1
    {
        [Fact]
        public void Test1()
        {
            var now = DateTime.Now;
            if (now.Year > 2000)
            {
                Console.WriteLine("Where is my rocket pack?");
            }
            else
            {
                Console.WriteLine("Twentieth Century, boy!");
            }

            Assert.True(now.Year > 2000);
        }
    }
}