using System;

namespace Sample14
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
            var x = new Sample5.Class1();
            Console.WriteLine(x.F1("Hello World!"));
        }
    }
}