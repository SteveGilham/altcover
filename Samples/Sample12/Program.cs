using System;

namespace Sample12
{
    internal class Program
    {
        private static int Main(string[] args)
        {
            Console.WriteLine("{ '" + String.Join("'; '", args) + "' }");
            return args.Length; // return an integer exit code
        }
    }
}