using System;

// test program
// for report generation

namespace TouchTest
{
    internal class Program
    {
        private static void Main(string[] args)
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
        }
    }
}