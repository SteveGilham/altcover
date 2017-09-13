using System;

// test program
// for report generation

namespace TouchTest
{
    class Program
    {
        static void Main(string[] args)
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