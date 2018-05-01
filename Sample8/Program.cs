using System;

namespace Sample8
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            Console.WriteLine("Starting");
            using (var stream = System.IO.File.Create(args[0]))
            {
                Console.WriteLine("Creating file");
            }
            Console.WriteLine("Created file");
            System.Threading.Thread.Sleep(1000);
            Console.WriteLine("Waited one second");
            System.Threading.Thread.Sleep(1000);
            Console.WriteLine("Waited one second");
            System.Threading.Thread.Sleep(1000);
            Console.WriteLine("Waited one second");

            System.IO.File.Delete(args[0]);
            Console.WriteLine("Deleted file");
            System.Threading.Thread.Sleep(1000);
            Console.WriteLine("Waited one second");
            System.Threading.Thread.Sleep(1000);
            Console.WriteLine("Waited one second");
        }
    }
}