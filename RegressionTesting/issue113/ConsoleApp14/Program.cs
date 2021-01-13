using System;

namespace ConsoleApp14
{
    public class Program
    {
        public static void Main() { }

        public static int Add(int a, int b)
        {
            if (a > 100)
            {
                if (b > 100)
                {
                    return 34;
                }

                return 1337;
            }

            return a + b;
        }
    }
}
