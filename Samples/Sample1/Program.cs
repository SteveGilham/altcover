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
			if (now.Year > 2023)
			{
				Console.WriteLine("Now "                      + String.Join("*", args));
			}
			else
			{
				Console.WriteLine("Th"        +        "en");
			}
		}
	}
}