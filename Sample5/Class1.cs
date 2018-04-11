using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace Sample5
{
    public class Class1
    {
        [ExcludeFromCodeCoverage]
        public int F1(string input)
        {
            // function Sample5.Class1.<>c.<F1>b__0_0
            Func<char, int> f = (c => { return (int)c; });

            // function Sample5.Class1.<>c_DisplayClass0_0.<F1>b__1
            return input.Select(x => f(x) % 6).Sum();
        }

        [ExcludeFromCodeCoverage]
        public IEnumerable<int> F2(string input)
        {
            // class Sample5.Class1.<F2>d__1
            foreach (char c in input)
            {
                yield return c;
            }
        }

        [ExcludeFromCodeCoverage]
        public async Task<string> F3(string input)
        {
            // class Sample5.Class1.<F3>d__2
            var f = new StreamReader(File.OpenRead(input));
            var r = await f.ReadToEndAsync();
            return r;
        }
    }
}
