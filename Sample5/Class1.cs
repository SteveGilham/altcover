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
            // function Sample5.Class1+<>c.<F1>b__0_0
            Func<char, int> f = (c => { return (int)c; });

            // function Sample5.Class1.<F1>g__Interior|0_1
            int Interior(int a, int b)
            {
                // Sample5.Class1.<F1>g__Recursive|0_3
                int Recursive(int c)
                {
                    return c * c;
                }

                return Recursive(a) % b;
            }

            // function Sample5.Class1+<>c__DisplayClass0_0.<F1>b__1
            // function Sample5.Class1+<>c__DisplayClass0_0.<F1>b__2
            return input.Select(x =>
            {
                var l = new List<char> { x };
                return Interior(l.Select(c => f(c)).Last(), 6);
            }).Sum();
        }

        [ExcludeFromCodeCoverage]
        public IEnumerable<int> F2(string input)
        {
            // class Sample5.Class1+<F2>d__1
            foreach (char c in input)
            {
                yield return c;
            }
        }

        [ExcludeFromCodeCoverage]
        public async Task<string> F3(string input)
        {
            // class Sample5.Class1+<F3>d__2
            var f = new StreamReader(File.OpenRead(input));
            var r = await f.ReadToEndAsync();
            return r;
        }

        private class Inner
        {
            [ExcludeFromCodeCoverage]
            public int G1(string input)
            {
                // function Sample5.Class1+Inner+<>c.<G1>b__0_0
                Func<char, int> f = (c => { return (int)c; });

                // function Sample5.Class1.<F1>g__Interior|0_1
                int Interior(int a, int b)
                {
                    // Sample5.Class1.<F1>g__Recursive|0_3
                    int Recursive(int c)
                    {
                        return c * c;
                    }

                    return Recursive(a) % b;
                }

                // function Sample5.Class1+Inner+<>c__DisplayClass0_0.<IF1>b__1
                // function Sample5.Class1+Inner+<>c__DisplayClass0_0.<IF1>b__2
                return input.Select(x =>
                {
                    var l = new List<char> { x };
                    return Interior(l.Select(c => f(c)).Last(), 6);
                }).Sum();
            }

            [ExcludeFromCodeCoverage]
            public IEnumerable<int> G2(string input)
            {
                // class Sample5.Class1+Inner+<G2>d__1
                foreach (char c in input)
                {
                    yield return c;
                }
            }

            [ExcludeFromCodeCoverage]
            public async Task<string> G3(string input)
            {
                // class Sample5.Class1+Inner+<G3>d__2
                var f = new StreamReader(File.OpenRead(input));
                var r = await f.ReadToEndAsync();
                return r;
            }

            public void G3(int label)
            {
                Console.WriteLine(2 * label);
            }
        }
    }
}