using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace Sample5
{
  public partial class Class1
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
        return Interior(l.Select(f).Last(), 6);
      }).Sum();
    }
  }
}