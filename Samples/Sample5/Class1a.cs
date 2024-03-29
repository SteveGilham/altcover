﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace Sample5
{
  public partial class Class1
  {
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

    [ExcludeFromCodeCoverage]
    private class Inner
    {
      [ExcludeFromCodeCoverage]
      public int G1(string input)
      {
        // function Sample5.Class1+Inner+<>c.<G1>b__0_0
        Func<char, int> f = (c => { return 5 + (int)c; });

        T[] InteriorToArray<T>(T v)
        {
          return new[] { v };
        }

        // function Sample5.Class1.<F1>g__Interior|0_1
        int Interior(int a, int b)
        {
          // Sample5.Class1.<F1>g__Recursive|0_3
          int Recursive(int c)
          {
            return c * c * InteriorToArray(c).Length;
          }

          return Recursive(a) % b;
        }

        // function Sample5.Class1+Inner+<>c__DisplayClass0_0.<IF1>b__1
        // function Sample5.Class1+Inner+<>c__DisplayClass0_0.<IF1>b__2
        return input.Select(x =>
        {
          var l = new List<char> { x };
          return Interior(l.Select(f).Last(), 6);
        }).Sum();
      }

      public void G1(int label)
      {
        Console.WriteLine(2 * label);
      }

      [ExcludeFromCodeCoverage]
      public IEnumerable<T> G2<T>(T input)
      {
        // class Sample5.Class1+Inner+<G2>d__1
        var source = new[] { input };
        foreach (var c in source)
        {
          yield return c;
        }
      }

      public void G2(int label)
      {
        Console.WriteLine(2 * label);
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

  [ExcludeFromCodeCoverage]
  public class RecursiveSyntheticInvocation<T, K>
        : IReadOnlyDictionary<T, K>
  {
    private int counter;

    // Replace the content of this property's getter with
    // "=> throw new NotImplementedException();"
    // and the whole project is instrumented successfully.
    // As long as this getter is left alone, instrumentation enters an
    // infinite loop.
    IEnumerable<K> IReadOnlyDictionary<T, K>.Values
    {
      get { yield break; }
    }

    // This works as is. It seems the problem doesn't appear unless the
    // property is specifically part of an explicit interface
    // implementation.
    public IEnumerable<K> ValuesWorks
    {
      get
      {
        yield return default(K);
        yield break;
      }
    }

    // This works as is. As long as there isn't a yield used inside this
    // getter then the property won't cause any trouble to the
    // instrumentation process.
    IEnumerable<T> IReadOnlyDictionary<T, K>.Keys
    {
      get { counter++; throw new NotImplementedException(); }
      // The counter variable is just a blind hedge against the compiler
      // doing something weird. It might optimize the getter out somehow
      // if the body of the getter was just the exception throw.
    }

    K IReadOnlyDictionary<T, K>.this[T key] => throw new NotImplementedException();
    int IReadOnlyCollection<KeyValuePair<T, K>>.Count => throw new NotImplementedException();

    bool IReadOnlyDictionary<T, K>.ContainsKey(T key) => throw new NotImplementedException();

    IEnumerator<KeyValuePair<T, K>> IEnumerable<KeyValuePair<T, K>>.GetEnumerator() => throw new NotImplementedException();

    IEnumerator IEnumerable.GetEnumerator() => throw new NotImplementedException();

    bool IReadOnlyDictionary<T, K>.TryGetValue(T key, out K value) => throw new NotImplementedException();
  }

  [ExcludeFromCodeCoverage]
  public class Issue135
  {
    private object field;

    public Issue135(object o)
    { // whole span is "Sample5.Issue135"::".ctor"
      field = o;
    }

#pragma warning disable CS1998
#pragma warning disable CS4014

    internal async Task OuterAsync2<T>(T data)
    { // span less inner function is "Sample5.Issue135/<OuterAsync2>d__2"::"MoveNext"
      [ExcludeFromCodeCoverage]
      async Task InnerAsync3(T aux)
      { // whole span is "Sample5.Issue135/<>c__DisplayClass2_0/<<OuterAsync2>g__InnerAsync3|0>d"::"MoveNext
        Console.WriteLine("{0} | {1} || {2}", field, data, aux);
        InnerAsync3((T)field);
        InnerAsync3(data);
        OuterAsync2(data);
      }

      InnerAsync3((T)field);
    } // Unless stated, all other methods have no sequence points

  internal void OuterSynch4<T>(T data)
  {
      async Task InnerAsync5(T aux)
      {
          Console.WriteLine("{0} |{1} || {2}", field, data, aux);
          InnerAsync5((T)field);
          InnerAsync5(data);
          OuterSynch4(data);
      }

      InnerAsync5((T)field);
  }
}

#pragma warning restore CS4014
#pragma warning restore CS1998


  [ExcludeFromCodeCoverage]
  public class Issue135Complex
  {
    private object field;

    public Issue135Complex(object o)
    { // whole span is "Sample5.Issue135"::".ctor"
      field = o;
    }

#pragma warning disable CS1998
#pragma warning disable CS4014
    public void OuterAsync2()
    {
      Console.WriteLine("Simple async");
    }

    public string OuterAsync2(object o)
    {
      return o.ToString();
    }

    public string OuterAsync2<T,U>(T a, U b)
    {
      return a.ToString() + b.ToString();
    }


    internal async Task OuterAsync2<T>(T data)
    { // span less inner function is "Sample5.Issue135/<OuterAsync2>d__2"::"MoveNext"
      [ExcludeFromCodeCoverage]
      async Task InnerAsync3(T aux)
      { // whole span is "Sample5.Issue135/<>c__DisplayClass2_0/<<OuterAsync2>g__InnerAsync3|0>d"::"MoveNext
        Console.WriteLine("{0} | {1} || {2}", field, data, aux);
        InnerAsync3((T)field);
        InnerAsync3(data);
        OuterAsync2(data);
      }

      InnerAsync3((T)field);
    } // Unless stated, all other methods have no sequence points

  internal void OuterSynch4<T>(T data)
  {
      async Task InnerAsync5(T aux)
      {
          Console.WriteLine("{0} |{1} || {2}", field, data, aux);
          InnerAsync5((T)field);
          InnerAsync5(data);
          OuterSynch4(data);
      }

      InnerAsync5((T)field);
  }

    public void OuterSynch4()
    {
      Console.WriteLine("Simple synch");
    }

    public string OuterSynch4(object o)
    {
      return o.ToString();
    }

    public string OuterSynch4<T,U>(T a, U b)
    {
      return a.ToString() + b.ToString();
    }

}

#pragma warning restore CS4014
#pragma warning restore CS1998

}