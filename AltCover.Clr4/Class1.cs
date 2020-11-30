using System;
using System.Threading;

namespace AltCover.Clr4
{
  public class Class1<T>
  {
    private static AsyncLocal<T> instance;

    public AsyncLocal<T> Instance()
    {
      return instance;
    }

    public T Value()
    {
      return instance.Value;
    }
  }
}