using System;

namespace issue222
{
  public interface IServiceCollection
  {
    void AddSingleton<T2, T>(Func<IServiceProvider, T> op);
  }

  public static class Class1
  {
    public static T GetService<T>(this IServiceProvider serviceProvider)
    {
      return default(T);
    }

    public static void AddSingleton<T1, T2, T>(this IServiceCollection services, Func<IServiceProvider, T> implementationFactory)
      where T : class, T1, T2 where T1 : class where T2 : class
    {
      services.AddSingleton<T2, T>(implementationFactory);
      services.AddSingleton<T1, T>(x => (T)x.GetService<T2>());
    }

    public static void AddSingleton<T1, T2, T3, T>(this IServiceCollection services, Func<IServiceProvider, T> implementationFactory)
      where T : class, T1, T2, T3 where T1 : class where T2 : class where T3 : class
    {
      services.AddSingleton<T2, T>(implementationFactory);
      services.AddSingleton<T1, T>(x => (T)x.GetService<T2>());
      services.AddSingleton<T3, T>(x => (T)x.GetService<T2>());
    }
  }
}