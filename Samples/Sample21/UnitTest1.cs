using NUnit.Framework;

namespace Sample21
{
  public class Product
  {
    public Product(string name)
    { Name = name; }

    public string Name { get; set; }

    public static string Junk(string name)
    {
      var product = new Product(name);
      var bonus = new { note = "You won!" };
      var shipmentWithBonus = new { address = "Somewhere St.", product, bonus };
      return shipmentWithBonus.ToString();
    }
  }

  public class Tests
  {
    [SetUp]
    public void Setup()
    {
    }

    [Test]
    public void Test1()
    {
      var t1 = new Traditional() as ITraditional;
#if !NET472
      var m1 = new Modern1() as IModern;
      var m2 = new Modern2() as IModern;
#endif
      Assert.That(t1.DoSomething(), Is.EqualTo("Sample21.Traditional"));
#if !NET472
      Assert.That(m1.DoSomething(), Is.EqualTo("Sample21.Modern1"));
      Assert.That(m2.DoSomething(), Is.EqualTo("** Sample21.Modern2"));
#endif
    }
  }

  public interface ITraditional
  {
    string DoSomething();
  }

  public class Traditional : ITraditional
  {
    public string DoSomething()
    {
      return this.ToString();
    }
  }

#if !NET472

  public interface IModern
  {
    public string DoSomething()
    {
      return this.ToString();
    }
  }

  public class Modern1 : IModern
  {
  }

  public class Modern2 : IModern
  {
    public string DoSomething()
    {
      return "** " + this.ToString();
    }
  }

#endif
}