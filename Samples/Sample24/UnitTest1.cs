using NUnit.Framework;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace NUnitTestProject1
{
  public class Tests
  {
    [SetUp]
    public void Setup()
    {
    }

    [Test]
    public async Task AddAsync_Returns_The_Sum_Of_X_And_Y()
    {
      int result = await AddAsync(1, 1);
      Assert.That(AddSynch(1, 1), Is.EqualTo(result));
    }

    public async Task<int> AddAsync(int x, int y)
    {
      // simulate long calculation
      await Task.Delay(100).ConfigureAwait(false);
      await Task.Delay(100);
      await Task.Delay(100);
      await Task.Delay(100);
      return AddSynch(x, y);
    }

    public int AddSynch(int x, int y)
    {
      return x + y;
    }

    public IEnumerable<int> Yielder()
    {
      yield return 1;
      yield return 2;
      yield return 3;
    }
  }
}