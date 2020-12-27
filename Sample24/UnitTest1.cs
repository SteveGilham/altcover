using NUnit.Framework;
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
      Assert.AreEqual(AddSynch(1, 1), result);
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
  }
}