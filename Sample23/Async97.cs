using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace Sample23
{
  public class Async97
  {
    public async Task<object> DoSomething(byte[] buffer, String file)
    {
      try
      {
        using (var source = File.OpenRead(file))
          return await source.ReadAsync(buffer, 0, buffer.Length);
      }
      catch (Exception)
      {
        return null;
      }
    }
  }
}