using System;
using System.Threading.Tasks;
using System.IO;

namespace Sample23
{
  public class Async97
  {
    public async Task<object> DoSomething(byte[] buffer, string file)
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

/*
IL_0000: ldarg.0 Some 0xfeefee
IL_0001: ldfld System.Int32 Sample23.Async97/<DoSomething>d__0::<>1__state
IL_0006: stloc.0
IL_0007: ldloc.0 Some 0xfeefee
IL_0008: brfalse.s IL_000c
IL_000a: br.s IL_000e
IL_000c: br.s IL_000f
IL_000e: nop Some 10
IL_000f: nop Some 0xfeefee
IL_0010: ldloc.0 Some 0xfeefee
IL_0011: brfalse.s IL_0015
IL_0013: br.s IL_0017
IL_0015: br.s IL_0029
IL_0017: nop Some 12
IL_0018: ldarg.0 Some 13
IL_0019: ldarg.0
IL_001a: ldfld System.String Sample23.Async97/<DoSomething>d__0::file
IL_001f: call System.IO.FileStream System.IO.File::OpenRead(System.String)
IL_0024: stfld System.IO.FileStream Sample23.Async97/<DoSomething>d__0::<source>5__1
IL_0029: nop Some 0xfeefee
IL_002a: ldloc.0 Some 0xfeefee
IL_002b: brfalse.s IL_002f
IL_002d: br.s IL_0031
IL_002f: br.s IL_0081
IL_0031: ldarg.0 Some 14
IL_0032: ldfld System.IO.FileStream Sample23.Async97/<DoSomething>d__0::<source>5__1
IL_0037: ldarg.0
IL_0038: ldfld System.Byte[] Sample23.Async97/<DoSomething>d__0::buffer
IL_003d: ldc.i4.0
IL_003e: ldarg.0
IL_003f: ldfld System.Byte[] Sample23.Async97/<DoSomething>d__0::buffer
IL_0044: ldlen
IL_0045: conv.i4
IL_0046: callvirt System.Threading.Tasks.Task`1<System.Int32> System.IO.Stream::ReadAsync(System.Byte[],System.Int32,System.Int32)
IL_004b: callvirt System.Runtime.CompilerServices.TaskAwaiter`1<!0> System.Threading.Tasks.Task`1<System.Int32>::GetAwaiter()
IL_0050: stloc.2
IL_0051: ldloca.s V_2 Some 0xfeefee
IL_0053: call System.Boolean System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32>::get_IsCompleted()
IL_0058: brtrue.s IL_009d
IL_005a: ldarg.0
IL_005b: ldc.i4.0
IL_005c: dup
IL_005d: stloc.0
IL_005e: stfld System.Int32 Sample23.Async97/<DoSomething>d__0::<>1__state
IL_0063: ldarg.0
IL_0064: ldloc.2
IL_0065: stfld System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32> Sample23.Async97/<DoSomething>d__0::<>u__1
IL_006a: ldarg.0
IL_006b: stloc.3
IL_006c: ldarg.0
IL_006d: ldflda System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object> Sample23.Async97/<DoSomething>d__0::<>t__builder
IL_0072: ldloca.s V_2
IL_0074: ldloca.s V_3
IL_0076: call System.Void System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object>::AwaitUnsafeOnCompleted<System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32>,Sample23.Async97/<DoSomething>d__0>(!!0&,!!1&)
IL_007b: nop
IL_007c: leave IL_0106
IL_0081: ldarg.0
IL_0082: ldfld System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32> Sample23.Async97/<DoSomething>d__0::<>u__1
IL_0087: stloc.2
IL_0088: ldarg.0
IL_0089: ldflda System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32> Sample23.Async97/<DoSomething>d__0::<>u__1
IL_008e: initobj System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32>
IL_0094: ldarg.0
IL_0095: ldc.i4.m1
IL_0096: dup
IL_0097: stloc.0
IL_0098: stfld System.Int32 Sample23.Async97/<DoSomething>d__0::<>1__state
IL_009d: ldarg.0
IL_009e: ldloca.s V_2
IL_00a0: call !0 System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32>::GetResult()
IL_00a5: stfld System.Int32 Sample23.Async97/<DoSomething>d__0::<>s__2
IL_00aa: ldarg.0
IL_00ab: ldfld System.Int32 Sample23.Async97/<DoSomething>d__0::<>s__2
IL_00b0: box System.Int32
IL_00b5: stloc.1
IL_00b6: leave.s IL_00f1
IL_00b8: ldloc.0 Some 0xfeefee
IL_00b9: ldc.i4.0
IL_00ba: bge.s IL_00d0
IL_00bc: ldarg.0
IL_00bd: ldfld System.IO.FileStream Sample23.Async97/<DoSomething>d__0::<source>5__1
IL_00c2: brfalse.s IL_00d0
IL_00c4: ldarg.0
IL_00c5: ldfld System.IO.FileStream Sample23.Async97/<DoSomething>d__0::<source>5__1
IL_00ca: callvirt System.Void System.IDisposable::Dispose()
IL_00cf: nop
IL_00d0: endfinally Some 0xfeefee
IL_00d1: pop Some 16
IL_00d2: nop Some 17
IL_00d3: ldnull Some 18
IL_00d4: stloc.1
IL_00d5: leave.s IL_00f1
IL_00d7: stloc.s V_4 Some 0xfeefee
IL_00d9: ldarg.0
IL_00da: ldc.i4.s -2
IL_00dc: stfld System.Int32 Sample23.Async97/<DoSomething>d__0::<>1__state
IL_00e1: ldarg.0
IL_00e2: ldflda System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object> Sample23.Async97/<DoSomething>d__0::<>t__builder
IL_00e7: ldloc.s V_4
IL_00e9: call System.Void System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object>::SetException(System.Exception)
IL_00ee: nop
IL_00ef: leave.s IL_0106
IL_00f1: ldarg.0 Some 20
IL_00f2: ldc.i4.s -2
IL_00f4: stfld System.Int32 Sample23.Async97/<DoSomething>d__0::<>1__state
IL_00f9: ldarg.0 Some 0xfeefee
IL_00fa: ldflda System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object> Sample23.Async97/<DoSomething>d__0::<>t__builder
IL_00ff: ldloc.1
IL_0100: call System.Void System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object>::SetResult(!0)
IL_0105: nop
IL_0106: ret
 */