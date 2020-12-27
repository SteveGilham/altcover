using System;
using System.Threading.Tasks;
using System.IO;

namespace Sample23
{
  public class Async97
  {
    public async Task<object> DoSomethingAsync(byte[] buffer, string file)
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

    public object DoSomethingSynch(byte[] buffer, string file)
    {
      try
      {
        using (var source = File.OpenRead(file))
          return source.Read(buffer, 0, buffer.Length);
      }
      catch (Exception)
      {
        return null;
      }
    }
  }
}

/*
IL_0000: nop Some 23
IL_0001: nop Some 25
IL_0002: ldarg.2 Some 26
IL_0003: call System.IO.FileStream System.IO.File::OpenRead(System.String) None
IL_0008: stloc.0 None
IL_0009: ldloc.0 Some 27
IL_000a: ldarg.1 None
IL_000b: ldc.i4.0 None
IL_000c: ldarg.1 None
IL_000d: ldlen None
IL_000e: conv.i4 None
IL_000f: callvirt System.Int32 System.IO.Stream::Read(System.Byte[],System.Int32,System.Int32) None
IL_0014: box System.Int32 None
IL_0019: stloc.1 None
IL_001a: leave.s IL_002d None
IL_001c: ldloc.0 Some 16707566
IL_001d: brfalse.s IL_0026 None
IL_001f: ldloc.0 None
IL_0020: callvirt System.Void System.IDisposable::Dispose() None
IL_0025: nop None
IL_0026: endfinally Some 16707566
IL_0027: pop Some 29
IL_0028: nop Some 30
IL_0029: ldnull Some 31
IL_002a: stloc.1 None
IL_002b: leave.s IL_002d None
IL_002d: ldloc.1 Some 33
IL_002e: ret None

*****************************************

IL_0000: ldarg.0 Some 0xfeefee
IL_0001: ldfld System.Int32 Sample23.Async97/<DoSomethingAsync>d__0::<>1__state None
IL_0006: stloc.0 None
IL_0007: ldloc.0 Some 16707566
IL_0008: brfalse.s IL_000c None
IL_000a: br.s IL_000e None
IL_000c: br.s IL_000f None
IL_000e: nop Some 10
IL_000f: nop Some 16707566
IL_0010: ldloc.0 Some 16707566
IL_0011: brfalse.s IL_0015 None
IL_0013: br.s IL_0017 None
IL_0015: br.s IL_0029 None
IL_0017: nop Some 12
IL_0018: ldarg.0 Some 13
IL_0019: ldarg.0 None
IL_001a: ldfld System.String Sample23.Async97/<DoSomethingAsync>d__0::file None
IL_001f: call System.IO.FileStream System.IO.File::OpenRead(System.String) None
IL_0024: stfld System.IO.FileStream Sample23.Async97/<DoSomethingAsync>d__0::<source>5__1 None
IL_0029: nop Some 16707566
IL_002a: ldloc.0 Some 16707566
IL_002b: brfalse.s IL_002f None
IL_002d: br.s IL_0031 None
IL_002f: br.s IL_0081 None
IL_0031: ldarg.0 Some 14
IL_0032: ldfld System.IO.FileStream Sample23.Async97/<DoSomethingAsync>d__0::<source>5__1 None
IL_0037: ldarg.0 None
IL_0038: ldfld System.Byte[] Sample23.Async97/<DoSomethingAsync>d__0::buffer None
IL_003d: ldc.i4.0 None
IL_003e: ldarg.0 None
IL_003f: ldfld System.Byte[] Sample23.Async97/<DoSomethingAsync>d__0::buffer None
IL_0044: ldlen None
IL_0045: conv.i4 None
IL_0046: callvirt System.Threading.Tasks.Task`1<System.Int32> System.IO.Stream::ReadAsync(System.Byte[],System.Int32,System.Int32) None
IL_004b: callvirt System.Runtime.CompilerServices.TaskAwaiter`1<!0> System.Threading.Tasks.Task`1<System.Int32>::GetAwaiter() None
IL_0050: stloc.2 None
IL_0051: ldloca.s V_2 Some 16707566
IL_0053: call System.Boolean System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32>::get_IsCompleted() None
IL_0058: brtrue.s IL_009d None
IL_005a: ldarg.0 None
IL_005b: ldc.i4.0 None
IL_005c: dup None
IL_005d: stloc.0 None
IL_005e: stfld System.Int32 Sample23.Async97/<DoSomethingAsync>d__0::<>1__state None
IL_0063: ldarg.0 None
IL_0064: ldloc.2 None
IL_0065: stfld System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32> Sample23.Async97/<DoSomethingAsync>d__0::<>u__1 None
IL_006a: ldarg.0 None
IL_006b: stloc.3 None
IL_006c: ldarg.0 None
IL_006d: ldflda System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object> Sample23.Async97/<DoSomethingAsync>d__0::<>t__builder None
IL_0072: ldloca.s V_2 None
IL_0074: ldloca.s V_3 None
IL_0076: call System.Void System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object>::AwaitUnsafeOnCompleted<System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32>,Sample23.Async97/<DoSomethingAsync>d__0>(!!0&,!!1&) None
IL_007b: nop None
IL_007c: leave IL_0106 None
IL_0081: ldarg.0 None
IL_0082: ldfld System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32> Sample23.Async97/<DoSomethingAsync>d__0::<>u__1 None
IL_0087: stloc.2 None
IL_0088: ldarg.0 None
IL_0089: ldflda System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32> Sample23.Async97/<DoSomethingAsync>d__0::<>u__1 None
IL_008e: initobj System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32> None
IL_0094: ldarg.0 None
IL_0095: ldc.i4.m1 None
IL_0096: dup None
IL_0097: stloc.0 None
IL_0098: stfld System.Int32 Sample23.Async97/<DoSomethingAsync>d__0::<>1__state None
IL_009d: ldarg.0 None
IL_009e: ldloca.s V_2 None
IL_00a0: call !0 System.Runtime.CompilerServices.TaskAwaiter`1<System.Int32>::GetResult() None
IL_00a5: stfld System.Int32 Sample23.Async97/<DoSomethingAsync>d__0::<>s__2 None
IL_00aa: ldarg.0 None
IL_00ab: ldfld System.Int32 Sample23.Async97/<DoSomethingAsync>d__0::<>s__2 None
IL_00b0: box System.Int32 None
IL_00b5: stloc.1 None
IL_00b6: leave.s IL_00f1 None
IL_00b8: ldloc.0 Some 16707566
IL_00b9: ldc.i4.0 None
IL_00ba: bge.s IL_00d0 None
IL_00bc: ldarg.0 None
IL_00bd: ldfld System.IO.FileStream Sample23.Async97/<DoSomethingAsync>d__0::<source>5__1 None
IL_00c2: brfalse.s IL_00d0 None
IL_00c4: ldarg.0 None
IL_00c5: ldfld System.IO.FileStream Sample23.Async97/<DoSomethingAsync>d__0::<source>5__1 None
IL_00ca: callvirt System.Void System.IDisposable::Dispose() None
IL_00cf: nop None
IL_00d0: endfinally Some 16707566
IL_00d1: pop Some 16
IL_00d2: nop Some 17
IL_00d3: ldnull Some 18
IL_00d4: stloc.1 None
IL_00d5: leave.s IL_00f1 None
IL_00d7: stloc.s V_4 Some 16707566
IL_00d9: ldarg.0 None
IL_00da: ldc.i4.s -2 None
IL_00dc: stfld System.Int32 Sample23.Async97/<DoSomethingAsync>d__0::<>1__state None
IL_00e1: ldarg.0 None
IL_00e2: ldflda System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object> Sample23.Async97/<DoSomethingAsync>d__0::<>t__builder None
IL_00e7: ldloc.s V_4 None
IL_00e9: call System.Void System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object>::SetException(System.Exception) None
IL_00ee: nop None
IL_00ef: leave.s IL_0106 None
IL_00f1: ldarg.0 Some 20
IL_00f2: ldc.i4.s -2 None
IL_00f4: stfld System.Int32 Sample23.Async97/<DoSomethingAsync>d__0::<>1__state None
IL_00f9: ldarg.0 Some 16707566
IL_00fa: ldflda System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object> Sample23.Async97/<DoSomethingAsync>d__0::<>t__builder None
IL_00ff: ldloc.1 None
IL_0100: call System.Void System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<System.Object>::SetResult(!0) None
IL_0105: nop None
IL_0106: ret None
 */