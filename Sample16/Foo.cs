using System.Collections.Generic;

namespace Sample16
{
  public static class Foo
  {
    public static List<string> Bar(List<string> values)
    {
      List<string> ret = new List<string>();

      foreach (string val in values)
      {
        if (string.IsNullOrEmpty(val)) continue;

        switch (val)
        {
          case "a":
            ret.Add("a");
            break;

          case "b":
            ret.Add("b");
            break;

          case "c":
            ret.Add("c");
            break;

          default:
            ret.Add("d");
            break;
        }
      }

      return ret;
    }
  }
}

/*
IL_0000: nop Some 8
IL_0001: newobj System.Void System.Collections.Generic.List`1<System.String>::.ctor() Some 9
IL_0006: stloc.0
IL_0007: nop Some 11
IL_0008: ldarg.0 Some 11
IL_0009: callvirt System.Collections.Generic.List`1/Enumerator<!0> System.Collections.Generic.List`1<System.String>::GetEnumerator()
IL_000e: stloc.1
IL_000f: br IL_0095 Some 0xfeefee
IL_0014: ldloca.s V_1 Some 11
IL_0016: call !0 System.Collections.Generic.List`1/Enumerator<System.String>::get_Current()
IL_001b: stloc.2
IL_001c: nop Some 12
IL_001d: ldloc.2 Some 13
IL_001e: call System.Boolean System.String::IsNullOrEmpty(System.String)
IL_0023: stloc.3
IL_0024: ldloc.3 Some 0xfeefee
IL_0025: brfalse.s IL_0029
IL_0027: br.s IL_0095 Some 13
IL_0029: ldloc.2 Some 15
IL_002a: stloc.s V_5
IL_002c: ldloc.s V_5 Some 0xfeefee
IL_002e: stloc.s V_4
IL_0030: ldloc.s V_4 Some 0xfeefee
IL_0032: ldstr "a"
IL_0037: call System.Boolean System.String::op_Equality(System.String,System.String)
IL_003c: brtrue.s IL_005c
IL_003e: ldloc.s V_4
IL_0040: ldstr "b"
IL_0045: call System.Boolean System.String::op_Equality(System.String,System.String)
IL_004a: brtrue.s IL_006a
IL_004c: ldloc.s V_4
IL_004e: ldstr "c"
IL_0053: call System.Boolean System.String::op_Equality(System.String,System.String)
IL_0058: brtrue.s IL_0078
IL_005a: br.s IL_0086
IL_005c: ldloc.0 Some 18
IL_005d: ldstr "a"
IL_0062: callvirt System.Void System.Collections.Generic.List`1<System.String>::Add(!0)
IL_0067: nop
IL_0068: br.s IL_0094 Some 19
IL_006a: ldloc.0 Some 22
IL_006b: ldstr "b"
IL_0070: callvirt System.Void System.Collections.Generic.List`1<System.String>::Add(!0)
IL_0075: nop
IL_0076: br.s IL_0094 Some 23
IL_0078: ldloc.0 Some 26
IL_0079: ldstr "c"
IL_007e: callvirt System.Void System.Collections.Generic.List`1<System.String>::Add(!0)
IL_0083: nop
IL_0084: br.s IL_0094 Some 27
IL_0086: ldloc.0 Some 30
IL_0087: ldstr "d"
IL_008c: callvirt System.Void System.Collections.Generic.List`1<System.String>::Add(!0)
IL_0091: nop
IL_0092: br.s IL_0094 Some 31
IL_0094: nop Some 33
IL_0095: ldloca.s V_1 Some 11
IL_0097: call System.Boolean System.Collections.Generic.List`1/Enumerator<System.String>::MoveNext()
IL_009c: brtrue IL_0014
IL_00a1: leave.s IL_00b2
IL_00a3: ldloca.s V_1 Some 0xfeefee
IL_00a5: constrained. System.Collections.Generic.List`1/Enumerator<System.String>
IL_00ab: callvirt System.Void System.IDisposable::Dispose()
IL_00b0: nop
IL_00b1: endfinally
IL_00b2: ldloc.0 Some 35
IL_00b3: stloc.s V_6
IL_00b5: br.s IL_00b7
IL_00b7: ldloc.s V_6 Some 36
IL_00b9: ret
*/