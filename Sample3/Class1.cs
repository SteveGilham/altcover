using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil.Cil;

namespace Sample3
{
    public class Class1
    {
        public int Property { get; set; }
    }

    public class Class2
    {
        private int _Property;
        public int Property { get { return _Property; } set { _Property = value > 0 ? value : 0; } }
    }

    public class Class3
    {
        public class Class4
        {
            public Class1 Property { get; set; }

            public List<T> ToList<T>(T item)
            {
                try
                {
                    return new List<T> { item };
                }
                catch (NullReferenceException)
                {
                    return Enumerable.Empty<T>().ToList();
                }
            }

            public string ReportFile { get; set; }
            public string Token { get; set; }
            public int CoverageFormat { get; set; }
            public int Sample { get; set; }
            public Int64 Timer { get; set; }
            public bool Defer { get; set; }
        }

        private static List<Tuple<string, int>> log = new List<Tuple<string, int>>();

        public List<Tuple<string, int>> Visits
        {
            get
            {
                return log;
            }
        }

        public static void Log(string id, int num)
        {
            log.Add(Tuple.Create(id, num));
        }

        public static int GetOperandType(Instruction self)
        {
            int i = 0;
            switch (self.OpCode.Code)
            {
                case Code.Ldarg_0:
                case Code.Ldarg_1:
                case Code.Ldarg_2:
                case Code.Ldarg_3:
                case Code.Ldarg:
                case Code.Ldarg_S:
                case Code.Ldarga:
                case Code.Ldarga_S:
                case Code.Starg:
                case Code.Starg_S:
                    i = 1;
                    Console.WriteLine("arguments");
                    break;

                case Code.Conv_R4:
                case Code.Ldc_R4:
                case Code.Ldelem_R4:
                case Code.Ldind_R4:
                case Code.Stelem_R4:
                case Code.Stind_R4:
                    i = 2;
                    Console.WriteLine("singles");
                    break;

                case Code.Conv_R8:
                case Code.Ldc_R8:
                case Code.Ldelem_R8:
                case Code.Ldind_R8:
                case Code.Stelem_R8:
                    i = 3;
                    Console.WriteLine("doubles");
                    break;

                case Code.Ldloc_0:
                case Code.Ldloc_1:
                case Code.Ldloc_2:
                case Code.Ldloc_3:
                case Code.Ldloc:
                case Code.Ldloc_S:
                case Code.Ldloca:
                case Code.Ldloca_S:
                case Code.Stloc_0:
                case Code.Stloc_1:
                case Code.Stloc_2:
                case Code.Stloc_3:
                case Code.Stloc:
                case Code.Stloc_S:
                    i = 4;
                    Console.WriteLine("locals");
                    break;

                case Code.Ldfld:
                case Code.Ldflda:
                case Code.Ldsfld:
                case Code.Ldsflda:
                case Code.Stfld:
                case Code.Stsfld:
                    i = 5;
                    Console.WriteLine("fields");
                    break;

                case Code.Call:
                case Code.Callvirt:
                case Code.Newobj:
                    i = 6;
                    Console.WriteLine("calls");
                    break;

                default:
                    i = 7;
                    Console.WriteLine("default");
                    break;
            }
            Console.WriteLine("end");
            return i;
        }
    }
}