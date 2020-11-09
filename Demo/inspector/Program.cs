using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace altcover.inspector
{
    internal class Program
    {
        private static IEnumerable<string> getFileExists(string f)
        {
            string full = null;

            try
            {
                full = Path.GetFullPath(f);
                if (!File.Exists(full))
                {
                    Console.Error.WriteLine("File {0} does not exist", full);
                }
            }
            catch (Exception e)
            {
                Console.Error.WriteLine("Treating {0} as a file caused {1}", f, e.ToString());
            }

            if (!string.IsNullOrEmpty(full)) yield return full;
        }

        private static IEnumerable<AssemblyDefinition> loadInCecil(string path)
        {
            AssemblyDefinition a = null;
            try
            {
                a = AssemblyDefinition.ReadAssembly(path);
            }
            catch (Exception e)
            {
                Console.Error.WriteLine("Treating {0} as an assembly caused {1}", path, e.ToString());
            }

            if (a != null) yield return a;
        }

        private static void checkCalls(MethodDefinition m)
        {
            var il = m.Body.Instructions;
            var list = il.ToList();
            il.Where(i => i.OpCode == OpCodes.Call)
                .Where(i =>
                {
                    var op = i.Operand as MethodReference;
                    return op.Name == "Visit" && op.DeclaringType.FullName == "AltCover.Recorder.Instance";
                }).ToList().ForEach(v =>
                {
                    var p = v.Previous;
                    if (!p.OpCode.ToString().StartsWith("ldc.i4", StringComparison.OrdinalIgnoreCase))
                        Console.Error.WriteLine("Suspicious call in {0} - visit number = {1}", m.FullName, p);
                    var p2 = p.Previous;
                    if (p2.OpCode.ToString() != "ldstr")
                        Console.Error.WriteLine("Suspicious call in {0} - module id = {1}", m.FullName, p2);

                    list.ForEach(o =>
                    {
                        switch (o.OpCode.OperandType)
                        {
                            case OperandType.InlineBrTarget:
                            case OperandType.ShortInlineBrTarget:
                                {
                                    var target = o.Operand as Instruction;
                                    if (target.Offset == v.Offset)
                                        Console.Error.WriteLine("Suspicious jump in {0} from {1} to {2}", m.FullName, o, v);
                                    if (target.Offset == p.Offset)
                                        Console.Error.WriteLine("Suspicious jump in {0} from {1} to {2}", m.FullName, o, p);
                                }
                                break;

                            case OperandType.InlineSwitch:
                                {
                                    var targets = o.Operand as Instruction[];
                                    targets.ToList().ForEach(target =>
                                    {
                                        if (target.Offset == v.Offset)
                                            Console.Error.WriteLine("Suspicious jump in {0} from {1} to {2}", m.FullName, o, v);
                                        if (target.Offset == p.Offset)
                                            Console.Error.WriteLine("Suspicious jump in {0} from {1} to {2}", m.FullName, o, p);
                                    });
                                }
                                break;

                            default: return;
                        }
                    });
                });
        }

        private static void inspect(AssemblyDefinition def)
        {
            def.MainModule.GetAllTypes().SelectMany(t => t.Methods).Where(m => m.HasBody)
               .ToList().ForEach(checkCalls);
            Console.WriteLine("Scan completed for {0}", def.FullName);
        }

        private static void Main(string[] args)
        {
            args.SelectMany(getFileExists).SelectMany(loadInCecil)
                .ToList().ForEach(inspect);
        }
    }
}