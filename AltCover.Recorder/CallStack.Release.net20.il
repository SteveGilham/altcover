.class nested assembly auto ansi sealed CallStack
	extends [mscorlib]System.Object
{
	.custom instance void Microsoft.FSharp.Core.SealedAttribute::.ctor() = (
		01 00 00 00
	)
	.custom instance void Microsoft.FSharp.Core.AutoSerializableAttribute::.ctor(bool) = (
		01 00 00 00 00
	)
	.custom instance void Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype Microsoft.FSharp.Core.SourceConstructFlags) = (
		01 00 03 00 00 00 00 00
	)
	// Fields
	.field assembly static class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> instance@
	.custom instance void [mscorlib]System.ThreadStaticAttribute::.ctor() = (
		01 00 00 00
	)
	.custom instance void Microsoft.FSharp.Core.DefaultValueAttribute::.ctor() = (
		01 00 00 00
	)
	.field assembly class Microsoft.FSharp.Collections.FSharpList`1<int32> caller

	// Methods
	.method assembly specialname static 
		class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> get_instance () cil managed 
	{
		// Method begins at RVA 0x3aac
		// Code size 6 (0x6)
		.maxstack 8

		// return instance@;
		IL_0000: ldsfld class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> AltCover.Recorder.Instance/I/CallStack::instance@
		// (no C# code)
		IL_0005: ret
	} // end of method CallStack::get_instance

	.method assembly specialname static 
		void set_instance (
			class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> 'value'
		) cil managed 
	{
		// Method begins at RVA 0x3ab4
		// Code size 7 (0x7)
		.maxstack 8

		// instance@ = value;
		IL_0000: ldarg.0
		IL_0001: stsfld class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> AltCover.Recorder.Instance/I/CallStack::instance@
		// }
		IL_0006: ret
	} // end of method CallStack::set_instance

	.method public specialname rtspecialname 
		instance void .ctor (
			int32 x
		) cil managed 
	{
		// Method begins at RVA 0x3abc
		// Code size 26 (0x1a)
		.maxstack 8

		// (no C# code)
		IL_0000: ldarg.0
		IL_0001: callvirt instance void [mscorlib]System.Object::.ctor()
		IL_0006: ldarg.0
		IL_0007: pop
		// caller = FSharpList<int>.Cons(x, FSharpList<int>.Empty);
		IL_0008: ldarg.0
		IL_0009: ldarg.1
		IL_000a: call class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_Empty()
		IL_000f: call class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0, class Microsoft.FSharp.Collections.FSharpList`1<!0>)
		IL_0014: stfld class Microsoft.FSharp.Collections.FSharpList`1<int32> AltCover.Recorder.Instance/I/CallStack::caller
		// }
		IL_0019: ret
	} // end of method CallStack::.ctor

	.method assembly specialname static 
		class AltCover.Recorder.Instance/I/CallStack get_Instance () cil managed 
	{
		// Method begins at RVA 0x3ad8
		// Code size 41 (0x29)
		.maxstack 8

		// if (instance@ == null)
		IL_0000: ldsfld class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> AltCover.Recorder.Instance/I/CallStack::instance@
		// (no C# code)
		IL_0005: brtrue.s IL_001a

		// instance = FSharpOption<CallStack>.Some(new CallStack(0));
		IL_0007: ldc.i4.0
		IL_0008: newobj instance void AltCover.Recorder.Instance/I/CallStack::.ctor(int32)
		IL_000d: call class Microsoft.FSharp.Core.FSharpOption`1<!0> class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack>::Some(!0)
		IL_0012: call void AltCover.Recorder.Instance/I/CallStack::set_instance(class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack>)
		// (no C# code)
		IL_0017: nop
		IL_0018: br.s IL_001c

		IL_001a: nop
		IL_001b: nop

		// return instance@.Value;
		IL_001c: ldsfld class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> AltCover.Recorder.Instance/I/CallStack::instance@
		IL_0021: tail.
		IL_0023: callvirt instance !0 class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack>::get_Value()
		// (no C# code)
		IL_0028: ret
	} // end of method CallStack::get_Instance

	.method assembly hidebysig 
		instance void Push (
			int32 x
		) cil managed 
	{
		// Method begins at RVA 0x3b04
		// Code size 19 (0x13)
		.maxstack 8

		// caller = FSharpList<int>.Cons(x, caller);
		IL_0000: ldarg.0
		IL_0001: ldarg.1
		IL_0002: ldarg.0
		IL_0003: ldfld class Microsoft.FSharp.Collections.FSharpList`1<int32> AltCover.Recorder.Instance/I/CallStack::caller
		IL_0008: call class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0, class Microsoft.FSharp.Collections.FSharpList`1<!0>)
		IL_000d: stfld class Microsoft.FSharp.Collections.FSharpList`1<int32> AltCover.Recorder.Instance/I/CallStack::caller
		// }
		IL_0012: ret
	} // end of method CallStack::Push

	.method assembly hidebysig 
		instance class Microsoft.FSharp.Core.FSharpOption`1<int32> Pop () cil managed 
	{
		// Method begins at RVA 0x3b18
		// Code size 139 (0x8b)
		.maxstack 4
		.locals init (
			[0] class System.Tuple`2<class Microsoft.FSharp.Collections.FSharpList`1<int32>, class Microsoft.FSharp.Core.FSharpOption`1<int32>>,
			[1] class Microsoft.FSharp.Collections.FSharpList`1<int32> stack,
			[2] class Microsoft.FSharp.Collections.FSharpList`1<int32>,
			[3] class Microsoft.FSharp.Collections.FSharpList`1<int32> xs,
			[4] int32,
			[5] class Microsoft.FSharp.Core.FSharpOption`1<int32> head
		)

		// FSharpList<int> fSharpList = caller;
		IL_0000: ldarg.0
		IL_0001: ldfld class Microsoft.FSharp.Collections.FSharpList`1<int32> AltCover.Recorder.Instance/I/CallStack::caller
		IL_0006: stloc.1
		// if (fSharpList.TailOrNull != null)
		IL_0007: ldloc.1
		IL_0008: call instance class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		// (no C# code)
		IL_000d: brfalse.s IL_004c

		// fSharpList2 = fSharpList;
		IL_000f: ldloc.1
		IL_0010: stloc.2
		// switch (fSharpList2.HeadOrDefault)
		IL_0011: ldloc.2
		IL_0012: call instance !0 class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_HeadOrDefault()
		// (no C# code)
		IL_0017: switch (IL_003f)

		// item = fSharpList2.TailOrNull;
		IL_0020: ldloc.2
		IL_0021: call instance class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		IL_0026: stloc.3
		// headOrDefault = fSharpList2.HeadOrDefault;
		IL_0027: ldloc.2
		IL_0028: call instance !0 class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_HeadOrDefault()
		IL_002d: stloc.s 4

		// Tuple<FSharpList<int>, FSharpOption<int>> tuple = new Tuple<FSharpList<int>, FSharpOption<int>>(item, FSharpOption<int>.Some(headOrDefault));
		IL_002f: ldloc.3
		IL_0030: ldloc.s 4
		IL_0032: call class Microsoft.FSharp.Core.FSharpOption`1<!0> class Microsoft.FSharp.Core.FSharpOption`1<int32>::Some(!0)
		IL_0037: newobj instance void class System.Tuple`2<class Microsoft.FSharp.Collections.FSharpList`1<int32>, class Microsoft.FSharp.Core.FSharpOption`1<int32>>::.ctor(!0, !1)
		// (no C# code)
		IL_003c: nop
		IL_003d: br.s IL_0071

		// if (fSharpList2.TailOrNull.TailOrNull != null)
		IL_003f: ldloc.2
		IL_0040: call instance class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		IL_0045: call instance class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		// (no C# code)
		IL_004a: brtrue.s IL_0060

		// tuple = new Tuple<FSharpList<int>, FSharpOption<int>>(FSharpList<int>.Cons(0, FSharpList<int>.Empty), null);
		IL_004c: ldc.i4.0
		IL_004d: call class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_Empty()
		IL_0052: call class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0, class Microsoft.FSharp.Collections.FSharpList`1<!0>)
		IL_0057: ldnull
		IL_0058: newobj instance void class System.Tuple`2<class Microsoft.FSharp.Collections.FSharpList`1<int32>, class Microsoft.FSharp.Core.FSharpOption`1<int32>>::.ctor(!0, !1)
		// (no C# code)
		IL_005d: nop
		IL_005e: br.s IL_0071

		// FSharpList<int> tailOrNull = fSharpList2.TailOrNull;
		IL_0060: ldloc.2
		IL_0061: call instance class Microsoft.FSharp.Collections.FSharpList`1<!0> class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		// headOrDefault = fSharpList2.HeadOrDefault;
		IL_0066: ldloc.2
		IL_0067: call instance !0 class Microsoft.FSharp.Collections.FSharpList`1<int32>::get_HeadOrDefault()
		IL_006c: stloc.s 4
		// item = tailOrNull;
		IL_006e: stloc.3
		// (no C# code)
		IL_006f: br.s IL_002f

		// Tuple<FSharpList<int>, FSharpOption<int>> tuple2 = tuple;
		IL_0071: stloc.0
		// fSharpList = tuple2.Item1;
		IL_0072: ldloc.0
		IL_0073: call instance !0 class System.Tuple`2<class Microsoft.FSharp.Collections.FSharpList`1<int32>, class Microsoft.FSharp.Core.FSharpOption`1<int32>>::get_Item1()
		IL_0078: stloc.1
		// FSharpOption<int> item2 = tuple2.Item2;
		IL_0079: ldloc.0
		IL_007a: call instance !1 class System.Tuple`2<class Microsoft.FSharp.Collections.FSharpList`1<int32>, class Microsoft.FSharp.Core.FSharpOption`1<int32>>::get_Item2()
		IL_007f: stloc.s 5
		// caller = fSharpList;
		IL_0081: ldarg.0
		IL_0082: ldloc.1
		IL_0083: stfld class Microsoft.FSharp.Collections.FSharpList`1<int32> AltCover.Recorder.Instance/I/CallStack::caller
		// return item2;
		IL_0088: ldloc.s 5
		// (no C# code)
		IL_008a: ret
	} // end of method CallStack::Pop

	.method assembly hidebysig 
		instance int32 CallerId () cil managed 
	{
		// Method begins at RVA 0x3bb0
		// Code size 14 (0xe)
		.maxstack 8

		// return SeqModule.Head(caller);
		IL_0000: ldarg.0
		IL_0001: ldfld class Microsoft.FSharp.Collections.FSharpList`1<int32> AltCover.Recorder.Instance/I/CallStack::caller
		IL_0006: tail.
		IL_0008: call !!0 Microsoft.FSharp.Collections.SeqModule::Head<int32>(class [mscorlib]System.Collections.Generic.IEnumerable`1<!!0>)
		// (no C# code)
		IL_000d: ret
	} // end of method CallStack::CallerId

	// Properties
	.property class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> 'instance'()
	{
		.custom instance void Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype Microsoft.FSharp.Core.SourceConstructFlags, int32) = (
			01 00 04 00 00 00 00 00 00 00 00 00
		)
		.get class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack> AltCover.Recorder.Instance/I/CallStack::get_instance()
		.set void AltCover.Recorder.Instance/I/CallStack::set_instance(class Microsoft.FSharp.Core.FSharpOption`1<class AltCover.Recorder.Instance/I/CallStack>)
	}
	.property class AltCover.Recorder.Instance/I/CallStack Instance()
	{
		.get class AltCover.Recorder.Instance/I/CallStack AltCover.Recorder.Instance/I/CallStack::get_Instance()
	}

} // end of class CallStack
