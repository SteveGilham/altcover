.class nested public auto ansi abstract sealed CallTrack
	extends [System.Runtime]System.Object
{
	.custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = (
		01 00 07 00 00 00 00 00
	)
	// Methods
	.method public specialname static 
		class [System.Threading]System.Threading.AsyncLocal`1<class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>> get_instance () cil managed 
	{
		// Method begins at RVA 0x22b4
		// Code size 6 (0x6)
		.maxstack 8

		IL_0000: ldsfld class [System.Threading]System.Threading.AsyncLocal`1<class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>> '<StartupCode$SoakTest1>.$Program'::instance@66
		IL_0005: ret
	} // end of method CallTrack::get_instance

	.method assembly static 
		void Update (
			class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> l
		) cil managed 
	{
		// Method begins at RVA 0x22bc
		// Code size 17 (0x11)
		.maxstack 8

		IL_0000: call class [System.Threading]System.Threading.AsyncLocal`1<class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>> Program/AsyncLevel/CallTrack::get_instance()
		IL_0005: ldarg.0
		IL_0006: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<!0> class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>::Some(!0)
		IL_000b: callvirt instance void class [System.Threading]System.Threading.AsyncLocal`1<class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>>::set_Value(!0)
		IL_0010: ret
	} // end of method CallTrack::Update

	.method public specialname static 
		class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> get_Instance () cil managed 
	{
		// Method begins at RVA 0x22d0
		// Code size 6 (0x6)
		.maxstack 8

		IL_0000: ldsfld class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> '<StartupCode$SoakTest1>.$Program'::Instance@72
		IL_0005: ret
	} // end of method CallTrack::get_Instance

	.method public specialname static 
		void set_Instance (
			class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> 'value'
		) cil managed 
	{
		// Method begins at RVA 0x22d8
		// Code size 7 (0x7)
		.maxstack 8

		IL_0000: ldarg.0
		IL_0001: stsfld class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> '<StartupCode$SoakTest1>.$Program'::Instance@72
		IL_0006: ret
	} // end of method CallTrack::set_Instance

	.method public static 
		class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>> Peek () cil managed 
	{
		// Method begins at RVA 0x22e0
		// Code size 57 (0x39)
		.maxstack 4
		.locals init (
			[0] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>,
			[1] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>,
			[2] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> xs,
			[3] int32 h
		)

		IL_0000: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Program/AsyncLevel/CallTrack::get_Instance()
		IL_0005: stloc.0
		IL_0006: ldloc.0
		IL_0007: call instance class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		IL_000c: brfalse.s IL_0010

		IL_000e: br.s IL_001c

		IL_0010: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_Empty()
		IL_0015: ldnull
		IL_0016: newobj instance void class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::.ctor(!0, !1)
		IL_001b: ret

		IL_001c: ldloc.0
		IL_001d: stloc.1
		IL_001e: ldloc.1
		IL_001f: call instance class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		IL_0024: stloc.2
		IL_0025: ldloc.1
		IL_0026: call instance !0 class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_HeadOrDefault()
		IL_002b: stloc.3
		IL_002c: ldloc.2
		IL_002d: ldloc.3
		IL_002e: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<!0> class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>::Some(!0)
		IL_0033: newobj instance void class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::.ctor(!0, !1)
		IL_0038: ret
	} // end of method CallTrack::Peek

	.method public static 
		void Push (
			int32 x
		) cil managed 
	{
		// Method begins at RVA 0x2328
		// Code size 18 (0x12)
		.maxstack 8

		IL_0000: ldarg.0
		IL_0001: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Program/AsyncLevel/CallTrack::get_Instance()
		IL_0006: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0, class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0>)
		IL_000b: call void Program/AsyncLevel/CallTrack::Update(class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>)
		IL_0010: nop
		IL_0011: ret
	} // end of method CallTrack::Push

	.method public static 
		class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32> Pop () cil managed 
	{
		// Method begins at RVA 0x233c
		// Code size 29 (0x1d)
		.maxstack 3
		.locals init (
			[0] class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>,
			[1] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> stack,
			[2] class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32> head
		)

		IL_0000: call class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>> Program/AsyncLevel/CallTrack::Peek()
		IL_0005: stloc.0
		IL_0006: ldloc.0
		IL_0007: call instance !0 class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::get_Item1()
		IL_000c: stloc.1
		IL_000d: ldloc.0
		IL_000e: call instance !1 class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::get_Item2()
		IL_0013: stloc.2
		IL_0014: ldloc.1
		IL_0015: call void Program/AsyncLevel/CallTrack::Update(class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>)
		IL_001a: nop
		IL_001b: ldloc.2
		IL_001c: ret
	} // end of method CallTrack::Pop

	// Properties
	.property class [System.Threading]System.Threading.AsyncLocal`1<class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>> 'instance'()
	{
		.custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = (
			01 00 09 00 00 00 00 00
		)
		.get class [System.Threading]System.Threading.AsyncLocal`1<class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>> Program/AsyncLevel/CallTrack::get_instance()
	}
	.property class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Instance()
	{
		.custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = (
			01 00 09 00 00 00 00 00
		)
		.get class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Program/AsyncLevel/CallTrack::get_Instance()
		.set void Program/AsyncLevel/CallTrack::set_Instance(class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>)
	}

} // end of class CallTrack
