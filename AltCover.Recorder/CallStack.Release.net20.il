.class nested assembly auto ansi abstract sealed serializable CallTrack
	extends [System.Runtime]System.Object
{
	.custom instance void [FSharp.Core]Microsoft.FSharp.Core.SealedAttribute::.ctor() = (
		01 00 00 00
	)
	.custom instance void [FSharp.Core]Microsoft.FSharp.Core.AbstractClassAttribute::.ctor() = (
		01 00 00 00
	)
	.custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = (
		01 00 03 00 00 00 00 00
	)
	// Fields
	.field assembly static class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> instance@
	.custom instance void [System.Runtime]System.ThreadStaticAttribute::.ctor() = (
		01 00 00 00
	)
	.custom instance void [FSharp.Core]Microsoft.FSharp.Core.DefaultValueAttribute::.ctor() = (
		01 00 00 00
	)

	// Methods
	.method assembly specialname static 
		class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> get_instance () cil managed 
	{
		// Method begins at RVA 0x24c8
		// Code size 6 (0x6)
		.maxstack 8

		IL_0000: ldsfld class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> Program/ThreadLevel/CallTrack::instance@
		IL_0005: ret
	} // end of method CallTrack::get_instance

	.method assembly specialname static 
		void set_instance (
			class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> 'value'
		) cil managed 
	{
		// Method begins at RVA 0x24d0
		// Code size 7 (0x7)
		.maxstack 8

		IL_0000: ldarg.0
		IL_0001: stsfld class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> Program/ThreadLevel/CallTrack::instance@
		IL_0006: ret
	} // end of method CallTrack::set_instance

	.method assembly static 
		void Update (
			class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> l
		) cil managed 
	{
		// Method begins at RVA 0x24d8
		// Code size 12 (0xc)
		.maxstack 8

		IL_0000: ldarg.0
		IL_0001: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<!0> class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>::Some(!0)
		IL_0006: call void Program/ThreadLevel/CallTrack::set_instance(class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>)
		IL_000b: ret
	} // end of method CallTrack::Update

	.method assembly specialname static 
		class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> get_Instance () cil managed 
	{
		// Method begins at RVA 0x24e8
		// Code size 42 (0x2a)
		.maxstack 3
		.locals init (
			[0] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>
		)

		IL_0000: ldsfld class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> Program/ThreadLevel/CallTrack::instance@
		IL_0005: brtrue.s IL_001b

		IL_0007: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_Empty()
		IL_000c: stloc.0
		IL_000d: ldloc.0
		IL_000e: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<!0> class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>::Some(!0)
		IL_0013: call void Program/ThreadLevel/CallTrack::set_instance(class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>)
		IL_0018: nop
		IL_0019: br.s IL_001d

		IL_001b: nop
		IL_001c: nop

		IL_001d: ldsfld class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> Program/ThreadLevel/CallTrack::instance@
		IL_0022: tail.
		IL_0024: callvirt instance !0 class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>::get_Value()
		IL_0029: ret
	} // end of method CallTrack::get_Instance

	.method assembly static 
		class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>> Peek () cil managed 
	{
		// Method begins at RVA 0x2520
		// Code size 55 (0x37)
		.maxstack 4
		.locals init (
			[0] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>,
			[1] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>,
			[2] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> xs,
			[3] int32 h
		)

		IL_0000: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Program/ThreadLevel/CallTrack::get_Instance()
		IL_0005: stloc.0
		IL_0006: ldloc.0
		IL_0007: call instance class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		IL_000c: brfalse.s IL_002b

		IL_000e: ldloc.0
		IL_000f: stloc.1
		IL_0010: ldloc.1
		IL_0011: call instance class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_TailOrNull()
		IL_0016: stloc.2
		IL_0017: ldloc.1
		IL_0018: call instance !0 class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_HeadOrDefault()
		IL_001d: stloc.3
		IL_001e: ldloc.2
		IL_001f: ldloc.3
		IL_0020: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<!0> class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>::Some(!0)
		IL_0025: newobj instance void class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::.ctor(!0, !1)
		IL_002a: ret

		IL_002b: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_Empty()
		IL_0030: ldnull
		IL_0031: newobj instance void class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::.ctor(!0, !1)
		IL_0036: ret
	} // end of method CallTrack::Peek

	.method assembly static 
		void Push (
			int32 x
		) cil managed 
	{
		// Method begins at RVA 0x2564
		// Code size 24 (0x18)
		.maxstack 4
		.locals init (
			[0] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>
		)

		IL_0000: ldarg.0
		IL_0001: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Program/ThreadLevel/CallTrack::get_Instance()
		IL_0006: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0, class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0>)
		IL_000b: stloc.0
		IL_000c: ldloc.0
		IL_000d: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<!0> class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>::Some(!0)
		IL_0012: call void Program/ThreadLevel/CallTrack::set_instance(class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>)
		IL_0017: ret
	} // end of method CallTrack::Push

	.method assembly static 
		class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32> Pop () cil managed 
	{
		// Method begins at RVA 0x2588
		// Code size 33 (0x21)
		.maxstack 3
		.locals init (
			[0] class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>,
			[1] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> stack,
			[2] class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32> head
		)

		IL_0000: call class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>> Program/ThreadLevel/CallTrack::Peek()
		IL_0005: stloc.0
		IL_0006: ldloc.0
		IL_0007: call instance !0 class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::get_Item1()
		IL_000c: stloc.1
		IL_000d: ldloc.0
		IL_000e: call instance !1 class [System.Runtime]System.Tuple`2<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>, class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>>::get_Item2()
		IL_0013: stloc.2
		IL_0014: ldloc.1
		IL_0015: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<!0> class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>::Some(!0)
		IL_001a: call void Program/ThreadLevel/CallTrack::set_instance(class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>)
		IL_001f: ldloc.2
		IL_0020: ret
	} // end of method CallTrack::Pop

	// Properties
	.property class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> 'instance'()
	{
		.custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags, int32) = (
			01 00 04 00 00 00 00 00 00 00 00 00
		)
		.get class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>> Program/ThreadLevel/CallTrack::get_instance()
		.set void Program/ThreadLevel/CallTrack::set_instance(class [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>>)
	}
	.property class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Instance()
	{
		.get class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> Program/ThreadLevel/CallTrack::get_Instance()
	}

} // end of class CallTrack
