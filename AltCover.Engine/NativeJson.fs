namespace AltCover

module NativeJson =
  let inspo = "src/coverlet.core/CoverageResult.cs"
(*
    internal class BranchInfo
    {
        public int Line { get; set; }
        public int Offset { get; set; }
        public int EndOffset { get; set; }
        public int Path { get; set; }
        public uint Ordinal { get; set; }
        public int Hits { get; set; }
    }

    internal class Lines : SortedDictionary<int, int> { }

    internal class Branches : List<BranchInfo> { }

    internal class Method
    {
        internal Method()
        {
            Lines = new Lines();
            Branches = new Branches();
        }
        public Lines Lines;
        public Branches Branches;
    }
    internal class Methods : Dictionary<string, Method> { }
    internal class Classes : Dictionary<string, Methods> { }
    internal class Documents : Dictionary<string, Classes> { }
    internal class Modules : Dictionary<string, Documents> { }

    internal class CoverageResult
    {
        public string Identifier;
        public Modules Modules;
        public bool UseSourceLink;
        internal List<InstrumenterResult> InstrumentedResults;

        internal CoverageResult() { }
*)