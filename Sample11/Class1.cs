using System.Diagnostics.CodeAnalysis;

namespace Sample11
{
    public class Class1
    {
        private const string p1 = "p1";

        [ExcludeFromCodeCoverage]
        public string P1
        {
            get
            {
                return p1;
            }
        }

        [ExcludeFromCodeCoverage]
        public string P2
        {
            get { return "P2"; }
        }

        [ExcludeFromCodeCoverage]
        public int P3 => 2;

        private string p4 = "p4";

        [ExcludeFromCodeCoverage]
        public string P4
        {
            get
            {
                return p4;
            }
            set
            {
                p4 = value;
            }
        }
    }
}