using System;
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

    readonly ref struct Token
    {
        public readonly int Start;
        public readonly int End;

        public Token(int start, int end)
        {
            Start = start;
            End = end;
        }
    }

    [Obsolete("Don't use this one, use 'Token' instead. Don't use this one, use 'Token' instead. ", true)]
    readonly ref struct OldToken
    {
        public readonly int Start;
        public readonly int End;

        public OldToken(int start, int end)
        {
            Start = start;
            End = end;
        }
    }

}