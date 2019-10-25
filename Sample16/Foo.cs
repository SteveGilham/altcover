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