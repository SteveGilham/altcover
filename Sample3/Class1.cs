using System;
using System.Collections.Generic;
using System.Linq;

namespace Sample3
{
    public class Class1
    {
        public int Property { get; set; }
    }

    public class Class2
    {
        private int _Property;
        public int Property { get { return _Property; } set { _Property = value; } }
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
    }
}