using System.Collections.Generic;

namespace Sample3
{
    public class Class1
    {
        public int Property { get; set; }
    }

    public class Class2
    {
        int _Property;
        public int Property { get { return _Property; } set { _Property = value; } }
    }

    public class Class3
    {
        public class Class4
        {
            public Class1 Property { get; set; }
            public List<T> ToList<T>(T item)
            {
                return new List<T> { item };
            }
        }
    }
}
