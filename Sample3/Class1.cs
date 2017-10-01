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

}
