namespace Sample16
{
    public static class Foo
    {
        public static string Bar(string value)
        {
            string ret;

            switch (value)
            {
                case "a":
                    ret = "a";
                    break;

                case "b":
                    ret = "b";
                    break;

                case "c":
                    ret = "c";
                    break;

                default:
                    ret = "d";
                    break;
            }

            return ret;
        }
    }
}