// based upon https://github.com/kkokosa/Strings.Abbreviations

namespace String.Abbreviations
{
  public static class Strings
  {
    /**
     * All strings are duplicated for various case, to have them interned.
     **/
    private const string Brb = "Be right back";
    private const string Brb_TitleCase = "Be Right Back";
    private const string Imho = "In my humble opinion";
    private const string Imho_TitleCase = "In My Humble Opinion";
    private const string Eos = "En osaa sanoa"; // "Eng. 'Cannot say / I have no opinion.'";
    private const string Lmao = "Laughing my ass off";
    private const string Lmao_TitleCase = "Laughing My Ass Off";
    private const string Rotfl = "Rolling on the floor laughing";
    private const string Rotfl_TitleCase = "Rolling On The Floor Laughing";
    private const string Ama = "Ask me anything";
    private const string Ama_TitleCase = "Ask Me Anything";
    private const string Yolo = "You only live once";
    private const string Yolo_TitleCase = "You Only Live Once";

    public static string BRB(bool titleCase = false) => !titleCase ? Brb : Brb_TitleCase;

    public static string IMHO(bool titleCase = false) => !titleCase ? Imho : Imho_TitleCase;

    public static string EOS() => Eos; // No title case in Finnish

    public static string LMAO(bool titleCase = false) => !titleCase ? Lmao : Lmao_TitleCase;

    public static string ROTFL(bool titleCase = false) => !titleCase ? Rotfl : Rotfl_TitleCase;

    public static string AMA(bool titleCase = false) => !titleCase ? Ama : Ama_TitleCase;

    public static string YOLO(bool titleCase = false) => !titleCase ? Yolo : Yolo_TitleCase;
  }
}