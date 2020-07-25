using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Runtime.InteropServices;

namespace AltCover.FontSupport
{
  internal static class NativeMethods
  {
    [DllImport("comdlg32", CharSet = CharSet.Ansi, EntryPoint = "ChooseFont", SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    public extern static bool ChooseFont(IntPtr lpcf);
  }

  public static class Fonts
  {
    public static IEnumerable<string> Wish()
    {
#if NETSTANDARD2_0
      return Fake.Core.ProcessUtils.findFilesOnPath("wish");
#else
      return Enumerable.Empty<string>();
#endif
    }

    public static LogFont SelectWish(string font)
    {
      var info = new ProcessStartInfo()
      {
        Arguments = String.Empty,
        CreateNoWindow = true,
        ErrorDialog = false,
        FileName = Wish().FirstOrDefault(),
        RedirectStandardError = true,
        RedirectStandardInput = true,
        RedirectStandardOutput = true
      };

      //Console.WriteLine("font in '{0}'", font);
      var _ = LogFont.TryParse(font, out var lf);
      var tkfont = lf.ToWishString();
      //Console.WriteLine("font in '{0}'", tkfont);

      using (var wish = Process.Start(info))
      {
        var si = wish.StandardInput;
        si.WriteLine("wm geometry . 1x1");
        si.WriteLine("wm attributes  . -alpha 0.0");
        si.WriteLine("proc fontchosen {f} {");
        si.WriteLine("  puts stdout \"***fontchosen***\"");
        si.WriteLine("  puts stdout $f");
        si.WriteLine("  destroy .");
        si.WriteLine("}");
        si.WriteLine("proc closeout {} {");
        si.WriteLine("  [expr {");
        si.WriteLine("  [tk fontchooser configure -visible] ?");
        si.WriteLine("  \"raise\" : \"destroy\" }] .");
        si.WriteLine("}");
        si.WriteLine("bind . <<TkFontchooserVisibility>> closeout");
        si.WriteLine("tk fontchooser configure -parent . -font "
          + tkfont + " -command fontchosen");
        si.WriteLine("tk fontchooser show");
        wish.WaitForExit();
        var found = false;

        var so = wish.StandardOutput;
        while (!found)
        {
          var sol = so.ReadLine();
          found = string.IsNullOrEmpty(sol) ||
            sol.Equals("***fontchosen***");
        }

        if (!found)
          return null;

        var line = so.ReadLine();
        //Console.WriteLine("font out '{0}'", line);

        if (!LogFont.TryWishParse(line, out var fontOut))
          return null;
        //Console.WriteLine("font out'{0}'", fontOut);
        return fontOut;
      }
    }

    private static IntPtr Allocate<T>()
    {
      var size = Marshal.SizeOf<T>();
      return Marshal.AllocHGlobal(size);
    }

    public static LogFont SelectWin32(string font, IntPtr handle)
    {
      var _ = LogFont.TryParse(font, out LogFont logfont);
      IntPtr pLogfont = Allocate<LogFont>(); //Marshal.AllocHGlobal(Marshal.SizeOf(logfont));
      try
      {
        // Fudge-factor here
        logfont.height = -(int)Math.Round(Math.Abs(logfont.height * 4.0 / 3.0));
        logfont.italic = (byte)(logfont.italic == (byte)0 ? 0 : 255);
        Marshal.StructureToPtr(logfont, pLogfont, false);

        ChooseFont choosefont = new ChooseFont();
        IntPtr pChoosefont = Allocate<ChooseFont>(); //Marshal.AllocHGlobal(Marshal.SizeOf(choosefont));
        try
        {
          choosefont.structSize = Marshal.SizeOf(choosefont);
          choosefont.owner = handle;
          choosefont.minSize = 64;
          choosefont.maxSize = 64;
          choosefont.options = (int)ChooseFontOptions.ScreenFonts
               | (int)ChooseFontOptions.ForceFontExist
               | (int)ChooseFontOptions.InactiveFonts
               | (int)ChooseFontOptions.InitToLogFont
               | (int)ChooseFontOptions.ScaledOnly
               | (int)ChooseFontOptions.FixedPitchOnly;
          choosefont.logFont = pLogfont;
          choosefont.pointSize = logfont.height * 10;

          Marshal.StructureToPtr(choosefont, pChoosefont, false);

          if (NativeMethods.ChooseFont(pChoosefont))
          {
            var chosen = Marshal.PtrToStructure(pChoosefont, typeof(ChooseFont)) as ChooseFont;
            var newfont = Marshal.PtrToStructure(chosen.logFont, typeof(LogFont)) as LogFont;
            newfont.height = chosen.pointSize / 10;
            return newfont;
          }

          return null;
        }
        finally
        {
          Marshal.FreeHGlobal(pChoosefont);
        }
      }
      finally
      {
        Marshal.FreeHGlobal(pLogfont);
      }
    }
  }

  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi),
   SuppressMessage("Gendarme.Rules.Performance", "AvoidUnusedPrivateFieldsRule",
    Justification = "Represents a native structure"),
   SuppressMessage("Gendarme.Rules.Design", "AvoidVisibleFieldsRule",
    Justification = "Represents a native structure"),
   SuppressMessage("Gendarme.Rules.Security", "NativeFieldsShouldNotBeVisibleRule",
    Justification = "Represents a native structure"),
   SuppressMessage("Gendarme.Rules.BadPractice", "PreferSafeHandleRule",
    Justification = "Represents a native structure")
  ]
  public class ChooseFont
  {
    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int structSize;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    [SuppressMessage("Microsoft.Security", "CA2111:PointersShouldNotBeVisible", Justification = "Represents a native structure")]
    public IntPtr owner;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    [SuppressMessage("Microsoft.Security", "CA2111:PointersShouldNotBeVisible", Justification = "Represents a native structure")]
    public IntPtr dc;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    [SuppressMessage("Microsoft.Security", "CA2111:PointersShouldNotBeVisible", Justification = "Represents a native structure")]
    public IntPtr logFont;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int pointSize;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int options; //flags

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int colours;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    [SuppressMessage("Microsoft.Security", "CA2111:PointersShouldNotBeVisible", Justification = "Represents a native structure")]
    public IntPtr customData;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    [SuppressMessage("Microsoft.Security", "CA2111:PointersShouldNotBeVisible", Justification = "Represents a native structure")]
    public IntPtr hook;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public string templateName;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    [SuppressMessage("Microsoft.Security", "CA2111:PointersShouldNotBeVisible", Justification = "Represents a native structure")]
    public IntPtr instance;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public string style;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public short fontType;

    private short alignmentDummy;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int minSize;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int maxSize;
  }

  [Flags,
   Serializable]
  public enum ChooseFontOptions
  {
    ScreenFonts = 0x00000001,
    PrinterFonts = 0x00000002,
    Both = (ScreenFonts | PrinterFonts),
    ShowHelp = 0x00000004,
    EnableHook = 0x00000008,
    EnableTemplate = 0x00000010,
    EnableTemplateHandle = 0x00000020,
    InitToLogFont = 0x00000040,
    UseStyle = 0x00000080,
    Effects = 0x00000100,
    Apply = 0x00000200,
    AnsiOnly = 0x00000400,
    ScriptsOnly = AnsiOnly,
    NoVectorFonts = 0x00000800,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    NoOemFonts = NoVectorFonts,

    NoSimulations = 0x00001000,
    LimitSize = 0x00002000,
    FixedPitchOnly = 0x00004000,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Wysiwyg = 0x00008000,

    ForceFontExist = 0x00010000,
    ScaledOnly = 0x00020000,
    TTOnly = 0x00040000,
    NoFace = 0x00080000,
    NoStyle = 0x00100000,
    NoSize = 0x00200000,
    SelectScript = 0x00400000,
    NoScript = 0x00800000,
    NoVerticalFonts = 0x01000000,
    InactiveFonts = 0x02000000
  }

  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi),
   SuppressMessage("Gendarme.Rules.Design", "AvoidVisibleFieldsRule",
    Justification = "Represents a native structure"),
   SuppressMessage("Gendarme.Rules.Smells", "AvoidSpeculativeGeneralityRule",
    Justification = "Seriously, u wot m8!?")
    ]
  public class LogFont
  {
    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int height;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int width;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int escapement;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int orientation;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public int weight;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public byte italic;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public byte underline;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly",
      Justification = "Seriously, u wot m8!?")]
    public byte strikeOut;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public byte charSet;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public byte outPrecision;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public byte clipPrecision;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public byte quality;

    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public byte pitchAndFamily;

    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
    [SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Justification = "Represents a native structure")]
    public string faceName = string.Empty;

    public LogFont()
    {
      var p = (int)Environment.OSVersion.Platform;
      var isWindows = p <= 3;

      faceName = isWindows ? "Courier New" : "Monospace";
      weight = (int)FontWeight.Normal;
      height = 10;
    }

    // Pango-style text
    [SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods",
      Justification = "validate local variable ''(*decode)'', which was reassigned from parameter 'decode', before using it -- u wot m8!?")]
    public static bool TryParse(string encoded, out LogFont decode)
    {
      decode = new LogFont();
      if (string.IsNullOrEmpty(encoded))
        return false;
      var core = encoded;

      // discard variations
      var at = core.IndexOf('@');
      if (at >= 0)
        core = core.Substring(0, at);

      core = core.TrimEnd();

      // look for a size
      var end = core.LastIndexOfAny(new char[] { ' ', ',' });
      var sized = false;
      if (end >= 0)
      {
        var size = core.Substring(end + 1).TrimEnd();
        if (size.EndsWith("px", StringComparison.Ordinal))
          size = size.Substring(0, size.Length - 2);
        sized = Int32.TryParse(size, out int fontsize);
        if (sized)
        {
          decode.height = fontsize;
          core = core.Substring(0, end);
        }
      }

      // style words
      end = core.LastIndexOfAny(new char[] { ' ', ',' });
      while (end >= 0)
      {
        var slug = core.Substring(end + 1).TrimEnd();
        bool matched(string item, LogFont font)
        {
          switch (item)
          {
            case "Normal":
              break;

            case "Roman":
              break; // todo
            case "Oblique":
              font.italic = 127;
              break;

            case "Italic":
              font.italic = 255;
              break;

            case "Small-Caps":
              break; // todo
            case "Thin":
              font.weight = (int)FontWeight.Thin;
              break;

            case "Ultra-Light":
            case "Extra-Light":
              font.weight = (int)FontWeight.Ultralight;
              break;

            case "Light":
              font.weight = (int)FontWeight.Light;
              break;

            case "Semi-Light":
            case "Demi-Light":
              font.weight = (int)FontWeight.Semilight;
              break;

            case "Book":
              font.weight = (int)FontWeight.Book;
              break;

            case "Regular":
              font.weight = (int)FontWeight.Normal;
              break;

            case "Medium":
              font.weight = (int)FontWeight.Medium;
              break;

            case "Semi-Bold":
            case "Demi-Bold":
              font.weight = (int)FontWeight.Semibold;
              break;

            case "Bold":
              font.weight = (int)FontWeight.Bold;
              break;

            case "Ultra-Bold":
            case "Extra-Bold":
              font.weight = (int)FontWeight.Ultrabold;
              break;

            case "Heavy":
            case "Black":
              font.weight = (int)FontWeight.Heavy;
              break;

            case "Ultra-Heavy":
            case "Extra-Heavy":
            case "Ultra-Black":
            case "Extra-Black":
              font.weight = (int)FontWeight.Ultraheavy;
              break;

            case "Ultra-Condensed":
              break; // todo
            case "Extra-Condensed":
              break; // todo
            case "Condensed":
              break; // todo
            case "Semi-Condensed":
              break; // todo
            case "Semi-Expanded":
              break; // todo
            case "Expanded":
              break; // todo
            case "Extra-Expanded":
              break; // todo
            case "Ultra-Expanded":
              break; // todo
            case "Not-Rotated":
              break; // todo
            case "South":
              break; // todo
            case "Upside-Down":
              break; // todo
            case "North":
              break; // todo
            case "Rotated-Left":
              break; // todo
            case "East":
              break; // todo
            case "Rotated-Right":
              break; // todo
            case "West":
              break; // todo
            default:
              return false;
          }
          return true;
        }

        if (!matched(slug, decode))
        {
          break;
        }

        core = core.Substring(0, end);
        end = core.LastIndexOfAny(new char[] { ' ', ',' });
      }

      // Family list, just want the first one
      core = core.TrimEnd(' ', ',');
      end = core.IndexOf(',');
      if (end >= 0)
        core = core.Substring(0, end);

      decode.faceName = core;

      return core.Length > 0;
    }

    // Tcl/Tk-style text like {Consolas} 12 bold roman
    [SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods",
      Justification = "validate local variable ''(*decode)'', which was reassigned from parameter 'decode', before using it -- u wot m8!?")]
    public static bool TryWishParse(string encoded, out LogFont decode)
    {
      decode = new LogFont();
      if (string.IsNullOrEmpty(encoded))
        return false;

      var core = encoded;
      core = core.TrimEnd();

      var curly = core.IndexOf('}');
      if (curly > 0)
      {
        decode.faceName = core.Substring(1, curly - 1);
        core = core.Substring(curly + 1);
      }
      else
      {
        var space = core.IndexOf(' ');
        if (space < 0)
          return false;
        decode.faceName = core.Substring(0, space);
        core = core.Substring(space + 1);
      }

      foreach (var token in core.Split(' '))
      {
        if (token.Equals("italic", StringComparison.OrdinalIgnoreCase))
          decode.italic = 255;
        if (token.Equals("bold", StringComparison.OrdinalIgnoreCase))
          decode.weight = (int)FontWeight.Bold;
        if (Int32.TryParse(token, out var points))
          decode.height = points;
      }

      return true;
    }

    public override string ToString()

    {
      // Pango names like Fira Code Bold Oblique 17
      // This is not quite sufficient
      // NORMAL: the font is upright.
      // OBLIQUE: the font is slanted, but in a roman style.
      // ITALIC: the font is slanted in an italic style.
      //
      // AOB??
      var slant = string.Empty;
      if (this.italic != 0) slant = "Italic ";
      return FormattableString.Invariant($"{faceName}, {(FontWeight)weight} {slant}{height}");
    }

    public string ToWishString()
    {
      // Tk names like {{Consolas} 12 bold roman}
      var w = "normal";
      if (weight > (int)FontWeight.Normal) w = "bold";
      var i = "italic";
      if (italic == 0) i = "roman";
      return FormattableString.Invariant($"{{{{{faceName}}} {height} {w} {i}}}");
    }
  }

  [Serializable]
  public enum FontWeight
  {
    DoNotCare = 0,
    Thin = 100,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Ultralight = 200,

    Light = 300,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Semilight = 350,

    Book = 380,
    Normal = 400, //aka Regular
    Medium = 500,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Semibold = 600,

    Bold = 700,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Ultrabold = 800,

    Heavy = 900,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Ultraheavy = 1000
  }

  [SuppressMessage("Gendarme.Rules.Design", "EnumsShouldUseInt32Rule",
    Justification = "Represents a byte-valued field"),
   SuppressMessage("Microsoft.Design", "CA1028:EnumStorageShouldBeInt32",
    Justification = "Represents a byte-valued field"),
   Serializable]
  public enum FontCharSet : byte
  {
    Ansi = 0,
    Default = 1,
    Symbol = 2,

    [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
      Justification = "Seriously, u wot m8!?")]
    ShiftJIS = 128,

    Hangeul = 129,
    Hangul = 129,
    GB2312 = 134,
    ChineseBig5 = 136,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Oem = 255,

    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification = "Seriously, u wot m8!?")]
    Johab = 130,

    Hebrew = 177,
    Arabic = 178,
    Greek = 161,
    Turkish = 162,
    Vietnamese = 163,
    Thai = 222,
    EastEurope = 238,
    Russian = 204,
    Mac = 77,
    Baltic = 186,
  }

  [SuppressMessage("Gendarme.Rules.Design", "EnumsShouldUseInt32Rule",
    Justification = "Represents a byte-valued field"),
   SuppressMessage("Microsoft.Design", "CA1028:EnumStorageShouldBeInt32",
    Justification = "Represents a byte-valued field"),
   Serializable]
  public enum FontPrecision : byte
  {
    Default = 0,
    String = 1,
    Character = 2,
    Stroke = 3,
    TT = 4,
    Device = 5,
    Raster = 6,
    TTOnly = 7,
    Outlines = 8,
    ScreenOutline = 9,
    PSOnly = 10,
  }

  [SuppressMessage("Gendarme.Rules.Design", "EnumsShouldUseInt32Rule",
    Justification = "Represents a byte-valued field"),
   SuppressMessage("Microsoft.Design", "CA1028:EnumStorageShouldBeInt32",
    Justification = "Represents a byte-valued field"),
   Serializable]
  public enum FontClipPrecision : byte
  {
    Default = 0,
    Character = 1,
    Stroke = 2,
    Mask = 0xf,
    LHAngles = (1 << 4),
    TTAlways = (2 << 4),

    [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
      Justification = "Seriously, u wot m8!?")]
    DFADisable = (4 << 4),

    Embedded = (8 << 4),
  }

  [SuppressMessage("Gendarme.Rules.Design", "EnumsShouldUseInt32Rule",
    Justification = "Represents a byte-valued field"),
   SuppressMessage("Microsoft.Design", "CA1028:EnumStorageShouldBeInt32",
    Justification = "Represents a byte-valued field"),
   Serializable]
  public enum FontQuality : byte
  {
    Default = 0,
    Draft = 1,
    Proof = 2,
    NonAntiAliased = 3,
    AntiAliased = 4,
    ClearType = 5,
    ClearTypeNatural = 6,
  }

  [Flags,
   SuppressMessage("Gendarme.Rules.Design", "EnumsShouldUseInt32Rule",
    Justification = "Represents a byte-valued field"),
   SuppressMessage("Microsoft.Design", "CA1028:EnumStorageShouldBeInt32",
    Justification = "Represents a byte-valued field"),
   SuppressMessage("Microsoft.Design", "CA1008:EnumsShouldHaveZeroValue",
    Justification = "Seriously, u wot m8!?"),
   Serializable]
  public enum FontPitchAndFamilyOptions : byte
  {
    Default = 0,
    Fixed = 1,
    Variable = 2,
    DoNotCare = (0 << 4),
    Roman = (1 << 4),
    Swiss = (2 << 4),
    Modern = (3 << 4),
    Script = (4 << 4),
    Decorative = (5 << 4),
  }
}