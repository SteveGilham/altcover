using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

namespace AltCover.FontSupport
{
  internal static class NativeMethods
  {
    [DllImport("comdlg32", CharSet = CharSet.Ansi, EntryPoint = "ChooseFont", SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    public extern static bool ChooseFont(IntPtr lpcf);

    [DllImport("libglib-2.0-0.dll", CharSet = CharSet.Auto, EntryPoint = "g_get_real_time", SetLastError = true),
     SuppressMessage("Gendarme.Rules.Naming", "UseCorrectCasingRule",
      Justification = "Represents a native function")]
    public extern static long g_get_real_time();
  }

  public static class Fonts
  {
    //TODO?? -- WindowFromPoint & GetAncestor(..., root) -> hwndOwner

    [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly",
      Justification = "Seriously, u wot m8!?")]
    public static IEnumerable<long> GTK()
    {
      long time;
      try
      {
        time = NativeMethods.g_get_real_time();
      }
      catch (System.DllNotFoundException)
      {
        yield break;
      }

      yield return time;
    }

    public static LogFont Select(string font)
    {
      var _ = LogFont.TryParse(font, out LogFont logfont);
      IntPtr pLogfont = Marshal.AllocHGlobal(Marshal.SizeOf(logfont));
      try
      {
        // Pango names like Fira Code Bold Oblique 17
        // This is not sufficient
        //var facets = font.Split(' ');
        //var face = string.Join(" ", facets.Take(facets.Length - 2));
        //var weight = FontWeight.Dontcare;
        //if (!Enum.TryParse<FontWeight>(facets[facets.Length - 2], true, out weight))
        //{
        //  weight = FontWeight.Dontcare;
        //}
        //logfont.faceName = face;
        Marshal.StructureToPtr(logfont, pLogfont, false);

        ChooseFont choosefont = new ChooseFont();
        IntPtr pChoosefont = Marshal.AllocHGlobal(Marshal.SizeOf(choosefont));
        try
        {
          choosefont.structSize = Marshal.SizeOf(choosefont);
          choosefont.minSize = 64;
          choosefont.maxSize = 64;
          choosefont.options = (int)ChooseFontOptions.ScreenFonts
               | (int)ChooseFontOptions.ForceFontExist
               | (int)ChooseFontOptions.InactiveFonts
               | (int)ChooseFontOptions.InitToLogFont
               | (int)ChooseFontOptions.ScaledOnly
               | (int)ChooseFontOptions.FixedPitchOnly
               | (int)ChooseFontOptions.UseStyle;
          choosefont.logFont = pLogfont;

          Marshal.StructureToPtr(choosefont, pChoosefont, false);

          if (NativeMethods.ChooseFont(pChoosefont))
          {
            var chosen = Marshal.PtrToStructure(pChoosefont, typeof(ChooseFont)) as ChooseFont;
            var newfont = Marshal.PtrToStructure(chosen.logFont, typeof(LogFont)) as LogFont;

            // TODO
            return newfont;
          }

          return new LogFont();
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
      faceName = "Monospace";
      weight = (int)FontWeight.Normal;
      height = 10;
    }

    // Pango-style text
    public static bool TryParse(string encoded, out LogFont decode)
    {
      decode = new LogFont();
      return String.IsNullOrEmpty(encoded);
    }

    [SuppressMessage("Microsoft.Globalization", "CA1305:SpecifyIFormatProvider",
      Justification = "Seriously, u wot m8!?")]
    public override string ToString()

    {
      // Pango names like Fira Code Bold Oblique 17
      // This is not quite sufficient
      return $"{faceName}, {(FontWeight)weight} {height}";
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
    Normal = 400,
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
   SuppressMessage("Gendarme.Rules.Design", "EnumsShouldDefineAZeroValueRule",
    Justification = "Seriously, u wot m8!?"),
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
   SuppressMessage("Gendarme.Rules.Design", "EnumsShouldDefineAZeroValueRule",
    Justification = "Seriously, u wot m8!?"),
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
   SuppressMessage("Gendarme.Rules.Design", "EnumsShouldDefineAZeroValueRule",
    Justification = "Seriously, u wot m8!?"),
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
   SuppressMessage("Gendarme.Rules.Design", "EnumsShouldDefineAZeroValueRule",
    Justification = "Seriously, u wot m8!?"),
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