using System;
using System.Runtime.InteropServices;

namespace AltCover.FontSupport
{
  internal static class NativeMethods
  {
    [DllImport("comdlg32.dll", CharSet = CharSet.Auto, EntryPoint = "ChooseFont", SetLastError = true)]
    public extern static bool ChooseFont(IntPtr lpcf);
  }

  public static class Fonts
  {
    public static string Select(string font)
    {
      LOGFONT logfont = new LOGFONT();
      IntPtr pLogfont = Marshal.AllocHGlobal(Marshal.SizeOf(logfont));
      try
      {
        logfont.lfFaceName = "Segoe UI"; // TODO
        Marshal.StructureToPtr(logfont, pLogfont, false);

        CHOOSEFONT choosefont = new CHOOSEFONT();
        IntPtr pChoosefont = Marshal.AllocHGlobal(Marshal.SizeOf(choosefont));
        try
        {
          choosefont.lStructSize = Marshal.SizeOf(choosefont);
          choosefont.nSizeMin = 64;
          choosefont.nSizeMax = 64;
          choosefont.Flags = (int)CHOOSEFONTFLAGS.CF_SCREENFONTS
               | (int)CHOOSEFONTFLAGS.CF_FORCEFONTEXIST
               | (int)CHOOSEFONTFLAGS.CF_INACTIVEFONTS
               | (int)CHOOSEFONTFLAGS.CF_INITTOLOGFONTSTRUCT
               | (int)CHOOSEFONTFLAGS.CF_SCALABLEONLY
               | (int)CHOOSEFONTFLAGS.CF_FIXEDPITCHONLY
               | (int)CHOOSEFONTFLAGS.CF_USESTYLE;
          choosefont.lpLogFont = pLogfont;

          Marshal.StructureToPtr(choosefont, pChoosefont, false);

          if (NativeMethods.ChooseFont(pChoosefont))
          {
            // TODO
            return "Monospace";
          }

          return String.Empty;
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

  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
  public struct CHOOSEFONT
  {
    public int lStructSize;
    public IntPtr hwndOwner;
    public IntPtr hDC;
    public IntPtr lpLogFont;
    public int iPointSize;
    public int Flags;
    public int rgbColors;
    public IntPtr lCustData;
    public IntPtr lpfnHook;
    public string lpTemplateName;
    public IntPtr hInstance;
    public string lpszStyle;
    public short nFontType;
    private short __MISSING_ALIGNMENT__;
    public int nSizeMin;
    public int nSizeMax;
  }

  [Flags]
  public enum CHOOSEFONTFLAGS
  {
    CF_SCREENFONTS = 0x00000001,
    CF_PRINTERFONTS = 0x00000002,
    CF_BOTH = (CF_SCREENFONTS | CF_PRINTERFONTS),
    CF_SHOWHELP = 0x00000004,
    CF_ENABLEHOOK = 0x00000008,
    CF_ENABLETEMPLATE = 0x00000010,
    CF_ENABLETEMPLATEHANDLE = 0x00000020,
    CF_INITTOLOGFONTSTRUCT = 0x00000040,
    CF_USESTYLE = 0x00000080,
    CF_EFFECTS = 0x00000100,
    CF_APPLY = 0x00000200,
    CF_ANSIONLY = 0x00000400,
    CF_SCRIPTSONLY = CF_ANSIONLY,
    CF_NOVECTORFONTS = 0x00000800,
    CF_NOOEMFONTS = CF_NOVECTORFONTS,
    CF_NOSIMULATIONS = 0x00001000,
    CF_LIMITSIZE = 0x00002000,
    CF_FIXEDPITCHONLY = 0x00004000,
    CF_WYSIWYG = 0x00008000,
    CF_FORCEFONTEXIST = 0x00010000,
    CF_SCALABLEONLY = 0x00020000,
    CF_TTONLY = 0x00040000,
    CF_NOFACESEL = 0x00080000,
    CF_NOSTYLESEL = 0x00100000,
    CF_NOSIZESEL = 0x00200000,
    CF_SELECTSCRIPT = 0x00400000,
    CF_NOSCRIPTSEL = 0x00800000,
    CF_NOVERTFONTS = 0x01000000,
    CF_INACTIVEFONTS = 0x02000000
  }

  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
  public class LOGFONT
  {
    public int lfHeight = 0;
    public int lfWidth = 0;
    public int lfEscapement = 0;
    public int lfOrientation = 0;
    public int lfWeight = 0;
    public byte lfItalic = 0;
    public byte lfUnderline = 0;
    public byte lfStrikeOut = 0;
    public byte lfCharSet = 0;
    public byte lfOutPrecision = 0;
    public byte lfClipPrecision = 0;
    public byte lfQuality = 0;
    public byte lfPitchAndFamily = 0;

    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
    public string lfFaceName = string.Empty;
  }

  public enum FontWeight : int
  {
    FW_DONTCARE = 0,
    FW_THIN = 100,
    FW_EXTRALIGHT = 200,
    FW_LIGHT = 300,
    FW_NORMAL = 400,
    FW_MEDIUM = 500,
    FW_SEMIBOLD = 600,
    FW_BOLD = 700,
    FW_EXTRABOLD = 800,
    FW_HEAVY = 900,
  }

  public enum FontCharSet : byte
  {
    ANSI_CHARSET = 0,
    DEFAULT_CHARSET = 1,
    SYMBOL_CHARSET = 2,
    SHIFTJIS_CHARSET = 128,
    HANGEUL_CHARSET = 129,
    HANGUL_CHARSET = 129,
    GB2312_CHARSET = 134,
    CHINESEBIG5_CHARSET = 136,
    OEM_CHARSET = 255,
    JOHAB_CHARSET = 130,
    HEBREW_CHARSET = 177,
    ARABIC_CHARSET = 178,
    GREEK_CHARSET = 161,
    TURKISH_CHARSET = 162,
    VIETNAMESE_CHARSET = 163,
    THAI_CHARSET = 222,
    EASTEUROPE_CHARSET = 238,
    RUSSIAN_CHARSET = 204,
    MAC_CHARSET = 77,
    BALTIC_CHARSET = 186,
  }

  public enum FontPrecision : byte
  {
    OUT_DEFAULT_PRECIS = 0,
    OUT_STRING_PRECIS = 1,
    OUT_CHARACTER_PRECIS = 2,
    OUT_STROKE_PRECIS = 3,
    OUT_TT_PRECIS = 4,
    OUT_DEVICE_PRECIS = 5,
    OUT_RASTER_PRECIS = 6,
    OUT_TT_ONLY_PRECIS = 7,
    OUT_OUTLINE_PRECIS = 8,
    OUT_SCREEN_OUTLINE_PRECIS = 9,
    OUT_PS_ONLY_PRECIS = 10,
  }

  public enum FontClipPrecision : byte
  {
    CLIP_DEFAULT_PRECIS = 0,
    CLIP_CHARACTER_PRECIS = 1,
    CLIP_STROKE_PRECIS = 2,
    CLIP_MASK = 0xf,
    CLIP_LH_ANGLES = (1 << 4),
    CLIP_TT_ALWAYS = (2 << 4),
    CLIP_DFA_DISABLE = (4 << 4),
    CLIP_EMBEDDED = (8 << 4),
  }

  public enum FontQuality : byte
  {
    DEFAULT_QUALITY = 0,
    DRAFT_QUALITY = 1,
    PROOF_QUALITY = 2,
    NONANTIALIASED_QUALITY = 3,
    ANTIALIASED_QUALITY = 4,
    CLEARTYPE_QUALITY = 5,
    CLEARTYPE_NATURAL_QUALITY = 6,
  }

  [Flags]
  public enum FontPitchAndFamily : byte
  {
    DEFAULT_PITCH = 0,
    FIXED_PITCH = 1,
    VARIABLE_PITCH = 2,
    FF_DONTCARE = (0 << 4),
    FF_ROMAN = (1 << 4),
    FF_SWISS = (2 << 4),
    FF_MODERN = (3 << 4),
    FF_SCRIPT = (4 << 4),
    FF_DECORATIVE = (5 << 4),
  }
}