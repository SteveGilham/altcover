using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

namespace AltCover.FontSupport
{
  namespace Win32
  {
    internal static class NativeMethods
    {
      [DllImport("comdlg32", CharSet = CharSet.Ansi, EntryPoint = "ChooseFont", SetLastError = true)]
      [return: MarshalAs(UnmanagedType.Bool)]
      public extern static bool ChooseFont(IntPtr lpcf);

      [DllImport("tcl86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_CreateInterp();

      [DllImport("tcl86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern int Tcl_Init(IntPtr interp);

      [DllImport("tcl86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_GetObjResult(IntPtr interp);

      [DllImport("tcl86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_GetStringFromObj(IntPtr tclObj, IntPtr length);

      [DllImport("tcl86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_GetVar(IntPtr interp, string varName, int flags);

      [DllImport("tcl86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern int Tcl_Eval(IntPtr interp, string script);

      [DllImport("tcl86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_DeleteInterp(IntPtr interp);

      [DllImport("tk86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern int Tk_Init(IntPtr interp);

      [DllImport("tk86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tk_MainWindow(IntPtr interp);

      [DllImport("tk86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern void Tk_MakeWindowExist(IntPtr tkwin);

      [DllImport("tk86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern void Tk_MapWindow(IntPtr tkwin);

      [DllImport("tk86", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern void Tk_DestroyWindow(IntPtr tkwin);
    }
  }

  namespace NonWindows
  {
    // I seem to need 'export PATH=/usr/lib/x86_64-linux-gnu:$PATH'??
    internal static class NativeMethods
    {
      [DllImport("tcl8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_CreateInterp();

      [DllImport("tcl8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern int Tcl_Init(IntPtr interp);

      [DllImport("tcl8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_GetObjResult(IntPtr interp);

      [DllImport("tcl8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_GetStringFromObj(IntPtr tclObj, IntPtr length);

      [DllImport("tcl8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_GetVar(IntPtr interp, string varName, int flags);

      [DllImport("tcl8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern int Tcl_Eval(IntPtr interp, string script);

      [DllImport("tcl8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tcl_DeleteInterp(IntPtr interp);

      [DllImport("tk8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern int Tk_Init(IntPtr interp);

      [DllImport("tk8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern IntPtr Tk_MainWindow(IntPtr interp);

      [DllImport("tk8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern void Tk_MakeWindowExist(IntPtr tkwin);

      [DllImport("tk8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern void Tk_MapWindow(IntPtr tkwin);

      [DllImport("tk8.6", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
      public static extern void Tk_DestroyWindow(IntPtr tkwin);
    }
  }

  internal static class Externals
  {
    private static bool isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows);

    public static IntPtr Tcl_CreateInterp()
    {
      if (isWindows)
        return Win32.NativeMethods.Tcl_CreateInterp();
      else
        return NonWindows.NativeMethods.Tcl_CreateInterp();
    }

    public static int Tcl_Init(IntPtr interp)
    {
      if (isWindows)
        return Win32.NativeMethods.Tcl_Init(interp);
      else
        return NonWindows.NativeMethods.Tcl_Init(interp);
    }

    public static IntPtr Tcl_GetObjResult(IntPtr interp)
    {
      if (isWindows)
        return Win32.NativeMethods.Tcl_GetObjResult(interp);
      else
        return NonWindows.NativeMethods.Tcl_GetObjResult(interp);
    }

    public static IntPtr Tcl_GetStringFromObj(IntPtr tclObj, IntPtr length)
    {
      if (isWindows)
        return Win32.NativeMethods.Tcl_GetStringFromObj(tclObj, length);
      else
        return NonWindows.NativeMethods.Tcl_GetStringFromObj(tclObj, length);
    }

    public static IntPtr Tcl_GetVar(IntPtr interp, string varName, int flags)
    {
      if (isWindows)
        return Win32.NativeMethods.Tcl_GetVar(interp, varName, flags);
      else
        return NonWindows.NativeMethods.Tcl_GetVar(interp, varName, flags);
    }

    public static int Tcl_Eval(IntPtr interp, string script)
    {
      if (isWindows)
        return Win32.NativeMethods.Tcl_Eval(interp, script);
      else
        return NonWindows.NativeMethods.Tcl_Eval(interp, script);
    }

    public static IntPtr Tcl_DeleteInterp(IntPtr interp)
    {
      if (isWindows)
        return Win32.NativeMethods.Tcl_DeleteInterp(interp);
      else
        return NonWindows.NativeMethods.Tcl_DeleteInterp(interp);
    }

    public static int Tk_Init(IntPtr interp)
    {
      if (isWindows)
        return Win32.NativeMethods.Tk_Init(interp);
      else
        return NonWindows.NativeMethods.Tk_Init(interp);
    }

    public static IntPtr Tk_MainWindow(IntPtr interp)
    {
      if (isWindows)
        return Win32.NativeMethods.Tk_MainWindow(interp);
      else
        return NonWindows.NativeMethods.Tk_MainWindow(interp);
    }

    public static void Tk_MakeWindowExist(IntPtr tkwin)
    {
      if (isWindows)
        Win32.NativeMethods.Tk_MakeWindowExist(tkwin);
      else
        NonWindows.NativeMethods.Tk_MakeWindowExist(tkwin);
    }

    public static void Tk_MapWindow(IntPtr tkwin)
    {
      if (isWindows)
        Win32.NativeMethods.Tk_MapWindow(tkwin);
      else
        NonWindows.NativeMethods.Tk_MapWindow(tkwin);
    }

    public static void Tk_DestroyWindow(IntPtr tkwin)
    {
      if (isWindows)
        Win32.NativeMethods.Tk_DestroyWindow(tkwin);
      else
        NonWindows.NativeMethods.Tk_DestroyWindow(tkwin);
    }
  }

  public class Tcl : IDisposable
  {
    private static bool isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows);
    private IntPtr interpreter = IntPtr.Zero;
    private IntPtr mainWindow = IntPtr.Zero;
    private bool disposedValue;

    public Tcl()
    {
      try
      {
        var tmp = Externals.Tcl_CreateInterp();
        var state = Externals.Tcl_Init(tmp);
        if (state != 0)
        {
          Externals.Tcl_DeleteInterp(tmp);
        }
        else
        {
          state = Externals.Tk_Init(tmp);
          if (state != 0)
          {
            Externals.Tcl_DeleteInterp(tmp);
          }
          else
          {
            interpreter = tmp;
            mainWindow = Externals.Tk_MainWindow(interpreter);
          }
        }
      }
      catch (System.DllNotFoundException dnf)
      {
        Console.WriteLine(dnf);
        interpreter = IntPtr.Zero;
      }
      catch (System.EntryPointNotFoundException epnf)
      {
        Console.WriteLine(epnf);
        interpreter = IntPtr.Zero;
      }
    }

    public bool IsValid => interpreter != IntPtr.Zero;

    private void Eval(string script)
    {
      Console.WriteLine("{0}", script);
      var tk = Externals.Tcl_Eval(interpreter, script);
      Console.WriteLine("result {0}", tk);
      if (tk != 0) Console.WriteLine("message {0}", GetErrorMessage());
    }

    public void FontChooser(LogFont font, Action<string> callback)
    {
      Console.WriteLine("main window {0}", mainWindow);
      Externals.Tk_MakeWindowExist(mainWindow);
      Eval("wm geometry . 1x1");
      //Eval("wm iconify  .");
      Eval("puts [winfo ismapped .]");
      Eval("wm attributes  . -alpha 0.0");
      Externals.Tk_MapWindow(mainWindow);

      Console.WriteLine("status {0}", GetErrorMessage());
      var handler = "proc fontchosen {f} {" + Environment.NewLine
      + "  puts stdout \"***fontchosen***\"" + Environment.NewLine
      + "  puts stdout $f" + Environment.NewLine // needs to be a callback
      + "}";
      Eval(handler);
      // TCL_CHANNEL_VERSION_5 = IntPtr(5)

      //      var handler2 = "proc fontchosen2 {} {" + Environment.NewLine
      //+ "  puts stdout \"***fontchosen2***\"" + Environment.NewLine
      //+ "  puts stdout [tk fontchooser configure -font]" + Environment.NewLine
      //+ "}";
      //      Eval(handler2);
      //      Eval("bind . <<TkFontchooserFontChanged>> [list fontchosen2]");

      Eval("puts [winfo ismapped .]");
      Eval("tk fontchooser configure -parent . -font " + font.ToTkString() + " -command fontchosen");
      Eval("tk fontchooser show"); // segfaults on linux

      //      Eval("set wfont [tk fontchooser configure -font]"); // seems pretty write-only
      //      Eval("puts stdout $wfont");
      Eval("wm withdraw .");
      //var wfont = Externals.Tcl_GetVar(interpreter, "wfont", 0);
      //Console.WriteLine("wfont {0}", wfont);
      //IntPtr ptr = Externals.Tcl_GetStringFromObj(wfont, IntPtr.Zero);
      //Console.WriteLine("font result {0}", Marshal.PtrToStringAnsi(ptr));
    }

    private string GetErrorMessage()
    {
      IntPtr obj = Externals.Tcl_GetObjResult(interpreter);
      if (obj == IntPtr.Zero)
      {
        return string.Empty;
      }
      else
      {
        IntPtr ptr = Externals.Tcl_GetStringFromObj(obj, IntPtr.Zero);
        return Marshal.PtrToStringAnsi(ptr);
      }
    }

    protected virtual void Dispose(bool disposing)
    {
      if (!disposedValue)
      {
        if (disposing)
        {
          // TODO: dispose managed state (managed objects)
        }

        // TODO: free unmanaged resources (unmanaged objects) and override finalizer
        // TODO: set large fields to null
        if (interpreter != IntPtr.Zero)
        {
          Externals.Tk_DestroyWindow(mainWindow);
          Externals.Tcl_DeleteInterp(interpreter);
          interpreter = IntPtr.Zero;
        }
        disposedValue = true;
      }
    }

    // TODO: override finalizer only if 'Dispose(bool disposing)' has code to free unmanaged resources
    ~Tcl()
    {
      // Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
      Dispose(disposing: false);
    }

    public void Dispose()
    {
      // Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
      Dispose(disposing: true);
      GC.SuppressFinalize(this);
    }
  }

  public static class Fonts
  {
    public static LogFont SelectWin32(string font, IntPtr handle)
    {
      var _ = LogFont.TryParse(font, out LogFont logfont);
      IntPtr pLogfont = Marshal.AllocHGlobal(Marshal.SizeOf(logfont));
      try
      {
        // Fudge-factor here
        logfont.height = -(int)Math.Round(Math.Abs(logfont.height * 4.0 / 3.0));
        logfont.italic = (byte)(logfont.italic == (byte)0 ? 0 : 255);
        Marshal.StructureToPtr(logfont, pLogfont, false);

        ChooseFont choosefont = new ChooseFont();
        IntPtr pChoosefont = Marshal.AllocHGlobal(Marshal.SizeOf(choosefont));
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

          if (Win32.NativeMethods.ChooseFont(pChoosefont))
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
      faceName = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ?
        "Courier New" : "Monospace";
      weight = (int)FontWeight.Normal;
      height = 10;
    }

    // Pango-style text
    [SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods",
      Justification = "validate local variable ''(*decode)'', which was reassigned from parameter 'decode', before using it -- u wot m8!?")]
    public static bool TryParse(string encoded, out LogFont decode)
    {
      decode = new LogFont();
      var core = encoded ?? string.Empty;

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

    public string ToTkString()
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

#if STANDALONE
    class Program
    {
        static void Main(string[] args)
        {
            using(var tcl = new Tcl())
            {
               LogFont.TryParse("Monospace normal 10", out LogFont f);
               tcl.FontChooser(f, Console.WriteLine);
            }
        }
    }
#endif
}