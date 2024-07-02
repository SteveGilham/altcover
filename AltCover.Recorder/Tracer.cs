namespace AltCover.Recorder
{
  using System;
  using System.Collections.Generic;
  using System.Diagnostics.CodeAnalysis;
  using System.Drawing;
  using System.IO;
  using System.IO.Compression;
  using System.Runtime.Remoting.Contexts;
  using System.Runtime.Serialization;
  using System.Text.RegularExpressions;

  //using AltCover.Shared;

  internal enum Close
  {
    DomainUnload,
    ProcessExit,
    Pause,
    Resume,
  }

  internal struct Tracer
  {
    public string TracerName;
    public bool Runner;
    public bool Definitive;
    public System.IO.Stream Stream;
    public System.IO.BinaryWriter Formatter;

    private Tracer(string name)
    {
      TracerName = name;
      Runner = false;
      Definitive = false;
      Stream = null;
      Formatter = null;
    }

    internal static Tracer Create(string name)
    {
      return new Tracer(name);
    }

    internal void Close()
    {
      try
      {
        this.Stream.Flush();

        this.Formatter.Close();
      }
      catch (ObjectDisposedException) { }
      catch (NullReferenceException) { }
    }

    internal bool IsConnected()
    {
      return (this.Stream != null) && this.Runner;
    }

    internal Tracer OnStart()
    {
      var running = this.Connect();
      running.Definitive = true;
      return running;
    }

    private Tracer MakeConnection(string f)
    {
      var fs = File.OpenWrite(f);

      var s =
        new DeflateStream(fs, CompressionMode.Compress);

      Stream = s;
      Formatter = new BinaryWriter(s);
      Runner = true;

      return this;
    }

    internal Tracer Connect()
    {
      if (File.Exists(this.TracerName))
      {
        for (var i = 0; true; ++i)
        {
          var extension = i.ToString(System.Globalization.CultureInfo.InvariantCulture);
          var path = Path.ChangeExtension(this.TracerName, "." + extension + ".acv");
          if (File.Exists(path)) { continue; }
          MakeConnection(path);
          return this;
        }
      }
      else
      { return this; }
    }
  }
}