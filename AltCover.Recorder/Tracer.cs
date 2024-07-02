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

    internal Tracer Connect()
    {
      if (File.Exists(this.TracerName))
      {
        throw new NotImplementedException();
        //    Seq.initInfinite (fun i -> Path.ChangeExtension(this.Tracer, sprintf ".%d.acv" i))
        //|> Seq.filter (File.Exists >> not)
        //|> Seq.map this.MakeConnection
        //|> Seq.head
      }
      else
      { return this; }
    }
  }
}