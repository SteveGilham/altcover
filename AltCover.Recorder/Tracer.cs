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

  [Serializable]
  internal enum Close
  {
    DomainUnload,
    ProcessExit,
    Pause,
    Resume,
  }

  [SuppressMessage("Gendarme.Rules.Performance",
                   "OverrideValueTypeDefaultsRule",
                   Justification = "Not actually used/NoComparison")]
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

    internal bool IsConnected
    {
      get
      {
        return (this.Stream != null) && this.Runner;
      }
    }

    [SuppressMessage("Gendarme.Rules.Correctness",
                     "EnsureLocalDisposalRule",
                     Justification = "s, fs : Closed as recording ends")]
    [SuppressMessage("Microsoft.Reliability",
                     "CA2000:Dispose objects before losing scope",
                     Justification = "s, fs : Closed as recording ends")]
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

    [SuppressMessage("Gendarme.Rules.Smells",
                     "AvoidLongMethodsRule",
                     Justification = "Well tested code")]
    private void PushContext(Track context)
    {
      if (context is Null)
        Formatter.Write((byte)Tag.Null);
      else if (context is Time t)
      {
        Formatter.Write((byte)Tag.Time);
        Formatter.Write(t.Value);
      }
      else if (context is Call c)
      {
        Formatter.Write((byte)Tag.Call);
        Formatter.Write(c.Value);
      }
      else if (context is Both b)
      {
        Formatter.Write((byte)Tag.Both);
        Formatter.Write(b.Value.Time);
        Formatter.Write(b.Value.Call);
      }
      else
      {
        var tx = ((Table)context).Value;
        Formatter.Write((byte)Tag.Table);
        foreach (var key in tx.Keys)
        {
          if (tx[key].Count == 0)
            continue;
          Formatter.Write(key);
          Formatter.Write(tx[key].Count);

          foreach (var p in tx[key].Keys)
          {
            Formatter.Write(p);
            var v = tx[key][p];
            Formatter.Write(v.Count);
            foreach (var x in v.Tracks)
              PushContext(x);
            PushContext(new Null());
          }
        }
        Formatter.Write(String.Empty);
      }
    }

    internal void Push(string moduleId, int hitPointId, Track context)
    {
      this.Formatter.Write(moduleId);
      this.Formatter.Write(hitPointId);
      this.PushContext(context);
    }

    internal void CatchUp(Dictionary<string, Dictionary<int, PointVisit>> visits)
    {
      foreach (var item in visits.Values)
      {
        if (item.Count > 0)
        {
          Push(String.Empty, 0, new Table(visits));
          return;
        }
      }
    }

    internal Tracer OnStart()
    {
      var running = Connect();
      running.Definitive = true;
      return running;
    }

    [SuppressMessage("Gendarme.Rules.Design.Generic",
                     "AvoidDeclaringCustomDelegatesRule",
                     Justification = "Net Framework 2.0")]
    public delegate void Act();

    //member internal this.OnConnected f g = if this.IsConnected then f () else g ()
    internal void OnConnected(Act f, Act g)
    {
      if (IsConnected) f(); else g();
    }

    internal void OnFinish(Dictionary<string, Dictionary<int, PointVisit>> visits)
    {
      CatchUp(visits);
      Close();
    }

    internal void OnVisit(Dictionary<string, Dictionary<int, PointVisit>> visits,
      string moduleId, int hitPointId, Track context)
    {
      CatchUp(visits);
      Push(moduleId, hitPointId, context);
      Formatter.Flush();
      Stream.Flush();
    }
  }
}