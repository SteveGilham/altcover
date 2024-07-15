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

    private void PushContext(Track context)
    {
      //member private this.PushContext context =
      //  match context with
      //  | Null -> this.Formatter.Write(Tag.Null |> byte)
      //  | Time t ->
      //    this.Formatter.Write(Tag.Time |> byte)
      //    this.Formatter.Write(t)
      //  | Call t ->
      //    this.Formatter.Write(Tag.Call |> byte)
      //    this.Formatter.Write(t)
      //  | Both b ->
      //    this.Formatter.Write(Tag.Both |> byte)
      //    this.Formatter.Write(b.Time)
      //    this.Formatter.Write(b.Call)
      //  | Table t ->
      //    this.Formatter.Write(Tag.Table |> byte)

      //    t.Keys
      //    |> Seq.filter (fun k -> t.[k].Count > 0)
      //    |> Seq.iter (fun m ->
      //      this.Formatter.Write m
      //      this.Formatter.Write t.[m].Count

      //      t.[m].Keys
      //      |> Seq.iter (fun p ->
      //        this.Formatter.Write p
      //        let v = t.[m].[p]
      //        this.Formatter.Write v.Count
      //        v.Tracks |> Seq.iter this.PushContext
      //        this.PushContext Null))

      //    this.Formatter.Write String.Empty
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

    //member internal this.OnConnected f g = if this.IsConnected then f () else g ()

    //member internal this.OnFinish visits =
    //  this.CatchUp visits
    //  this.Close()

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