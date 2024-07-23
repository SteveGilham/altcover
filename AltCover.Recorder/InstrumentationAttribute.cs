namespace AltCover.Recorder
{
  using System;

  /// <summary>
  /// An attribute to label an instrumented assembly by provenance
  /// </summary>
  [AttributeUsage(AttributeTargets.Assembly)]
  public sealed class InstrumentationAttribute : Attribute
  {
    /// <summary>
    /// SHA-256 hash of the original assembly
    /// </summary>
    public string Assembly { get; set; }

    /// <summary>
    /// SHA-256 hash of instrumentation parameters
    /// </summary>
    public string Configuration { get; set; }

    public InstrumentationAttribute()
    {
      Assembly = "AltCover.Recorder.g!";
      Configuration = "Uninstrumented!!";
    }
  }
}