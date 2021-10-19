#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.IO

[<AutoOpen>]
module internal Canonical =

  let canonicalPath (path:string) =
    Uri(Path.GetFullPath path, UriKind.Absolute).LocalPath

#if !FAKE && !NoCanonicalDirectories
  let canonicalDirectory (path:string) =
    canonicalPath (path + Path.DirectorySeparatorChar.ToString())
#endif