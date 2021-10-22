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
    // Mono+Linux barfs at a path of "/_" without the "file://" prefix
    Uri("file://" + (Path.GetFullPath path), UriKind.Absolute).LocalPath

#if !FAKE && !NoCanonicalDirectories
  let canonicalDirectory (path:string) =
    let last = path |> Seq.last
    canonicalPath (if last = Path.DirectorySeparatorChar ||
                      last = Path.AltDirectorySeparatorChar
                   then path
                   else path + Path.DirectorySeparatorChar.ToString())
#endif