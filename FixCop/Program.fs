open System
open System.IO
open System.Reflection
open Microsoft.VisualStudio.CodeAnalysis
open Microsoft.VisualStudio.CodeAnalysis.Common
open Microsoft.FxCop.Sdk
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let plat = argv
               |> Seq.tryFind (fun a -> a.StartsWith("/plat"))
    let platformPath = match plat with
                       | Some p -> p.Substring(p.IndexOf(":") + 1)
                       | _ -> "Platform folder not specified"
                              |> InvalidDataException
                              |> raise
    printfn "Platformpath = %s" platformPath

// Would it help to populate this cache from nuget??
//// Microsoft.FxCop.Sdk.Reader
//using System;
//using System.Collections;
//using System.Collections.Generic;

//internal static readonly IDictionary StaticAssemblyCache = new SynchronizedWeakDictionary();
//Strong name => Assembly node

    let here = Assembly.GetExecutingAssembly().Location
    here
    |> Path.GetDirectoryName
    |> Directory.GetFiles
    |> Seq.filter (fun f -> f.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
    |> Seq.sortDescending
    |> Seq.iter (fun n -> try
                            Path.Combine ( here |> Path.GetDirectoryName, n)
                            |> Assembly.LoadFile
                            |> ignore
                          with
                          | _ -> (sprintf "Could not load %s" n)
                                 |> System.Diagnostics.Debug.WriteLine)

    // tracing
    let ca = Path.Combine ( here |> Path.GetDirectoryName, "Microsoft.VisualStudio.CodeAnalysis.dll")
                   |> Assembly.LoadFile
    let catrace = ca.GetType("Microsoft.VisualStudio.CodeAnalysis.Diagnostics.CATrace")
    let verbose = System.Diagnostics.TraceLevel.Verbose
    catrace.GetProperty("TraceLevel").SetValue(null, verbose)

    // interop info
    let cainterop = Path.Combine ( here |> Path.GetDirectoryName, "Microsoft.VisualStudio.CodeAnalysis.Interop.dll")
                   |> Assembly.LoadFile

    let info = cainterop.GetType("Microsoft.VisualStudio.CodeAnalysis.Common.AssemblyInfo")
    let getInfo = info.GetMethod("GetAssemblyInfo", BindingFlags.Public|||BindingFlags.Static)
    let props = info.GetProperties(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static)

    // key assembly
    let cacommon = Path.Combine ( here |> Path.GetDirectoryName, "Microsoft.VisualStudio.CodeAnalysis.Common.dll")
                   |> Assembly.LoadFile

    // interesting types
    let platform = cacommon.GetType("Microsoft.VisualStudio.CodeAnalysis.Common.Platform")
    let unification = cacommon.GetType("Microsoft.VisualStudio.CodeAnalysis.Common.UnificationAssemblyNameMap")
    let ptype = typeof<PlatformInfo>.Assembly.GetType("Microsoft.VisualStudio.CodeAnalysis.PlatformType")
    let unknown = Enum.Parse(ptype, "Unknown")

    // interesting calls
    let makeUnify = unification.GetConstructor(BindingFlags.Public|||BindingFlags.Instance, null, [| |], null)
    let makePlatform = platform.GetConstructor(BindingFlags.NonPublic|||BindingFlags.Instance, null,
                           [| ptype; typeof<AssemblyName>; typeof<IAssemblyNameMap>; typeof<IEnumerable<string>>; typeof<string> |], null)
    let adder = unification.GetMethod("AddMapping", BindingFlags.Public|||BindingFlags.Instance)
    let platforms = platform.GetField("s_platforms", BindingFlags.Static ||| BindingFlags.NonPublic).GetValue(null) :?> System.Collections.IList
    let alt = platform.GetField("m_alternatePlatform", BindingFlags.NonPublic|||BindingFlags.Instance)

    // interesting platform assemblies
    let netstd2 = Path.Combine(platformPath, "netstandard.dll")
    //let core = Path.Combine(platformPath, "System.Private.CoreLib.dll")
    //let corelib = core |> AssemblyName.GetAssemblyName // throws, but name seems fixed anyway

    let netinfo = getInfo.Invoke(null, [| netstd2 :> obj|])
    let refs = (props
                |> Array.find (fun p -> p.Name = "AssemblyReferences" )).GetValue(netinfo, null) :?> IList<AssemblyName>
                |> Seq.sortBy (fun n -> n.Name)
                |> Seq.toArray

    let dirpath = netstd2 |> Path.GetDirectoryName
    let refpaths = Directory.GetFiles(platformPath, "*.dll")
    let cci = typeof<Identifier>.Assembly
    let tp = cci.GetType("Microsoft.FxCop.Sdk.TargetPlatform")
    let areff = tp.GetProperty("AssemblyReferenceFor").GetValue(null)
    let areffi = areff.GetType().GetMethod("set_Item",
                                           BindingFlags.Instance |||
                                           BindingFlags.Public |||
                                           BindingFlags.NonPublic,
                                           null,
                                           [| typeof<Int32>; typeof<obj> |],
                                           null)

    let aref = cci.GetType("Microsoft.FxCop.Sdk.AssemblyReference")
    let build = aref.GetConstructor(BindingFlags.Instance |||
                                    BindingFlags.Public |||
                                    BindingFlags.NonPublic,
                                    null, [|typeof<AssemblyNode>|], null)

    let refnames = refpaths
                   |> Seq.map (fun p -> try
                                          let an = p |> AssemblyName.GetAssemblyName
                                          let key = Identifier.For(an.Name).UniqueIdKey
                                          let node = AssemblyNode.GetAssembly(p)
                                          let ar = build.Invoke( [| node :> obj |])
                                          areffi.Invoke(areff, [|
                                                  key :> obj
                                                  ar
                                               |]) |> ignore

                                          an |> Some
                                        with
                                        | _ -> None)
                   |> Seq.choose id
                   |> Seq.toArray

    let refmap = refs
                 |> Seq.map (fun n -> if n.Version.ToString() <> "0.0.0.0"
                                      then n
                                      else refnames
                                           |> Seq.find (fun r -> r.Name = n.Name))
                 |> Seq.toArray

    let uMap = Convert.ChangeType(makeUnify.Invoke([| |]), unification)
    let netstandard20 = AssemblyName("netstandard, Version=2.0.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51")
    let netstandard21 = AssemblyName("netstandard, Version=2.1.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51")
    adder.Invoke(uMap, [| netstandard20 :> obj; netstandard21 :> obj|]) |> ignore
    //adder.Invoke(uMap, [| netstandard20 :> obj; corelib :> obj|]) |> ignore
    //adder.Invoke(uMap, [| netstandard20 :> obj; corelib :> obj|]) |> ignore
    refpaths
    |> Seq.iter (fun f -> try
                            let name = f |> AssemblyName.GetAssemblyName
                            let neutral = AssemblyName(name.FullName)
                            neutral.Version <- Version("0.0.0.0")
                            adder.Invoke(uMap, [| neutral :> obj; name :> obj|]) |> ignore
                          with
                          | _ -> (sprintf "Could not map %s" f)
                                 |> System.Diagnostics.Debug.WriteLine)

    let add = makePlatform.Invoke([| unknown; netstandard20; uMap; refpaths ; netstd2 |])
    let add2 = makePlatform.Invoke([| unknown; netstandard21; uMap; [dirpath] ; netstd2 |])
    let pi = platform.GetProperty("PlatformInfo")
    let pi1 = pi.GetValue(add) :?> PlatformInfo
    //let pi2 = pi.GetValue(add2) :?> PlatformInfo
    let pin = typeof<PlatformInfo>.GetProperty("PlatformType", BindingFlags.NonPublic ||| BindingFlags.Instance)
    //pin.SetValue(pi2, pin.GetValue(pi1))
    let piv = typeof<PlatformInfo>.GetProperty("PlatformVersion", BindingFlags.Public ||| BindingFlags.Instance)
    //piv.SetValue(pi2, piv.GetValue(pi1))

    alt.SetValue(add, add2)
    alt.SetValue(add2, add)

    platforms.Add add |> ignore
    let fxcop = Path.Combine ( here |> Path.GetDirectoryName, "FxCopCmd.exe")
    let driven = fxcop
                 |> Assembly.LoadFile
    let command = driven.GetType("Microsoft.FxCop.Command.FxCopCommand")
    let main = command.GetMethod("Main", BindingFlags.Static ||| BindingFlags.Public)
    let r = main.Invoke(null, [| argv :> obj |])
    r:?> int 