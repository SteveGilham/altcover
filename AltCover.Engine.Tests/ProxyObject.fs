namespace Tests

open System
open System.Reflection

[<NUnit.Framework.IncludeExcludeAttribute>]
type ProxyObject() =
  inherit MarshalByRefObject()

  member val Type: Type option = None with get, set
  member val Object = null with get, set

#if !NET472
  member val Context: System.Runtime.Loader.AssemblyLoadContext = null with get, set
#endif

  member this.InstantiateObject(assemblyPath: string, typeName: string, args: obj[]) =
#if !NET472
    let assembly =
      this.Context.LoadFromAssemblyPath(assemblyPath) //LoadFrom loads dependent DLLs (assuming they are in the app domain's base directory
#else
    let assembly =
      Assembly.LoadFrom(assemblyPath) //LoadFrom loads dependent DLLs (assuming they are in the app domain's base directory
#endif
    let t =
      assembly.ExportedTypes
      |> Seq.filter (fun t -> t.FullName = typeName)

    this.Type <- Seq.tryHead t
    this.Object <- Activator.CreateInstance(this.Type |> Option.get, args)

  member this.InvokeMethod(methodName: string, args: obj[]) =
    let t = this.Type |> Option.get
    let methodinfo = t.GetMethod(methodName)
    methodinfo.Invoke(this.Object, args)