namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Resources

module Resource =
    let  GetResourceString(key : string) =
      let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
      let resources = ResourceManager("AltCover.UICommon.Resource", executingAssembly)
      resources.GetString(key)

    let Format(resourceName, args) =
      String.Format(
        CultureInfo.CurrentCulture,
        GetResourceString resourceName,
        args |> Seq.cast<obj> |> Seq.toArray)