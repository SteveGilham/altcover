namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.XPath
open Augment
open GuiCommon

[<NoComparison>]
type CoverageTreeContext<'TModel, 'TRow> =
  {
    Model : 'TModel
    Row : 'TRow
  }

[<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Design.Generic",
    "AvoidExcessiveParametersOnGenericTypesRule",
    Justification = "Does this act excessive to you?"
)>]
[<SuppressMessage("Microsoft.Design", "CA1005:AvoidExcessiveParametersOnGenericTypes",
  Justification="Does this act excessive to you?")>]
[<AutoSerializable(false)>]
type CoverageModelDisplay<'TModel, 'TRow, 'TIcon> =
  {
    Icons : Icons<'TIcon>
    Display : MessageType -> string -> unit
    GetFileInfo : int -> FileInfo
    UpdateMRUFailure : FileInfo -> unit
    UpdateUISuccess : FileInfo -> unit
    SetXmlNode : String -> CoverageTreeContext<'TModel, 'TRow>
    AddNode : CoverageTreeContext<'TModel, 'TRow> -> Lazy<'TIcon> -> String -> CoverageTreeContext<'TModel, 'TRow>
    Map : CoverageTreeContext<'TModel, 'TRow> -> XPathNavigator -> unit
  }

module CoverageFileTree =
  let private populateClassNode
      (environment : CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
      (model : CoverageTreeContext<'TModel, 'TRow>)
      (nodes : seq<MethodKey>) =
    let applyToModel (theModel : CoverageTreeContext<'TModel, 'TRow>)
        (item : (string * MethodType) * MethodKey seq) =
      let ((display, special), keys) = item

      let applyMethod (mmodel : CoverageTreeContext<'TModel, 'TRow>) (x : MethodKey) =
        let fullname = x.Navigator.GetAttribute("fullname", String.Empty)

        let args =
          if String.IsNullOrEmpty(fullname) || x.Name.IndexOf("(", StringComparison.Ordinal) > 0 then
            String.Empty
          else
            let bracket = fullname.IndexOf("(",StringComparison.Ordinal)
            if bracket < 0 then String.Empty else fullname.Substring(bracket)

        let displayname = x.Name + args

        let offset =
          match displayname.LastIndexOf("::", StringComparison.Ordinal) with
          | -1 -> 0
          | o -> o + 2

        let newrow = environment.AddNode mmodel environment.Icons.Method (displayname.Substring(offset))
        environment.Map newrow x.Navigator

      if special <> MethodType.Normal then
        let newrow =
          environment.AddNode theModel (if special = MethodType.Property
                                        then environment.Icons.Property
                                        else environment.Icons.Event) display
        keys
          |> Seq.sortBy (fun key -> key.Name |> DisplayName)
          |> Seq.iter (applyMethod newrow)
      else
        applyMethod theModel (keys |> Seq.head)

    let methods =
      nodes
      |> Seq.groupBy (fun key ->
           key.Name
           |> DisplayName
           |> HandleSpecialName)
      |> Seq.toArray

    let orderMethods array =
      array
      |> Array.sortInPlaceWith (fun ((l, (lb : MethodType)), _) ((r, rb), _) ->
           let sort1 = String.Compare(l, r, StringComparison.OrdinalIgnoreCase)

           let sort2 =
             if sort1 = 0
             then String.Compare(l, r, StringComparison.Ordinal)
             else sort1
           if sort2 = 0 then lb.CompareTo rb else sort2)

    let applyMethods array =
      array |> Array.iter (applyToModel model)

    methods |> orderMethods
    methods |> applyMethods

  let private populateNamespaceNode
      (environment : CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
      (model : CoverageTreeContext<'TModel, 'TRow>)
      (nodes : seq<MethodKey>) =
    let applyToModel (theModel : CoverageTreeContext<'TModel, 'TRow>)
        (group : string * seq<MethodKey>) =
      let name = fst group

      let icon =
        if group
           |> snd
           |> Seq.isEmpty then
          environment.Icons.Module
        else if group
                |> snd
                |> Seq.exists (fun key ->
                     let d = key.Name |> DisplayName
                     (d.StartsWith(".", StringComparison.Ordinal) || d.Equals("Invoke"))
                     |> not) then
          environment.Icons.Class
        else
          environment.Icons.Effect

      let newrow = environment.AddNode theModel icon name

      populateClassNode environment newrow (snd group)
      newrow

    let isNested (name : string) n =
      name.StartsWith(n + "+", StringComparison.Ordinal)
      || name.StartsWith(n + "/", StringComparison.Ordinal)

    let classlist =
      nodes
      |> Seq.groupBy (fun x -> x.ClassName)
      |> Seq.toList

    let classnames =
      classlist
      |> Seq.map fst
      |> Set.ofSeq

    let modularize =
      let contains (s:String) c =
        s.IndexOf(c, StringComparison.Ordinal) >= 0

      classnames
      |> Seq.filter (fun cn -> contains cn "+" || contains cn "/")
      |> Seq.map
           (fun cn -> cn.Split([| "+"; "/" |], StringSplitOptions.RemoveEmptyEntries).[0])
      |> Seq.distinct
      |> Seq.filter (fun mn ->
           classnames
           |> Set.contains mn
           |> not)
      |> Seq.map (fun mn -> (mn, Seq.empty<MethodKey>))
      |> Seq.toList

    let classes = Seq.append classlist modularize |> Seq.toArray

    Array.sortInPlaceWith (fun l r ->
      let left = fst l
      let right = fst r
      let sort = String.Compare(left, right, StringComparison.OrdinalIgnoreCase)
      if sort = 0
      then String.Compare(left, right, StringComparison.Ordinal)
      else sort) classes
    classes
    |> Seq.fold (fun stack c ->
         let name = fst c
         let restack = stack |> List.filter (fst >> (isNested name))

         let pr =
           match restack with
           | [] -> model
           | (_, r) :: _ -> r

         let nr = applyToModel pr c
         (name, nr) :: restack) []
    |> ignore

  let private populateAssemblyNode
      (environment : CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
      (model : CoverageTreeContext<'TModel, 'TRow>)
      (node : XPathNavigator) =
    // within the <module> we have <method> nodes with name="get_module" class="AltCover.Coverage.CoverageSchema.coverage"
    let applyToModel (theModel : CoverageTreeContext<'TModel, 'TRow>)
        (group : string * seq<MethodKey>) =
      let name = fst group

      let newrow = environment.AddNode theModel environment.Icons.Namespace name
      populateNamespaceNode environment newrow (snd group)

    let methods =
      node.SelectChildren("method", String.Empty)
      |> Seq.cast<XPathNavigator>
      |> Seq.map (fun m ->
           let classfullname = m.GetAttribute("class", String.Empty)
           let lastdot = classfullname.LastIndexOf('.')
           { Navigator = m
             NameSpace =
               if lastdot < 0 then String.Empty else classfullname.Substring(0, lastdot)
             ClassName =
               if lastdot < 0 then classfullname else classfullname.Substring(1 + lastdot)
             Name = m.GetAttribute("name", String.Empty) })
      |> Seq.groupBy (fun x -> x.NameSpace)
      |> Seq.sortBy fst

    methods |> Seq.iter (applyToModel model)

  [<SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling",
         Justification="Where it all comes together")>]
  let DoSelected (environment:CoverageModelDisplay<'TModel, 'TRow, 'TIcon>) index =
      let current = environment.GetFileInfo index
      match CoverageFile.LoadCoverageFile current with
      | Left failed ->
          Messages.InvalidCoverageFileMessage environment.Display failed
          environment.UpdateMRUFailure current
      | Right coverage ->
          // check if coverage is newer that the source files
          let sourceFiles =
            coverage.Document.CreateNavigator().Select("//seqpnt/@document")
            |> Seq.cast<XPathNavigator>
            |> Seq.map (fun x -> x.Value)
            |> Seq.distinct

          let missing =
            sourceFiles
            |> Seq.map GetSource
            |> Seq.filter (fun f -> not f.Exists)

          if not (Seq.isEmpty missing) then
            Messages.MissingSourceFileMessage environment.Display current
          let newer =
            sourceFiles
            |> Seq.map GetSource
            |> Seq.filter (fun f -> f.Exists && f.Outdated current.LastWriteTimeUtc)
          // warn if not
          if not (Seq.isEmpty newer) then
            Messages.OutdatedCoverageFileMessage environment.Display current

          let model = environment.SetXmlNode current.Name

          let applyToModel (theModel : CoverageTreeContext<'TModel, 'TRow>)
              (group : XPathNavigator * string) =
            let name = snd group
            let newModel = environment.AddNode theModel
                                        environment.Icons.Assembly name
            populateAssemblyNode environment newModel (fst group)

          let assemblies =
            coverage.Document.CreateNavigator().Select("//module")
            |> Seq.cast<XPathNavigator>
          assemblies
          |> Seq.map (fun node ->
              (node,
                node.GetAttribute("assemblyIdentity", String.Empty).Split(',')
                |> Seq.head))
          |> Seq.sortBy snd
          |> Seq.iter (applyToModel model)

          environment.UpdateUISuccess current