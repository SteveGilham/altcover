namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.XPath
open AltCover.Shared
open Augment
open GuiCommon

[<AutoSerializable(false)>]
type TreeNode<'TIcon> =
  { Icon: 'TIcon
    Points: int
    Visited: int
    Name: String
    Tooltip: String option
    Mapping: XPathNavigator option
    Children: System.Collections.Generic.List<TreeNode<'TIcon>> option }
  static member internal Make (icon: Lazy<'TIcon>) name =
    { Icon = icon.Force()
      Name = name
      Points = 0
      Visited = 0
      Tooltip = None
      Mapping = None
      Children = None }

  member node.Percent =
    let percent p v =
      if p > 0 then
        (100.0 * (v |> float) / (p |> float))
        |> Math.Floor
        |> int
      else
        0

    let pc =
      if node.Points > 0 then
        percent node.Points node.Visited
      else
        match node.Children with
        | None -> 0
        | Some l ->
          percent
            (l |> Seq.sumBy (fun n -> n.Points))
            (l |> Seq.sumBy (fun n -> n.Visited))

    sprintf "%3i%%" pc

[<SuppressMessage("Gendarme.Rules.Design.Generic",
                  "AvoidExcessiveParametersOnGenericTypesRule",
                  Justification = "Does this act excessive to you?")>]
[<SuppressMessage("Microsoft.Design",
                  "CA1005:AvoidExcessiveParametersOnGenericTypes",
                  Justification = "Does this act excessive to you?")>]
[<NoComparison; AutoSerializable(false)>]
type CoverageTreeContext<'TModel, 'TRow, 'TIcon> =
  { Model: 'TModel
    Row: 'TRow
    Root: TreeNode<'TIcon>
    Current: TreeNode<'TIcon> }

[<SuppressMessage("Gendarme.Rules.Design.Generic",
                  "AvoidExcessiveParametersOnGenericTypesRule",
                  Justification = "Does this act excessive to you?")>]
[<SuppressMessage("Microsoft.Design",
                  "CA1005:AvoidExcessiveParametersOnGenericTypes",
                  Justification = "Does this act excessive to you?")>]
[<AutoSerializable(false)>]
type CoverageModelDisplay<'TModel, 'TRow, 'TIcon> =
  { Icons: Icons<'TIcon>
    Display: MessageType -> string -> unit
    GetFileInfo: int -> FileInfo
    UpdateMRUFailure: FileInfo -> unit
    UpdateUISuccess: FileInfo -> unit
    SetXmlNode: TreeNode<'TIcon> -> CoverageTreeContext<'TModel, 'TRow, 'TIcon>
    TreeUIDispatch: (unit -> unit) -> unit
    AddNode: CoverageTreeContext<'TModel, 'TRow, 'TIcon>
      -> TreeNode<'TIcon>
      -> CoverageTreeContext<'TModel, 'TRow, 'TIcon>
    AddLeafNode: CoverageTreeContext<'TModel, 'TRow, 'TIcon>
      -> TreeNode<'TIcon>
      -> CoverageTreeContext<'TModel, 'TRow, 'TIcon>
    OnRowExpanded: 'TRow -> (unit -> unit) -> unit
    Map: CoverageTreeContext<'TModel, 'TRow, 'TIcon> -> XPathNavigator -> unit }

type CoverageRowState =
  | New
  | Unexpanded of (unit -> unit)
  | Expanded

module CoverageFileTree =

  [<AutoSerializable(false)>]
  type SourceFile<'TIcon> =
    { FullName: string
      FileName: string
      Navigator: XPathNavigator
      Icon: Lazy<'TIcon>
      Exists: bool
      Stale: bool }

  let private addTreeNode
    (context: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
    (node: TreeNode<'TIcon>)
    =
    context.Current.Children.Value.Add node
    { context with Current = node }

  let private addLeafNode
    (environment: CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
    (context: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
    (node: TreeNode<'TIcon>)
    =
    let newrow =
      environment.AddLeafNode context node

    match node.Mapping with
    | None -> ()
    | Some navigator -> environment.Map newrow navigator

    newrow

  let private withMapping (predicate: bool) (navigator: XPathNavigator) node =
    { node with
        Mapping =
          if predicate then
            Some navigator
          else
            None }

  let private withCoverage (points: int, visited: int) node =
    { node with
        Points = points
        Visited = visited }

  let private withChildren node =
    { node with
        Children =
          Some
          <| System.Collections.Generic.List<TreeNode<'TIcon>>() }

  let private withTooltip tip node = { node with Tooltip = tip }

  [<SuppressMessage("Gendarme.Rules.Correctness",
                    "ReviewSelfAssignmentRule",
                    Justification =
                      "Final line is a self-assignment for 'depth' -- compiler fault")>]
  let rec private scan (s: string) index depth =
    match s.[index] with
    | '<' -> scan s (index + 1) (depth + 1)
    | '>' ->
      let d = depth - 1

      if d = 0 then
        index
      else
        scan s (index + 1) d
    | _ -> scan s (index + 1) depth

  let private coverPoints (points: XPathNavigator seq) =
    if points |> Seq.isEmpty then
      (0, 0)
    else
      let visited =
        points
        |> Seq.filter (fun p -> p.GetAttribute("visitcount", String.Empty) != "0")

      (points |> Seq.length, visited |> Seq.length)

  let private cover (navigator: XPathNavigator seq) =
    let points =
      navigator
      |> Seq.collect (fun n ->
        n.SelectDescendants("seqpnt", String.Empty, false)
        |> Seq.cast<XPathNavigator>)

    coverPoints points

  let private pcCover (navigator: XPathNavigator seq) = navigator |> cover

  let private populateClassNode
    (environment: CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
    (model: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
    (nodes: seq<MethodKey>)
    (epoch: DateTime)
    =
    let applyToModel
      (theModel: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
      (item: (string * MethodType) * MethodKey seq)
      =
      let ((display, special), keys) = item

      // C++/CLI fixups -- probably more will be needed
      let modopt s =
        System.Text.RegularExpressions.Regex.Replace(
          s,
          @"modopt\(System\.Runtime\.\w+\.(\w+)?\)",
          "[$1]"
        )

      let gcroot (s: string) =
        let rec step (s: string) (i: int) =
          let next = // <- this line
            s.IndexOf("gcroot<", i, StringComparison.Ordinal)

          if next < 0 then
            s
          else
            let index = next + 7
            let stop = scan s index 1
            let start = s.Substring(0, index)

            let middle =
              s
                .Substring(index, stop - index)
                .Replace("::", ".")

            let finish = s.Substring(stop)
            step (start + middle + finish) index

        step s 0

      let fixup = modopt >> gcroot

      let extractArgs name fullname =
        if
          String.IsNullOrEmpty(fullname)
          || charIndexOf name '(' > 0
        then
          String.Empty
        else
          let bracket = charIndexOf fullname '('

          if bracket < 0 then
            String.Empty
          else
            fullname.Substring(bracket)

      let applyMethod
        (mmodel: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
        (x: MethodKey)
        =
        let fullname = // <- this line
          fixup
          <| x.Navigator.GetAttribute("fullname", String.Empty)

        let name = fixup x.Name

        let args = extractArgs name fullname

        let displayname = name + args

        let offset =
          match displayname.LastIndexOf("::", StringComparison.Ordinal) with
          | -1 -> 0
          | o -> o + 2

        // multi-source??
        let upcase (s: string) = s.ToUpperInvariant()

        let getFileName (s: string) =
          if
            s.StartsWith("http://", StringComparison.Ordinal)
            || s.StartsWith("https://", StringComparison.Ordinal)
          then
            { FullName = s
              FileName = System.Uri(s).LocalPath |> Path.GetFileName
              Navigator = null
              Icon = environment.Icons.SourceLink
              Exists = true
              Stale = false }

          else
            let info = GetSource(s)
            let x = info.Exists
            let stale = info.Outdated epoch

            { FullName = s
              FileName = Path.GetFileName s
              Navigator = null
              Icon =
                match (x, stale) with
                | (false, _) -> environment.Icons.NoSource
                | (_, true) -> environment.Icons.SourceDated
                | _ -> environment.Icons.Source
              Exists = x
              Stale = stale }

        let sources =
          x.Navigator.SelectDescendants("seqpnt", String.Empty, false)
          |> Seq.cast<XPathNavigator>
          |> Seq.map (fun s ->
            let d =
              s.GetAttribute("document", String.Empty)

            let state =
              { (d |> getFileName) with Navigator = s }
            // get any embed and tweak state and icon accordingly
            match GuiCommon.Embed s d with
            | None -> state
            | Some _ ->
              { state with
                  Exists = true
                  Stale = false
                  Icon = environment.Icons.Source })
          |> Seq.distinctBy (fun s -> s.FullName) // allows for same name, different path
          |> Seq.sortBy (fun s -> s.FileName |> upcase)
          |> Seq.toList

        let hasSource =
          sources |> List.exists (fun s -> s.Exists)

        let icon =
          if hasSource then
            environment.Icons.Method
          else
            environment.Icons.MethodMissingSource

        match sources with
        | [] -> // leaf
          (TreeNode.Make environment.Icons.MethodNoSource (displayname.Substring(offset)))
          |> addTreeNode mmodel
          |> ignore

        | [ source ] ->
          TreeNode.Make
            (if source.Stale then
               environment.Icons.MethodDated
             else
               icon)
            (displayname.Substring(offset))
          |> withTooltip (
            if hasSource then
              if source.Stale then
                Some
                <| Resource.Format("FileNewerThanReport", [| source.FullName |])
              else
                None
            else
              Some
              <| Resource.Format("FileNotFound", [| source.FullName |])
          )
          |> withCoverage (pcCover [ x.Navigator ])
          |> withMapping (hasSource && (not source.Stale)) x.Navigator
          |> addTreeNode mmodel
          |> ignore

        | _ ->
          // If multi-source (has inlines), add the source file nodes to the hittable map
          let newrow =
            TreeNode.Make icon (displayname.Substring(offset))
            |> withChildren
            |> withCoverage (pcCover [ x.Navigator ])
            |> addTreeNode mmodel

          sources
          |> List.iter (fun s ->
            TreeNode.Make
              (if s.Stale then
                 environment.Icons.SourceDated
               else
                 icon)
              s.FileName
            |> withTooltip (
              if s.Exists then
                None
              else
                Some
                <| Resource.Format("FileNotFound", [| s.FullName |])
            )
            |> withCoverage (
              x.Navigator.SelectDescendants("seqpnt", String.Empty, false)
              |> Seq.cast<XPathNavigator>
              |> Seq.filter (fun n ->
                n.GetAttribute("document", String.Empty)
                == s.FullName)
              |> coverPoints
            )
            |> withMapping (s.Exists && (not s.Stale)) s.Navigator
            |> addTreeNode newrow
            |> ignore)

      if special <> MethodType.Normal then
        let pc =
          keys |> Seq.map (fun x -> x.Navigator) |> pcCover

        let newrow =
          TreeNode.Make
            (if special = MethodType.Property then
               environment.Icons.Property
             else
               environment.Icons.Event)
            display
          |> withChildren
          |> withCoverage pc
          |> addTreeNode theModel

        keys
        |> Seq.sortBy (fun key -> key.Name |> DisplayName)
        |> Seq.iter (applyMethod newrow)
      else
        applyMethod theModel (keys |> Seq.head)

    let methods =
      nodes
      |> Seq.groupBy (fun key -> key.Name |> DisplayName |> HandleSpecialName)
      |> Seq.toArray

    let orderMethods array =
      array
      |> Array.sortInPlaceWith (fun ((l, (lb: MethodType)), _) ((r, rb), _) ->
        let sort1 =
          String.Compare(l, r, StringComparison.OrdinalIgnoreCase)

        let sort2 =
          if sort1 = 0 then
            String.Compare(l, r, StringComparison.Ordinal)
          else
            sort1

        if sort2 = 0 then
          lb.CompareTo rb
        else
          sort2)

    let applyMethods array =
      array |> Array.iter (applyToModel model)

    methods |> orderMethods
    methods |> applyMethods

  let private populateNamespaceNode
    (environment: CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
    (model: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
    (nodes: seq<MethodKey>)
    (epoch: DateTime)
    =
    let applyToModel
      (theModel: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
      (group: string * seq<MethodKey>)
      =
      let name = fst group

      let icon =
        if group |> snd |> Seq.isEmpty then
          (environment.Icons.Module, (0, 0)) // TODO marker
        else
          let pc =
            group
            |> snd
            |> Seq.map (fun x -> x.Navigator)
            |> pcCover

          let names =
            group
            |> snd
            |> Seq.map (fun key -> key.Name |> DisplayName)
            |> Seq.filter (fun d -> d.[0] <> '.')
            |> Seq.toList

          if
            names |> List.isEmpty
            || names |> List.exists (fun d -> d != "Invoke")
          then
            (environment.Icons.Class, pc)
          else
            (environment.Icons.Effect, pc)

      let newrow =
        TreeNode.Make (fst icon) name
        |> withChildren
        |> withCoverage (snd icon)
        |> addTreeNode theModel

      environment.OnRowExpanded newrow.Row (fun () ->
        populateClassNode environment newrow (snd group) epoch)

      newrow

    let isNested (name: string) n =
      name.StartsWith(n + "+", StringComparison.Ordinal)
      || name.StartsWith(n + "/", StringComparison.Ordinal)

    let classlist =
      nodes
      |> Seq.groupBy (fun x -> x.ClassName)
      |> Seq.toList

    let classnames =
      classlist |> Seq.map fst |> Set.ofSeq

    let modularize =
      let contains (s: String) c =
        s.IndexOf(c, StringComparison.Ordinal) >= 0

      classnames
      |> Seq.filter (fun cn -> contains cn "+" || contains cn "/")
      |> Seq.map (fun cn ->
        cn.Split([| "+"; "/" |], StringSplitOptions.RemoveEmptyEntries).[0])
      |> Seq.distinct
      |> Seq.filter (fun mn -> classnames |> Set.contains mn |> not)
      |> Seq.map (fun mn -> (mn, Seq.empty<MethodKey>))
      |> Seq.toList

    let classes =
      Seq.append classlist modularize |> Seq.toArray

    Array.sortInPlaceWith
      (fun l r ->
        let left = fst l
        let right = fst r

        let sort =
          String.Compare(left, right, StringComparison.OrdinalIgnoreCase)

        if sort = 0 then
          String.Compare(left, right, StringComparison.Ordinal)
        else
          sort)
      classes

    classes
    |> Seq.fold
         (fun stack c ->
           let name = fst c

           let restack =
             stack |> List.filter (fst >> (isNested name))

           let pr =
             match restack with
             | [] -> model
             | (_, r) :: _ -> r

           let nr = applyToModel pr c
           (name, nr) :: restack)
         []
    |> ignore

  let private populateAssemblyNode
    (environment: CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
    (model: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
    (node: XPathNavigator)
    (epoch: DateTime)
    =
    // within the <module> we have <method> nodes with name="get_module" class="AltCover.Coverage.CoverageSchema.coverage"
    let applyToModel
      (theModel: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
      (group: string * seq<MethodKey>)
      =
      let name = fst group

      let pc =
        group
        |> snd
        |> Seq.map (fun x -> x.Navigator)
        |> pcCover

      let newrow =
        TreeNode.Make environment.Icons.Namespace name
        |> withChildren
        |> withCoverage pc
        |> addTreeNode theModel

      populateNamespaceNode environment newrow (snd group) epoch

    let methods =
      node.SelectChildren("method", String.Empty)
      |> Seq.cast<XPathNavigator>
      |> Seq.map (fun m ->
        let classfullname =
          m.GetAttribute("class", String.Empty)

        let lastdot = classfullname.LastIndexOf('.')

        { Navigator = m
          NameSpace =
            if lastdot < 0 then
              String.Empty
            else
              classfullname.Substring(0, lastdot)
          ClassName =
            if lastdot < 0 then
              classfullname
            else
              classfullname.Substring(1 + lastdot)
          Name = m.GetAttribute("name", String.Empty) })
      |> Seq.groupBy (fun x -> x.NameSpace)
      |> Seq.sortBy fst

    methods |> Seq.iter (applyToModel model)

  let DoSelected (environment: CoverageModelDisplay<'TModel, 'TRow, 'TIcon>) index =
    let current = environment.GetFileInfo index

    let f0 () =
      environment.SetXmlNode
        { Icon = environment.Icons.Progress.Force()
          Points = 0
          Visited = 0
          Name = current.Name
          Tooltip = None
          Mapping = None
          Children = None // N/A
        }
      |> ignore

      environment.UpdateUISuccess null

    environment.TreeUIDispatch f0

    match CoverageFile.LoadCoverageFile current with
    | Left failed ->
      Messages.InvalidCoverageFileMessage environment.Display failed
      environment.UpdateMRUFailure current
    | Right coverage ->
      let navigator =
        coverage.Document.CreateNavigator()

      // check if coverage is newer that the source files
      let sourceFiles =
        navigator.Select("//seqpnt/@document")
        |> Seq.cast<XPathNavigator>
        |> Seq.map (fun x -> x.Value)
        |> Seq.distinct

      let missing =
        sourceFiles
        |> Seq.map GetSource
        |> Seq.filter (fun f -> not f.Exists)

      let newer =
        sourceFiles
        |> Seq.map GetSource
        |> Seq.filter (fun f -> f.Exists && f.Outdated current.LastWriteTimeUtc)

      let message =
        [ (missing, "NotAllSourcePresent")
          (newer, "SomeSourceModified") ]
        |> Seq.filter (fun (x, y) -> x |> Seq.isEmpty |> not)
        |> Seq.map (fun (x, y) -> Resource.Format(y, [||]))

      let pc = pcCover [ navigator ]

      let applyToModel
        (theModel: CoverageTreeContext<'TModel, 'TRow, 'TIcon>)
        (group: XPathNavigator * string)
        =
        let name = snd group
        let pc = ([ group |> fst ] |> pcCover)

        let newModel =
          TreeNode.Make environment.Icons.Assembly name
          |> withChildren
          |> withCoverage pc
          |> addTreeNode theModel

        populateAssemblyNode environment newModel (fst group) current.LastWriteTimeUtc

      let assemblies =
        coverage
          .Document
          .CreateNavigator()
          .Select("//module")
        |> Seq.cast<XPathNavigator>

      // UI thread needed here
      let f () =
        let model =
          environment.SetXmlNode
            { Icon =
                (if Seq.isEmpty missing then
                   if Seq.isEmpty newer then
                     environment.Icons.Report
                   else
                     environment.Icons.ReportDated
                 else
                   environment.Icons.ReportWarning)
                  .Force()
              Points = fst pc
              Visited = snd pc
              Name = current.Name
              Tooltip = Some(String.Join(Environment.NewLine, message))
              Mapping = None
              Children = None // N/A
            }

        environment.OnRowExpanded model.Row (fun () ->
          assemblies
          |> Seq.map (fun node ->
            (node,
             node
               .GetAttribute("assemblyIdentity", String.Empty)
               .Split(',')
             |> Seq.head))
          |> Seq.sortBy snd
          |> Seq.iter (applyToModel model))

        environment.UpdateUISuccess current

      environment.TreeUIDispatch f

[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member",  // MethodDefinition
                            Target = "AltCover.CoverageFileTree/step@205::Invoke(System.String,System.Int32)",
                            Justification = "Replace overload not in netstandard2.0")>]
// TODO -- get rid of these
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member",  // MethodDefinition
                            Target = "AltCover.CoverageFileTree/applyMethod@245::Invoke(AltCover.CoverageTreeContext`3<TModel,TRow,TIcon>,AltCover.GuiCommon/MethodKey)",
                            Justification = "Needs more refactoring")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member",  // MethodDefinition
                            Target = "AltCover.CoverageFileTree::addLeafNode(AltCover.CoverageModelDisplay`3<TModel,TRow,TIcon>,AltCover.CoverageTreeContext`3<TModel,TRow,TIcon>,AltCover.TreeNode`1<TIcon>)",
                            Justification = "To be used in replay pass (TODO)")>]
()