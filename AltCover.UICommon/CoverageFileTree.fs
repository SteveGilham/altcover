namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.XPath
open Augment
open GuiCommon

[<NoComparison>]
type CoverageTreeContext<'TModel, 'TRow> = { Model: 'TModel; Row: 'TRow }

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Design.Generic",
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
    SetXmlNode: String -> Lazy<'TIcon> -> CoverageTreeContext<'TModel, 'TRow>
    AddNode: CoverageTreeContext<'TModel, 'TRow> -> Lazy<'TIcon> -> String -> String option -> CoverageTreeContext<'TModel, 'TRow>
    Map: CoverageTreeContext<'TModel, 'TRow> -> XPathNavigator -> unit }

module CoverageFileTree =

  type SourceFile<'TIcon> = {
    FullName : string
    FileName : string
    X : XPathNavigator
    Icon : Lazy<'TIcon>
    Exists : bool
    Stale : bool
  }

  [<SuppressMessage("Gendarme.Rules.Correctness",
            "ReviewSelfAssignmentRule",
            Justification = "Final line is a self-assignment for 'depth' -- compiler fault")>]
  let rec private scan (s:string) index depth =
    match s.[index] with
    | '<' -> scan s (index + 1) (depth + 1)
    | '>' -> let d = depth - 1
             if d = 0
             then index
             else scan s (index + 1) d
    | _ -> scan s (index + 1) depth

  let private populateClassNode
    (environment: CoverageModelDisplay<'TModel, 'TRow, 'TIcon>)
    (model: CoverageTreeContext<'TModel, 'TRow>)
    (nodes: seq<MethodKey>)
    (epoch : DateTime)
    =
    let applyToModel
      (theModel: CoverageTreeContext<'TModel, 'TRow>)
      (item: (string * MethodType) * MethodKey seq)
      =
      let ((display, special), keys) = item

      // C++/CLI fixups -- probably more will be needed
      let modopt s = System.Text.RegularExpressions.Regex.Replace(s,
                       @"modopt\(System\.Runtime\.\w+\.(\w+)?\)", "[$1]")

      let gcroot (s: string) =
        let rec step (s: string) (i:int) =
          let next = s.IndexOf("gcroot<", i, StringComparison.Ordinal)
          if next < 0
          then s
          else
            let index = next + 7
            let stop = scan s index 1
            let start = s.Substring(0, index)
            let middle = s.Substring(index, stop - index).Replace("::", ".")
            let finish = s.Substring(stop)
            step (start + middle + finish) index
        step s 0

      let fixup = modopt >> gcroot

      let applyMethod (mmodel: CoverageTreeContext<'TModel, 'TRow>) (x: MethodKey) =
        let fullname =
          fixup <| x.Navigator.GetAttribute("fullname", String.Empty)

        let name = fixup x.Name

        //printfn "%s ||| %s" name fullname

        let args =
          if String.IsNullOrEmpty(fullname)
             || charIndexOf name '(' > 0 then
            String.Empty
          else
            let bracket = charIndexOf fullname '('

            if bracket < 0 then
              String.Empty
            else
              fullname.Substring(bracket)

        let displayname = name + args

        let offset =
          match displayname.LastIndexOf("::", StringComparison.Ordinal) with
          | -1 -> 0
          | o -> o + 2

        // multi-source??
        let upcase (s:string) = s.ToUpperInvariant()
        let getFileName (s:string) =
          if
            s.StartsWith("http://", StringComparison.Ordinal)
            || s.StartsWith
              (
                "https://",
                StringComparison.Ordinal
              )
          then
            {
              FullName = s
              FileName = System.Uri(s).LocalPath |> Path.GetFileName
              X = null
              Icon = environment.Icons.SourceLink
              Exists = true
              Stale = false
            }

          else
            let info = GetSource(s)
            let stale = info.Outdated epoch
            {
              FullName = s
              FileName = Path.GetFileName s
              X = null
              Icon = match (info.Exists, stale) with
                     | (false, _) -> environment.Icons.NoSource
                     | (_, true) -> environment.Icons.SourceDated
                     | _ -> environment.Icons.Source
              Exists = info.Exists
              Stale = stale
            }

        let sources =
          x.Navigator.SelectDescendants("seqpnt", String.Empty, false)
          |> Seq.cast<XPathNavigator>
          |> Seq.map
           (fun s ->
              let d = s.GetAttribute("document", String.Empty)
              { (d |> getFileName) with X = s })
          |> Seq.distinctBy (fun s -> s.FullName) // allows for same name, different path
          |> Seq.sortBy (fun s -> s.FileName |> upcase)
          |> Seq.toList

        let hasSource = sources
                        |> List.exists (fun s -> s.Exists)
        let icon = if hasSource
                   then environment.Icons.Method
                   else environment.Icons.MethodMissingSource

        match sources with
        | [] ->
          environment.AddNode
            mmodel
            environment.Icons.MethodNoSource
            (displayname.Substring(offset))
            None |> ignore

        | [source] ->
          let newrow =
            environment.AddNode
              mmodel
              (if source.Stale then environment.Icons.MethodDated else icon)
              (displayname.Substring(offset))
              (if hasSource
               then None
               else Some <| Resource.Format("FileNotFound", [| source.FullName |]))

          if hasSource && (not source.Stale)
          then environment.Map newrow x.Navigator

        | _ ->
          // If multi-source (has inlines), add the source file nodes to the hittable map
          let newrow =
            environment.AddNode
              mmodel
              icon
              (displayname.Substring(offset))
              None
          sources
          |> List.iter (fun s ->
            let srow =
                environment.AddNode
                  newrow
                  (if s.Stale then environment.Icons.SourceDated else icon)
                  s.FileName
                  (if s.Exists
                   then None
                   else Some <| Resource.Format("FileNotFound", [| s.FullName |]))
            if s.Exists && (not s.Stale) then environment.Map srow s.X
          )

      if special <> MethodType.Normal then
        let newrow =
          environment.AddNode
            theModel
            (if special = MethodType.Property then
               environment.Icons.Property
             else
               environment.Icons.Event)
            display
            None

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
      |> Array.sortInPlaceWith
           (fun ((l, (lb: MethodType)), _) ((r, rb), _) ->
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
    (model: CoverageTreeContext<'TModel, 'TRow>)
    (nodes: seq<MethodKey>)
    (epoch : DateTime)
    =
    let applyToModel
      (theModel: CoverageTreeContext<'TModel, 'TRow>)
      (group: string * seq<MethodKey>)
      =
      let name = fst group

      let icon =
        if group |> snd |> Seq.isEmpty then
          environment.Icons.Module
        else
          let names = group
                      |> snd
                      |> Seq.map
                          (fun key -> key.Name |> DisplayName)
                      |> Seq.filter (fun d -> d.[0] <> '.')
                      |> Seq.toList

          if names |> List.isEmpty ||
             names |> List.exists
                     (fun d -> d.Equals("Invoke") |> not )
          then
            environment.Icons.Class
          else
            environment.Icons.Effect

      let newrow = environment.AddNode theModel icon name None

      populateClassNode environment newrow (snd group) epoch
      newrow

    let isNested (name: string) n =
      name.StartsWith(n + "+", StringComparison.Ordinal)
      || name.StartsWith(n + "/", StringComparison.Ordinal)

    let classlist =
      nodes
      |> Seq.groupBy (fun x -> x.ClassName)
      |> Seq.toList

    let classnames = classlist |> Seq.map fst |> Set.ofSeq

    let modularize =
      let contains (s: String) c =
        s.IndexOf(c, StringComparison.Ordinal) >= 0

      classnames
      |> Seq.filter (fun cn -> contains cn "+" || contains cn "/")
      |> Seq.map
           (fun cn -> cn.Split([| "+"; "/" |], StringSplitOptions.RemoveEmptyEntries).[0])
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
    (model: CoverageTreeContext<'TModel, 'TRow>)
    (node: XPathNavigator)
    (epoch : DateTime)
    =
    // within the <module> we have <method> nodes with name="get_module" class="AltCover.Coverage.CoverageSchema.coverage"
    let applyToModel
      (theModel: CoverageTreeContext<'TModel, 'TRow>)
      (group: string * seq<MethodKey>)
      =
      let name = fst group

      let newrow =
        environment.AddNode theModel environment.Icons.Namespace name None

      populateNamespaceNode environment newrow (snd group) epoch

    let methods =
      node.SelectChildren("method", String.Empty)
      |> Seq.cast<XPathNavigator>
      |> Seq.map
           (fun m ->
             let classfullname = m.GetAttribute("class", String.Empty)
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

    match CoverageFile.LoadCoverageFile current with
    | Left failed ->
        Messages.InvalidCoverageFileMessage environment.Display failed
        environment.UpdateMRUFailure current
    | Right coverage ->
        // check if coverage is newer that the source files
        let sourceFiles =
          coverage
            .Document
            .CreateNavigator()
            .Select("//seqpnt/@document")
          |> Seq.cast<XPathNavigator>
          |> Seq.map (fun x -> x.Value)
          |> Seq.distinct

        let missing =
          sourceFiles
          |> Seq.map GetSource
          |> Seq.filter (fun f -> not f.Exists)

        //if not (Seq.isEmpty missing) then
        //  Messages.MissingSourceFileMessage environment.Display current

        let newer =
          sourceFiles
          |> Seq.map GetSource
          |> Seq.filter (fun f -> f.Exists && f.Outdated current.LastWriteTimeUtc)
        // warn if not
        //if not (Seq.isEmpty newer) then
        //  Messages.OutdatedCoverageFileMessage environment.Display current

        let model = environment.SetXmlNode current.Name (if Seq.isEmpty missing
                                                         then
                                                          if Seq.isEmpty newer
                                                          then environment.Icons.Report
                                                          else environment.Icons.ReportDated
                                                         else
                                                          environment.Icons.ReportWarning)

        let applyToModel
          (theModel: CoverageTreeContext<'TModel, 'TRow>)
          (group: XPathNavigator * string)
          =
          let name = snd group

          let newModel =
            environment.AddNode theModel environment.Icons.Assembly name None

          populateAssemblyNode environment newModel (fst group) current.LastWriteTimeUtc

        let assemblies =
          coverage
            .Document
            .CreateNavigator()
            .Select("//module")
          |> Seq.cast<XPathNavigator>

        assemblies
        |> Seq.map
             (fun node ->
               (node,
                node
                  .GetAttribute("assemblyIdentity", String.Empty)
                  .Split(',')
                |> Seq.head))
        |> Seq.sortBy snd
        |> Seq.iter (applyToModel model)

        environment.UpdateUISuccess current