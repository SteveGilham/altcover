namespace AltCover

open System.Diagnostics.CodeAnalysis

open Avalonia
open Avalonia.Controls
open Avalonia.Logging

open Mono.Options

module VisualizerMain =
#if !NETSTANDARD2_0 // no AppBuilder here
  let BuildAvaloniaApp () =
    AppBuilderBase<AppBuilder>
      .Configure<App>()
      .UsePlatformDetect()
      .LogToTrace(LogEventLevel.Warning)
#endif

  [<EntryPoint>]
  let private main arguments =
    let options =
      [ ("g|geometry",
         (fun _ ->
           Persistence.clearGeometry ()
           Persistence.save <- false))
        ("r|recentFiles", (fun _ -> Persistence.saveCoverageFiles [])) ]
      |> List.fold
           (fun (o: OptionSet) (p, a) ->
             o.Add(p, Resource.GetResourceString p, System.Action<string>(a)))
           (OptionSet())

    options.Parse(arguments) |> ignore

#if !NETSTANDARD2_0
    BuildAvaloniaApp()
      .StartWithClassicDesktopLifetime(arguments)
#else
    0
#endif

[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidLargeNumberOfLocalVariablesRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml::.ctor()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml::.ctor()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulate(System.IServiceProvider,AltCover.AboutBox)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml::.ctor()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow/XamlClosure_1::Build(System.IServiceProvider)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlHelpers::Avalonia.Markup.Xaml.MarkupExtensions.ReflectionBindingExtension,Avalonia.Markup.Xaml.RelativeSource!Property()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.!XamlLoader::TryLoad(System.String)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.XamlIlContext",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "EnsureLocalDisposalRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "EnsureLocalDisposalRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulate(System.IServiceProvider,AltCover.AboutBox)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "DisposableFieldsShouldBeDisposedRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Maintainability",
                            "AvoidAlwaysNullFieldRule",
                            Scope = "type",
                            Target = "AltCover.MainWindow",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Maintainability",
                            "AvoidAlwaysNullFieldRule",
                            Scope = "type", // TypeDefinition
                            Target = "AltCover.AboutBox",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Maintainability",
                            "AvoidAlwaysNullFieldRule",
                            Scope = "type",
                            Target = "AltCover.App",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLargeClassesRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLargeClassesRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidSpeculativeGeneralityRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.XamlIlHelpers",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target = "AltCover.MainWindow/XamlClosure_1",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.XamlIlHelpers",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.XamlIlContext",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.XamlIlContext/Context`1",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type",
                            Target = "AltCover.MainWindow/XamlClosure_1",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.XamlIlHelpers",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.XamlIlContext",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "EnumeratorsShouldBeStronglyTypedRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "ParameterNamesShouldMatchOverriddenMethodRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/Context`1::set_BaseUri(System.Uri)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "ParameterNamesShouldMatchOverriddenMethodRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/Context`1::GetService(System.Type)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulateTrampoline(AltCover.MainWindow)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member",
                            Target =
                              "AltCover.App::!XamlIlPopulate(System.IServiceProvider,AltCover.App)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member",
                            Target =
                              "AltCover.App::!XamlIlPopulateTrampoline(AltCover.App)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "type",
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulate(System.IServiceProvider,AltCover.AboutBox)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulateTrampoline(AltCover.AboutBox)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "UseCorrectDisposeSignaturesRule",
                            Scope = "type",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Exceptions",
                            "UseObjectDisposedExceptionRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator::MoveNext()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml::.ctor()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml::.ctor()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml::.ctor()",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.BadPractice",
                            "DoNotDecreaseVisibilityRule",
                            Scope = "member",
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulateTrampoline(AltCover.MainWindow)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.BadPractice",
                            "DoNotDecreaseVisibilityRule",
                            Scope = "member",
                            Target =
                              "AltCover.App::!XamlIlPopulateTrampoline(AltCover.App)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.BadPractice",
                            "DoNotDecreaseVisibilityRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulateTrampoline(AltCover.AboutBox)",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member",
                            Target =
                              "CompiledAvaloniaXaml.!XamlLoader::TryLoad(System.String)",
                            Justification = "Generated code")>]
()