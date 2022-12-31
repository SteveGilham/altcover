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

#if AVALONIA11
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.AboutBox.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.App11.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.MainWindow11.xaml",
                            Justification = "Generated Code")>]
#else
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Security",
                            "StaticConstructorsShouldBePrivateRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml",
                            Justification = "Generated Code")>]
#endif
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
#if !AVALONIA11
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member", // MethodDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml::.ctor()",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.AboutBox::!XamlIlPopulate(System.IServiceProvider,AltCover.AboutBox)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member", // MethodDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml::.ctor()",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLongMethodsRule",
                            Scope = "member", // MethodDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml::.ctor()",
                            Justification = "Generated Code")>]
#endif
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow/XamlClosure_1::Build(System.IServiceProvider)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlHelpers::Avalonia.Markup.Xaml.MarkupExtensions.ReflectionBindingExtension,Avalonia.Markup.Xaml.RelativeSource!Property()",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!XamlLoader::TryLoad(System.String)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlTrampolines",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlContext",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUninstantiatedInternalClassesRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!AvaloniaResources",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "DisposableFieldsShouldBeDisposedRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.BadPractice",
                            "DoNotDecreaseVisibilityRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulateTrampoline(AltCover.AboutBox)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.BadPractice",
                            "DoNotDecreaseVisibilityRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulateTrampoline(AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.BadPractice",
                            "DoNotDecreaseVisibilityRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.App::!XamlIlPopulateTrampoline(AltCover.App)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "EnsureLocalDisposalRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulate(System.IServiceProvider,AltCover.AboutBox)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "EnsureLocalDisposalRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulate(System.IServiceProvider,AltCover.AboutBox)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "EnsureLocalDisposalRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "EnsureLocalDisposalRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "EnsureLocalDisposalRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Maintainability",
                            "AvoidAlwaysNullFieldRule",
                            Scope = "type", // TypeDefinition
                            Target = "AltCover.AboutBox",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Maintainability",
                            "AvoidAlwaysNullFieldRule",
                            Scope = "type", // TypeDefinition
                            Target = "AltCover.MainWindow",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Maintainability",
                            "AvoidAlwaysNullFieldRule",
                            Scope = "type", // TypeDefinition
                            Target = "AltCover.App",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLargeClassesRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLargeClassesRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidSpeculativeGeneralityRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlHelpers",
                            Justification = "Generated Code")>]
#if !AVALONIA11
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml",
                            Justification = "Generated Code")>]
#endif
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "AltCover.MainWindow/XamlClosure_1",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlHelpers",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlTrampolines",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlContext",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlContext/Context`1",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!AvaloniaResources",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.AboutBox.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.App11.xaml",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnsealedUninheritedInternalTypeRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.MainWindow11.xaml",
                            Justification = "Generated Code")>]
#if !AVALONIA11
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated Code")>]
#endif
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "AltCover.MainWindow/XamlClosure_1",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlHelpers",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlTrampolines",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.XamlIlContext",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "ConsiderUsingStaticTypeRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!AvaloniaResources",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "EnumeratorsShouldBeStronglyTypedRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "ParameterNamesShouldMatchOverriddenMethodRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/Context`1::set_BaseUri(System.Uri)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "ParameterNamesShouldMatchOverriddenMethodRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/Context`1::GetService(System.Type)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!XamlLoader::TryLoad(System.String)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!XamlLoader::TryLoad(System.String)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!XamlLoader::TryLoad(System.String)",
                            Justification = "Generated Code")>]
#if !AVALONIA11
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource",
                            Justification = "Generated Code")>]
#endif
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulate(System.IServiceProvider,AltCover.AboutBox)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.AboutBox::!XamlIlPopulateTrampoline(AltCover.AboutBox)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulateTrampoline(AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.App::!XamlIlPopulate(System.IServiceProvider,AltCover.App)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.App::!XamlIlPopulateTrampoline(AltCover.App)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!IndexerAccessorFactoryClosure",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!XamlLoader",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "type", // TypeDefinition
                            Target = "CompiledAvaloniaXaml.!AvaloniaResources",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "UseCorrectDisposeSignaturesRule",
                            Scope = "type", // TypeDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Exceptions",
                            "UseObjectDisposedExceptionRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.XamlIlContext/ParentStackEnumerable/Enumerator::MoveNext()",
                            Justification = "Generated Code")>]
#if !AVALONIA11
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.AboutBox.xaml::.ctor()",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.App.xaml::.ctor()",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target = "CompiledAvaloniaXaml.!EmbeddedResource/NamespaceInfo:AltCover.MainWindow.xaml::.ctor()",
                            Justification = "Generated Code")>]
#endif
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow::!XamlIlPopulate(System.IServiceProvider,AltCover.MainWindow)",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.AboutBox.xaml::CreateNamespaces()",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.App11.xaml::CreateNamespaces()",
                            Justification = "Generated Code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "UseStringEmptyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "CompiledAvaloniaXaml.!AvaloniaResources/NamespaceInfo:/AltCover.MainWindow11.xaml::CreateNamespaces()",
                            Justification = "Generated Code")>]
()