namespace FSharp.Editing.ProjectSystem

open System
open System.IO
open Microsoft.CodeAnalysis
open System.Runtime.Versioning
open FSharp.Editing


/// Specifies the version of the F# compiler that should be used
type LanguageVersion =
    | FSharp2
    | FSharp3
    | FSharp4

type PlatformType =
    | X86
    | X64
    | AnyCPU

    override self.ToString() =
        self |> function
        | X86 -> Constants.X86
        | X64 -> Constants.X64
        | AnyCPU -> Constants.AnyCPU

    static member Parse text =
        text |> function
        | EqualsIC Constants.X86 -> X86
        | EqualsIC Constants.X64 -> X64
        | EqualsIC "Any CPU" | EqualsIC Constants.AnyCPU -> AnyCPU
        | _ -> failwithf "Could not parse '%s' into a `PlatformType`" text

    static member TryParse text =
        text |> function
        | EqualsIC Constants.X86 -> Some X86
        | EqualsIC Constants.X64 -> Some X64
        | EqualsIC "Any CPU" | EqualsIC Constants.AnyCPU -> Some AnyCPU
        | _ -> Option.None

/// Determines the output of compiling the F# Project
type OutputType =
    ///  An .exe with an entry point and a console.
    | Exe
    ///   An .exe with an entry point but no console.
    | Winexe
    /// a dynamically linked library (.dll)
    | Library
    /// Build a module that can be added to another assembly (.netmodule)
    | Module

    override self.ToString() = self |> function
        | Exe     -> Constants.Exe
        | Winexe  -> Constants.Winexe
        | Library -> Constants.Library
        | Module  -> Constants.Module

    static member Parse text = text |> function
        | EqualsIC Constants.Exe     -> Exe
        | EqualsIC Constants.Winexe  -> Winexe
        | EqualsIC Constants.Library -> Library
        | EqualsIC Constants.Module  -> Module
        | _ -> failwithf "Could not parse '%s' into a `OutputType`" text

    static member TryParse text = text |> function
        | EqualsIC Constants.Exe     -> Some Exe
        | EqualsIC Constants.Winexe  -> Some Winexe
        | EqualsIC Constants.Library -> Some Library
        | EqualsIC Constants.Module  -> Some Module
        | _ -> None



// TODO - add another field to store `AdditionalDocuments` for use during ProjectInfo creation
/// ProjectFileInfo combines the information needed to create a ProjectInfo for the workspace
/// and the configuration info to create a FSharpProjectOptions
[<NoComparison>]
type ProjectFileInfo = {
    ProjectId                 : ProjectId
    ProjectGuid               : Guid option
    Name                      : string option
    ProjectFilePath           : string
    TargetFramework           : FrameworkName option
    FrameworkVersion          : string option
    AssemblyName              : string
    OutputPath                : string
    OutputType                : OutputType
    SignAssembly              : bool
    AssemblyOriginatorKeyFile : string option
    GenerateXmlDocumentation  : string option
    Options                   : string []
    PreprocessorSymbolNames   : string []
    CompileFiles              : string []
    ResourceFiles             : string []
    EmbeddedResourceFiles     : string []
    ContentFiles              : string []
    PageFiles                 : string []
    ScriptFiles               : string []
    OtherFiles                : string []
    References                : string []
    /// Collection of paths fsproj files for the project references
    ProjectReferences         : string []
    Analyzers                 : string []
//    LogOutput                 : string
} with
    member self.ProjectDirectory = Path.GetDirectoryName self.ProjectFilePath


