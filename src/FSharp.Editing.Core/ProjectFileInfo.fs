namespace FSharp.Editing.ProjectSystem

open System
open System.IO
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Diagnostics
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

    override self.ToString () = self |> function
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
//    ProjectId                 : ProjectId
    ProjectGuid               : Guid option
    Name                      : string option
    ProjectFilePath           : string
//    TargetFramework           : FrameworkName option
    TargetFramework           : string option
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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProjectFileInfo =

    let private createSrcDocs directory projectId filePaths srcCodeKind =
        filePaths |> Seq.map ^ fun path ->
            let fullpath = Path.Combine(directory,path)
            DocumentInfo.Create
                (   DocumentId.CreateNewId projectId
                ,   Path.GetFileNameWithoutExtension path
                ,   sourceCodeKind = srcCodeKind
                ,   filePath = fullpath
                ,   loader = FileTextLoader(fullpath,Text.Encoding.UTF8)
                ,   isGenerated = false
                )

    let createSrcDocInfos projectId (projectFileInfo:ProjectFileInfo) =
        createSrcDocs  projectFileInfo.ProjectDirectory
                    projectId
                    projectFileInfo.CompileFiles
                    SourceCodeKind.Regular

    let createScriptDocInfos projectId (projectFileInfo:ProjectFileInfo) =
        createSrcDocs projectFileInfo.ProjectDirectory
                    projectId
                    projectFileInfo.ScriptFiles
                    SourceCodeKind.Script

    let createOtherDocInfos projectId (projectFileInfo:ProjectFileInfo) =
        projectFileInfo.OtherFiles |> Seq.map ^ fun path ->
            let fullpath = Path.Combine(projectFileInfo.ProjectDirectory,path)
            DocumentInfo.Create
                (   DocumentId.CreateNewId projectId
                ,   Path.GetFileNameWithoutExtension path
                ,   filePath = fullpath
                ,   loader = FileTextLoader(fullpath,Text.Encoding.UTF8)
                ,   isGenerated = false
                )

    let createAdditionalDocuments projectId projectFileInfo =
        Seq.append  (createScriptDocInfos projectId projectFileInfo)
                    (createOtherDocInfos  projectId projectFileInfo)

    let createAnalyzerReferences (projectFileInfo:ProjectFileInfo) =
        if projectFileInfo.Analyzers.Length = 0 then Seq.empty else
        projectFileInfo.Analyzers |> Seq.map ^ fun path ->
            AnalyzerFileReference(path,AnalyzerAssemblyLoader())
            :> AnalyzerReference

    /// Converts into the Microsoft.CodeAnalysis ProjectInfo used by workspaces
    // TODO -
    //  change the internals to a recusive generation of projectInfo for all project references
    //  without creating duplicate projects
    let toProjectInfo (workspace:'a when 'a :> Workspace) (projectFileInfo : ProjectFileInfo) =

        let projectRefs =
                let projIds, paths = (workspace :> Workspace).GetProjectIdsFromPaths projectFileInfo.ProjectReferences
                // TODO - this is a temporary impl, projectInfos need to be generated for the paths to projects
                // that aren't contained in the workspace
                Seq.append
                    [ for projId in projIds -> ProjectReference projId ]
                    [ for path in paths -> ProjectReference ^ ProjectId.CreateNewId(path) ]

        let _projDict = workspace.ProjectDictionary()

        let projectId = 
            match projectFileInfo.ProjectGuid with
            | Some guid -> ProjectId.CreateFromSerialized (guid, projectFileInfo.ProjectFilePath)
            | None -> ProjectId.CreateNewId projectFileInfo.ProjectFilePath
            

        ProjectInfo.Create
            (   id                  = projectId
            ,   version             = VersionStamp.Create()
            ,   name                = defaultArg projectFileInfo.Name String.Empty
            ,   assemblyName        = projectFileInfo.AssemblyName
            ,   language            = Constants.FSharpLanguageName
            ,   filePath            = projectFileInfo.ProjectFilePath
            ,   outputFilePath      = projectFileInfo.OutputPath
            (*  - TODO -
                Correctly adding projectreferences is going to be an issue
                ProjectReference is created using a project id, which means a collection of
                projectFileInfos should be passed to this function to prevent the creation
                of duplicate projectfile infos for referenced projects that have different ids
            *)
            ,   projectReferences   = projectRefs
            ,   metadataReferences  = seq[]
            ,   analyzerReferences  = createAnalyzerReferences projectFileInfo
            ,   documents           = createSrcDocInfos projectId projectFileInfo
            ,   additionalDocuments = createAdditionalDocuments projectId projectFileInfo
            //,   compilationOptions=
            //,   parseOptions=
            //,   isSubmission=
            //,   hostObjectType=
            )

//
//    open Microsoft.FSharp.Compiler.SourceCodeServices
//
//    let toFSharpProjectOptions (workspace:'a when 'a :> Workspace) (projectFileInfo:ProjectFileInfo) : FSharpProjectOptions =
//        (projectFileInfo |> toProjectInfo workspace).ToFSharpProjectOptions workspace