namespace FSharp.Editing

open System

module Constants =

    let [<Literal>]  FSharpProjectGuidStr = "F2A71F9B-5D33-465A-A702-920D77279786"
    let FSharpProjectGuid = Guid FSharpProjectGuidStr
    let [<Literal>]  SolutionFolderGuidStr = "2150E333-8FDC-42A3-9474-1A3956D46DE8"
    let SolutionFolderGuid  = Guid SolutionFolderGuidStr
    let [<Literal>]  FSharpLanguageName = "F#"
    let [<Literal>]  FSharpContentTypeName = "F#"

    // Common Constants
    let [<Literal>]  Name = "Name"
    let [<Literal>]  None = "None"
    let [<Literal>]  Reference = "Reference"

    // Platform Constants
    let [<Literal>]  X86 = "x86"
    let [<Literal>]  X64 = "x64"
    let [<Literal>]  AnyCPU = "AnyCPU"

    // BuildAction Constants
    let [<Literal>]  Compile = "Compile"
    let [<Literal>]  Content = "Content"
    let [<Literal>]  Resource = "Resource"
    let [<Literal>]  EmbeddedResource = "EmbeddedResource"

    // CopyToOutputDirectory Constants
    let [<Literal>]  Never = "Never"
    let [<Literal>]  Always = "Always"
    let [<Literal>]  PreserveNewest = "PreserveNewest"

    // DebugType Constants
    let [<Literal>]  PdbOnly = "PdbOnly"
    let [<Literal>]  Full = "Full"

    // OutputType Constants
    let [<Literal>]  Exe = "Exe"
    let [<Literal>]  Winexe = "Winexe"
    let [<Literal>]  Library = "Library"
    let [<Literal>]  Module = "Module"

    // XML Attribute Name Constants
    let [<Literal>]  DefaultTargets = "DefaultTargets"
    let [<Literal>]  ToolsVersion = "ToolsVersion"
    let [<Literal>]  Include = "Include"
    let [<Literal>]  Condition = "Condition"

    // MSBuild XML Element Constants

    let [<Literal>]  Project = "Project"
    let [<Literal>]  ItemGroup = "ItemGroup"
    let [<Literal>]  PropertyGroup = "PropertyGroup"
    let [<Literal>]  ProjectReference = "ProjectReference"

    // XML Property Constants (found in PropertyGroups)
    let [<Literal>]  AssemblyName = "AssemblyName"
    let [<Literal>]  RootNamespace = "RootNamespace"
    let [<Literal>]  Configuration = "Configuration"
    let [<Literal>]  Platform = "Platform"
    let [<Literal>]  SchemaVersion = "SchemaVersion"
    let [<Literal>]  ProjectGuid = "ProjectGuid"
    let [<Literal>]  ProjectType = "ProjectType"
    let [<Literal>]  OutputType = "OutputType"
    let [<Literal>]  TargetFrameworkVersion = "TargetFrameworkVersion"
    let [<Literal>]  TargetFrameworkProfile = "TargetFrameworkProfile"
    let [<Literal>]  AutoGenerateBindingRedirects = "AutoGenerateBindingRedirects"
    let [<Literal>]  TargetFSharpCoreVersion = "TargetFSharpCoreVersion"
    let [<Literal>]  DebugSymbols = "DebugSymbols"
    let [<Literal>]  DebugType = "DebugType"
    let [<Literal>]  Optimize = "Optimize"
    let [<Literal>]  Tailcalls = "Tailcalls"
    let [<Literal>]  OutputPath = "OutputPath"
    let [<Literal>]  CompilationConstants = "DefineConstants"
    let [<Literal>]  WarningLevel = "WarningLevel"
    let [<Literal>]  PlatformTarget = "PlatformTarget"
    let [<Literal>]  DocumentationFile = "DocumentationFile"
    let [<Literal>]  Prefer32Bit = "Prefer32Bit"
    let [<Literal>]  OtherFlags = "OtherFlags"

    // XML Elements
    let [<Literal>]  CopyToOutputDirectory = "CopyToOutputDirectory"
    let [<Literal>]  HintPath = "HintPath"
    let [<Literal>]  Private = "Private"
    let [<Literal>]  SpecificVersion = "SpecificVersion"
    let [<Literal>]  Link = "Link"
    let [<Literal>]  Paket = "Paket"
    let [<Literal>]  XmlDecl = @"<?xml version='1.0' encoding='utf-8'?>"
    let [<Literal>]  Xmlns = "http://schemas.microsoft.com/developer/msbuild/2003"

    let fsharpKeywords = 
        [   "abstract"
            "and"        
            "as"         
            "assert"     
            "asr"        
            "base"       
            "begin"      
            "class"      
            "const"      
            "default"    
            "delegate"   
            "do"         
            "done"       
            "downcast"   
            "downto"     
            "elif"       
            "else"       
            "end"        
            "exception"  
            "extern"     
            "false"      
            "finally"    
            "fixed"      
            "for"        
            "fun"        
            "function"   
            "global"     
            "if"         
            "in"         
            "inherit"    
            "inline"     
            "interface"  
            "internal"   
            "land"       
            "lazy"       
            "let"        
            "lor"        
            "lsl"        
            "lsr"        
            "lxor"       
            "match"      
            "member"     
            "mod"        
            "module"     
            "mutable"    
            "namespace"  
            "new"        
            "null"       
            "of"         
            "open"       
            "or"         
            "override"   
            "private"    
            "public"     
            "rec"        
            "return"     
            "sig"        
            "static"     
            "struct"     
            "then"       
            "to"         
            "true"       
            "try"        
            "type"       
            "upcast"     
            "use"        
            "val"        
            "void"       
            "when"       
            "while"      
            "with"       
            "yield"   
        ] |> Set.ofList



[<RequireQualifiedAccess>]
/// MSBuild Properties
module Property =
    let [<Literal>] AllowUnsafeBlocks = "AllowUnsafeBlocks"
    let [<Literal>] AssemblyName = "AssemblyName"
    let [<Literal>] AssemblyOriginatorKeyFile = "AssemblyOriginatorKeyFile"
    let [<Literal>] BuildProjectReferences = "BuildProjectReferences"
    let [<Literal>] DefineConstants = "DefineConstants"
    let [<Literal>] DesignTimeBuild = "DesignTimeBuild"
    let [<Literal>] DocumentationFile = "DocumentationFile"
    let [<Literal>] LangVersion = "LangVersion"
    let [<Literal>] OutputType = "OutputType"
    let [<Literal>] MSBuildExtensionsPath = "MSBuildExtensionsPath"
    let [<Literal>] ProjectGuid = "ProjectGuid"
    let [<Literal>] ProjectName = "ProjectName"
    let [<Literal>] ResolveReferenceDependencies = "ResolveReferenceDependencies"
    let [<Literal>] SignAssembly = "SignAssembly"
    let [<Literal>] SolutionDir = "SolutionDir"
    let [<Literal>] TargetFrameworkMoniker = "TargetFrameworkMoniker"
    let [<Literal>] TargetPath = "TargetPath"
    let [<Literal>] VisualStudioVersion = "VisualStudioVersion"


[<RequireQualifiedAccess>]
/// MSBuild Project Target Names
module TargetName =

    let [<Literal>] ResolveReferences = "ResolveReferences"


[<RequireQualifiedAccess>]
/// MSBuild Project Item Names
module ItemName =
    let [<Literal>] Analyzer = "Analyzer"
    let [<Literal>] Compile = "Compile"
    let [<Literal>] None = "None"
    let [<Literal>] ProjectReference = "ProjectReference"
    let [<Literal>] ReferencePath = "ReferencePath"

[<RequireQualifiedAccess>]
/// MSBuild Project Metadata Names
module MetadataName =
    let [<Literal>] FullPath = "FullPath"
    let [<Literal>] Project = "Project"
    let [<Literal>] ReferenceSourceTarget = "ReferenceSourceTarget"

