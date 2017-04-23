// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"
#r "System.IO.Compression.FileSystem"

open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
open System.Xml

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSharp.Editing"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Rich F# language support for editors"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "IDE-agnostic features intended to be used in different F# IDEs and editors."
// List of author names (for NuGet package)
let authors = [ "Jared Hester"; "Vasily Kirichenko" ]
// Tags for your project (for NuGet package)
let tags = "F# fsharp languageService tooling editor formatting editing highlighting navigation refactoring"

// File system information 
// (<solutionFile>.sln is built during the building process)
let solutionFile  = "FSharp.Editing"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitOwner = "fsharp-editing"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "FSharp.Editing"
let cloneUrl = "https://github.com/fsprojects/FSharp.Editing.git"


let (^) = (<|)
let buildDir = "bin"
let tempDir = "temp"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (File.ReadAllLines "RELEASE_NOTES.md")

let isAppVeyorBuild = environVar "APPVEYOR" = "true"
let buildVersion = sprintf "%s-a%s" release.NugetVersion (DateTime.UtcNow.ToString "yyMMddHHmm")

let msbuild14 = ProgramFilesX86</>"MSBuild"</>"14.0"</>"Bin"</>"MSBuild.exe"

if isWindows && fileExists msbuild14 then 
    setEnvironVar "MSBUILD"  msbuild14

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let shared = [   
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion 
    ] 

    CreateFSharpAssemblyInfo "src/FSharp.Editing.Messages/AssemblyInfo.fs" 
        (Attribute.Title "FSharp.Editing.Messages" :: shared)
    CreateFSharpAssemblyInfo "src/FSharp.Editing.Server/AssemblyInfo.fs" 
        (Attribute.Title "FSharp.Editing.Server" :: shared)

    CreateFSharpAssemblyInfo "src/FSharp.Editing/AssemblyInfo.fs"
        (Attribute.InternalsVisibleTo "FSharp.Editing.Tests" :: Attribute.Title "FSharp.Editing" :: shared)
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" ^ fun _ -> 
    !! "src/**/obj" 
    ++ "src/**/bin" 
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    ++ "bin" ++ "temp" ++ "nuget"
    |> CleanDirs 

Target "CleanDocs" (fun _ -> CleanDirs ["docs/output"])

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    // We would like to build only one solution
    !! (solutionFile + ".sln")   
    |> MSBuildReleaseExt "" [ 
            "ToolsVersion", "14.0"  
            "VisualStudioVersion", "14.0"
        ]
        "Rebuild"
    |> ignore
)

// Build test projects in Debug mode in order to provide correct paths for multi-project scenarios
Target "BuildTests" (fun _ ->    
    !! "tests/data/**/*.sln"
    |> MSBuildDebug "" "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

open Fake.Testing

Target "UnitTests" (fun _ ->
    [@"tests/FSharp.Editing.Tests/bin/Release/FSharp.Editing.Tests.dll"]
    |> NUnit3 (fun p ->
        let param =
            { p with
                ShadowCopy = false
                TimeOut = TimeSpan.FromMinutes 20.
                Domain = NUnit3DomainModel.MultipleDomainModel 
                ResultSpecs = ["TestResults.xml"] }
        if isAppVeyorBuild then { param with Where = "cat != AppVeyorLongRunning" } else param)
)


let dotnetcliVersion = "2.0.0-preview2-005840"

let dotnetSDKPath = System.Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) </> "dotnetcore" </> dotnetcliVersion |> FullName

let dotnetExePath =
    dotnetSDKPath </> (if isWindows then "dotnet.exe" else "dotnet") |> FullName



Target "InstallDotNetCore" (fun _ ->
    let correctVersionInstalled = 
        try if FileInfo(dotnetExePath |> Path.GetFullPath).Exists then
                let processResult = 
                    ExecProcessAndReturnMessages (fun info ->  
                    info.FileName <- dotnetExePath
                    info.WorkingDirectory <- Environment.CurrentDirectory
                    info.Arguments <- "--version") (TimeSpan.FromMinutes 30.)
                processResult.Messages |> separated "" = dotnetcliVersion
            else false
        with  _ -> false

    if correctVersionInstalled then tracefn "dotnetcli %s already installed" dotnetcliVersion else
    CleanDir dotnetSDKPath
    let archiveFileName = 
        if isLinux then
            sprintf "dotnet-dev-ubuntu-x64.%s.tar.gz" dotnetcliVersion
        else if Fake.EnvironmentHelper.isMacOS then
            sprintf "dotnet-dev-osx-x64.%s.tar.gz" dotnetcliVersion
        else
            sprintf "dotnet-dev-win-x64.%s.zip" dotnetcliVersion
    let downloadPath = 
            sprintf "https://dotnetcli.azureedge.net/dotnet/Sdk/%s/%s" dotnetcliVersion archiveFileName
    let localPath = Path.Combine(dotnetSDKPath, archiveFileName)

    tracefn "Installing '%s' to '%s'" downloadPath localPath
        
    use webclient = new Net.WebClient()
    webclient.DownloadFile(downloadPath, localPath)

    if isLinux || isMacOS then
        let assertExitCodeZero x =
            if x = 0 then () else
            failwithf "Command failed with exit code %i" x

        Shell.Exec("tar", sprintf """-xvf "%s" -C "%s" """ localPath dotnetSDKPath)
        |> assertExitCodeZero
    else  
        global.System.IO.Compression.ZipFile.ExtractToDirectory(localPath, dotnetSDKPath)
        
        tracefn "dotnet cli path - %s" dotnetSDKPath
        System.IO.Directory.EnumerateFiles dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s" path)
        System.IO.Directory.EnumerateDirectories dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s%c" path System.IO.Path.DirectorySeparatorChar)

    let oldPath = System.Environment.GetEnvironmentVariable("PATH")
    System.Environment.SetEnvironmentVariable("PATH", sprintf "%s%s%s" dotnetSDKPath (System.IO.Path.PathSeparator.ToString()) oldPath)
)


let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x

let runCmdIn workDir exe = 
    Printf.ksprintf ^ fun args -> 
        tracefn "%s %s" exe args
        Shell.Exec(exe, args, workDir) |> assertExitCodeZero

/// Execute a dotnet cli command
let dotnet workDir = runCmdIn workDir "dotnet"

#load "scripts/GenNetcore.fsx"

Target "DotnetGenerate" ^ fun _ -> 
    GenNetcore.generateNetcoreProjects __SOURCE_DIRECTORY__    

let netcoreFiles = !! "src/netcore/*/*.fsproj" |> Seq.toList

Target "DotnetRestore" ^ fun _ ->
    try netcoreFiles |> Seq.iter ^ fun proj ->
        DotNetCli.Restore ^ fun c ->
        { c with 
            Project = proj 
            ToolPath = dotnetExePath 
            //AdditionalArgs = 
            //[   "-s https://dotnet.myget.org/F/roslyn/api/v3/index.json"
            //    "-s https://dotnet.myget.org/F/dotnet-corefxlab/api/v3/index.json"
            //]}
        }
    with ex ->  traceError ex.Message

Target "DotnetBuild" ^ fun _ ->
    netcoreFiles |> Seq.iter ^ fun proj ->
        DotNetCli.Build ^ fun c ->
        { c with 
            Project = proj 
            ToolPath = dotnetExePath 
            //Configuration = "Release"
            //AdditionalArgs = [ "/ds"; "/m"; (*"/pp"*) ]
        }
    

Target "DotnetPackage" ^ fun _ ->
    netcoreFiles |> Seq.iter ^ fun proj ->
        DotNetCli.Pack ^ fun c ->
        { c with
            Project = proj
            ToolPath = dotnetExePath
            AdditionalArgs = [(sprintf "-o %s" currentDirectory </> tempDir </> "dotnetcore"); (sprintf "/p:Version=%s" release.NugetVersion)]
        }




// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "PublishNuGet" (fun _ ->
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "bin"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = true
            Dependencies = [ "FSharp.Compiler.Service", GetPackageVersion "packages" "FSharp.Compiler.Service" ] })
        (project + ".Core.nuspec")
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" cloneUrl "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Git.Commit.Commit tempDocsDir (sprintf "[skip ci] Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)


Target "Main"  DoNothing
Target "ReleaseAll"  DoNothing
Target "BuildBoth"  DoNothing


"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "BuildTests"
  ==> "UnitTests"
  ==> "Main"

"Clean"
  ==> "InstallDotNetCore"
  ==> "DotnetGenerate"
  ==> "DotnetRestore"
  ==> "DotnetBuild"
  ==> "DotnetPackage"


"Build" ==> "BuildBoth" 
"DotnetBuild" ==> "BuildBoth"
//
//"Release"
//  ==> "PublishNuGet"
//  ==> "ReleaseAll"
//
//"Main"
//  ==> "All"
//
//"Main" 
//  ==> "CleanDocs"
//  ==> "GenerateDocs"
//  ==> "ReleaseDocs"
//  ==> "Release"

RunTargetOrDefault "BuildBoth"
