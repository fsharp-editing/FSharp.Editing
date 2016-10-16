// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
open System.Xml
open Fake.Testing

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
let authors = [ "Anh-Dung Phan"; "Vasily Kirichenko"; "Denis Ok" ]
// Tags for your project (for NuGet package)
let tags = "F# fsharp formatting editing highlighting navigation refactoring"

// File system information 
// (<solutionFile>.sln is built during the building process)
let solutionFile  = "FSharp.Editing"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitOwner = "fsprojects"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "FSharp.Editing"
let cloneUrl = "https://github.com/fsprojects/FSharp.Editing.git"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (File.ReadAllLines "RELEASE_NOTES.md")

let isAppVeyorBuild = environVar "APPVEYOR" <> null
let buildVersion = sprintf "%s-a%s" release.NugetVersion (DateTime.UtcNow.ToString "yyMMddHHmm")

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let shared =
    [   Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ] 

  CreateFSharpAssemblyInfo "src/FSharp.Editing.Messages/AssemblyInfo.fs" 
      (Attribute.Title "FSharp.Editing.Messages" :: shared)
  CreateFSharpAssemblyInfo "src/FSharp.Editing.Server/AssemblyInfo.fs" 
      (Attribute.Title "FSharp.Editing.Server" :: shared)

  CreateFSharpAssemblyInfo "src/FSharp.Editing/AssemblyInfo.fs"
      (Attribute.InternalsVisibleTo "FSharp.Editing.Tests" :: Attribute.Title "FSharp.Editing" :: shared)
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ -> CleanDirs ["bin"; "temp"; "nuget"])
Target "CleanDocs" (fun _ -> CleanDirs ["docs/output"])

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    // We would like to build only one solution
    !! (solutionFile + ".sln")
    |> MSBuildRelease "" "Rebuild"
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

Target "UnitTests" (fun _ ->
    [@"tests/FSharp.Editing.Tests/bin/Release/FSharp.Editing.Tests.dll"]
    |> NUnit3 (fun p ->
        let param =
            { p with
                ShadowCopy = false
                TimeOut = TimeSpan.FromMinutes 20.
                //Framework = NUnit3Runtime.Net45
                Domain = NUnit3DomainModel.MultipleDomainModel 
                //Workers = Some 1
                ResultSpecs = ["TestResults.xml"] }
        if isAppVeyorBuild then { param with Where = "cat != AppVeyorLongRunning" } else param)
)

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

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

let readString prompt echo : string =
    let rec loop cs =
        let key = Console.ReadKey(not echo)
        match key.Key with
        | ConsoleKey.Backspace -> 
            match cs with [] -> loop [] | _::cs -> loop cs
        | ConsoleKey.Enter -> cs
        | _ -> loop (key.KeyChar :: cs)

    printf "%s" prompt
    let input =
        loop []
        |> List.rev
        |> Array.ofList
        |> fun cs -> String cs
    if not echo then printfn ""
    input

#r @"packages/build/Selenium.WebDriver/lib/net40/WebDriver.dll"
#r @"packages/build/canopy/lib/canopy.dll"

Target "Release" (fun _ ->
    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion

    // let user = readString "Username: " true
    // let pw = readString "Password: " false

    // // release on github
    // createClient user pw
    // |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    // |> uploadFile "./bin/FSharpVSPowerTools.vsix"
    // |> releaseDraft
    // |> Async.RunSynchronously
)

Target "ReleaseAll"  DoNothing

// --------------------------------------------------------------------------------------
// Run main targets by default. Invoke 'build <Target>' to override

Target "Main" DoNothing

Target "All" DoNothing

Target "TravisCI" (fun _ -> 
  [ "src/FSharp.Editing/FSharp.Editing.fsproj"
    "tests/FSharp.Editing.Tests/FSharp.Editing.Tests.fsproj"
  ]
  |> MSBuildRelease "" "Rebuild"
  |> ignore
  
  let additionalFiles = 
    ["./packages/FSharp.Core/lib/net40/FSharp.Core.sigdata";
     "./packages/FSharp.Core/lib/net40/FSharp.Core.optdata";
     "./packages/FSharp.Core/lib/net40/FSharp.Core.xml";]
  CopyTo "tests/FSharp.Editing.Tests/bin/Release" additionalFiles

  ["tests/FSharp.Editing.Tests/bin/Release/FSharp.Editing.Tests.dll"]
  |> NUnit3 (fun p ->
    let param =
        { p with
            ShadowCopy = false
            TimeOut = TimeSpan.FromMinutes 20.
            Framework = NUnit3Runtime.Mono40
            Domain = NUnit3DomainModel.MultipleDomainModel 
            Workers = Some 1
            ResultSpecs = ["TestResults.xml"]      
        }
    if Environment.OSVersion.Platform = PlatformID.Win32NT then param else { param with Where = "cat != IgnoreOnUnix" }
  )
)

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "BuildTests"
  ==> "UnitTests"
  ==> "Main"

"Clean"
  ==> "AssemblyInfo"
  ==> "TravisCI"

"Release"
  ==> "PublishNuGet"
  ==> "ReleaseAll"

"Main"
  ==> "All"

"Main" 
  ==> "CleanDocs"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "Main"
