System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#load "xlinq.fsx"

#r "../packages/build/Fake/tools/Fakelib.dll"
open System
open System.IO
open System.Xml
open System.Xml.Linq
open Xlinq
open Fake


let inline (|?|) (pred1:'a->bool) (pred2:'a->bool) = fun a -> pred1 a || pred2 a

let inline (|&|) (pred1:'a->bool) (pred2:'a->bool) = fun a -> pred1 a && pred2 a

let printseq sqs = sqs |> Seq.iter ^ printfn "%A"


//
type FsProjInfo = {
    Path : string
    Name : string
    AssemblyName : string
    RootNamespace : string
    OutputType : string
    SourceFiles : (string * string * string) list  // itemType * sourcePath * hintPath 
    ProjectReferences : string list
}


let extractInfo (fsprojPath:string) = 
    
    let xdoc = (XDocument.Load fsprojPath).Root

    let filterDoc name : XElement seq = 
        xdoc |> XElem.elements |> Seq.filter ^ fun xelem ->  XElem.isNamed name xelem

    let props = filterDoc "PropertyGroup" |> Seq.collect XElem.descendants
    let items = filterDoc "ItemGroup" |> Seq.collect XElem.descendants 

    let srcFiles = // itemType * sourcePath * hintPath option
        items |> Seq.filter ^ fun xelem ->
            xelem
            |>( XElem.isNamed "Compile"
            |?| XElem.isNamed "Content"
            |?| XElem.isNamed "Resource"
            |?| XElem.isNamed "EmbeddedResource"
            )
        |> Seq.map ^ fun xelem ->
            let hintPath =
                match XElem.tryGetAttributeValue "HintPath" xelem with
                | Some path -> Some path
                | None ->  
                    match XElem.tryGetElement "HintPath" xelem with
                    | Some xelm -> XElem.value xelm |> Some | None -> None
            (XElem.getName xelem, XElem.getAttributeValue "Include" xelem, hintPath)

    let projRefs = 
        items |> Seq.filter ^ fun xelem -> XElem.isNamed "ProjectReference" xelem
        |> Seq.map ^ fun x -> XElem.getAttributeValue "Include" x
        

    let fsprojName = fsprojPath |> Path.GetFileNameWithoutExtension
    let netcoreProj =  fsprojName + ".netcore.fsproj"
    let netcoreDir = (DirectoryInfo(Path.GetDirectoryName fsprojPath)).Name + ".netcore"
    let adjustPath srcpath = sprintf @"..\%s\%s" fsprojName srcpath

    let srcFiles = 
        srcFiles |> Seq.map ^ fun (itemType,sourcePath,hintPath) ->
            if hintPath.IsSome then (itemType,sourcePath,hintPath.Value) else
            (itemType, adjustPath  sourcePath, sourcePath)
        |> Seq.filter ^ fun (itemType, sourcePath, hintpath) ->
            not (String.Equals(Path.GetFileName sourcePath,"App.Config", StringComparison.OrdinalIgnoreCase))
        |> Seq.append [("Content","app.config","app.config")]


    let findProperty name (xelems: XElement seq) = 
        let result = 
            xelems |> Seq.tryPick ^ fun xelm -> 
                if XElem.isNamed name xelm then Some xelm else None
        defaultArg (result|> Option.map XElem.value) ""

    {   Path                = sprintf @"\src\%s\%s" netcoreDir netcoreProj
        Name                = findProperty "Name"          props
        RootNamespace       = findProperty "RootNamespace" props
        AssemblyName        = findProperty "AssemblyName"  props
        OutputType          = findProperty "OutputType"    props
        SourceFiles         = srcFiles |> List.ofSeq
        ProjectReferences   = projRefs |> List.ofSeq
    }



let netcoreTemplate (info:FsProjInfo) =

    let makeProjectRef projectPath = 
        let dirName = (DirectoryInfo(Path.GetDirectoryName projectPath)).Name + ".netcore"
        let projName = (Path.GetFileNameWithoutExtension projectPath)  + ".netcore"
        XElem.singleAttr "ProjectReference" "Include" (sprintf @"..\%s\%s.fsproj" dirName projName)

    let xml =
        XElem.create "Project" []
        |> XElem.setAttribute "Sdk" "FSharp.NET.Sdk;Microsoft.NET.Sdk"
        |>(fun root -> 
            let parent =
                XElem.create "PropertyGroup" [
                    XElem.make "Name"                       info.Name
                    XElem.make "RootNamespace"              info.RootNamespace
                    XElem.make "AssemblyName"               info.AssemblyName
                    XElem.make "DefineConstants"            "NETCORE"
                    XElem.make "DebugType"                  "portable"
                    XElem.make "OutputPath"                 @"..\..\build\$(TargetFramework)\$(Configuration)"
                    XElem.make "DocumentationFile"          @"..\..\build\$(TargetFramework)\$(Configuration)\$(AssemblyName).XML"
                    XElem.make "PackageTargetFallback"      "portable-net45+win8+wp8+wpa81"
                    XElem.make "EnableDefaultCompileItems"  "false"
                ] 
            let parent =
                if info.OutputType = "Exe" then 
                    parent 
                    |> XElem.addElem "OutputType" info.OutputType
                    |> XElem.addElem "TargetFramework" "netcoreapp1.0"
                else
                    parent    
                    |> XElem.addElem "OutputType" info.OutputType
                    |> XElem.addElem "TargetFramework" "netstandard1.6"
            XElem.addElement parent root
        )
        // source files
        |> XElem.addElem  "ItemGroup" (
            info.SourceFiles |> List.map ^ fun (itemType,sourcePath,hintPath)  ->
            XElem.withAttrs itemType [("Include",sourcePath);("HintPath",hintPath)]
        )

        // project references
        |> XElem.addElem  "ItemGroup" (info.ProjectReferences |> List.map makeProjectRef)
        |> XElem.addSingleAttr "Import" "Project" "..\..\.paket\Paket.Restore.targets"
    let doc = XDocument()
    doc.Add xml
    info.Path, doc

let emptyConfig = 
    """<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <runtime>
    <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">

    </assemblyBinding>
  </runtime>
</configuration>
"""


let genNetcoreProj = extractInfo >> netcoreTemplate

let generateNetcoreProjects root =

    let fsprojs = 
        !! (root + "/src/*/*.fsproj") 
        -- (root + "/src/*.netcore/*.fsproj")
    
    printfn "\nDiscovered Target F# Projects\n"

    printseq fsprojs

    let projDirs = fsprojs |> Seq.map Path.GetDirectoryName 

    let netcoreDirs = projDirs |> Seq.map ^ fun dir -> dir + ".netcore" 

    netcoreDirs |> Seq.iter ensureDirectory

    let netcoreXDocs =  fsprojs |> Seq.map   genNetcoreProj

    printfn "\nGenerating netcore .fsproj files\n"
    netcoreXDocs |> Seq.iter ^ fun (path,xdoc) -> 
        printfn   "-- '%s'" path

        let fsprojPath = Path.GetFullPath (root + path)
        let configFilePath  = (Path.GetDirectoryName fsprojPath) </> "app.config"
        if not ^ fileExists configFilePath then
            File.WriteAllText (configFilePath, emptyConfig)

        File.WriteAllText (Path.GetFullPath (root + path), xdoc.ToString())

    printfn "\n"
    traceLine ()


