open System
open System.Threading
open Microsoft.CodeAnalysis
open System.IO
open FSharp.Editing
open FSharp.Editing.ProjectSystem
open Newtonsoft.Json


type OutputReader (sr:StreamReader) =
    let mutable text = ""
    member __.Exec () = 
        text <- sr.ReadToEnd()
    member __.Text = text


[<EntryPoint>]
let main argv = 


    System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
    let clientFsproj  = "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj"

    #if DEBUG
    let inspectorPath = "../Fsharp.Editing.ProjectInspector/bin/Debug/Fsharp.Editing.ProjectInspector.exe"
    let workDir = Path.GetFullPath "../Fsharp.Editing.ProjectInspector/bin/Debug/"
    #else
    let inspectorPath = "../Fsharp.Editing.ProjectInspector/bin/Release/Fsharp.Editing.ProjectInspector.exe"
    let workDir = Path.GetFullPath "../Fsharp.Editing.ProjectInspector/bin/Release/"
    #endif

    let fsprojPath = Path.GetFullPath clientFsproj
    let inspectorTool = Path.GetFullPath inspectorPath
    printfn "%s" fsprojPath
    printfn "%s" inspectorTool

    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- inspectorTool
    p.StartInfo.Arguments <-  fsprojPath
    p.StartInfo.WorkingDirectory <- workDir
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.CreateNoWindow <- true
    p.StartInfo.RedirectStandardOutput <- true

    printfn "Starting msbuild project evaluation\n"

    ignore <| p.Start()
    

    let json = p.StandardOutput.ReadToEnd()
    let info = JsonConvert.DeserializeObject<ProjectFileInfo>(json)

    let fsls = FSharp.Editing.LanguageService()
    let projinfo = FSharp.Editing.ProjectSystem.ProjectFileInfo.toProjectInfo fsls.Workspace info
    let proj = fsls.Workspace.AddProject projinfo

    fsls.Workspace.CurrentSolution.Projects
    |> Seq.iter (fun x -> printfn "%s - %s" x.Name x.AssemblyName)
    p.WaitForExit()
    Console.ReadLine()|> ignore

    0 // return an integer exit code
