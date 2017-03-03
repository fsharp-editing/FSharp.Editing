open System
open System.Threading
open Microsoft.CodeAnalysis
open System.IO
open FSharp.Editing
open FSharp.Editing.ProjectSystem
open Wire
open Newtonsoft.Json


type OutputReader (sr:StreamReader) =
    let mutable text = ""
    member __.Exec () = 
        text <- sr.ReadToEnd()
    member __.Text = text


[<EntryPoint>]
let main argv = 


    System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

    let inspectorPath = "../Fsharp.Editing.ProjectInspector/bin/Debug/Fsharp.Editing.ProjectInspector.exe"
    let clientFsproj  = "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj"
    let workDir = Path.GetFullPath "../Fsharp.Editing.ProjectInspector/bin/Debug/"
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
//    p.StartInfo.RedirectStandardInput <- true
    printfn "Starting msbuild project evaluation"

//    let outputReader = OutputReader p.StandardOutput
//    let std_out = new Thread(ParameterizedThreadStart(fun _ -> outputReader.Exec()))
//    std_out.Start()
//    std_out.Join()
//    p.WaitForExit()
    ignore <| p.Start()
    
//    p.OutputDataReceived.Subscribe(fun output -> 
//        

//        
//            
//    )
    let json = p.StandardOutput.ReadToEnd()
    printfn "%A" json
////    let json = outputReader.Text
//    p.WaitForExit()
//    p.Close()

//        let reader = new JsonTextReader(new StringReader(output.Data))
//        let info  = JsonSerializer().Deserialize<ProjectFileInfo>(reader)

    
//    use reader = new StreamReader(Console.r OpenStandardOutput ())
//    use jsonReader = new JsonTextReader(reader)      
//    let ser = new JsonSerializer()
//    let info = ser.Deserialize<ProjectFileInfo>(jsonReader)


//    let json = p.StandardOutput.ReadToEnd()
//    
//    p.WaitForExit()
//    printfn "%A" json
    let info = JsonConvert.DeserializeObject<ProjectFileInfo>(json)
//    
//    



//
//    let reader = new JsonTextReader(new StringReader(json))
//    let info  = JsonSerializer().Deserialize<ProjectFileInfo>(reader)
////    let ser = Serializer()
////    let info = ser.Deserialize<ProjectFileInfo>(p.StandardOutput.BaseStream)
////
//    let ser = new System.Runtime.Serialization.Json.DataContractJsonSerializer(typeof<ProjectFileInfo>)
//    let info = ser.ReadObject(p.StandardOutput.BaseStream) :?> ProjectFileInfo
//    //let fsproj = ProjectFileInfo.create "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj"
//    //let fsproj = ProjectFileInfo.create "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj"
//    printfn "%A" info
//

//    printfn "%A\n\n" info

    let fsls = FSharp.Editing.LanguageService()
    let projinfo = FSharp.Editing.ProjectSystem.ProjectFileInfo.toProjectInfo fsls.Workspace info
    let proj = fsls.Workspace.AddProject projinfo

    fsls.Workspace.CurrentSolution.Projects
    |> Seq.iter (fun x -> printfn "%s - %s" x.Name x.AssemblyName)
    p.WaitForExit()
    Console.ReadLine()|> ignore

    0 // return an integer exit code
