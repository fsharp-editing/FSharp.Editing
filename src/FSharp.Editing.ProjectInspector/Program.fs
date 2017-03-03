module FSharp.Editing.ProjectInspector.Program
open System
open System.IO
open Wire
open System.Runtime.Serialization.Json
open Microsoft.CodeAnalysis
open Newtonsoft.Json
open FSharp.Editing
open FSharp.Editing.ProjectSystem


[<EntryPoint>]
let main argv = 
//    System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

    let target = 
        if argv = [||] ||  String.IsNullOrWhiteSpace argv.[0] then 
            Path.Combine(__SOURCE_DIRECTORY__, "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj")
        else argv.[0]
    if not (String.IsNullOrWhiteSpace target) then
        let fileInfo = FSharp.Editing.ProjectSystem.ProfectFileInfo.create target

//        Console.WriteLine "Hello from the child"
//        printfn "%A" fileInfo
//        Console.ReadLine()|>ignore
//        let writer = Serializer()
//        writer.Serialize(fileInfo,Console.OpenStandardOutput())
        use writer = new StreamWriter(Console.OpenStandardOutput())
        use jsonWriter = new JsonTextWriter(writer)      
        let ser = new JsonSerializer()
        ser.Serialize(jsonWriter,fileInfo)
        jsonWriter.Flush()
//        let json = JsonConvert.SerializeObject(fileInfo)
//        use writer = new StreamWriter( Console.OpenStandardOutput() )
//        writer.WriteLine json
//        writer.Close()
//        Console.Write json
//        let ser = new DataContractJsonSerializer(typeof<ProjectFileInfo>)
//        ser.WriteObject(Console.OpenStandardOutput(), fileInfo)
        printfn "%A" fileInfo
        0
    else
        1


