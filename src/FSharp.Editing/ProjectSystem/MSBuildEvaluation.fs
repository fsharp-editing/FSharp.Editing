module FSharp.Editing.ProjectSystem.MSBuildEvaluation

open System
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Security
open System.Security.AccessControl
open System.Threading
open Microsoft.Build.Evaluation
open Microsoft.Build.Execution
open Wire
open FSharp.Editing



//
//type NamedPipeServerStream with
//    member x.WaitForConnectionAsync() =
//        Async.FromBeginEnd(x.BeginWaitForConnection, x.EndWaitForConnection)

type NamedPipeClientStream with
    member self.ConnectionAsync () =
        let delConnect = Action self.Connect
        Async.FromBeginEnd( delConnect.BeginInvoke, delConnect.EndInvoke)

type Agent<'a> = MailboxProcessor<'a>
    
           
let byteArray( s:string ) =
    s.ToCharArray() |> Array.map byte

let genFileWriter( s:string ) =
    new System.IO.FileStream(s,
                    FileMode.OpenOrCreate, 
                    FileAccess.Write, 
                    FileShare.Read )

let writeToFileStream (fs:FileStream) ( s:string) =
    let ts = byteArray <| ( DateTime.Now.ToString() + ": " )
    fs.Write(ts,0,ts.Length)
    let data = byteArray s
    fs.Write(data,0,data.Length)
    let endl = byteArray System.Environment.NewLine
    fs.Write(endl,0,endl.Length)
    fs.Flush()


let doubleWrite (fs:FileStream) (s:string) =
    writeToFileStream fs s
    System.Console.WriteLine(s)


let server_to_bot  = "SERVER_TO_BOT"
let bot_to_server  = "BOT_TO_SERVER"


let client_in =
    new NamedPipeClientStream( ".",
        server_to_bot, 
        PipeDirection.In, 
        PipeOptions.WriteThrough ||| PipeOptions.Asynchronous  
    )
let client_out =
    new NamedPipeClientStream( ".",
        bot_to_server, 
        PipeDirection.Out, 
        PipeOptions.WriteThrough ||| PipeOptions.Asynchronous  
    )


let serializeToBinary (writeSession:SerializerSession) o = 
    use stream = new System.IO.MemoryStream()
    writeSession.Serializer.Serialize(o,stream)
    stream.ToArray()

let deserializeFromBinary<'t> (readSession:DeserializerSession )(bytes: byte array) =
    use stream = new System.IO.MemoryStream(bytes)
    readSession.Serializer. Deserialize<'t> stream


let _doubleWrite (fs:FileStream) (s:string) =
    writeToFileStream fs s
    System.Console.WriteLine(s)


//
//type Message = 
//    | Evaluate of string
//    | Status of string
//    | Options of ProjectFileInfo 
//    | Failure of string
//
//
//let MessageEvent = Event<Message>()

//
//type MSBuildPipe () =
//    let writeSession = SerializerSession ^ Serializer()
//    let readSession  = DeserializerSession ^ Serializer()
//
//    let serverPipe =
//        new NamedPipeServerStream 
//            (   server_to_bot           // name of the pipe,
//            ,   PipeDirection.InOut     // diretcion of the pipe
//            ,   1                       // max number of server instances
//            ,   PipeTransmissionMode.Message // Transmissione Mode
//            ,   PipeOptions.WriteThrough ||| PipeOptions.Asynchronous      // the operation will not return the control untill the write is completed
//        )
//
//    let clientPipe =
//        new NamedPipeClientStream
//            (   "."
//            ,   server_to_bot
//            ,   PipeDirection.InOut
//            ,   PipeOptions.WriteThrough ||| PipeOptions.Asynchronous  
//        )
//
//
////    let ps = new PipeSecurity()
////    do ps.AddAccessRule(new PipeAccessRule("Users", PipeAccessRights.ReadWrite, AccessControlType.Allow))
//
//
//
//    let readingMessages bytes = 
//        let bufferResizable = ResizeArray<byte>()                                            
//        let rec readingMessage (buffer: byte array) = async {
//            let! bytesRead = serverPipe.AsyncRead(buffer, 0, buffer.Length)
//            // add the bytes read to a "Resizable" collection 
//            bufferResizable.AddRange(buffer.[0..bytesRead])
//                  
//            if serverPipe.IsMessageComplete then 
//                // if the message is completed fire OnMessageReceived event
//                // including the total bytes part of the message
//                let message = bufferResizable.ToArray() 
//                // clear the resizable collection to be ready for the next income messagw
//                bufferResizable.Clear()
//                let msg = deserializeFromBinary<Message> readSession message
//                MessageEvent.Trigger msg
//                do! readingMessage buffer
//            else
//                // the message is not completed, keep reading
//                do! readingMessage buffer 
//        }
//        readingMessage bytes 
//
//
//
//    member x.Write (message:Message) =
//        if clientPipe.IsConnected && clientPipe.CanWrite then
//            async {
//                let msg = serializeToBinary writeSession message
//                do! clientPipe.AsyncWrite (msg,0, msg.Length)
//                do! clientPipe.FlushAsync() |> Async.AwaitTaskCorrect
//                clientPipe.WaitForPipeDrain() 
//            } |> Async.Start 
//
//    member x.InitClient () =
//        async {
//            
//        
//        
//        }
//        Async.SwitchToNewThread
//
//    member  x.Eval (path:string) =
//        let startInfo = ProcessStartInfo (clientPath, senderID + " " + receiverID)
//        startInfo.UseShellExecute <- false
//        startInfo
//        //let clientProcess = Process.Start(startInfo)
// 
//        
//
//    member x.StartListening () =
//        if not ^ serverPipe.IsConnected then
//            let startListening = async {
//                do! (serverPipe.WaitForConnectionAsync ()|> Async.AwaitTaskCorrect)
//                MessageEvent.Trigger ^ Status "Server Pipe has connected"
//                do! readingMessages ^ Array.zeroCreate 0x1000
//            } 
//            startListening |> Async.Start

////    member __.Listen () =  
//        async { 
//            let cnt = agentIn.CurrentQueueLength
//            for _x = 0 to cnt-1 do
//                let! msg = agentIn.Receive()
//                let msg = deserializeFromBinary<Message> readSession msg
//                match msg with
//                | Status txt   -> printfn "%s: %s" bot_to_server txt 
//                | Evaluate txt -> printfn "server should not get this message from client" 
//                | Options opts -> printfn "server got project options from the client\n%A" opts
//                | Failure txt -> printfn "msbuild evaluation failed %s txt" txt
//        } |> Async.Start
//
//    member __.Post(msg:Message) = 
//        
//        let msg = serializeToBinary writeSession msg
//        agentOut.Post msg
//
//   let agentIn  = Agent.Start ^ fun inbox ->
//        let rec loop () = async{ 
//            try
//                let! (msg:byte []) = inbox.Receive()
//                let msg = deserializeFromBinary<Message> readSession msg
//                MessageEvent.Trigger msg
//            with
//            | e -> printfn "Failed to read from client -\n%s" e.Message 
//            return! loop()
//            } 
//        loop()
//
//    let agentOut = Agent.Start(fun inbox ->
//        let rec loop() = async{ 
//            try
//                let! msg =  inbox.Receive() 
//                let msg = serializeToBinary writeSession msg
//                //sprintf "Bytes for pipes -\n%A" charr |> write
//                do! serverPipe.AsyncWrite msg
//                serverPipe.Flush()
//                serverPipe.WaitForPipeDrain()
////                    bw.Write(barr)
////                    server_out.WaitForPipeDrain()
////                    bw.Flush()
//            with
//            | e -> printfn "Failed to post to client -\n%s"  e.Message
//            return! loop()
//            } 
//        loop()
//        )
//
//
//    member __.Status() =
//        printfn "ServerIn   Connected = %A\n\
//                            Can Read  = %A\n\
//                            Can Write = %A"     server_in.IsConnected server_in.CanRead server_in.CanWrite
//        printfn "ServerOut  Connected = %A\n\
//                            Can Read  = %A\n\
//                            Can Write = %A"     server_out.IsConnected server_out.CanRead server_out.CanWrite
