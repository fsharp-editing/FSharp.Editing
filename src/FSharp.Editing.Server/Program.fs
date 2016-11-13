open System
open System.IO
open AustinHarris.JsonRpc
open FSharp.Editing.Server

module SocketListener =
    open System.Net.Sockets
    open System.Text
    open System.Net

    let start (port: int) (handleRequest: StreamWriter -> string -> unit) =
        let server = new TcpListener(IPAddress.Parse "127.0.0.1", port)
        server.Start()
        printfn "Listening on 127.0.0.1:%d" port
        while true do
            try
                use client = server.AcceptTcpClient()
                use stream = client.GetStream()
                printfn "Client Connected."
                use reader = new StreamReader(stream, Encoding.UTF8)
                use writer = new StreamWriter(stream, UTF8Encoding false)

                while not reader.EndOfStream do
                    let line = reader.ReadLine()
                    handleRequest writer line
                    printfn "REQUEST: %s" line
            with e -> printfn "RPCServer exception %O" e
        
[<EntryPoint>]
let main _  =
    let _service = Service()
    let rpcResultHandler = AsyncCallback(fun state ->
        let async = state :?> JsonRpcStateAsync
        let result = async.Result
        let writer = async.AsyncState :?> StreamWriter

        writer.WriteLine result
        writer.FlushAsync() |> ignore)

    SocketListener.start 32009 <| fun writer line ->
        let async = JsonRpcStateAsync(rpcResultHandler, writer, JsonRpc = line)
        JsonRpcProcessor.Process(async, writer)
    0