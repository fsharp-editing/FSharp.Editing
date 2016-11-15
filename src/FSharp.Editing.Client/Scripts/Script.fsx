#load "load-project-debug.fsx"

open FSharp.Editing.Client
open System

let client = JsonRpcClient(Uri("http://127.0.0.1:32009"))

client.Invoke("foo", box 1) |> Async.RunSynchronously