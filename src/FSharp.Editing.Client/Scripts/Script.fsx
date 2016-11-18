#load "load-project-debug.fsx"

open FSharp.Editing.Messages
open FSharp.Editing.Client
open System

let client = Client (Uri "http://127.0.0.1:8083")

{ Type = MessageType.Info
  Message = "foo"
  Actions = [] }
|> client.ShowMessage
|> Async.RunSynchronously