#load "load-project-debug.fsx"

open Newtonsoft.Json
open System.IO
open FSharp.Editing.Messages
open FSharp.Editing.Messages.Serialization

let msg: Message =
    Message.Request
        { Id = 25
          Request = 
            Request.ShowMessage
              { Type = MessageType.Info
                Message = "a message"
                Actions = 
                  [ { Title = "a title 1" }
                    { Title = "a title 2" } ] }}
    
Serializer.serialize msg
