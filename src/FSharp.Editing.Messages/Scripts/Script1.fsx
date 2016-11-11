#load "load-project-debug.fsx"

open Newtonsoft.Json
open System.IO
open FSharp.Editing.Messages
open FSharp.Editing.Messages.Serialization

let msg: RequestMessage = 
    { Id = 25
      Request = 
        Request.ShowMessage
          { Type = MessageType.Info
            Message = "a message"
            Actions = 
              [ { Title = "a title 1" }
                { Title = "a title 2" } ] }}

let s = JsonSerializer()
s.Formatting <- Formatting.Indented
let w = new StringWriter()

s.Serialize (w, msg)
w.ToString()