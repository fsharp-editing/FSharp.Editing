#load "load-project-debug.fsx"

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
    
let json = Serializer.serialize msg

let msg1 = Serializer.deserialize json
//Serializer.serialize (Message.Request { Id = 24; Request = Request.Shutdown })