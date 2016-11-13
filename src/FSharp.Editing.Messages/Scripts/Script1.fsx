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
printfn "%s" json
let msg1 = Serializer.deserialize json

#time
let n = 500000
for i in 1..n do Serializer.serialize msg |> ignore
// Newtonsoft.Json (500000) Real: 00:00:02.011, CPU: 00:00:02.015, GC gen0: 356, gen1: 0, gen2: 0
for i in 1..n do Serializer.deserialize json |> ignore
// Newtonsoft.Json (500000) Real: 00:00:07.882, CPU: 00:00:07.890, GC gen0: 1854, gen1: 0, gen2: 0
