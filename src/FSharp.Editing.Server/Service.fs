namespace FSharp.Editing.Server

open FSharp.Editing.Messages

type Service() =
    inherit JsonRpcService() 

    [<JsonRpcMethod "window/showMessageRequest">]
    member __.ShowMessage(p: ShowMessageRequestParams) : MessageActionItem = 
        { Title = "a title" }

