module FSharp.Editing.Messages.Serialization

open Newtonsoft.Json
open System.IO
open Newtonsoft.Json.Linq

let private ($) = (<|)

let private serializer = 
    JsonSerializer(
        Formatting = Formatting.Indented, 
        DefaultValueHandling = DefaultValueHandling.Ignore)

module Request =
    [<RequireQualifiedAccess>]
    module private Method =
        let [<Literal>] Initialize               = "initialize"
        let [<Literal>] Shutdown                 = "shutdown"
        let [<Literal>] ShowMessage              = "window/showMessageRequest"
        let [<Literal>] Completion               = "textDocument/completion"
        let [<Literal>] CompletionItemResolve    = "completionItem/resolve"
        let [<Literal>] Hover                    = "textDocument/hover"
        let [<Literal>] SignatureHelp            = "textDocument/signatureHelp"
        let [<Literal>] GotoDefinition           = "textDocument/definition"
        let [<Literal>] FindReferences           = "textDocument/references"
        let [<Literal>] DocumentHighlights       = "textDocument/documentHighlight"
        let [<Literal>] DocumentSymbols          = "textDocument/documentSymbol"
        let [<Literal>] WorkspaceSymbols         = "workspace/symbol"
        let [<Literal>] CodeAction               = "textDocument/codeAction"
        let [<Literal>] CodeLens                 = "textDocument/codeLens"
        let [<Literal>] CodeLensResolve          = "codeLens/resolve"
        let [<Literal>] DocumentFormatting       = "textDocument/formatting"
        let [<Literal>] DocumentRangeFormatting  = "textDocument/rangeFormatting"
        let [<Literal>] DocumentOnTypeFormatting = "textDocument/onTypeFormatting"
        let [<Literal>] Rename                   = "textDocument/rename"

    let (|RequestMethod|_|) = function
        | Method.Initialize              
        | Method.Shutdown                
        | Method.ShowMessage             
        | Method.Completion              
        | Method.CompletionItemResolve   
        | Method.Hover                   
        | Method.SignatureHelp           
        | Method.GotoDefinition          
        | Method.FindReferences          
        | Method.DocumentHighlights      
        | Method.DocumentSymbols         
        | Method.WorkspaceSymbols        
        | Method.CodeAction              
        | Method.CodeLens                
        | Method.CodeLensResolve         
        | Method.DocumentFormatting      
        | Method.DocumentRangeFormatting 
        | Method.DocumentOnTypeFormatting
        | Method.Rename -> Some()
        | _ -> None

    let serialize (request: RequestWithId) : string = 
        let ``method``, parameters =
            match request.Request with
            | Request.Initialize x               -> Method.Initialize,               box x
            | Request.Shutdown                   -> Method.Shutdown,                 null
            | Request.ShowMessage x              -> Method.ShowMessage,              box x
            | Request.Completion x               -> Method.Completion,               box x
            | Request.CompletionItemResolve x    -> Method.CompletionItemResolve,    box x
            | Request.Hover x                    -> Method.Hover,                    box x
            | Request.SignatureHelp x            -> Method.SignatureHelp,            box x
            | Request.GotoDefinition x           -> Method.GotoDefinition,           box x
            | Request.FindReferences x           -> Method.FindReferences,           box x
            | Request.DocumentHighlights x       -> Method.DocumentHighlights,       box x
            | Request.DocumentSymbols x          -> Method.DocumentSymbols,          box x
            | Request.WorkspaceSymbols x         -> Method.WorkspaceSymbols,         box x
            | Request.CodeAction x               -> Method.CodeAction,               box x
            | Request.CodeLens x                 -> Method.CodeLens,                 box x
            | Request.CodeLensResolve x          -> Method.CodeLensResolve,          box x
            | Request.DocumentFormatting x       -> Method.DocumentFormatting,       box x
            | Request.DocumentRangeFormatting x  -> Method.DocumentRangeFormatting,  box x
            | Request.DocumentOnTypeFormatting x -> Method.DocumentOnTypeFormatting, box x
            | Request.Rename x                   -> Method.Rename,                   box x

        let requestMessage: RequestMessage =
            { Jsonrpc = "2.0"
              Id = request.Id
              Method = ``method``
              Params = parameters }

        use writer = new StringWriter()
        serializer.Serialize(writer, requestMessage)
        writer.ToString()
        
    let inline private fromJObject<'a>(parameters: obj): 'a = (parameters :?> JObject).ToObject<'a>()

    let deserialize (json: string) : RequestWithId =
        use reader = new StringReader(json)
        let msg = serializer.Deserialize(reader, typeof<RequestMessage>) :?> RequestMessage
        let request =
            match msg.Method with
            | Method.Initialize               -> Request.Initialize               $ fromJObject msg.Params
            | Method.Shutdown                 -> Request.Shutdown
            | Method.ShowMessage              -> Request.ShowMessage              $ fromJObject msg.Params
            | Method.Completion               -> Request.Completion               $ fromJObject msg.Params
            | Method.CompletionItemResolve    -> Request.CompletionItemResolve    $ fromJObject msg.Params
            | Method.Hover                    -> Request.Hover                    $ fromJObject msg.Params
            | Method.SignatureHelp            -> Request.SignatureHelp            $ fromJObject msg.Params
            | Method.GotoDefinition           -> Request.GotoDefinition           $ fromJObject msg.Params
            | Method.FindReferences           -> Request.FindReferences           $ fromJObject msg.Params
            | Method.DocumentHighlights       -> Request.DocumentHighlights       $ fromJObject msg.Params
            | Method.DocumentSymbols          -> Request.DocumentSymbols          $ fromJObject msg.Params
            | Method.WorkspaceSymbols         -> Request.WorkspaceSymbols         $ fromJObject msg.Params
            | Method.CodeAction               -> Request.CodeAction               $ fromJObject msg.Params
            | Method.CodeLens                 -> Request.CodeLens                 $ fromJObject msg.Params
            | Method.CodeLensResolve          -> Request.CodeLensResolve          $ fromJObject msg.Params
            | Method.DocumentFormatting       -> Request.DocumentFormatting       $ fromJObject msg.Params
            | Method.DocumentRangeFormatting  -> Request.DocumentRangeFormatting  $ fromJObject msg.Params
            | Method.DocumentOnTypeFormatting -> Request.DocumentOnTypeFormatting $ fromJObject msg.Params
            | Method.Rename                   -> Request.Rename                   $ fromJObject msg.Params
            | _ -> failwithf "Unsupported request method '%s'" msg.Method

        { Id = msg.Id
          Request = request }

module Response =
    let serialize (response: ResponseWithId) : string = 
        let result =
            match response.Response with
            | Some (Response.Initialize x)               -> box x
            | Some (Response.ShowMessage x)              -> box x
            | Some (Response.Completion x)               -> box x
            | Some (Response.CompletionItemResolve x)    -> box x
            | Some (Response.Hover x)                    -> box x
            | Some (Response.SignatureHelp x)            -> box x
            | Some (Response.GotoDefinition x)           -> box x
            | Some (Response.FindReferences x)           -> box x
            | Some (Response.DocumentHighlights x)       -> box x
            | Some (Response.DocumentSymbols x)          -> box x
            | Some (Response.WorkspaceSymbols x)         -> box x
            | Some (Response.CodeAction x)               -> box x
            | Some (Response.CodeLens x)                 -> box x
            | Some (Response.CodeLensResolve x)          -> box x
            | Some (Response.DocumentFormatting x)       -> box x
            | Some (Response.DocumentRangeFormatting x)  -> box x
            | Some (Response.DocumentOnTypeFormatting x) -> box x
            | Some (Response.Rename x)                   -> box x
            | None                                       -> null

        let requestMessage: ResponseMessage =
            { Jsonrpc = "2.0"
              Id = request.Id
              Result = parameters }

        use writer = new StringWriter()
        serializer.Serialize(writer, requestMessage)
        writer.ToString()
        
    let inline private fromJObject<'a>(parameters: obj): 'a = (parameters :?> JObject).ToObject<'a>()

    let deserialize (json: string) : RequestWithId =
        use reader = new StringReader(json)
        let msg = serializer.Deserialize(reader, typeof<RequestMessage>) :?> RequestMessage
        let request =
            match msg.Method with
            | Method.Initialize               -> Request.Initialize               $ fromJObject msg.Params
            | Method.Shutdown                 -> Request.Shutdown
            | Method.ShowMessage              -> Request.ShowMessage              $ fromJObject msg.Params
            | Method.Completion               -> Request.Completion               $ fromJObject msg.Params
            | Method.CompletionItemResolve    -> Request.CompletionItemResolve    $ fromJObject msg.Params
            | Method.Hover                    -> Request.Hover                    $ fromJObject msg.Params
            | Method.SignatureHelp            -> Request.SignatureHelp            $ fromJObject msg.Params
            | Method.GotoDefinition           -> Request.GotoDefinition           $ fromJObject msg.Params
            | Method.FindReferences           -> Request.FindReferences           $ fromJObject msg.Params
            | Method.DocumentHighlights       -> Request.DocumentHighlights       $ fromJObject msg.Params
            | Method.DocumentSymbols          -> Request.DocumentSymbols          $ fromJObject msg.Params
            | Method.WorkspaceSymbols         -> Request.WorkspaceSymbols         $ fromJObject msg.Params
            | Method.CodeAction               -> Request.CodeAction               $ fromJObject msg.Params
            | Method.CodeLens                 -> Request.CodeLens                 $ fromJObject msg.Params
            | Method.CodeLensResolve          -> Request.CodeLensResolve          $ fromJObject msg.Params
            | Method.DocumentFormatting       -> Request.DocumentFormatting       $ fromJObject msg.Params
            | Method.DocumentRangeFormatting  -> Request.DocumentRangeFormatting  $ fromJObject msg.Params
            | Method.DocumentOnTypeFormatting -> Request.DocumentOnTypeFormatting $ fromJObject msg.Params
            | Method.Rename                   -> Request.Rename                   $ fromJObject msg.Params
            | _ -> failwithf "Unsupported request method '%s'" msg.Method

        { Id = msg.Id
          Request = request }

module Notification =
    let serialize (notification: Notification) : string = ""

module Serializer =
    let serialize (message: Message) : string =
        match message with
        | Message.Request x -> Request.serialize x
        | Message.Response x -> Response.serialize x
        | Message.Notification x -> Notification.serialize x

    let deserialize (json: string): Message =
        let jsonObj = JObject.Parse json
        let ``method`` = jsonObj.["Method"].Value<string>()
        match ``method`` with
        | Request.RequestMethod -> Message.Request $ Request.deserialize json
        | _ -> failwithf "Unsupported method '%s'" $ ``method``

//        
//
//    override __.ReadJson (reader, objectType, existingValue, serializer) =
//        reader.Read()
//        let jsonObject = JObject.Load reader
//        let parametersReader = jsonObject.Property("params")
//        let parameters = serializer.Populate (reader)
//        let ``method`` =
//            match (JToken.op_Explicit $ jsonObject.Property("method").Value) : string with
//            | "initialize" -> serializer.Deserialize Request.Initialize x -> "initialize", box x
//            | Request.Shutdown -> "shutdown", None
//            | Request.ShowMessage x -> "window/showMessageRequest", box x
//            | Request.Completion x -> "textDocument/completion", box x
//            | Request.CompletionItemResolve x -> "completionItem/resolve", box x
//            | Request.Hover x -> "textDocument/hover", box x
//            | Request.SignatureHelp x -> "textDocument/signatureHelp", box x
//            | Request.GotoDefinition x -> "textDocument/definition", box x
//            | Request.FindReferences x -> "textDocument/references", box x
//            | Request.DocumentHighlights x -> "textDocument/documentHighlight", box x
//            | Request.DocumentSymbols x -> "textDocument/documentSymbol", box x
//            | Request.WorkspaceSymbols x -> "workspace/symbol", box x
//            | Request.CodeAction x -> "textDocument/codeAction", box x
//            | Request.CodeLens x -> "textDocument/codeLens", box x
//            | Request.CodeLensResolve x -> "codeLens/resolve", box x
//            | Request.DocumentFormatting x -> "textDocument/formatting", box x
//            | Request.DocumentRangeFormatting x -> "textDocument/rangeFormatting", box x
//            | Request.DocumentOnTypeFormatting x -> "textDocument/onTypeFormatting", box x
//            | Request.Rename x -> "textDocument/rename", box x
//
//    override __.CanConvert objectType = objectType = typeof<RequestMessage>
//        
//module Serialize =
//    open System.IO
//
//    let request (request: RequestMessage) =
//        use writer = new StringWriter()
//        serializer.Serialize (writer, request)
//        writer.ToString()
//        
//    let notification (id: int) (notification: Notification) =
//        let ``method`` =
//            match notification with
//            | Notification.Exit -> "exit"
//            | Notification.ShowMessage _ -> "window/showMessage"
//            | Notification.LogMessage _ -> "window/logMessage"
//            | Notification.Telemetry _ -> "telemetry/event"
//            | Notification.DidChangeConfiguration _ -> "workspace/didChangeConfiguration"
//            | Notification.DidOpenTextDocument _ -> "textDocument/didOpen"
//            | Notification.DidChangeTextDocument _ -> "textDocument/didChange"
//            | Notification.DidCloseTextDocument _ -> "textDocument/didClose"
//            | Notification.DidSaveTextDocument _ -> "textDocument/didSave"
//            | Notification.DidChangeWatchedFiles _ -> "workspace/didChangeWatchedFiles"
//            | Notification.PublishDiagnostics _ -> "textDocument/publishDiagnostics"
//        ()
