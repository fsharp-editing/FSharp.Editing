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

    let deserialize (json: string) : RequestWithId =
        use reader = new StringReader(json)
        let msg = serializer.Deserialize(reader, typeof<RequestMessage>) :?> RequestMessage
        let request =
            match msg.Method with
            | Method.Initialize               -> Request.Initialize               (msg.Params :?> InitializeParams)
            | Method.Shutdown                 -> Request.Shutdown
            | Method.ShowMessage              -> Request.ShowMessage              (msg.Params :?> ShowMessageRequestParams)
            | Method.Completion               -> Request.Completion               (msg.Params :?> TextDocumentPositionParams)
            | Method.CompletionItemResolve    -> Request.CompletionItemResolve    (msg.Params :?> CompletionItem)
            | Method.Hover                    -> Request.Hover                    (msg.Params :?> TextDocumentPositionParams)
            | Method.SignatureHelp            -> Request.SignatureHelp            (msg.Params :?> TextDocumentPositionParams)
            | Method.GotoDefinition           -> Request.GotoDefinition           (msg.Params :?> TextDocumentPositionParams)
            | Method.FindReferences           -> Request.FindReferences           (msg.Params :?> ReferenceParams)
            | Method.DocumentHighlights       -> Request.DocumentHighlights       (msg.Params :?> TextDocumentPositionParams)
            | Method.DocumentSymbols          -> Request.DocumentSymbols          (msg.Params :?> DocumentSymbolsParams)
            | Method.WorkspaceSymbols         -> Request.WorkspaceSymbols         (msg.Params :?> WorkspaceSymbolsParams)
            | Method.CodeAction               -> Request.CodeAction               (msg.Params :?> CodeActionParams)
            | Method.CodeLens                 -> Request.CodeLens                 (msg.Params :?> CodeLensParams)
            | Method.CodeLensResolve          -> Request.CodeLensResolve          (msg.Params :?> CodeLens)
            | Method.DocumentFormatting       -> Request.DocumentFormatting       (msg.Params :?> DocumentFormattingParams)
            | Method.DocumentRangeFormatting  -> Request.DocumentRangeFormatting  (msg.Params :?> DocumentRangeFormattingParams)
            | Method.DocumentOnTypeFormatting -> Request.DocumentOnTypeFormatting (msg.Params :?> DocumentOnTypeFormattingParams)
            | Method.Rename                   -> Request.Rename                   (msg.Params :?> RenameParams)
            | _ -> failwithf "Unsupported request method '%s'" msg.Method


        { Id = msg.Id
          Request = request }

//        w.WriteStartObject()
//        w.WritePropertyName "jsonrpc"
//        w.WriteValue "2.0"
//        w.WritePropertyName "method"
//        w.WriteValue ``method``
//        match parameters with
//        | Some x ->
//            w.WritePropertyName "params"
//            serializer.Serialize (w, x)
//        | None -> ()
//        w.WriteEndObject()

module Response =
    let serialize (response: ResponseWithId) : string = ""

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
