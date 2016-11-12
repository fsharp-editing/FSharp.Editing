module FSharp.Editing.Messages.Serialization

open Newtonsoft.Json

let private ($) = (<|)

let private serializer = 
    JsonSerializer(
        Formatting = Formatting.Indented, 
        DefaultValueHandling = DefaultValueHandling.Ignore)

module Request =
    open System.IO

    let serialize (request: RequestWithId) : string = 
        let ``method``, parameters =
            match request.Request with
            | Request.Initialize x -> "initialize", box x
            | Request.Shutdown -> "shutdown", null
            | Request.ShowMessage x -> "window/showMessageRequest", box x
            | Request.Completion x -> "textDocument/completion", box x
            | Request.CompletionItemResolve x -> "completionItem/resolve", box x
            | Request.Hover x -> "textDocument/hover", box x
            | Request.SignatureHelp x -> "textDocument/signatureHelp", box x
            | Request.GotoDefinition x -> "textDocument/definition", box x
            | Request.FindReferences x -> "textDocument/references", box x
            | Request.DocumentHighlights x -> "textDocument/documentHighlight", box x
            | Request.DocumentSymbols x -> "textDocument/documentSymbol", box x
            | Request.WorkspaceSymbols x -> "workspace/symbol", box x
            | Request.CodeAction x -> "textDocument/codeAction", box x
            | Request.CodeLens x -> "textDocument/codeLens", box x
            | Request.CodeLensResolve x -> "codeLens/resolve", box x
            | Request.DocumentFormatting x -> "textDocument/formatting", box x
            | Request.DocumentRangeFormatting x -> "textDocument/rangeFormatting", box x
            | Request.DocumentOnTypeFormatting x -> "textDocument/onTypeFormatting", box x
            | Request.Rename x -> "textDocument/rename", box x

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
        let requestMessage = serializer.Deserialize(reader, typeof<RequestMessage>) :?> RequestMessage
        { Id = requestMessage.Id
          Request = Unchecked.defaultof<_> }

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
        match message with
        | Message.Request x -> Request.serialize x
        | Message.Response x -> Response.serialize x
        | Message.Notification x -> Notification.serialize x

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
