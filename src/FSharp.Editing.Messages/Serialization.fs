module FSharp.Editing.Messages.Serialization

open Newtonsoft.Json
open System
open Newtonsoft.Json.Linq

let private ($) = (<|)

/// A request message to describe a request between the client and the server. Every processed request must 
/// send a response back to the sender of the request.
[<NoComparison; JsonConverter(typeof<RequestConverter>)>]
type RequestMessage =
    { /// The request id.
      Id: int
      /// The method's params.
      Request: Request }

and RequestConverter() =
    inherit JsonConverter()

    override __.WriteJson (w, value, serializer) =
        let message = value :?> RequestMessage

        let ``method``, parameters =
            match message.Request with
            | Request.Initialize x -> "initialize", Some $ box x
            | Request.Shutdown -> "shutdown", None
            | Request.ShowMessage x -> "window/showMessageRequest", Some $ box x
            | Request.Completion x -> "textDocument/completion", Some $ box x
            | Request.CompletionItemResolve x -> "completionItem/resolve", Some $ box x
            | Request.Hover x -> "textDocument/hover", Some $ box x
            | Request.SignatureHelp x -> "textDocument/signatureHelp", Some $ box x
            | Request.GotoDefinition x -> "textDocument/definition", Some $ box x
            | Request.FindReferences x -> "textDocument/references", Some $ box x
            | Request.DocumentHighlights x -> "textDocument/documentHighlight", Some $ box x
            | Request.DocumentSymbols x -> "textDocument/documentSymbol", Some $ box x
            | Request.WorkspaceSymbols x -> "workspace/symbol", Some $ box x
            | Request.CodeAction x -> "textDocument/codeAction", Some $ box x
            | Request.CodeLens x -> "textDocument/codeLens", Some $ box x
            | Request.CodeLensResolve x -> "codeLens/resolve", Some $ box x
            | Request.DocumentFormatting x -> "textDocument/formatting", Some $ box x
            | Request.DocumentRangeFormatting x -> "textDocument/rangeFormatting", Some $ box x
            | Request.DocumentOnTypeFormatting x -> "textDocument/onTypeFormatting", Some $ box x
            | Request.Rename x -> "textDocument/rename", Some $ box x

        w.WriteStartObject()
        w.WritePropertyName "jsonrpc"
        w.WriteValue "2.0"
        w.WritePropertyName "method"
        w.WriteValue ``method``
        match parameters with
        | Some x ->
            w.WritePropertyName "params"
            serializer.Serialize (w, x)
        | None -> ()
        w.WriteEndObject()

    override __.ReadJson (reader, objectType, existingValue, serializer) =
        reader.Read()
        let jsonObject = JObject.Load reader
        let parametersReader = jsonObject.Property("params")
        let parameters = serializer.Populate (reader)
        let ``method`` =
            match (JToken.op_Explicit $ jsonObject.Property("method").Value) : string with
            | "initialize" -> serializer.Deserialize Request.Initialize x -> "initialize", Some $ box x
            | Request.Shutdown -> "shutdown", None
            | Request.ShowMessage x -> "window/showMessageRequest", Some $ box x
            | Request.Completion x -> "textDocument/completion", Some $ box x
            | Request.CompletionItemResolve x -> "completionItem/resolve", Some $ box x
            | Request.Hover x -> "textDocument/hover", Some $ box x
            | Request.SignatureHelp x -> "textDocument/signatureHelp", Some $ box x
            | Request.GotoDefinition x -> "textDocument/definition", Some $ box x
            | Request.FindReferences x -> "textDocument/references", Some $ box x
            | Request.DocumentHighlights x -> "textDocument/documentHighlight", Some $ box x
            | Request.DocumentSymbols x -> "textDocument/documentSymbol", Some $ box x
            | Request.WorkspaceSymbols x -> "workspace/symbol", Some $ box x
            | Request.CodeAction x -> "textDocument/codeAction", Some $ box x
            | Request.CodeLens x -> "textDocument/codeLens", Some $ box x
            | Request.CodeLensResolve x -> "codeLens/resolve", Some $ box x
            | Request.DocumentFormatting x -> "textDocument/formatting", Some $ box x
            | Request.DocumentRangeFormatting x -> "textDocument/rangeFormatting", Some $ box x
            | Request.DocumentOnTypeFormatting x -> "textDocument/onTypeFormatting", Some $ box x
            | Request.Rename x -> "textDocument/rename", Some $ box x

    override __.CanConvert objectType = objectType = typeof<RequestMessage>
        
let serializer = JsonSerializer(Formatting = Formatting.Indented)

module Serialize =
    open System.IO

    let request (request: RequestMessage) =
        use writer = new StringWriter()
        serializer.Serialize (writer, request)
        writer.ToString()
        
    let notification (id: int) (notification: Notification) =
        let ``method`` =
            match notification with
            | Notification.Exit -> "exit"
            | Notification.ShowMessage _ -> "window/showMessage"
            | Notification.LogMessage _ -> "window/logMessage"
            | Notification.Telemetry _ -> "telemetry/event"
            | Notification.DidChangeConfiguration _ -> "workspace/didChangeConfiguration"
            | Notification.DidOpenTextDocument _ -> "textDocument/didOpen"
            | Notification.DidChangeTextDocument _ -> "textDocument/didChange"
            | Notification.DidCloseTextDocument _ -> "textDocument/didClose"
            | Notification.DidSaveTextDocument _ -> "textDocument/didSave"
            | Notification.DidChangeWatchedFiles _ -> "workspace/didChangeWatchedFiles"
            | Notification.PublishDiagnostics _ -> "textDocument/publishDiagnostics"
        ()
