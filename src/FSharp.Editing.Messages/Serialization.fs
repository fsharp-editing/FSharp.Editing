namespace FSharp.Editing.Messages

module Serialize =
    let request (id: int) (request: Request) =
        let ``method`` =
            match request with
            | Request.Initialize _ -> "initialize"
            | Request.Shutdown -> "shutdown"
            | Request.ShowMessage _ -> "window/showMessageRequest"
            | Request.Completion _ -> "textDocument/completion"
            | Request.CompletionItemResolve _ -> "completionItem/resolve"
            | Request.Hover _ -> "textDocument/hover"
            | Request.SignatureHelp _ -> "textDocument/signatureHelp"
            | Request.GotoDefinition _ -> "textDocument/definition"
            | Request.FindReferences _ -> "textDocument/references"
            | Request.DocumentHighlights _ -> "textDocument/documentHighlight"
            | Request.DocumentSymbols _ -> "textDocument/documentSymbol"
            | Request.WorkspaceSymbols _ -> "workspace/symbol"
            | Request.CodeAction _ -> "textDocument/codeAction"
            | Request.CodeLens _ -> "textDocument/codeLens"
            | Request.CodeLensResolve _ -> "codeLens/resolve"
            | Request.DocumentFormatting _ -> "textDocument/formatting"
            | Request.DocumentRangeFormatting _ -> "textDocument/rangeFormatting"
            | Request.DocumentOnTypeFormatting _ -> "textDocument/onTypeFormatting"
            | Request.Rename _ -> "textDocument/rename"
        ()
        
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
