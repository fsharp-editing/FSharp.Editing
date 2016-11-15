namespace FSharp.Editing.Messages

/// Position in a text document expressed as zero-based line and character offset. 
/// A position is between two characters like an 'insert' cursor in a editor.
type Position =
    { /// Line position in a document (zero-based).
      Line: int 
      /// Character offset on a line in a document (zero-based).
      Character: int }

/// A range in a text document expressed as (zero-based) start and end positions. 
/// A range is comparable to a selection in an editor. Therefore the end position is exclusive.
type Range =
    { /// The range's start position.
      Start: Position
      /// The range's end position.
      End: Position }

/// Represents a location inside a resource, such as a line inside a text file.
type Location =
    { Uri: string 
      Range: Range }

// todo: there is no such a type in the spec, but we must return Location | Location [] and
// this is the best I've come with. But it must be serialized in a special way...
type Locations = 
    Locations of Location list

type DiagnosticSeverity =
    | Error = 1
    | Warning = 2
    | Information = 3
    | Hint = 4

/// Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid 
/// in the scope of a resource.
type Diagnostic = 
    { /// The range at which the message applies.
      Range: Range
      /// The diagnostic's severity. Can be omitted. If omitted it is up to the
      /// client to interpret diagnostics as error, warning, info or hint.
      Severity: DiagnosticSeverity option
      /// The diagnostic's code. Can be omitted.
      Code: int option
      /// A human-readable string describing the source of this
      /// diagnostic, e.g. 'typescript' or 'super lint'.
      Source: string option
      /// The diagnostic's message.
      Message: string }

/// Represents a reference to a command. Provides a title which will be used to represent a command in the UI. 
/// Commands are identitifed using a string identifier and the protocol currently doesn't specify a set of well 
/// known commands. So executing a command requires some tool extension code.
[<NoComparison>]
type Command =
    { /// Title of the command, like `save`.
      Title: string
      /// The identifier of the actual command handler.
      Command: string
      /// Arguments that the command handler should be invoked with.
      Arguments: obj list }

/// A textual edit applicable to a text document.
type TextEdit =
    { /// The range of the text document to be manipulated. To insert
      /// text into a document create a range where start = end.
      Range: Range
      /// The string to be inserted. For delete operations use an empty string.
      NewText: string }

type FileChange = string * TextEdit list

/// A workspace edit represents changes to many resources managed in the workspace.
type WorkspaceEdit =
    { /// Holds changes to existing resources.
      Changes: FileChange list }

type TextDocumentIdentifier =
    { /// The text document's URI.
      Uri: string }

/// An item to transfer a text document from the client to the server.
type TextDocumentItem =
    { /// The text document's URI.
      Uri: string
      /// The text document's language identifier.
      LanguageId: string
      /// The version number of this document (it will strictly increase after each change, including undo/redo).
      Version: int
      /// The content of the opened text document.
      Text: string }

/// An identifier to denote a specific version of a text document.
type VersionedTextDocumentIdentifier =
    { /// The text document's URI.
      Uri: string
      /// The version number of this document.
      Version: int }

/// A parameter literal used in requests to pass a text document and a position inside that document.
type TextDocumentPositionParams =
    { /// The text document.
      TextDocument: TextDocumentIdentifier
      /// The position inside the text document.
      Position: Position }

type MessageType =
    | Error = 1
    | Warning = 2
    | Info = 3
    | Log = 4

/// ClientCapabilities are currently empty.
type ClientCapabilities = ClientCapabilities

[<NoComparison>]
type InitializeParams =
    { /// The process Id of the parent process that started
      /// the server. Is null if the process has not been started by another process.
      /// If the parent process is not alive then the server should exit (see exit notification) its process.
      ProcessId: int option
      /// The rootPath of the workspace. Is null if no folder is open.
      RootPath: string option
      /// User provided initialization options.
      InitializationOptions: obj option
      /// The capabilities provided by the client (editor)
      Capabilities: ClientCapabilities }

/// error.data
type InitializeError =
    { /// Indicates whether the client should retry to send the
      /// initilize request after showing the message provided in the ResponseError.
      Retry: bool }

/// Defines how the host (editor) should sync document changes to the language server.
type TextDocumentSyncKind =
      /// Documents should not be synced at all.
    | None = 0
      /// Documents are synced by always sending the full content of the document.
    | Full = 1
      /// Documents are synced by sending the full content on open. After that only incremental 
      /// updates to the document are sent.
    | Incremental = 2

/// Completion options.
type CompletionOptions =
    { /// The server provides support to resolve additional information for a completion item.
      ResolveProvider: bool option
      /// The characters that trigger completion automatically.
      TriggerCharacters: string list }

/// Signature help options.
type SignatureHelpOptions =
    { /// The characters that trigger signature help automatically.
      TriggerCharacters: string list }

/// Code Lens options.
type CodeLensOptions =
    { /// Code lens has a resolve provider as well.
      ResolveProvider: bool option }

/// Format document on type options
type DocumentOnTypeFormattingOptions =
    { /// A character on which formatting should be triggered, like `}`.
      FirstTriggerCharacter: string
      /// More trigger characters.
      MoreTriggerCharacter: string list }

type ServerCapabilities =
    { /// Defines how text documents are synced.
      TextDocumentSync: int option
      /// The server provides hover support.
      HoverProvider: bool option
      /// The server provides completion support.
      CompletionProvider: CompletionOptions option
      /// The server provides signature help support.
      SignatureHelpProvider: SignatureHelpOptions option
      /// The server provides goto definition support.
      DefinitionProvider: bool option
      ///The server provides find references support.
      ReferencesProvider: bool option
      /// The server provides document highlight support.
      DocumentHighlightProvider: bool option
      /// The server provides document symbol support.
      DocumentSymbolProvider: bool option
      /// The server provides workspace symbol support.
      WorkspaceSymbolProvider: bool option
      /// The server provides code actions.
      CodeActionProvider: bool option
      /// The server provides code lens.
      CodeLensProvider: CodeLensOptions option
      /// The server provides document formatting.
      DocumentFormattingProvider: bool option
      /// The server provides document range formatting.
      CocumentRangeFormattingProvider: bool option
      /// The server provides document formatting on typing.
      DocumentOnTypeFormattingProvider: DocumentOnTypeFormattingOptions option
      /// The server provides rename support.
      RenameProvider: bool option }

type InitializeResult =
    { /// The capabilities the language server provides.
      Capabilities: ServerCapabilities }

type ShowMessageParams = 
    { /// The message type.
      Type: MessageType
      /// The actual message.
      Message: string }

type MessageActionItem =
    { /// A short title like 'Retry', 'Open Log' etc.
      Title: string }

type ShowMessageRequestParams = 
    { /// The message type.
      Type: MessageType
      /// The actual message
      Message: string
      /// The message action items to present.
      Actions: MessageActionItem list }

type LogMessageParams = 
    { /// The message type.
      Type: MessageType
      /// The actual message
      Message: string }

[<NoComparison>]
type DidChangeConfigurationParams =
    { /// The actual changed settings
      Settings: obj }

type DidOpenTextDocumentParams = 
    { /// The document that was opened.
      TextDocument: TextDocumentItem }

/// An event describing a change to a text document. If range and rangeLength are omitted
/// the new text is considered to be the full content of the document.
type TextDocumentContentChangeEvent =
    { /// The range of the document that changed.
      Range: Range option
      /// The length of the range that got replaced.
      RangeLength: int option
      /// The new text of the document.
      Ttext: string }

// params: DidChangeTextDocumentParams defined as follows:

type DidChangeTextDocumentParams =
    { /// The document that did change. The version number points
      /// to the version after all provided content changes have been applied.
      TextDocument: VersionedTextDocumentIdentifier
      /// The actual content changes.
      ContentChanges: TextDocumentContentChangeEvent list }

type DidCloseTextDocumentParams =
    { /// The document that was closed.
      TextDocument: TextDocumentIdentifier }

type DidSaveTextDocumentParams = 
    { /// The document that was saved.
      TextDocument: TextDocumentIdentifier }

/// The file event type.
type FileChangeType =
      /// The file got created.
    | Created = 1
      /// The file got changed.
    | Changed = 2
      /// The file got deleted.
    | Deleted = 3

/// An event describing a file change.
type FileEvent =
    { /// The file's URI.
      Uri: string
      /// The change type.
      Type: FileChangeType }

type DidChangeWatchedFilesParams =
    { /// The actual file events.
      Changes: FileEvent list }

type PublishDiagnosticsParams =
    { /// The URI for which diagnostic information is reported.
      Uri: string
      /// An array of diagnostic information items.
      Diagnostics: Diagnostic list }

/// The kind of a completion entry.
type CompletionItemKind =
    | Text = 1
    | Method = 2
    | Function = 3
    | Constructor = 4
    | Field = 5
    | Variable = 6
    | Class = 7
    | Interface = 8
    | Module = 9
    | Property = 10
    | Unit = 11
    | Value = 12
    | Enum = 13
    | Keyword = 14
    | Snippet = 15
    | Color = 16
    | File = 17
    | Reference = 18

[<NoComparison>]
type CompletionItem =
    { /// The label of this completion item. By default also the text that is inserted when selecting this completion.
      Label: string
      /// The kind of this completion item. Based of the kind an icon is chosen by the editor.
      Kind: CompletionItemKind option
      /// A human-readable string with additional information about this item, like type or symbol information.
      Detail: string option
      /// A human-readable string that represents a doc-comment.
      Documentation: string option
      /// A string that shoud be used when comparing this item with other items. When `falsy` the label is used.
      SortText: string option
      /// A string that should be used when filtering a set of completion items. When `falsy` the label is used.
      FilterText: string option
      /// A string that should be inserted a document when selecting this completion. When `falsy` the label is used.
      InsertText: string option
      /// An edit which is applied to a document when selecting this completion. When an edit is provided the value of
      // insertText is ignored.
      TextEdit: TextEdit option
      /// An optional array of additional text edits that are applied when
      /// selecting this completion. Edits must not overlap with the main edit nor with themselves.
      AdditionalTextEdits: TextEdit list
      /// An optional command that is executed *after* inserting this completion. *Note* that
      /// additional modifications to the current document should be described with the additionalTextEdits-property.
      Command: Command option
      /// An data entry field that is preserved on a completion item between a completion and a completion resolve request.
      Data: obj option }

/// Represents a collection of [completion items](#CompletionItem) to be presented in the editor.
[<NoComparison>]
type CompletionList = 
    { /// This list it not complete. Further typing should result in recomputing this list.
      IsIncomplete: bool
      /// The completion items.
      Items: CompletionItem list }

/// The marked string is rendered:
/// - as markdown if it is represented as a string
/// - as code block of the given langauge if it is represented as a pair of a language and a value
///
/// The pair of a language and a value is an equivalent to markdown:
/// ```${language}
/// ${value}
/// ```
type MarkedString = 
    { Language: string
      Value: string }

/// The result of a hover request.
type Hover = 
    { /// The hover's content
      Contents: MarkedString list
      /// An optional range is a range inside a text document 
      /// that is used to visualize a hover, e.g. by changing the background color.
      Range: Range option }

    // params: TextDocumentPositionParams
    // result: SignatureHelp defined as follows:
    
    /// Represents a parameter of a callable-signature. A parameter can have a label and a doc-comment.
    type ParameterInformation =
        { /// The label of this parameter. Will be shown in the UI.
          Label: string
          /// The human-readable doc-comment of this parameter. Will be shown in the UI but can be omitted.
          Documentation: string option }

    /// Represents the signature of something callable. 
    /// A signature can have a label, like a function-name, a doc-comment, and a set of parameters.
    type SignatureInformation =
        { /// The label of this signature. Will be shown in the UI.
          Label: string
          /// The human-readable doc-comment of this signature. Will be shown in the UI but can be omitted.
          Documentation: string option
          /// The parameters of this signature.
          Parameters: ParameterInformation list }

    /// Signature help represents the signature of something callable. There can be multiple 
    /// signature but only one active and only one active parameter.
    type SignatureHelp =
        { /// One or more signatures.
          Signatures: SignatureInformation list
          /// The active signature.
          ActiveSignature: int option
          /// The active parameter of the active signature.
          ActiveParameter: int }
    
type ReferenceContext =
    { /// Include the declaration of the current symbol.
      IncludeDeclaration: bool }

type ReferenceParams =
    { /// The text document.
      TextDocument: TextDocumentIdentifier
      /// The position inside the text document.
      Position: Position
      Context: ReferenceContext }
    
/// A document highlight kind.
type DocumentHighlightKind =
     /// A textual occurrance.
    | Text = 1
     /// Read-access of a symbol, like reading a variable.
    | Read = 2
     /// Write-access of a symbol, like writing to a variable.
    | Write = 3

/// A document highlight is a range inside a text document which deserves special attention. 
/// Usually a document highlight is visualized by changing the background color of its range.
type DocumentHighlight =
    { /// The range this highlight applies to.
      Range: Range
      /// The highlight kind, default is DocumentHighlightKind.Text.
      Kind: DocumentHighlightKind option }

/// A symbol kind.
type SymbolKind =
    | File = 1
    | Module = 2
    | Namespace = 3
    | Package = 4
    | Class = 5
    | Method = 6
    | Property = 7
    | Field = 8
    | Constructor = 9
    | Enum = 10
    | Interface = 11
    | Function = 12
    | Variable = 13
    | Constant = 14
    | String = 15
    | Number = 16
    | Boolean = 17
    | Array = 18

/// Represents information about programming constructs like variables, classes, interfaces etc.
type SymbolInformation = 
    { /// The name of this symbol.
      Name: string
      /// The kind of this symbol.
      Kind: SymbolKind
      /// The location of this symbol.
      Location: Location
      /// The name of the symbol containing this symbol.
      ContainerName: string option }


type DocumentSymbolsParams =
    { /// The text document.
      TextDocument: TextDocumentIdentifier }

/// The parameters of a Workspace Symbol Request.
type WorkspaceSymbolsParams =
    { /// A non-empty query string
      Query: string }
    
/// Contains additional diagnostic information about the context in which a code action is run.
type CodeActionContext =
    { /// An array of diagnostics.
      Diagnostics: Diagnostic list }

/// Params for the CodeActionRequest
type CodeActionParams =
    { /// The document in which the command was invoked.
      TextDocument: TextDocumentIdentifier
      /// The range for which the command was invoked.
      Range: Range
      /// Context carrying additional information.
      Context: CodeActionContext }
    
/// A code lens represents a command that should be shown along with
/// source text, like the number of references, a way to run tests, etc.
///
/// A code lens is _unresolved_ when no command is associated to it. For performance
/// reasons the creation of a code lens and resolving should be done in two stages.
[<NoComparison>]
type CodeLens = 
    { /// The range in which this code lens is valid. Should only span a single line.
      Range: Range
      /// The command this code lens represents.
      Command: Command option
      /// A data entry field that is preserved on a code lens item between
      /// a code lens and a code lens resolve request.
      Data: obj option }

type CodeLensParams =
    { /// The document to request code lens for.
      TextDocument: TextDocumentIdentifier }
    
/// Value-object describing what options formatting should use.
type FormattingOptions = 
    { /// Size of a tab in spaces.
      TabSize: int
      /// Prefer spaces over tabs.
      InsertSpaces: bool
      /// Signature for further properties.
      // [key: string]: boolean | number | string; 
    }

type DocumentFormattingParams =
    { /// The document to format.
      TextDocument: TextDocumentIdentifier
      /// The format options.
      Options: FormattingOptions }
    
type DocumentRangeFormattingParams =
    { /// The document to format.
      TextDocument: TextDocumentIdentifier
      /// The range to format
      Range: Range
      /// The format options
      Options: FormattingOptions }

type DocumentOnTypeFormattingParams =
    { /// The document to format.
      TextDocument: TextDocumentIdentifier
      /// The position at which this request was sent.
      Position: Position
      /// The character that has been typed.
      Ch: string
      /// The format options.
      Options: FormattingOptions }
    
type RenameParams =
    { /// The document to format.
      TextDocument: TextDocumentIdentifier
      /// The position at which this request was sent.
      Position: Position
      /// The new name of the symbol. If the given name is not valid the
      /// request must return a [ResponseError](#ResponseError) with an appropriate message set.
      NewName: string }
    
[<NoComparison; RequireQualifiedAccess>]
type Request =
      /// The initialize request is sent as the first request from the client to the server.
    | Initialize of InitializeParams
      /// The shutdown request is sent from the client to the server. It asks the server to shut down, 
      /// but to not exit (otherwise the response might not be delivered correctly to the client). 
      /// There is a separate exit notification that asks the server to exit.
    | Shutdown
      /// The show message request is sent from a server to a client to ask the client to display 
      /// a particular message in the user interface. In addition to the show message notification the request 
      /// allows to pass actions and to wait for an answer from the client.
    | ShowMessage of ShowMessageRequestParams
      /// The Completion request is sent from the client to the server to compute completion items at a given cursor position. 
      /// Completion items are presented in the IntelliSense user interface. If computing full completion items is expensive, 
      /// servers can additionally provide a handler for the completion item resolve request ('completionItem/resolve'). 
      /// This request is sent when a completion item is selected in the user interface. A typically use case is for example: 
      /// the 'textDocument/completion' request doesn't fill in the documentation property for returned completion items since 
      /// it is expensive to compute. When the item is selected in the user interface then a 'completionItem/resolve' request 
      /// is sent with the selected completion item as a param. The returned completion item should have the documentation property 
      /// filled in.
    | Completion of TextDocumentPositionParams
      /// The request is sent from the client to the server to resolve additional information for a given completion item.
    | CompletionItemResolve of CompletionItem
      /// The hover request is sent from the client to the server to request hover information at a given text document position.
    | Hover of TextDocumentPositionParams
      /// The signature help request is sent from the client to the server to request signature information at a given cursor position.
    | SignatureHelp of TextDocumentPositionParams
      /// The goto definition request is sent from the client to the server to resolve the definition location of a symbol 
      /// at a given text document position.
    | GotoDefinition of TextDocumentPositionParams
      /// The references request is sent from the client to the server to resolve project-wide references for the symbol denoted 
      /// by the given text document position.
    | FindReferences of ReferenceParams
      /// The document highlight request is sent from the client to the server to resolve a document highlights 
      /// for a given text document position. For programming languages this usually highlights all references to the symbol 
      /// scoped to this file. However we kept 'textDocument/documentHighlight' and 'textDocument/references' separate 
      /// requests since the first one is allowed to be more fuzzy. Symbol matches usually have a DocumentHighlightKind of 
      /// Read or Write whereas fuzzy or textual matches use Textas the kind.
    | DocumentHighlights of TextDocumentPositionParams
      /// The document symbol request is sent from the client to the server to list all symbols found in a given text document.
    | DocumentSymbols of DocumentSymbolsParams
      /// The workspace symbol request is sent from the client to the server to list project-wide symbols matching the query string.
    | WorkspaceSymbols of WorkspaceSymbolsParams
      /// The code action request is sent from the client to the server to compute commands for a given text document and range. 
      /// The request is triggered when the user moves the cursor into a problem marker in the editor or presses the lightbulb 
      /// associated with a marker.
    | CodeAction of CodeActionParams
      /// The code lens request is sent from the client to the server to compute code lenses for a given text document.
    | CodeLens of CodeLensParams
      /// The code lens resolve request is sent from the client to the server to resolve the command for a given code lens item.
    | CodeLensResolve of CodeLens
      /// The document formatting request is sent from the server to the client to format a whole document.
    | DocumentFormatting of DocumentFormattingParams
      /// The document range formatting request is sent from the client to the server to format a given range in a document.
    | DocumentRangeFormatting of DocumentRangeFormattingParams
      /// The document on type formatting request is sent from the client to the server to format parts of the document during typing.
    | DocumentOnTypeFormatting of DocumentOnTypeFormattingParams
      /// The rename request is sent from the client to the server to perform a workspace-wide rename of a symbol.
    | Rename of RenameParams

//[<NoComparison>]
//type Response =
//    | Initialize of InitializeResult
//    | ShowMessage of MessageActionItem
//    | Completion of CompletionList
//    | CompletionItemResolve of CompletionItem
//    | Hover of Hover
//    | SignatureHelp of SignatureHelp
//    | GotoDefinition of Location list
//    | FindReferences of Location list
//    | DocumentHighlights of DocumentHighlight list
//    | DocumentSymbols of SymbolInformation list
//    | WorkspaceSymbols of SymbolInformation list
//    | CodeAction of Command list
//    | CodeLens of CodeLens list
//    | CodeLensResolve of CodeLens
//    | DocumentFormatting of TextEdit list
//    | DocumentRangeFormatting of TextEdit list
//    | DocumentOnTypeFormatting of TextEdit list
//    | Rename of WorkspaceEdit

/// Notification sent from client to server.
[<NoComparison>]
type ClientNotification =
      /// A notification to ask the server to exit its process. The server should exit with success code 0 
      /// if the shutdown request has been received before; otherwise with error code 1.
    | Exit
      /// A notification sent from the client to the server to signal the change of configuration settings.
    | DidChangeConfiguration of DidChangeConfigurationParams
      /// The document open notification is sent from the client to the server to signal newly opened text documents. 
      /// The document's truth is now managed by the client and the server must not try to read the document's 
      /// truth using the document's uri.
    | DidOpenTextDocument of DidOpenTextDocumentParams
      /// The document change notification is sent from the client to the server to signal changes to a text document. 
      /// In 2.0 the shape of the params has changed to include proper version numbers and language ids.
    | DidChangeTextDocument of DidChangeTextDocumentParams
      /// The document close notification is sent from the client to the server when the document got closed in the client. 
      /// The document's truth now exists where the document's uri points to (e.g. if the document's uri is a file uri the truth now exists on disk).
    | DidCloseTextDocument of DidCloseTextDocumentParams
      /// The document save notification is sent from the client to the server when the document was saved in the client.
    | DidSaveTextDocument of DidSaveTextDocumentParams
      /// The watched files notification is sent from the client to the server when the client detects changes to files 
      /// watched by the language client.
    | DidChangeWatchedFiles of DidChangeWatchedFilesParams

/// Notification sent from server to client.
[<NoComparison>]
type ServerNotification =
      /// The show message notification is sent from a server to a client to ask the client to display 
      /// a particular message in the user interface.
    | ShowMessage of ShowMessageParams
      /// The log message notification is sent from the server to the client to ask the client to log a particular message.
    | LogMessage of LogMessageParams
      /// The telemetry notification is sent from the server to the client to ask the client to log a telemetry event.
    | Telemetry of obj
      /// Diagnostics notification are sent from the server to the client to signal results of validation runs.
    | PublishDiagnostics of PublishDiagnosticsParams

[<NoComparison>]
type Notification =
    | Client of ClientNotification
    | Server of ServerNotification

type ErrorCode =
    | ParseError = -32700
    | InvalidRequest = -32600
    | MethodNotFound = -32601
    | InvalidParams = -32602
    | InternalError = -32603
    | ServerErrorStart = -32099
    | ServerErrorEnd = -32000

[<NoComparison>]
type ResponseError =
    { /// A number indicating the error type that occurred.
      Code: ErrorCode
      /// A string providing a short description of the error.
      Message: string
      /// A Primitive or Structured value that contains additional
      /// information about the error. Can be omitted.
      Data: obj }

[<NoComparison>]
type RequestWithId =
    { Id: int
      Request: Request }

[<NoComparison>]
type ResponseWithId<'result> =
    { Id: int
      Result: 'result option
      Error: ResponseError option }

[<NoComparison>]
type ClientMessage =
    | Request of RequestWithId
    | ClientNotification of ClientNotification

// ****** 1-to-1 Json representation ******

/// A request message to describe a request between the client and the server. Every processed request must 
/// send a response back to the sender of the request.
[<NoComparison>]
type RequestMessage =
    { /// JSON-RPC version.
      Jsonrpc: string
      /// The request id.
      Id: int
      /// The method to be invoked.
      Method: string
      /// The method's params.
      Params: obj }

/// Response Message sent as a result of a request.
[<NoComparison>]
type ResponseMessage =
    { /// JSON-RPC version.
      Jsonrpc: string
      /// The request id.
      Id: int
      /// The result of a request. This can be omitted in the case of an error.
      Result: obj
      /// The error object in case a request fails.
      Error: ResponseError option }

/// A notification message. A processed notification message must not send a response back. They work like events.
[<NoComparison; RequireQualifiedAccess>]
type NotificationMessage =
     { /// JSON-RPC version.
       Jsonrpc: string
       /// The method to be invoked.
       Method: string
       /// The notification's params.
       Params: Notification option }
