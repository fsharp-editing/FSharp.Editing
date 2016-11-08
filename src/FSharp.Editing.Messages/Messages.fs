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

/// The initialize request is sent as the first request from the client to the server.
module InitializeRequest =
    let ``method`` = "initialize"

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

/// The shutdown request is sent from the client to the server. It asks the server to shut down, 
/// but to not exit (otherwise the response might not be delivered correctly to the client). 
/// There is a separate exit notification that asks the server to exit.
module ShutdownRequest =
    let ``method`` = "shutdown"

/// A notification to ask the server to exit its process. The server should exit with success code 0 
/// if the shutdown request has been received before; otherwise with error code 1.
module ExitNotification =
    let ``method`` = "exit"

/// The show message notification is sent from a server to a client to ask the client to display 
/// a particular message in the user interface.
module ShowMessageNotification =
    let ``method`` = "window/showMessage"
    
    // params: ShowMessageParams defined as follows:
    
    type ShowMessageParams = 
        { /// The message type.
          Type: MessageType
          /// The actual message.
          Message: string }

/// The show message request is sent from a server to a client to ask the client to display 
// a particular message in the user interface. In addition to the show message notification the request 
/// allows to pass actions and to wait for an answer from the client.
module ShowMessageRequest =
    let ``method`` = "window/showMessageRequest"
    
    // params: ShowMessageRequestParams defined as follows:
    // Response:
    // result: the selected MessageActionItem
    // error: code and message set in case an exception happens during showing a message.
    
    type MessageActionItem =
        { /// A short title like 'Retry', 'Open Log' etc.
          Title: string }

    type ShowMessageRequestParams = 
        { /// The message type.
          Type: MessageType
          /// The actual message
          Message: string
          /// The message action items to present.
          Aactions: MessageActionItem list }

/// The log message notification is sent from the server to the client to ask the client to log a particular message.
module LogMessageNotification =
    let ``method`` = "window/logMessage"
    
    // params: LogMessageParams defined as follows:
    
    type LogMessageParams = 
        { /// The message type.
          Type: MessageType
          /// The actual message
          Message: string }
    

/// The telemetry notification is sent from the server to the client to ask the client to log a telemetry event.
module TelemetryNotification =
    let ``method`` = "telemetry/event"
    // params: 'any'

/// A notification sent from the client to the server to signal the change of configuration settings.
module DidChangeConfigurationNotification =
    let ``method`` = "workspace/didChangeConfiguration"
    
    // params: DidChangeConfigurationParams defined as follows:
    
    [<NoComparison>]
    type DidChangeConfigurationParams =
        { /// The actual changed settings
          Settings: obj }

/// The document open notification is sent from the client to the server to signal newly opened text documents. 
/// The document's truth is now managed by the client and the server must not try to read the document's 
/// truth using the document's uri.
module DidOpenTextDocumentNotification =
    let ``method`` = "textDocument/didOpen"
    
    // params: DidOpenTextDocumentParams defined as follows:

    type DidOpenTextDocumentParams = 
        { /// The document that was opened.
          TextDocument: TextDocumentItem }


/// The document change notification is sent from the client to the server to signal changes to a text document. 
/// In 2.0 the shape of the params has changed to include proper version numbers and language ids.
module DidChangeTextDocumentNotification =
    let ``method`` = "textDocument/didChange"

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

/// The document close notification is sent from the client to the server when the document got closed in the client. 
/// The document's truth now exists where the document's uri points to (e.g. if the document's uri is a file uri the truth now exists on disk).
module DidCloseTextDocumentNotification =
    let ``method`` = "textDocument/didClose"
    
    // params: DidCloseTextDocumentParams defined as follows:

    type DidCloseTextDocumentParams =
        { /// The document that was closed.
          TextDocument: TextDocumentIdentifier }

/// The document save notification is sent from the client to the server when the document was saved in the client.
module DidSaveTextDocumentNotification =
    let ``method`` = "textDocument/didSave"
    
    // params: DidSaveTextDocumentParams defined as follows:
    
    type DidSaveTextDocumentParams = 
        { /// The document that was saved.
          TextDocument: TextDocumentIdentifier }

/// The watched files notification is sent from the client to the server when the client detects changes to files 
/// watched by the language client.
module DidChangeWatchedFilesNotification =
    let ``method`` = "workspace/didChangeWatchedFiles"

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
    
    // params: DidChangeWatchedFilesParams defined as follows:

    type DidChangeWatchedFilesParams =
        { /// The actual file events.
          Changes: FileEvent list }




/// An event describing a change to a text document. If range and rangeLength are omitted
/// the new text is considered to be the full content of the document.
type TextDocumentContentChangeEvent =
    { /// The range of the document that changed.
      Range: Range option
      /// The length of the range that got replaced.
      RangeLength: int option
      /// The new text of the document.
      Text: string }

type ReferenceContext =
    { /// Include the declaration of the current symbol.
      IncludeDeclaration: bool }

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
