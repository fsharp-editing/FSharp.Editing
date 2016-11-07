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

/// The initialize request is sent as the first request from the client to the server.
type InitializeParams =
    { /// The process Id of the parent process that started
      /// the server. Is `None` if the process has not been started by another process.
      /// If the parent process is not alive then the server should exit (see exit notification) its process.
      processId: int option
      /// The rootPath of the workspace. Is `None` if no folder is open.
      rootPath: string option }

/// Text documents are identified using a URI.
type TextDocumentIdentifier =
    { /// The text document's URI.
      Uri: string }

/// An identifier to denote a specific version of a text document.
type VersionedTextDocumentIdentifier =
    { /// The version number of this document.
      Version: uint64
      /// The text document's URI.
      Uri: string }

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
