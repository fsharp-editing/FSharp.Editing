namespace FSharp.Editing.ProjectSystem

open System
open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range

[<NoComparison>]
type EditorBuffer = { 
    Text : string
    Range : range
    IsDirty : bool
    Encoding : Encoding
    LastChangeTime : DateTime
    ViewCount : int 
} with
    static member Create text range isDirty encoding lastChangeTime = { 
        Text = text
        Range = range
        IsDirty = isDirty
        Encoding = encoding
        LastChangeTime = lastChangeTime
        ViewCount = 1 
    }

type IBufferTracker =
    abstract MapEditorBuffers : (KeyValuePair<string, EditorBuffer> -> 'a) -> seq<'a>
    abstract TryFindEditorBuffer : string -> EditorBuffer option
    abstract TryGetBufferText : string -> string option
    abstract BufferChanged : string IEvent
    abstract BufferClosed : string IEvent



type IOpenDocument =
    abstract Text : Lazy<string>

type IOpenDocumentsTracker<'OpenDoc when 'OpenDoc :> IOpenDocument> =
    abstract MapOpenDocuments: (KeyValuePair<string, 'OpenDoc> -> 'a) -> seq<'a>
    abstract TryFindOpenDocument: string -> 'OpenDoc option
    abstract TryGetDocumentText: string -> string option
    abstract DocumentChanged: IEvent<string>
    abstract DocumentClosed: IEvent<string>

