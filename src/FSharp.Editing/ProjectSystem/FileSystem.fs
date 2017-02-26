namespace FSharp.Editing.ProjectSystem

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System.IO
open System.ComponentModel.Composition
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Editing
open Microsoft.CodeAnalysis.Text
open FSharp.Editing
open FSharp.Editing.ProjectSystem

type Version = int

//[<Export>]
type WorkspaceFileSystem (workspace:FSharpWorkspace) =

    static let defaultFileSystem = Shim.DefaultFileSystem () :> IFileSystem

    let getDocumentContent (fileName: string) =
        workspace.TryGetDocument fileName
        |> Option.map ^ fun doc -> 
            doc.GetText().GetBytes()

    interface IFileSystem with
        member __.FileStreamReadShim fileName = 
            getDocumentContent fileName
            |> Option.map ^ fun bytes -> new MemoryStream (bytes) :> Stream
            |> Option.getOrTry ^ fun () -> defaultFileSystem.FileStreamReadShim fileName
        
        member __.ReadAllBytesShim fileName =
            getDocumentContent fileName 
            |> Option.getOrTry ^ fun () -> defaultFileSystem.ReadAllBytesShim fileName
        
        member __.GetLastWriteTimeShim fileName =
            workspace.TryGetDocumentId fileName            
            |> Option.bind ^ fun docId -> workspace.GetLastWriteTime docId
            |> Option.getOrTry ^ fun () -> defaultFileSystem.GetLastWriteTimeShim fileName
        
        member __.GetTempPathShim () = defaultFileSystem.GetTempPathShim()
        member __.FileStreamCreateShim fileName = defaultFileSystem.FileStreamCreateShim fileName
        member __.FileStreamWriteExistingShim fileName = defaultFileSystem.FileStreamWriteExistingShim fileName
        member __.GetFullPathShim fileName = defaultFileSystem.GetFullPathShim fileName
        member __.IsInvalidPathShim fileName = defaultFileSystem.IsInvalidPathShim fileName
        member __.IsPathRootedShim fileName = defaultFileSystem.IsPathRootedShim fileName
        member __.SafeExists fileName = defaultFileSystem.SafeExists fileName
        member __.FileDelete fileName = defaultFileSystem.FileDelete fileName
        member __.AssemblyLoadFrom fileName = defaultFileSystem.AssemblyLoadFrom fileName
        member __.AssemblyLoad assemblyName = defaultFileSystem.AssemblyLoad assemblyName
