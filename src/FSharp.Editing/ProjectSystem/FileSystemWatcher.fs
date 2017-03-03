module FSharp.Editing.ProjectSystem.FileSystemWatcher

open System
open System.IO
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Completion
open FSharp.Control



open FSharp.Editing


let FsEvent = Event<_>()
let fseventstream = FsEvent.Publish

let createWatchStream (rootDir:string) =
    let watchers = 
        [ "fs";"fsi";"fsx";"fsscript";"fsproj"]
        |> List.map ^ fun ext -> 
            new FileSystemWatcher(ext,Path=rootDir,IncludeSubdirectories=true)
    
    ((fseventstream :> IObservable<_>), watchers)
    ||> List.fold (fun acc elem -> Observable.merge elem.Changed acc)
    
    


    