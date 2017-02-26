module FSharp.Editing.Navigation.NavigableItemCache

open System
open System.IO
open System.Text
open System.Threading
open System.Collections.Concurrent
open System.Security.Cryptography
open Wire
open Microsoft.CodeAnalysis
open FSharp.Editing

type Source = string

type FileDescriptor = { 
    Path : FilePath
    LastWriteTime : DateTime 
}

type FileNavigableItems = { 
    Descriptor : FileDescriptor
    Items : NavigableItem [] 
}

let solutionPathHash (solutionPath: FilePath): string =
    use digest = SHA1.Create ()
    let bytes = Encoding.UTF8.GetBytes solutionPath
    digest.ComputeHash bytes |> Array.toShortHexString


let cacheFilePath solutionPath =
    let solutionHash = solutionPathHash solutionPath
    let root = Environment.ExpandEnvironmentVariables @"%LOCALAPPDATA%\FSharp.Editing\SolutionCaches"
    root </> solutionHash </> "navigable_items.cache"

let loadFromDisk (cache:ConcurrentDictionary<FilePath, FileNavigableItems>) 
                 (session:DeserializerSession) (solutionPath: FilePath) =
    protect <| fun _ ->
        cache.Clear ()
        let profiler = Profiler ()
        let filePath = cacheFilePath solutionPath
        let items = profiler.Time "Read from file" <| fun _ ->
            if File.Exists filePath then
                use fileStream = File.OpenRead filePath
                let items: FileNavigableItems[] = session.Serializer.Deserialize<FileNavigableItems[]>(fileStream)
                items
            else [||]
                
        let items = profiler.Time "Filter by LastWriteTime" <| fun _ ->
            for item in items do
                match File.tryGetLastWriteTime item.Descriptor.Path with
                | Some lastWriteTime when item.Descriptor.LastWriteTime = lastWriteTime ->
                    cache.[item.Descriptor.Path] <- item
                | _ -> ()
            items

        profiler.Stop ()
        Logging.logInfo ^ sprintf 
            "[NavigableItemCache] Loaded: %d items for %d files. Sutable: %d items for %d files %s" 
                (items |> Array.sumBy (fun x -> x.Items.Length)) items.Length
                (cache.Values |> Seq.sumBy (fun x -> x.Items.Length)) cache.Count
                profiler.Result


let saveToDisk  (cache:ConcurrentDictionary<FilePath, FileNavigableItems>) (dirty:bool byref) 
                (session:SerializerSession) (solutionPath: FilePath) =
    if dirty then
        dirty <- false
        protect <| fun _ ->
            let filePath = cacheFilePath solutionPath
            let profiler = Profiler ()
            let items = profiler.Time "Save to file" <| fun _ ->
                let items = cache.Values |> Seq.toArray
                Directory.CreateDirectory (Path.GetDirectoryName filePath) |> ignore
                use fileStream = new FileStream (filePath, FileMode.Create, FileAccess.Write, FileShare.Read)
                session.Serializer.Serialize(items,fileStream)
                items
            profiler.Stop ()
            Logging.logInfo ^ sprintf 
                "[NavigableItemCache] Saved %d items for %d files to %s %s" 
                    (items |> Array.sumBy (fun x -> x.Items.Length)) 
                    items.Length 
                    filePath 
                    profiler.Result

// TODO ::
//  - Should this be changed to use projects instead of solutions? 
type NavigableItemCache (solution:Solution) as navcache =
    let cache = ConcurrentDictionary<FilePath, FileNavigableItems>(StringComparer.Ordinal)
    let mutable dirty = false
    let writeSession = SerializerSession ^ Serializer()
    let readSession  = DeserializerSession ^ Serializer()
    
    let tryLoadFromFile path = 
        if String.IsNullOrWhiteSpace path then () else 
        loadFromDisk cache readSession path

    let trySaveToDisk path = 
        if String.IsNullOrWhiteSpace path then () else
        saveToDisk cache &dirty writeSession path

    do tryLoadFromFile solution.FilePath

    let saveTimer = new Timer((fun _ -> trySaveToDisk solution.FilePath), null, 0, 5000)

    let workspaceEventStream = solution.Workspace.WorkspaceChanged

    let onWorkspaceEvent (args:WorkspaceChangeEventArgs) =
        match args.Kind with
        | WorkspaceChangeKind.SolutionAdded 
            ->  tryLoadFromFile args.NewSolution.FilePath
                trySaveToDisk args.OldSolution.FilePath        
        | WorkspaceChangeKind.DocumentRemoved
            ->  let doc = solution.GetDocument args.DocumentId 
                navcache.Remove doc.FilePath
        | WorkspaceChangeKind.ProjectRemoved
            ->  let proj = solution.GetProject args.ProjectId
                proj.GetDocumentFilePaths()
                |> Seq.iter ^ fun path -> navcache.Remove path
        | WorkspaceChangeKind.SolutionRemoved
        | WorkspaceChangeKind.SolutionCleared
            -> cache.Clear ()
        | _ -> ()

    let subscription = workspaceEventStream.Subscribe onWorkspaceEvent 

        
    member __.TryGet (file: FileDescriptor): NavigableItem[] option =
        match cache.TryGetValue file.Path with
        | true, x when x.Descriptor.LastWriteTime = file.LastWriteTime -> 
            //Logging.logInfo (fun _ -> sprintf "[NavigableItemCache] Found for %s, %O" file.Path file.LastWriteTime)
            Some x.Items
        | true, _x ->
            //Logging.logInfo (fun _ -> sprintf "[NavigableItemCache] Found with different LastWriteTime for %s, %O <> %O" 
                //                                file.Path x.Descriptor.LastWriteTime file.LastWriteTime)
            None
        | _ -> 
            //Logging.logInfo (fun _ -> sprintf "[NavigableItemCache] Not found for %s" file.Path)
            None
    
    member __.Add (file: FileDescriptor, items: NavigableItem[]): unit = 
        cache.[file.Path] <- { Descriptor = file; Items = items }
        dirty <- true
    
    member __.Remove (filePath: FilePath): unit = cache.TryRemove filePath |> ignore

    interface IDisposable with
        member __.Dispose () = 
            dispose saveTimer
            dispose subscription

    