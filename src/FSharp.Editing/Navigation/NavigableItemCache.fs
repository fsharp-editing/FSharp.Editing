module FSharp.Editing.Navigation.NavigableItemCache

open System
open System.IO
open System.Text
open System.Threading
open System.Collections.Concurrent
open System.Security.Cryptography
open MBrace.FsPickler
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
                    (pickler:BinarySerializer) (solutionPath: FilePath) =
    protect <| fun _ ->
        cache.Clear ()
        let profiler = Profiler ()
        let filePath = cacheFilePath solutionPath
        let items = profiler.Time "Read from file" <| fun _ ->
            if File.Exists filePath then
                use file = File.OpenRead filePath
                let items: FileNavigableItems[] = pickler.Deserialize file
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
                (pickler:BinarySerializer) (solutionPath: FilePath) =
    if dirty then
        dirty <- false
        protect <| fun _ ->
            let filePath = cacheFilePath solutionPath
            let profiler = Profiler ()
            let items = profiler.Time "Save to file" <| fun _ ->
                let items = cache.Values |> Seq.toArray
                Directory.CreateDirectory (Path.GetDirectoryName filePath) |> ignore
                use file = new FileStream (filePath, FileMode.Create, FileAccess.Write, FileShare.Read)
                pickler.Serialize (file, items)
                items
            profiler.Stop ()
            Logging.logInfo ^ sprintf 
                "[NavigableItemCache] Saved %d items for %d files to %s %s" 
                    (items |> Array.sumBy (fun x -> x.Items.Length)) 
                    items.Length 
                    filePath 
                    profiler.Result

// TODO ::
//  - set this up to work with the active FSharp Workspace 
//  - subscribe to the workspace event equivalent of the VS solution events
type NavigableItemCache (solutionPath:FilePath) =
    let cache = ConcurrentDictionary<FilePath, FileNavigableItems>(StringComparer.Ordinal)
    let mutable dirty = false
    let pickler = FsPickler.CreateBinarySerializer ()
    let tryLoadFromFile path = loadFromDisk cache pickler path
    let trySaveToDisk path = saveToDisk cache &dirty pickler path

//    let dte = serviceProvider.GetService<DTE, SDTE>()
//    let tryGetSolutionPath() = Option.attempt (fun () -> dte.Solution.FullName)  
//    let afterSolutionClosing = _dispSolutionEvents_AfterClosingEventHandler (fun () -> cache.Clear())

//    let solutionOpened = _dispSolutionEvents_OpenedEventHandler tryLoadFromFile
//    let mutable solutionEvents: SolutionEvents = null
//    
//    do match dte.Events with
//       | :? Events2 as events ->
//           solutionEvents <- events.SolutionEvents
//           solutionEvents.add_Opened solutionOpened
//           solutionEvents.add_AfterClosing afterSolutionClosing
//       | _ -> ()
    do tryLoadFromFile solutionPath

    let saveTimer = new Timer((fun _ -> trySaveToDisk solutionPath), null, 0, 5000)

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
        member __.Dispose() =
            saveTimer.Dispose()
//            solutionEvents.remove_Opened solutionOpened
//            solutionEvents.remove_AfterClosing afterSolutionClosing

    