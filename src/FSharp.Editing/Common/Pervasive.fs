[<AutoOpen>]
module FSharp.Editing.Pervasive

open System
open System.IO
open System.Diagnostics
open System.Threading

/// Right Associative Backpipe Operator (<|)
let (^) = (<|) 

/// Path.Combine
let (</>) path1 path2 = Path.Combine (path1, path2)

/// obj.ReferenceEquals
let inline (==) a b = obj.ReferenceEquals(a, b)
/// LanguagePrimitives.PhysicalEquality
let inline (===) a b = LanguagePrimitives.PhysicalEquality a b
let inline debug msg = Printf.kprintf Debug.WriteLine msg

// Redirect debug output to F# Interactive for debugging purpose.
// It requires adding '-d:DEBUG' setting in F# Interactive Options.
#if INTERACTIVE
Debug.Listeners.Add(new TextWriterTraceListener(System.Console.Out)) |> ignore
Debug.AutoFlush <- true
#endif

let inline fail msg = Printf.kprintf Debug.Fail msg
let inline isNotNull v = not ^ isNull v
let inline dispose (disposable:#IDisposable) = disposable.Dispose ()

let inline Ok a = Choice1Of2 a
let inline Fail a = Choice2Of2 a
let inline (|Ok|Fail|) a = a

let getEnvInteger e defaultValue = 
    match System.Environment.GetEnvironmentVariable e with 
    | null -> defaultValue 
    | t -> try int t with _ -> defaultValue

let tryCast<'T> (o: obj): 'T option = 
    match o with
    | null -> None
    | :? 'T as a -> Some a
    | _ -> 
        debug "Cannot cast %O to %O" (o.GetType()) typeof<'T>.Name
        None

let inline liftAsync (computation : Async<'T>) : Async<'T option> = async {
    let! a = computation
    return Some a 
}

/// Load times used to reset type checking properly on script/project load/unload. It just has to be unique for each project load/reload.
/// Not yet sure if this works for scripts.
let fakeDateTimeRepresentingTimeLoaded x = DateTime(abs (int64 (match x with null -> 0 | _ -> x.GetHashCode())) % 103231L)
    
open System.Threading


let synchronize f = 
    let ctx = SynchronizationContext.Current
        
    let thread = 
        match ctx with
        | null -> null // saving a thread-local access
        | _ -> Thread.CurrentThread
    f ^ fun g arg -> 
        let nctx = SynchronizationContext.Current
        match ctx, nctx with
        | null, _ -> g arg
        | _, _ when Object.Equals(ctx, nctx) && thread.Equals Thread.CurrentThread -> g arg
        | _ -> ctx.Post ((fun _ -> g arg), null)


let memoize f =
    let cache = System.Collections.Generic.Dictionary()
    fun x ->
        match cache.TryGetValue x with
        | true, x -> x
        | _ -> let res = f x in cache.[x] <- res; res

type FileName = string
type FilePath = string

[<Measure>] type FCS

type Point<[<Measure>]'t> = { Line : int; Column : int }
type Range<[<Measure>]'t> = { Start : Point<'t>; End: Point<'t> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Point =
    let make line column : Point<'t> = { Line = line; Column = column }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Range =
    let make startLine startColumn endLine endColumn : Range<'t> =
        { Start = Point.make startLine startColumn
          End = Point.make endLine endColumn }
    
type CurrentLine<[<Measure>]'t> = 
    { Line: string
      File: FileName; Range: Range<'t> }
    member x.EndLine = x.Range.End.Line 

[<NoComparison>]
type PointInDocument<[<Measure>]'t> = 
    { Point: Point<'t>
      Line: string
      Document: string
      File: FileName }
    member x.LineIndex = x.Point.Line
    member x.ColumnIndex = x.Point.Column
    member x.CurrentLine : Lazy<CurrentLine<'t>> = 
        lazy
          { Line = x.Line
            File = x.File
            Range = Range.make x.LineIndex x.ColumnIndex x.LineIndex x.Line.Length }


type Atom<'T when 'T: not struct>(value: 'T) = 
    let refCell = ref value
        
    let rec swap f = 
        let currentValue = !refCell
        let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
        if obj.ReferenceEquals(result, currentValue) then result
        else 
            Thread.SpinWait 20
            swap f
        
    member __.Value = !refCell
    member __.Swap (f: 'T -> 'T) = swap f   

open System.Text
open System.Diagnostics

type Profiler () =
    let measures = ResizeArray()
    let total = Stopwatch.StartNew()

    member __.Time msg f = 
        let sw = Stopwatch.StartNew()
        let res = f() in measures.Add (msg, sw.Elapsed); res

    member __.TimeAsync msg f = async {
        let sw = Stopwatch.StartNew()
        let! res = f() in measures.Add (msg, sw.Elapsed); return res 
    }

    member __.Stop() = total.Stop()
    
    member __.Result =
        let measurestr = 
            measures 
            |> Seq.groupBy ^ fun (msg, _) -> msg
            |> Seq.map ^ fun (msg, ts) -> 
                msg, TimeSpan.FromTicks (ts |> Seq.sumBy ^ fun (_, t) -> t.Ticks)
            |> Seq.sortBy ^ fun (_, t) -> -t
            |> Seq.fold (fun (acc: StringBuilder) (msg, t) -> 
                acc.AppendLine (sprintf "%s, %O" msg t)) (StringBuilder())
            |> string
        sprintf
            "\nTotal = %O\n%s" total.Elapsed measurestr

    member __.Elapsed = total.Elapsed        

/// Assert helpers
type internal Assert() = 
    /// Display a good exception for this error message and then rethrow.
    static member Exception(e:Exception) =  
        System.Diagnostics.Debug.Assert(false, "Unexpected exception seen in language service", e.ToString())
       


      
/// Maybe computation expression builder, copied from ExtCore library
/// https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Control.fs
[<Sealed>]
type MaybeBuilder () =
    // 'T -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Return value: 'T option = Some value

    // M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.ReturnFrom value: 'T option = value

    // unit -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Zero (): unit option = Some ()     // TODO: Should this be None?

    // (unit -> M<'T>) -> M<'T>
    [<DebuggerStepThrough>]
    member __.Delay (f: unit -> 'T option): 'T option = f ()

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Combine (r1, r2: 'T option): 'T option =
        match r1 with
        | None -> None
        | Some () -> r2

    // M<'T> * ('T -> M<'U>) -> M<'U>
    [<DebuggerStepThrough>]
    member inline __.Bind (value, f: 'T -> 'U option): 'U option = Option.bind f value

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    [<DebuggerStepThrough>]
    member __.Using (resource: ('T :> System.IDisposable), body: _ -> _ option): _ option =
        try body resource
        finally if not <| obj.ReferenceEquals (null, box resource) then resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member x.While (guard, body: _ option): _ option =
        if guard () then
            // OPTIMIZE: This could be simplified so we don't need to make calls to Bind and While.
            x.Bind (body, (fun () -> x.While (guard, body)))
        else x.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    [<DebuggerStepThrough>]
    member x.For (sequence: seq<_>, body: 'T -> unit option): _ option =
        // OPTIMIZE: This could be simplified so we don't need to make calls to Using, While, Delay.
        x.Using (sequence.GetEnumerator (), fun enum ->
            x.While (enum.MoveNext,
                x.Delay (fun () -> body enum.Current)
            )
        )

let maybe = MaybeBuilder()

[<Sealed>]
type AsyncMaybeBuilder () =
    [<DebuggerStepThrough>]
    member __.Return value : Async<'T option> = Some value |> async.Return

    [<DebuggerStepThrough>]
    member __.ReturnFrom value : Async<'T option> = value

    [<DebuggerStepThrough>]
    member __.ReturnFrom (value: 'T option) : Async<'T option> = async.Return value

    [<DebuggerStepThrough>]
    member __.Zero () : Async<unit option> = Some () |> async.Return

    [<DebuggerStepThrough>]
    member __.Delay (f : unit -> Async<'T option>) : Async<'T option> = async.Delay f

    [<DebuggerStepThrough>]
    member __.Combine (r1, r2 : Async<'T option>) : Async<'T option> = async {
        let! r1' = r1
        match r1' with
        | None -> return None
        | Some () -> return! r2
    }

    [<DebuggerStepThrough>]
    member __.Bind (value: Async<'T option>, f : 'T -> Async<'U option>) : Async<'U option> = async {
        let! value' = value
        match value' with
        | None -> return None
        | Some result -> return! f result
    }

    [<DebuggerStepThrough>]
    member __.Bind (value: System.Threading.Tasks.Task<'T>, f : 'T -> Async<'U option>) : Async<'U option> = async {
        let! value' = Async.AwaitTask value
        return! f value'
    }

    [<DebuggerStepThrough>]
    member __.Bind (value: 'T option, f : 'T -> Async<'U option>) : Async<'U option> = async {
        match value with
        | None -> return None
        | Some result -> return! f result
    }

    [<DebuggerStepThrough>]
    member __.Using (resource : ('T :> IDisposable), body : _ -> Async<_ option>) : Async<_ option> =
        try body resource
        finally if not (isNull resource) then resource.Dispose ()

    [<DebuggerStepThrough>]
    member x.While (guard, body : Async<_ option>) : Async<_ option> =
        if guard () then x.Bind (body, (fun () -> x.While (guard, body)))
        else x.Zero ()

    [<DebuggerStepThrough>]
    member x.For (sequence : seq<_>, body : 'T -> Async<unit option>) : Async<_ option> =
        x.Using (sequence.GetEnumerator (), fun enum ->
            x.While (enum.MoveNext, 
                x.Delay (fun () -> body enum.Current)
            )
        )

    [<DebuggerStepThrough>]
    member inline __.TryWith (computation : Async<'T option>, catchHandler : exn -> Async<'T option>) : Async<'T option> =
        async.TryWith (computation, catchHandler)

    [<DebuggerStepThrough>]
    member inline __.TryFinally (computation : Async<'T option>, compensation : unit -> unit) : Async<'T option> =
        async.TryFinally (computation, compensation)

let asyncMaybe = AsyncMaybeBuilder()


