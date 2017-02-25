[<AutoOpen>]
module FSharp.Editing.Utils

open System
open System.Diagnostics


/// Try to run a given function, resorting to a default value if it throws exceptions
let protectOrDefault f defaultVal =
    try f ()
    with e -> Logging.logException e; defaultVal

/// Try to run a given async computation, catch and log its exceptions
let protectAsync a = async {
    let! res = Async.Catch a
    return 
        match res with 
        | Choice1Of2 () -> ()
        | Choice2Of2 e -> Logging.logException e; ()
}

/// Try to run a given function and catch its exceptions
let protect f = protectOrDefault f ()










