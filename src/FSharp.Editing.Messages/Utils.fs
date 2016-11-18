[<AutoOpen>]
module FSharp.Editing.Utils
    
let inline (|Ok|Fail|) x = match x with Choice1Of2 a -> Ok a | Choice2Of2 e -> Fail e
let Ok = Choice1Of2
let Fail = Choice2Of2
let inline s fmt = sprintf fmt