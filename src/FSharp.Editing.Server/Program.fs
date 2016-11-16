open System
open System.IO
open FSharp.Editing.Server
open Suave
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Writers

[<EntryPoint>]
let main _  =
    POST 
    >=> path "/" 
    >=> Service.handle
    >=> setMimeType "application/json; charset=utf-8"
    |> startWebServer defaultConfig 
    0