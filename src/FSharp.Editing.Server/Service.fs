namespace FSharp.Editing.Server

open FSharp.Editing
open FSharp.Editing.Messages
open FSharp.Editing.Messages.Serialization

module Service =
    open Suave
    open System.Text

    let private handleRequest (request: RequestWithId) : RequestResult<obj> =
        asyncChoice {
            match request.Request with
            | Request.ShowMessage _p -> 
                return box { MessageActionItem.Title = "a title" }
            | Request.CodeLens _p ->
                return box 
                    [ { CodeLens.Command = None
                        CodeLens.Range =
                         { Start = { Line = 1; Character = 2 }
                           End = { Line = 2; Character = 3 }}
                        CodeLens.Data = None } ]
            | _ -> 
                return!
                    { Code = ErrorCode.MethodNotFound
                      Message = s"%A is not supported." request.Request
                      Data = null }
                    |> Fail
                    |> async.Return
        }


    let handle: WebPart = fun (ctx: HttpContext) ->
        async {
            let request = 
                ctx.request.rawForm
                |> Encoding.UTF8.GetString
                |> Request.deserialize
            
            let! result = handleRequest request

            let response: ResponseWithId<obj> =
                match result with
                | Ok x -> 
                    { Id = request.Id
                      Result = Some x
                      Error = None }
                | Fail e ->
                    { Id = request.Id
                      Result = None
                      Error = Some e }
            
            return! Response.response HttpCode.HTTP_200 (response |> Response.toMessage |> Json.toJson) ctx
        }
