namespace FSharp.Editing.Server

open FSharp.Editing.Messages

module Service =
    open Suave

    let handle (req: HttpRequest) : WebPart =
        async {
            return! Response.response HttpCode.HTTP_200 [||]
        }
