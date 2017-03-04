namespace FSharp.Editing.Client

open System.Threading
open System.IO
open System.Net
open System
open FSharp.Editing
open FSharp.Editing.Messages
open FSharp.Editing.Messages.Serialization

type Client(serviceEndpoint: Uri) =
    let id = ref 0

    let copyAndClose (inputStream: Stream) =
        let readSize = 256
        let buffer = Array.zeroCreate<byte> readSize
        use ms = new MemoryStream()
        let mutable count = inputStream.Read (buffer, 0, readSize)
        while count > 0 do
            ms.Write(buffer, 0, count)
            count <- inputStream.Read(buffer, 0, readSize)
        ms.Position <- 0L
        inputStream.Flush ()
        inputStream.Dispose ()
        ms

    member __.Request<'a> (``method``: string, parameters: obj) : RequestResult<'a> =
        async {
            let id = Interlocked.Increment id
            
            let requestMessage =
                { RequestMessage.Id = id
                  Jsonrpc = "2.0"
                  Method = ``method``
                  Params = parameters }

            let req = HttpWebRequest.Create serviceEndpoint
            req.Method <- "Post"
            req.ContentType <- "application/json-rpc"
            use! reqStream = req.GetRequestStreamAsync() |> Async.AwaitTask
            use writer = new StreamWriter (reqStream)
            let json = serialize requestMessage
            writer.Write json
            writer.Flush ()
            reqStream.Dispose ()
        
            let! resp = req.AsyncGetResponse()
            use respStream = resp.GetResponseStream()
            use reader = new StreamReader (copyAndClose respStream)
            let responseJson = reader.ReadToEnd()
            let response: ResponseWithId<'a> = Response.deserialize responseJson
            return 
                match response.Result, response.Error with
                | _, Some error -> Fail error
                | Some result, _ -> Ok result
                | None, None ->
                    Fail { ResponseError.Code = ErrorCode.InternalError
                           Message = "Server returned inconsistent response. Either Result or Error must be filled."
                           Data = null }
        }

    member this.ShowMessage (p: ShowMessageRequestParams) : RequestResult<MessageActionItem> = 
        this.Request (Request.Method.Initialize, p)
        
        //asyncChoice.Return { Title = "a title" }