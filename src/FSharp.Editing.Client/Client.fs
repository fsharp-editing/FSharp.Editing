namespace FSharp.Editing.Client

open System.Threading
open System.IO
open System.Net
open System
open FSharp.Editing.Messages

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
        inputStream.Close()
        ms

    member __.Invoke<'a> (jsonRpc: JsonRequest) : Async<JsonResponse<'a>> =
        async {
            let id = Interlocked.Increment id
            jsonRpc.Id <- string id
            let req = HttpWebRequest.Create(Uri(serviceEndpoint, sprintf "?callid=%d" id))
            req.Method <- "Post"
            req.ContentType <- "application/json-rpc"
            use! reqStream = req.GetRequestStreamAsync() |> Async.AwaitTask
            use writer = new StreamWriter(reqStream)
            let json = JsonConvert.SerializeObject jsonRpc
            writer.Write json
            writer.Flush()
            reqStream.Close()
        
            let! resp = req.AsyncGetResponse()
            use respStream = resp.GetResponseStream()
            use reader = new StreamReader(copyAndClose respStream)
            let contentStr = reader.ReadToEnd()
            let rjson = JsonConvert.DeserializeObject<JsonResponse<'a>>(contentStr)

            if isNull rjson then
                if not (String.IsNullOrEmpty contentStr) then
                    let jo = JsonConvert.DeserializeObject contentStr :?> JObject
                    failwithf "%O" jo.["Error"]
                else
                    failwithf "Empty response"

            return rjson
        }

    member this.Invoke<'a>(``method``: string, args: obj[]) =
        async {
            let req = new JsonRequest(Method = ``method``, Params = args)
            return! this.Invoke<'a> req
        }
    
    member this.Invoke<'a>(``method``: string, arg: obj) = this.Invoke(``method``, [|arg|])

    member __.ShowMessage (p: ShowMessageRequestParams) : Async<MessageActionItem> = 
        { Title = "a title" }