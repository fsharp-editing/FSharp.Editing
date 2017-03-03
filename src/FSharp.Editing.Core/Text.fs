[<AutoOpen>]
module FSharp.Editing.Text

open System
open System.Collections.Generic
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Editing
open Microsoft.CodeAnalysis.Differencing
open Newtonsoft.Json


type ZeroBasedIndexConverter() =
    inherit JsonConverter()

    override __.CanConvert (objectType:Type) =
        objectType
            |>( (=) typeof<int>
            |?| (=) typeof<int Nullable>
            |?| (=) typeof<int seq>
            |?| (=) typeof<int []>
        )

    override __.ReadJson (reader:JsonReader, objectType:Type, _existingValue:obj, serializer:JsonSerializer) =
        if reader.TokenType = JsonToken.Null then null
        elif objectType = typeof<int []> then
            serializer.Deserialize<int[]> reader :> obj
        elif objectType = typeof<int seq> then
            serializer.Deserialize<int seq> reader
            |> Seq.map ^ fun x -> x - 1
            :> obj
        elif objectType = typeof<int Nullable> then
            let result = serializer.Deserialize<int Nullable> reader
            if result.HasValue then
                result.Value :> obj
            else null
        else null

    (*  Omnisharp has a configuration on whether to use zerobasedindices or not
                if (Configuration.ZeroBasedIndices)
            {
                return serializer.Deserialize(reader, objectType);
            }
    *)


    override __.WriteJson(writer:JsonWriter, value: obj, serializer:JsonSerializer) =
        if isNull value then
            serializer.Serialize(writer,null)
        else
        let objectType = value.GetType()
        let results =
            if objectType = typeof<int[]> then
                let results = value :?> int[]
                for i=0 to results.Length-1 do
                    results.[i] <- results.[i] + 1
                results :> obj
            elif objectType = typeof<int seq> then
                let results = value :?> int seq
                results |> Seq.map ^ fun x -> x + 1
                :> obj
            elif objectType = typeof<int Nullable> then
                let result = value :?> int Nullable
                if result.HasValue then
                    result.Value + 1 :> obj
                else
                    null
            else
                null
        serializer.Serialize(writer,results)



type LinePositionSpanTextChange () =

    member val NewText     : string  = "" with get, set
    //    [<JsonConverter(typeof<ZeroBasedIndexConverter>)>] 
    [<ZeroBasedIndexConverter>]
    member val StartLine   : int     = 0  with get, set
    [<ZeroBasedIndexConverter>]
    member val StartColumn : int     = 0  with get, set
    [<ZeroBasedIndexConverter>]
    member val EndLine     : int     = 0  with get, set
    [<ZeroBasedIndexConverter>]
    member val EndColumn   : int     = 0  with get, set

    override self.Equals obj =
        match obj with
        | :? LinePositionSpanTextChange as other ->
            self.NewText        = other.NewText
            && self.StartLine   = other.StartLine
            && self.StartColumn = other.StartColumn
            && self.EndLine     = other.EndLine
            && self.EndColumn   = other.EndColumn
        | _ -> false


    override self.GetHashCode() =
        self.NewText.GetHashCode()
        * (23 + self.StartLine)
        * (29 + self.StartColumn)
        * (31 + self.EndLine)
        * (37 + self.EndColumn)


    override self.ToString () =
        if isNull self.NewText then String.Empty else
        let displayText = self.NewText.Replace("\r", @"\r").Replace("\n", @"\n").Replace("\t", @"\t")
        sprintf "StartLine=%i, StartColumn=%i, Endline=%i, EndColumn=%i,NewText='%s'"
            self.StartLine self.StartColumn self.EndLine self.EndColumn displayText


    member __.Convert (document:Document, changes: TextChange seq) : LinePositionSpanTextChange seq Async = async {
        let! (text:SourceText)  = document.GetTextAsync()

        return changes
        |> Seq.sortWithDescending ^ fun c1 c2 -> c1.Span.CompareTo c2.Span
        |> Seq.map ^ fun change ->
            let span = change.Span
            let newText = change.NewText
            let span, prefix, suffix =
                if newText.Length <= 0 then span, String.Empty, String.Empty else
                // Roslyn computes text changes on character arrays. So it might happen that a
                // change starts inbetween \r\n which is OK when you are offset-based but a problem
                // when you are line,column-based. This code extends text edits which just overlap
                // a with a line break to its full line break
                let span, prefix =
                    if span.Start > 0 && newText.[0] = '\n' && text.[span.Start - 1] = '\r' then
                        // text: foo\r\nbar\r\nfoo
                        // edit:      [----)
                        TextSpan.FromBounds(span.Start - 1, span.End), "\r"
                    else
                        span, String.Empty
                let span, suffix =
                    if span.End < text.Length - 1 && newText.[newText.Length - 1] = '\r' && text.[span.End] = '\n' then
                        // text: foo\r\nbar\r\nfoo
                        // edit:        [----)
                        TextSpan.FromBounds(span.Start, span.End + 1), "\n"
                    else
                        span, String.Empty
                span, prefix, suffix
            let linePositionSpan = text.Lines.GetLinePositionSpan span
            LinePositionSpanTextChange
                (   NewText     = prefix + newText + suffix
                ,   StartLine   = linePositionSpan.Start.Line
                ,   StartColumn = linePositionSpan.Start.Character
                ,   EndLine     = linePositionSpan.End.Line
                ,   EndColumn   = linePositionSpan.End.Character
                )
    }

/// Insert a new block of text into a textBuffer
type BufferUpdate = {
    FileName : string
    NewText : string
    [<ZeroBasedIndexConverter>]
    StartLine : int
    [<ZeroBasedIndexConverter>]
    StartColumn : int
    [<ZeroBasedIndexConverter>]
    EndLine : int
    [<ZeroBasedIndexConverter>]
    EndColumn : int
}