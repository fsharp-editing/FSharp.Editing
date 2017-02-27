namespace FSharp.Editing.Coloring

open System
open System.Collections.Immutable
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open FSharp.Editing


module DocumentHighlights =

    [<NoComparison>]
    type FSharpHighlightSpan = { 
        IsDefinition: bool
        TextSpan: TextSpan 
    } with
        override this.ToString() = sprintf "%+A" this
    

    let fixInvalidSymbolSpans (sourceText: SourceText) (lastIdent: string) (spans: FSharpHighlightSpan []) =
        spans
        |> Seq.choose (fun (span: FSharpHighlightSpan) ->
            let newLastIdent = sourceText.GetSubText(span.TextSpan).ToString()
            let index = newLastIdent.LastIndexOf(lastIdent, StringComparison.Ordinal)
            if index > 0 then 
                // Sometimes FCS returns a composite identifier for a short symbol, so we truncate the prefix
                // Example: newLastIdent --> "x.Length", lastIdent --> "Length"
                Some { span with TextSpan = TextSpan(span.TextSpan.Start + index, span.TextSpan.Length - index) }
            elif index = 0 && newLastIdent.Length > lastIdent.Length then
                // The returned symbol use is too long; we truncate its redundant suffix
                // Example: newLastIdent --> "Length<'T>", lastIdent --> "Length"
                Some { span with TextSpan = TextSpan(span.TextSpan.Start, lastIdent.Length) }
            elif index = 0 then
                Some span
            else
                // In the case of attributes, a returned symbol use may be a part of original text
                // Example: newLastIdent --> "Sample", lastIdent --> "SampleAttribute"
                let index = lastIdent.LastIndexOf(newLastIdent, StringComparison.Ordinal)
                if index >= 0 then
                    Some span
                else None)
        |> Seq.distinctBy (fun span -> span.TextSpan.Start)
        |> Seq.toArray


    let getDocumentHighlights  (checker: FSharpChecker, documentKey: DocumentId, sourceText: SourceText, filePath: string, position: int, 
                                defines: string list, options: FSharpProjectOptions, textVersionHash: int) : Async<FSharpHighlightSpan[] option> =
        asyncMaybe {
            let textLine = sourceText.Lines.GetLineFromPosition(position)
            let textLinePos = sourceText.Lines.GetLinePosition(position)
            let fcsTextLineNumber = Line.fromZ textLinePos.Line
            let! symbol = getSymbolAtPosition(documentKey, sourceText, position, filePath, defines, SymbolRangeLookup.Greedy)
            let! _, _, checkFileResults = checker.ParseAndCheckDocument(filePath, textVersionHash, sourceText.ToString(), options, allowStaleResults = true)
            let! symbolUse = checkFileResults.GetSymbolUseAtLocation(fcsTextLineNumber, symbol.Ident.idRange.EndColumn, textLine.ToString(), symbol.FullIsland)
            let! symbolUses = checkFileResults.GetUsesOfSymbolInFile(symbolUse.Symbol) |> liftAsync
            return 
                [| for symbolUse in symbolUses do
                    yield { 
                        IsDefinition = symbolUse.IsFromDefinition
                        TextSpan = fsharpRangeToTextSpan sourceText symbolUse.RangeAlternate 
                    } 
                |]
                |> fixInvalidSymbolSpans sourceText symbol.Ident.idText
        }



