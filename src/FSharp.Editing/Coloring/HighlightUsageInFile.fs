namespace FSharp.Editing.Coloring

open FSharp.Editing
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text


module Symbols = 
    open System.IO
    
    open Microsoft.FSharp.Compiler.SourceCodeServices
    
    let filterSymbolUsesDuplicates (uses: FSharpSymbolUse []) =
        uses
        |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse))
        |> Seq.groupBy (fst >> Path.GetFullPathSafe)
        |> Seq.collect (fun (_, symbolUses) -> 
            symbolUses 
            |> Seq.map snd 
            |> Seq.distinctBy (fun s -> s.RangeAlternate))
        |> Seq.toArray

type GetCheckResults = FileName -> Async<ParseAndCheckResults option>

module HighlightUsageInFile =
    open Microsoft.FSharp.Compiler.SourceCodeServices
    
    [<NoComparisonAttribute(* due to FSharpSymbol *)>]
    type HighlightUsageInFileResult =
        | UsageInFile of FSharpSymbol * LongIdent * FSharpSymbolUse array

    let findUsageInFile file (currentLine: TextLine ) (symbol: Symbol) (getCheckResults: GetCheckResults) = 
        asyncMaybe {
            let! parseAndCheckResults = getCheckResults file
            let! _ = parseAndCheckResults.GetSymbolUseAtLocation (currentLine.LineNumber, symbol.RightColumn, currentLine.ToString(), [symbol.Text])
            let! (symbol, ident, refs) = parseAndCheckResults.GetUsesOfSymbolInFileAtLocation (currentLine.LineNumber, symbol.RightColumn, currentLine.ToString(), symbol.Text)
            return UsageInFile (symbol, ident, Symbols.filterSymbolUsesDuplicates refs)
        }