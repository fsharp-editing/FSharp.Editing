module FSharp.Editing.Navigation.GoToDefinition

open System
open System.Linq
open System.Collections.Generic
open System.Collections.Immutable

open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.ProjectSystem

let findDefinition 
    (   checker: FSharpChecker
    ,   documentKey: DocumentId
    ,   sourceText: SourceText
    ,   filePath: string
    ,   position: int
    ,   defines: string list
    ,   options: FSharpProjectOptions
    ,   textVersionHash: int
    )   : Async<Option<range>> = 
    asyncMaybe {
        let textLine = sourceText.Lines.GetLineFromPosition(position)
        let textLinePos = sourceText.Lines.GetLinePosition(position)
        let fcsTextLineNumber = textLinePos.Line + 1 // Roslyn line numbers are zero-based, FSharp.Compiler.Service line numbers are 1-based
        let! symbol = getSymbolAtPosition(documentKey, sourceText, position, filePath, defines, SymbolRangeLookup.Greedy)
        let! _parseResults, _ast, checkFileResults = checker.ParseAndCheckDocument(filePath, textVersionHash, sourceText.ToString(), options, allowStaleResults = true)
        let! _declarationSymbols = checkFileResults.GetDeclarationListSymbols(Some _parseResults,symbol.Ident.idRange.StartLine, symbol.Ident.idRange.EndColumn,textLine.ToString(),symbol.QualifyingNames(),symbol.Ident.idText) |> liftAsync
        let! declarations = checkFileResults.GetDeclarationLocationAlternate (fcsTextLineNumber, symbol.Ident.idRange.EndColumn, textLine.ToString(), symbol.FullIsland, false) |> liftAsync
            
        match declarations with
        | FSharpFindDeclResult.DeclFound(range) -> return range
        | _ -> return! None
    }

// FSROSLYNTODO: Since we are not integrated with the Roslyn project system yet, the below call
// document.Project.Solution.GetDocumentIdsWithFilePath() will only access files in the same project.
// Either Roslyn INavigableItem needs to be extended to allow arbitary full paths, or we need to
// fully integrate with their project system.
let findDefinitionsAsyncAux(fsworkspace:FSharpWorkspace, fsChecker:FSharpChecker, document: Document, position: int, cancellationToken: CancellationToken) =
        asyncMaybe {
            let! option = fsworkspace.GetDocumentProjectOptions document.Id
            let! sourceText = document.GetTextAsync(cancellationToken)
            let! textVersion = document.GetTextVersionAsync(cancellationToken)
            let defines = CompilerEnvironment.GetCompilationDefinesForEditing(document.Name, option.OtherOptions |> Seq.toList)
            let! range = findDefinition(fsChecker, document.Id, sourceText, document.FilePath, position, defines, option, textVersion.GetHashCode())
            // REVIEW: 
            let fileName = try System.IO.Path.GetFullPath(range.FileName) with _ -> range.FileName
//            let refDocumentIds = document.Project.Solution.GetDocumentIdsWithFilePath(fileName)
//            if not refDocumentIds.IsEmpty then 
//                let refDocumentId = refDocumentIds.First()
//                let refDocument = document.Project.Solution.GetDocument(refDocumentId)
//                let! refSourceText = refDocument.GetTextAsync(cancellationToken)
//                let refTextSpan = fsharpRangeToTextSpan refSourceText range
//                return 
//                    (refDocumentIds |> Seq.map ^ fun docId -> fsworkspace.getd )
            return fileName
         }
//         |> Async.map (Option.defaultValue Seq.empty)


type LanguageService with

    member self.FindDefinition (document,position,cancellationToken)  =
        findDefinitionsAsyncAux(self.Workspace,self.RawChecker,document,position,cancellationToken)


    
         