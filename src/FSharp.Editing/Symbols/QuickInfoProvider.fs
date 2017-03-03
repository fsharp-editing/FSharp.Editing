module FSharp.Editing.Symbols. QuickInfoProvider



open System
open System.Composition
open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Editing
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Classification
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.ProjectSystem
open FSharp.Editing.Documentation


//[<Shared>]
//[<ExportQuickInfoProvider(PredefinedQuickInfoProviderNames.Semantic, FSharpCommonConstants.FSharpLanguageName)>]
type internal FSharpQuickInfoProvider (checkerProvider: FSharpCheckerProvider) =
//    [<System.ComponentModel.Composition.ImportingConstructor>] 
//    (
//        [<System.ComponentModel.Composition.Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
//        checkerProvider: FSharpCheckerProvider,
//        projectInfoManager: ProjectInfoManager,
//        typeMap: Shared.Utilities.ClassificationTypeMap,
//        glyphService: IGlyphService
//    ) =

//    let xmlMemberIndexService = serviceProvider.GetService(typeof<SVsXMLMemberIndexService>) :?> IVsXMLMemberIndexService
//    let documentationBuilder = XmlDocumentation.CreateDocumentationBuilder(xmlMemberIndexService, serviceProvider.DTE)
//    
    static member ProvideQuickInfo(checker: FSharpChecker, documentId: DocumentId, sourceText: SourceText, filePath: string, position: int, options: FSharpProjectOptions, textVersionHash: int) =
        asyncMaybe {
            let! _, _, checkFileResults = checker.ParseAndCheckDocument(filePath, textVersionHash, sourceText.ToString(), options, allowStaleResults = true)
            let textLine = sourceText.Lines.GetLineFromPosition(position)
            let textLineNumber = textLine.LineNumber + 1 // Roslyn line numbers are zero-based
            let defines = CompilerEnvironment.GetCompilationDefinesForEditing(filePath, options.OtherOptions |> Seq.toList)
            let! symbol = getSymbolAtPosition(documentId, sourceText, position, filePath, defines, SymbolRangeLookup.Precise)
            let! res = checkFileResults.GetStructuredToolTipTextAlternate(textLineNumber, symbol.Ident.idRange.EndColumn, textLine.ToString(), symbol.FullIsland, FSharpTokenTag.IDENT) |> liftAsync
            match res with
            | FSharpToolTipText [] 
//            | FSharpToolTipText [FSharpStructuredToolTipElement.None] -> return! None
            | FSharpToolTipText [] -> return! None
            | _ -> 
                let! symbolUse = checkFileResults.GetSymbolUseAtLocation(textLineNumber, symbol.Ident.idRange.EndColumn, textLine.ToString(), symbol.FullIsland)
                return! Some(res, fsharpRangeToTextSpan sourceText symbol.Range, symbolUse.Symbol)
        }
    
//    interface IQuickInfoProvider with
//    member this.GetItemAsync(document: Document, position: int, cancellationToken: CancellationToken): Task<QuickInfoItem> =
    member this.GetItemAsync(document: Document, position: int, cancellationToken: CancellationToken,optional:FSharpProjectOptions,_documentationBuilder) =
            asyncMaybe {
                let! sourceText = document.GetTextAsync(cancellationToken)
                let defines =  [] //projectInfoManager.GetCompilationDefinesForEditingDocument(document)  
                let! _ = getSymbolAtPosition(document.Id, sourceText, position, document.FilePath, defines, SymbolRangeLookup.Precise)
//                let! options = projectInfoManager.TryGetOptionsForEditingDocumentOrProject(document)
                let! textVersion = document.GetTextVersionAsync(cancellationToken)
                let! _toolTipElement, _textSpan, _symbol = 
                    FSharpQuickInfoProvider.ProvideQuickInfo(checkerProvider.Checker, document.Id, sourceText, document.FilePath, position, optional, textVersion.GetHashCode())
                let _mainDescription = Collections.Generic.List()
                let _documentation = Collections.Generic.List()
//                XmlDocumentation.BuildDataTipText(
//                    documentationBuilder, 
//                    CollectTaggedText mainDescription, 
//                    CollectTaggedText documentation, 
//                    FSharpToolTipText  FSharpToolTipText<string>())
//                let empty = ClassifiableDeferredContent(Array.Empty<TaggedText>(), typeMap);
//                let content = 
////                    QuickInfoDisplayDeferredContent
////                        (
////                            symbolGlyph = SymbolGlyphDeferredContent(CommonRoslynHelpers.GetGlyphForSymbol(symbol), glyphService),
////                            warningGlyph = null,
////                            mainDescription = ClassifiableDeferredContent(mainDescription, typeMap),
////                            documentation = ClassifiableDeferredContent(documentation, typeMap),
////                            typeParameterMap = empty,
////                            anonymousTypes = empty,
////                            usageText = empty,
////                            exceptionText = empty
////                        )
////                return QuickInfoItem(textSpan, content)
//                    obj()
                ()
            } 
//            |> Async.map Option.toObj
//            |> Async.startAsyncAsTask(cancellationToken)
