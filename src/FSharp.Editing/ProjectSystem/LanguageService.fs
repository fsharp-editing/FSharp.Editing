namespace FSharp.Editing

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Diagnostics
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Options
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing.ProjectSystem


// Exposes FSharpChecker as MEF export
//[<Export(typeof<FSharpCheckerProvider>); Composition.Shared>]
type internal FSharpCheckerProvider 
    //[<ImportingConstructor>]
    (
        //analyzerService: DiagnosticAnalyzer
    ) =
    let checker =
        lazy
            let checker = FSharpChecker.Create(projectCacheSize = 200, keepAllBackgroundResolutions = false)

            // This is one half of the bridge between the F# background builder and the Roslyn analysis engine.
            // When the F# background builder refreshes the background semantic build context for a file,
            // we request Roslyn to reanalyze that individual file.
            checker.BeforeBackgroundFileCheck.Add ^ fun (fileName, extraProjectInfo) -> 
               async {
                try match extraProjectInfo with 
                    | Some (:? Workspace as workspace) -> 
                        let solution = workspace.CurrentSolution
                        let documentIds = solution.GetDocumentIdsWithFilePath(fileName)
                        if not documentIds.IsEmpty then 
                            //analyzerService.Reanalyze(workspace,documentIds=documentIds)
                            ()
                    | _ -> ()
                with ex -> Assert.Exception ex
                } |> Async.StartImmediate            
            checker

    member this.Checker = checker.Value


module LanguageService =

    let entityCache = EntityCache()


    let getSymbolUsing (kind:SymbolLookupKind) (position: LinePosition) (source:SourceText) (config:ProjectSettings) = maybe {
            let args = config.CompilerOptions
            let line = source.GetLineAtPosition position
            let! symbol = 
                Lexer.getSymbol (source.ToString()) position.Line position.Character  
                                (line.Text.ToString()) kind args Lexer.queryLexState 
            return 
                fsharpRangeToTextSpan source symbol.Range, symbol
        }


    let getSymbol position source config =
        getSymbolUsing SymbolLookupKind.Fuzzy position source config


    let getLongIdentSymbol position source config =
        getSymbolUsing SymbolLookupKind.ByLongIdent position source config

    let tokenizeLine (source:SourceText)  (args:string[])  lineNum = maybe {
            let! line = source.TryGetNthLine lineNum
            return Lexer.tokenizeLine source.Text args lineNum (line.ToString())  Lexer.queryLexState
    }

//
//    let parseFileInProject fileName (config:ProjectSettings) = asyncMaybe {
//        let! opts = config.
//        
//        projectProvider.GetProjectCheckerOptions instance |> liftAsync
//        let! source = openDocumentsTracker.TryGetDocumentText fileName
//        return! instance.ParseFileInProject(opts, fileName, source) |> liftAsync
//    }
//
//    member __.ParseFileInProject (fileName, source, projectProvider: IProjectProvider) =
//        async {
//            let! opts = projectProvider.GetProjectCheckerOptions instance
//            return! instance.ParseFileInProject(opts, fileName, source)
//        }
//
//    member __.ParseAndCheckFileInProject (currentFile: string, projectProvider: IProjectProvider, ?allowStaleResults) =
//        let allowStaleResults = defaultArg allowStaleResults AllowStaleResults.No
//        asyncMaybe {
//            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
//            let! source = openDocumentsTracker.TryGetDocumentText currentFile
//            return! instance.ParseAndCheckFileInProject(opts, currentFile, source, allowStaleResults) |> liftAsync
//        }
//
//    member __.FindUsages (word: SnapshotSpan, currentFile: string, currentProject: IProjectProvider, projectsToCheck: IProjectProvider list, ?progress: ShowProgress) =
//        asyncMaybe {
//            Debug.Assert(mayReferToSameBuffer word.Snapshot currentFile, 
//                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." word.Snapshot currentFile)
//            try
//                let range = word.ToRange()
//                let endLine = range.End.Line
//                let endCol = range.End.Column
//                let! source = openDocumentsTracker.TryGetDocumentText currentFile
//                let currentLine = word.Start.GetContainingLine().GetText()
//                let framework = currentProject.TargetFramework
//                let args = currentProject.CompilerOptions
//            
//                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
//                      (word.GetText()) endLine endCol framework (String.concat " " args)
//            
//                reportProgress progress (Reporting Resource.findSymbolUseCurrentProject)
//                let! currentProjectOptions = currentProject.GetProjectCheckerOptions instance |> liftAsync
//                reportProgress progress (Reporting Resource.findSymbolUseOtherProjects)
//                let! projectsToCheckOptions = 
//                    projectsToCheck 
//                    |> List.toArray
//                    |> Async.Array.map (fun p -> p.GetProjectCheckerOptions instance)
//                    |> liftAsync
//
//                reportProgress progress (Reporting Resource.findSymbolUseAllProjects)
//
//                let newReportProgress projectName index length = 
//                    reportProgress progress (Executing(sprintf "Finding usages in %s [%d of %d]..." projectName (index + 1) length, index, length))
//                
//                let! symbol, lastIdent, refs =
//                    instance.GetUsesOfSymbolInProjectAtLocationInFile
//                        (currentProjectOptions, projectsToCheckOptions, currentFile, source, endLine, endCol, 
//                         currentLine, args, buildQueryLexState word.Snapshot.TextBuffer, Some newReportProgress)
//                return symbol, lastIdent, filterSymbolUsesDuplicates refs
//            with e ->
//                debug "[Language Service] %O exception occurs while finding usages." e
//                Logging.logExceptionWithContext(e, "Exception occurs while finding usages.")
//                return! None }
//
//    member __.FindUsagesInFile (word: SnapshotSpan, sym: Symbol, fileScopedCheckResults: ParseAndCheckResults) =
//        async {
//            try 
//                let range = word.ToRange()
//                let endLine = range.End.Line
//                let currentLine = word.Start.GetContainingLine().GetText()
//            
//                debug "[Language Service] Get symbol references for '%s' at line %d col %d" (word.GetText()) endLine sym.RightColumn
//                let! res = fileScopedCheckResults.GetUsesOfSymbolInFileAtLocation (endLine, sym.RightColumn, currentLine, sym.Text)
//                return res |> Option.map (fun (symbol, ident, refs) -> symbol, ident, filterSymbolUsesDuplicates refs)
//            with e ->
//                debug "[Language Service] %O exception occurs while finding usages in file." e
//                Logging.logExceptionWithContext(e, "Exception occurs while finding usages in file.")
//                return None
//        }
//
//    member __.FindUsagesInFile (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) =
//        asyncMaybe {
//            let currentLine = { Line = word.Start.GetContainingLine().GetText(); Range = word.ToRange(); File = currentFile }
//            let getCheckResults currentFile = 
//                asyncMaybe {
//                    let! source = openDocumentsTracker.TryGetDocumentText currentFile
//                    let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
//                    let! results = instance.ParseAndCheckFileInProject(opts, currentFile, source, stale) |> liftAsync
//                    return results
//                }
//            return! HighlightUsageInFile.findUsageInFile currentLine symbol getCheckResults
//        }
//
//    member __.GetFSharpSymbolUse (currentLine: CurrentLine<FCS>, symbol: Symbol, projectProvider: IProjectProvider, stale) = 
//        asyncMaybe {
//            let! source = openDocumentsTracker.TryGetDocumentText currentLine.File
//            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
//            let! results = instance.ParseAndCheckFileInProject(opts, currentLine.File, source, stale) |> liftAsync
//            let! symbol = results.GetSymbolUseAtLocation (currentLine.EndLine + 1, symbol.RightColumn, currentLine.Line, [symbol.Text])
//            return symbol, results
//        }
//    member __.GetFSharpSymbolUse (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) = 
//        asyncMaybe {
//            Debug.Assert(mayReferToSameBuffer word.Snapshot currentFile, 
//                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." word.Snapshot currentFile)
//            let range = word.ToRange()
//            let endLine = range.End.Line
//            let! source = openDocumentsTracker.TryGetDocumentText currentFile
//            let currentLine = word.Start.GetContainingLine().GetText()
//            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
//            let! results = instance.ParseAndCheckFileInProject(opts, currentFile, source, stale) |> liftAsync
//            let! symbol = results.GetSymbolUseAtLocation (endLine+1, symbol.RightColumn, currentLine, [symbol.Text])
//            return symbol, results
//        }
//
//    member __.CreateLexer (fileName, snapshot, args) =
//        maybe {
//            let range = SnapshotSpan(snapshot, 0, snapshot.Length).ToRange()
//            let lineStart = range.Start.Line
//            let lineEnd = range.End.Line
//            
//            let getLineStr line =
//                let lineNumber = line - lineStart
//                snapshot.GetLineFromLineNumber(lineNumber).GetText() 
//            
//            let! source = openDocumentsTracker.TryGetDocumentText fileName
//            
//            return 
//                { new LexerBase() with
//                    member __.GetSymbolFromTokensAtLocation (tokens, line, rightCol) =
//                        Lexer.getSymbolFromTokens tokens line rightCol (getLineStr line) SymbolLookupKind.ByRightColumn
//                    member __.TokenizeLine line =
//                        Lexer.tokenizeLine source args line (getLineStr line) (buildQueryLexState snapshot.TextBuffer)
//                    member __.LineCount = lineEnd + 1 }
//        }
//
//    member x.GetAllUsesOfAllSymbolsInFile (snapshot: ITextSnapshot, currentFile: string, project: IProjectProvider, stale, checkForUnusedOpens: bool) = 
//        asyncMaybe {
//            Debug.Assert(mayReferToSameBuffer snapshot currentFile, 
//                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." snapshot currentFile)
//            let! source = openDocumentsTracker.TryGetDocumentText currentFile
//            return! x.GetAllUsesOfAllSymbolsInSourceString(source, currentFile, project, stale, checkForUnusedOpens) |> liftAsync
//        }
//
//    member __.GetAllUsesOfAllSymbolsInSourceString (source: string, currentFile: string, project: IProjectProvider, stale, checkForUnusedOpens: bool) = 
//        async {
//            let! opts = project.GetProjectCheckerOptions instance
//            let! allSymbolsUses = instance.GetAllUsesOfAllSymbolsInFile(opts, currentFile, source, stale, checkForUnusedOpens)
//            return allSymbolsUses
//        }
//
//    member __.GetSymbolDeclProjects getSymbolDeclLocation currentProject (symbol: FSharpSymbol) =
//         async {
//             let projects =
//                 match getSymbolDeclLocation symbol with
//                 | Some SymbolDeclarationLocation.File -> Some [currentProject]
//                 | Some (SymbolDeclarationLocation.Projects (declProjects, _)) -> Some declProjects
//                 | None -> None
//         
//             match projects with
//             | Some projects ->
//                 return! 
//                     projects
//                     |> List.toArray
//                     |> Async.Array.map (fun p -> p.GetProjectCheckerOptions instance)
//                     |> Async.map Some
//             | None -> return None
//         }
//
//    member __.GetProjectCheckerOptions (project: IProjectProvider) = project.GetProjectCheckerOptions instance
//
//    member x.GetUnusedDeclarations (symbolUses, currentProject: IProjectProvider, getSymbolDeclLocation) =
//        async {
//            let! opts = currentProject.GetProjectCheckerOptions instance
//            return! instance.GetUnusedDeclarations(symbolUses, opts, x.GetSymbolDeclProjects getSymbolDeclLocation currentProject)
//        }
//
//    member __.GetAllEntities (fileName, project: IProjectProvider) =
//        asyncMaybe { 
//            let! opts = project.GetProjectCheckerOptions instance |> liftAsync
//            let! source = openDocumentsTracker.TryGetDocumentText fileName
//            try 
//                return! instance.GetAllEntitiesInProjectAndReferencedAssemblies (opts, fileName, source, entityCache.Locking)
//            with e ->
//                debug "[Language Service] GetAllSymbols raises an exception: %O" e
//                Logging.logExceptionWithContext(e, "GetAllSymbols raises an exception.")
//                return! None
//        }
//
//    member x.GetLoadDirectiveFileNameAtCursor (fileName, view: Microsoft.VisualStudio.Text.Editor.ITextView, project) =
//        asyncMaybe {
//            let! parseResult = x.ParseFileInProject(fileName, project)
//            let! pos = view.PosAtCaretPosition()
//            let! ast = parseResult.ParseTree
//            return! UntypedAstUtils.HashDirectiveInfo.getHashLoadDirectiveResolvedPathAtPosition pos ast
//        }
//
//    member __.GetOpenDeclarationTooltip (line, colAtEndOfNames, lineStr, names, project: IProjectProvider, file) =
//        asyncMaybe {
//            let! source = openDocumentsTracker.TryGetDocumentText file
//            let! opts = project.GetProjectCheckerOptions instance |> liftAsync
//            try return! instance.GetIdentTooltip (line, colAtEndOfNames, lineStr, names, opts, file, source)
//            with _ -> return! None
//        }
//
//    member __.InvalidateProject (projectProvider: IProjectProvider) = 
//        async {
//            let! opts = projectProvider.GetProjectCheckerOptions(instance) 
//            return! instance.InvalidateConfiguration opts
//        }
//
//    member __.ClearCaches() = 
//        debug "[Language Service] Clearing FCS caches."
//        instance.RawChecker.InvalidateAll()
//        entityCache.Clear()
//    
//    member __.CheckProjectInBackground (opts: FSharpProjectOptions) =
//        debug "[LanguageService] StartBackgroundCompile (%s)" opts.ProjectFileName
//        instance.RawChecker.CheckProjectInBackground opts
//
//    member __.RawChecker = instance.RawChecker



open System.Collections.Generic
open System.Collections.Concurrent
// Exposes project information as MEF component
//[<Export(typeof<ProjectInfoManager>); Composition.Shared>]
type ProjectInfoManager () =
    //[<ImportingConstructor>]
//    (_checkerProvider: FSharpCheckerProvider
        //[<Import(typeof<SVsServiceProvider>)>] serviceProvider: System.IServiceProvider
//    ) =
    // A table of information about projects, excluding single-file projects.  
    let projectTable = ConcurrentDictionary<ProjectId, FSharpProjectOptions>()

    // A table of information about single-file projects.  Currently we only need the load time of each such file, plus
    // the original options for editing
    let singleFileProjectTable = ConcurrentDictionary<ProjectId, DateTime * FSharpProjectOptions>()

    member this.AddSingleFileProject (projectId, timeStampAndOptions) =
        singleFileProjectTable.TryAdd (projectId, timeStampAndOptions) |> ignore

    member this.RemoveSingleFileProject projectId =
        singleFileProjectTable.TryRemove projectId |> ignore

    /// Clear a project from the project table
    member this.ClearProjectInfo (projectId: ProjectId) =
        projectTable.TryRemove projectId |> ignore

    // TODO Reimplement on top of a workspace based project system
        
    ///// Get the exact options for a single-file script
    //member this.ComputeSingleFileOptions (fileName, loadTime, fileContents, workspace: Workspace) = async {
    //    let extraProjectInfo = Some(box workspace)
    //    if SourceFile.MustBeSingleFileProject(fileName) then 
    //        let! options = checkerProvider.Checker.GetProjectOptionsFromScript(fileName, fileContents, loadTime, [| |], ?extraProjectInfo=extraProjectInfo) 
    //        let site = ProjectSitesAndFiles.CreateProjectSiteForScript(fileName, options)
    //        return ProjectSitesAndFiles.GetProjectOptionsForProjectSite(site,fileName,options.ExtraProjectInfo,serviceProvider)
    //    else
    //        let site = ProjectSitesAndFiles.ProjectSiteOfSingleFile(fileName)
    //        return ProjectSitesAndFiles.GetProjectOptionsForProjectSite(site,fileName,extraProjectInfo,serviceProvider)
    //  }

    ///// Update the info for a project in the project table
    //member this.UpdateProjectInfo(projectId: ProjectId, site: IProjectSite, workspace: Workspace) =
    //    let extraProjectInfo = Some(box workspace)
    //    let options = ProjectSitesAndFiles.GetProjectOptionsForProjectSite(site, site.ProjectFileName(), extraProjectInfo, serviceProvider)
    //    checkerProvider.Checker.InvalidateConfiguration(options)
    //    projectTable.[projectId] <- options

    /// Get compilation defines relevant for syntax processing.  
    /// Quicker then TryGetOptionsForDocumentOrProject as it doesn't need to recompute the exact project 
    /// options for a script.
    member this.GetCompilationDefinesForEditingDocument (document: Document) = 
        let projectOptionsOpt = this.TryGetOptionsForProject(document.Project.Id)  
        let otherOptions = 
            match projectOptionsOpt with 
            | None -> []
            | Some options -> options.OtherOptions |> Array.toList
        CompilerEnvironment.GetCompilationDefinesForEditing (document.Name, otherOptions)

    /// Get the options for a project
    member this.TryGetOptionsForProject (projectId: ProjectId) = 
        if projectTable.ContainsKey projectId then
            Some projectTable.[projectId]
        else None

    /// Get the exact options for a document or project
    member this.TryGetOptionsForDocumentOrProject (document: Document) = async { 
        let projectId = document.Project.Id
        
        // The options for a single-file script project are re-requested each time the file is analyzed.  This is because the
        // single-file project may contain #load and #r references which are changing as the user edits, and we may need to re-analyze
        // to determine the latest settings.  FCS keeps a cache to help ensure these are up-to-date.
        if singleFileProjectTable.ContainsKey projectId then 
            try
                let loadTime,_ = singleFileProjectTable.[projectId]
                let _fileName = document.FilePath
                let! cancellationToken = Async.CancellationToken
                let! _sourceText = document.GetTextAsync(cancellationToken) |> Async.AwaitTask
                //let! options = this.ComputeSingleFileOptions (fileName, loadTime, sourceText.ToString(), document.Project.Solution.Workspace) |> Async.AwaitTask
                let options = Unchecked.defaultof<FSharpProjectOptions>
                singleFileProjectTable.[projectId] <- (loadTime, options)
                return Some options
            with ex -> 
                Assert.Exception ex
                return None
        else return this.TryGetOptionsForProject projectId
     }

    /// Get the options for a document or project relevant for syntax processing.
    /// Quicker then TryGetOptionsForDocumentOrProject as it doesn't need to recompute the exact project options for a script.
    member this.TryGetOptionsForEditingDocumentOrProject (document: Document) = 
        let projectId = document.Project.Id
        if singleFileProjectTable.ContainsKey projectId then 
            let _loadTime, originalOptions = singleFileProjectTable.[projectId]
            Some originalOptions
        else 
            this.TryGetOptionsForProject projectId


// Used to expose FSharpChecker/ProjectInfo manager to diagnostic providers
// Diagnostic providers can be executed in environment that does not use MEF so they can rely only
// on services exposed by the workspace
type internal FSharpCheckerWorkspaceService =
    inherit Microsoft.CodeAnalysis.Host.IWorkspaceService
    abstract Checker: FSharpChecker
    abstract ProjectInfoManager: ProjectInfoManager

type internal RoamingProfileStorageLocation(keyName: string) =
    inherit OptionStorageLocation()
    
    member __.GetKeyNameForLanguage(languageName: string) =
        let unsubstitutedKeyName = keyName
 
        match languageName with
        | null -> unsubstitutedKeyName
        | _ ->
            let substituteLanguageName = if languageName = Constants.FSharpLanguageName then "FSharp" else languageName
            unsubstitutedKeyName.Replace("%LANGUAGE%", substituteLanguageName)



//[<Composition.Shared>]
//[<Microsoft.CodeAnalysis.Host.Mef.ExportWorkspaceServiceFactory(typeof<FSharpCheckerWorkspaceService>, Microsoft.CodeAnalysis.Host.Mef.ServiceLayer.Default)>]
type internal FSharpCheckerWorkspaceServiceFactory
//    [<Composition.ImportingConstructor>]
    (
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: ProjectInfoManager
    ) =
    interface Microsoft.CodeAnalysis.Host.Mef.IWorkspaceServiceFactory with
        member this.CreateService(_workspaceServices) =
            upcast { new FSharpCheckerWorkspaceService with
                member this.Checker = checkerProvider.Checker
                member this.ProjectInfoManager = projectInfoManager }


// --------------------------------------------------------------------------------------
/// Wraps the result of type-checking and provides methods for implementing
/// various IntelliSense functions (such as completion & tool tips). Provides default
/// empty/negative results if information is missing.
type ParseAndCheckResults private (infoOpt: (FSharpCheckFileResults * FSharpParseFileResults) option) =
    new (checkResults, parseResults) = ParseAndCheckResults  (Some (checkResults, parseResults))
    static member Empty = ParseAndCheckResults None

    member __.GetSymbolUseAtLocation (line, colAtEndOfNames, lineStr, identIsland) = asyncMaybe {
        let! checkResults, _ = infoOpt 
        return! checkResults.GetSymbolUseAtLocation (line, colAtEndOfNames, lineStr, identIsland)
    }

    member __.GetUsesOfSymbolInFile symbol = async {
        match infoOpt with 
        | None -> return [||]
        | Some (checkResults, _parseResults) -> 
            return! checkResults.GetUsesOfSymbolInFile symbol
    }

    member __.GetAllUsesOfAllSymbolsInFile () = async {
        match infoOpt with
        | None -> return [||]
        | Some (checkResults, _) -> return! checkResults.GetAllUsesOfAllSymbolsInFile()
    }

    member __.ParseTree   = infoOpt |> Option.bind ^ fun (_, parseResults) -> parseResults.ParseTree
    member __.CheckErrors = infoOpt |> Option.map  ^ fun (checkResults, _) -> checkResults.Errors
    member __.ParseErrors = infoOpt |> Option.map  ^ fun (_, parseResults) -> parseResults.Errors
    
    member x.Errors =
        x.ParseErrors 
        |> Option.getOrElse [||]
        |> Array.append (x.CheckErrors |> Option.getOrElse [||])

    member __.GetFormatSpecifierLocationsAndArity() =
        infoOpt |> Option.map ^ fun (checkResults, _) -> checkResults.GetFormatSpecifierLocationsAndArity ()

    member __.PartialAssemblySignature =
        infoOpt |> Option.map ^ fun (checkResults, _) -> checkResults.PartialAssemblySignature

    member __.ProjectContext =
        infoOpt |> Option.map ^ fun (checkResults, _) -> checkResults.ProjectContext

    member x.GetUsesOfSymbolInFileAtLocation (line, col, lineStr, ident) = asyncMaybe {
        let! symbolUse = x.GetSymbolUseAtLocation (line + 1, col, lineStr, [ident]) 
        let! refs = x.GetUsesOfSymbolInFile symbolUse.Symbol |> liftAsync
        return symbolUse.Symbol, ident, refs
    }

    member __.GetDeclarationLocation(line, col, lineStr, ident, preferSignature) = async {
        match infoOpt with 
        | None -> return FSharpFindDeclResult.DeclNotFound FSharpFindDeclFailureReason.Unknown
        | Some (checkResults, _parseResults) -> 
            return! checkResults.GetDeclarationLocationAlternate (line+1, col, lineStr, [ident], preferSignature)
    }

    member __.GetDeclarationLocation (symbol, lineStr, preferSignature) =
        __.GetDeclarationLocation (symbol.Line, symbol.RightColumn, lineStr, symbol.Text, preferSignature)

    member __.GetIdentTooltip (line, colAtEndOfNames, lineText, names) =
        Debug.Assert (names <> [], "The names should not be empty (for which GetToolTip raises exceptions).")
        asyncMaybe {
            let! checkResults, _ = infoOpt 
            let tokenTag = FSharpTokenTag.IDENT
            return! 
                checkResults.GetToolTipTextAlternate(line, colAtEndOfNames, lineText, names, tokenTag) |> liftAsync
        }

[<RequireQualifiedAccess>]
type AllowStaleResults = 
    /// Allow checker results where the source doesn't even match
    | MatchingFileName
    /// Allow checker results where the source matches but where the background builder may not have caught up yet after some other change
    /// (such as a saved change in an earlier file in the compilation order, or a saved change in a project or DLL this project depends on).
    ///
    /// This gives good, fast, accurate results for repeated requests to the same file text. Semantic responsiveness will be degraded
    /// during edition of the file.
    | MatchingSource
    /// Don't allow stale results. This waits for all background changes relevant to the file to propagate, and forces a recheck of the file text
    /// regardless of whether if has been recently checked or not.
    | No

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System.Collections.Concurrent

type private FileState =
    | Checked
    | NeedChecking
    | BeingChecked
    | Cancelled



   // ----------------------------------------------------------------------------------//
  //  F# LANGUAGE SERVICE                                                              //
 //-----------------------------------------------------------------------------------//
/// Provides functionality for working with the F# interactive checker running in background


type FSharpLanguageService (workspace:FSharpWorkspace,?backgroundCompilation: bool, ?projectCacheSize: int) =
    let fileSystem = WorkspaceFileSystem workspace 

    do Shim.FileSystem <- fileSystem :> IFileSystem

    let mutable errorHandler = None
  
    let handleCriticalErrors e file source opts = 
        errorHandler |> Option.iter ^ fun handle -> handle e file source opts

    // Create an instance of interactive checker.
    let checkerInstance = 
        FSharpChecker.Create(
            projectCacheSize = defaultArg projectCacheSize 50, 
            keepAllBackgroundResolutions = false,
            keepAssemblyContents = false,
            ImplicitlyStartBackgroundWork = defaultArg backgroundCompilation true)
  
    let checkerAsync (f: FSharpChecker -> Async<'a>) = 
        let ctx = System.Threading.SynchronizationContext.Current
        async {
            do! Async.SwitchToThreadPool ()
            let! result = f checkerInstance
            do! Async.SwitchToContext ctx
            return result
        }

    /// When creating new script file on Mac, the filename we get sometimes 
    /// has a name //foo.fsx, and as a result 'Path.GetFullPath' throws in the F#
    /// language service - this fixes the issue by inventing nicer file name.
    let fixFileName path = 
        if (try Path.GetFullPath path |> ignore; true with _ -> false) then path 
        elif onLinux () || onOSX () then 
            Environment.GetEnvironmentVariable "HOME"
        else
            Environment.ExpandEnvironmentVariables "%HOMEDRIVE%%HOMEPATH%" </> Path.GetFileName path

    let files = ConcurrentDictionary<string, FileState>()
  
    let _isResultObsolete fileName = 
        match files.TryGetValue fileName with
        | true, Cancelled -> true
        | _ -> false
  
    let parseAndCheckFileInProject(filePath, source, options) = async { 
        debug "[LanguageService] ParseAndCheckFileInProject - enter"
        let fixedFilePath = fixFileName filePath
        let! res = Async.Catch ^ checkerAsync ^ fun x -> async {
            try
                // wait until the previous checking completed
                while files.ContainsKey filePath &&
                        (not (files.TryUpdate (filePath, BeingChecked, Checked)
                            || files.TryUpdate (filePath, BeingChecked, NeedChecking))) do
                    do! Async.Sleep 20
                   
                debug "[LanguageService] Change state for %s to `BeingChecked`" filePath
                debug "[LanguageService] Parse and typecheck source..."
                return! x.ParseAndCheckFileInProject (fixedFilePath, 0, source, options) 
            finally 
                if files.TryUpdate (filePath, Checked, BeingChecked) then
                    debug "[LanguageService] %s: BeingChecked => Checked" filePath
                elif files.TryUpdate (filePath, Checked, Cancelled) then
                    debug "[LanguageService] %s: Cancelled => Checked" filePath 
        }

        debug "[LanguageService]: Parse completed"
        // Construct new typed parse result if the task succeeded
        let results = 
            match res with
            | Choice1Of2 (parseResults, FSharpCheckFileAnswer.Succeeded checkResults) ->
                // Handle errors on the GUI thread
                debug "[LanguageService] ParseAndCheckFileInProject - HasFullTypeCheckInfo? %b" checkResults.HasFullTypeCheckInfo
                debug "[LanguageService] ParseAndCheckFileInProject - Errors? %A" checkResults.Errors
                ParseAndCheckResults(checkResults, parseResults)
            | Choice1Of2 (_, FSharpCheckFileAnswer.Aborted) ->
                debug "[LanguageService] ParseAndCheckFileInProject - Aborted"
                ParseAndCheckResults.Empty
            | Choice2Of2 e -> 
                fail "[LanguageService] Unexpected type checking errors occurred for '%s' with %A" fixedFilePath options
                fail "[LanguageService] Calling checker.ParseAndCheckFileInProject failed: %A" e
                debug "[LanguageService] Type checking fails for '%s' with content=%A and %A.\nResulting exception: %A" fixedFilePath source options e
                handleCriticalErrors e fixedFilePath source options
                ParseAndCheckResults.Empty
        return results
    }

    new ([<Optional>] backgroundCompilation: bool, [<Optional>] projectCacheSize: int) = 
        FSharpLanguageService (new FSharpWorkspace (), backgroundCompilation,projectCacheSize)



    member __.Workspace = workspace


    member __.OnFileChanged filePath = 
        files.AddOrUpdate (filePath, NeedChecking, (fun _ oldState -> 
            match oldState with
            | BeingChecked -> Cancelled
            | Cancelled -> Cancelled
            | NeedChecking -> NeedChecking
            | Checked -> NeedChecking))
        |> debug "[LanguageService] %s changed: set status to %A" filePath

    member __.OnFileClosed filePath = 
        match files.TryRemove filePath with
        | true, _ -> debug "[LanguageService] %s was removed from `files` dictionary" filePath
        | _ -> ()

    /// Constructs options for the interactive checker for the given file in the project under the given configuration.
    member x.GetCheckerOptions (fileName, projFilename, source, files, args, referencedProjects, fscVersion) =
        let ext = Path.GetExtension fileName
        let opts = 
            if ext = ".fsx" || ext = ".fsscript" then
               // We are in a stand-alone file or we are in a project, but currently editing a script file
               x.GetScriptCheckerOptions (fileName, projFilename, source, fscVersion)
          
            // We are in a project - construct options using current properties
            else async { return x.GetProjectCheckerOptions(projFilename, files, args, referencedProjects) }
        opts

    /// Constructs options for the interactive checker for the given script file in the project under the given configuration. 
    member __.GetScriptCheckerOptions(fileName, projFilename, source, fscVersion) = async {
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        try 
            let fileName = fixFileName fileName
            debug "GetScriptCheckerOptions: Creating for stand-alone file or script: '%s'" fileName
            let! opts = checkerInstance.GetProjectOptionsFromScript (fileName, source, fakeDateTimeRepresentingTimeLoaded projFilename)
                
            let results =
                // The FSharpChecker resolution sometimes doesn't include FSharp.Core and other essential assemblies, so we need to include them by hand
                if (opts.OtherOptions |> Seq.exists ^ fun s -> s.Contains "FSharp.Core.dll") then
                    match fscVersion with
                    | FSharpCompilerVersion.FSharp_3_0
                    | FSharpCompilerVersion.FSharp_3_1 ->
                        let dirs = 
                        #if !NETCORE
                            FSharpEnvironment.getDefaultDirectories (fscVersion, FSharpTargetFramework.NET_4_5)
                        #else 
                            []
                        #endif 
                        FSharpEnvironment.resolveAssembly dirs "FSharp.Core"
                        |> Option.map ^ fun path -> 
                            let fsharpCoreRef = sprintf "-r:%s" path
                            { opts with 
                                OtherOptions = 
                                [|  yield fsharpCoreRef
                                    yield! opts.OtherOptions |> Seq.filter ^ fun s -> not ^ s.Contains "FSharp.Core.dll"
                                |] 
                            }
                        |> Option.getOrElse opts
                    | _ -> opts
                else 
                // Add assemblies that may be missing in the standard assembly resolution
                debug "GetScriptCheckerOptions: Adding missing core assemblies."
                let dirs = 
                #if !NETCORE
                    FSharpEnvironment.getDefaultDirectories(fscVersion, FSharpTargetFramework.NET_4_5)
                #else
                    []
                #endif
                { opts with 
                    OtherOptions = 
                    [|  yield! opts.OtherOptions
                        match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
                        | Some fn -> yield sprintf "-r:%s" fn
                        | None -> debug "Resolution: FSharp.Core assembly resolution failed!"
                        match FSharpEnvironment.resolveAssembly dirs "FSharp.Compiler.Interactive.Settings" with
                        | Some fn -> yield sprintf "-r:%s" fn
                        | None -> debug "Resolution: FSharp.Compiler.Interactive.Settings assembly resolution failed!" 
                    |] 
                }
              
            // Print contents of check option for debugging purposes
            debug "GetScriptCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, FSharpProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
                results.ProjectFileName results.ProjectFileNames results.OtherOptions results.IsIncompleteTypeCheckEnvironment results.UseScriptResolutionRules
            return results
        with e -> 
            return failwithf "Exception when getting check options for '%s'\n.Details: %A" fileName e
      }
  
    /// Constructs options for the interactive checker for a project under the given configuration. 
    member __.GetProjectCheckerOptions (projFilename, files, args, referencedProjects) =
        let opts = { 
            ProjectFileName = projFilename
            ProjectFileNames = files
            OtherOptions = args
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = fakeDateTimeRepresentingTimeLoaded projFilename
            UnresolvedReferences = None
            ReferencedProjects = referencedProjects 
            OriginalLoadReferences = []
            ExtraProjectInfo = None
        }
        debug "GetProjectCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, FSharpProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A, ReferencedProjects: %A" 
            opts.ProjectFileName opts.ProjectFileNames opts.OtherOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules opts.ReferencedProjects
        opts

    member __.ParseFileInProject(projectOptions, fileName: string, src) = async {
        debug "Parsing: Get untyped parse result (fileName=%s)" fileName
        return! checkerAsync ^ fun x -> x.ParseFileInProject (fileName, src, projectOptions)
    }

    member internal __.TryGetStaleTypedParseResult(fileName:string, options, src, stale)  = 
        // Try to get recent results from the F# service
        let res = 
            match stale with 
            | AllowStaleResults.MatchingFileName -> checkerInstance.TryGetRecentCheckResultsForFile (fileName, options) 
            | AllowStaleResults.MatchingSource -> checkerInstance.TryGetRecentCheckResultsForFile (fileName, options, source=src)
            | AllowStaleResults.No -> None
        match res with 
        | Some (untyped,typed,_) when typed.HasFullTypeCheckInfo  -> Some (ParseAndCheckResults(typed, untyped))
        | _ -> None

  /// Parses and checks the given file in the given project under the given configuration. Asynchronously
  /// returns the results of checking the file.
    member x.ParseAndCheckFileInProject (opts, fileName: string, src, stale) = async { 
        match x.TryGetStaleTypedParseResult (fileName, opts, src, stale) with
        | Some results -> return results
        | None -> 
            debug "Parsing: Trigger parse (fileName=%s)" fileName
            return! parseAndCheckFileInProject (fileName, src, opts)
    }

    /// Get all the uses in the project of a symbol in the given file (using 'source' as the source for the file)
    member x.GetUsesOfSymbolInProjectAtLocationInFile
        (   currentProjectOptions: FSharpProjectOptions
        ,   dependentProjectsOptions: FSharpProjectOptions seq
        ,   fileName: string
        ,   source: string
        ,   line: int
        ,   col: int
        ,   lineStr: string
        ,   args: string[]
        ,   queryLexState: string -> string list -> int -> FSharpTokenizerLexState
        ,   reportProgress: (string -> int -> int -> unit) option
        ) = async { 
        match Lexer.getSymbol source line col lineStr SymbolLookupKind.Fuzzy args queryLexState with
        | Some symbol ->
            let! fileCheckResults = x.ParseAndCheckFileInProject (currentProjectOptions, fileName, source, AllowStaleResults.MatchingSource)
            let! result = fileCheckResults.GetSymbolUseAtLocation (line + 1, symbol.RightColumn, lineStr, [symbol.Text])
            match result with
            | Some fsSymbolUse ->
                let! refs =
                    let dependentProjects = dependentProjectsOptions |> Seq.toArray
                    dependentProjects |> Async.Array.mapi ^ fun index opts ->
                        async {
                            try
                                let projectName = Path.GetFileNameWithoutExtension(opts.ProjectFileName)
                                reportProgress |> Option.iter (fun progress -> progress projectName index dependentProjects.Length)
                                let! projectResults = checkerAsync (fun x -> x.ParseAndCheckProject opts)
                                let! refs = projectResults.GetUsesOfSymbol fsSymbolUse.Symbol
                                return refs 
                            with e ->
                                handleCriticalErrors e fileName source opts
                                return [||]
                        }
                let refs = Array.concat refs
                return Some (fsSymbolUse.Symbol, symbol.Text, refs)
            | None -> return None
         | _ -> return None 
    }

    member __.InvalidateConfiguration options =
        checkerAsync <| fun checker -> async { checker.InvalidateConfiguration options }

    member __.RawChecker = checkerInstance

    member x.GetAllUsesOfAllSymbolsInFile (projectOptions, fileName, source: string, stale, checkForUnusedOpens) : SymbolUse[] Async = async {
        let! results = x.ParseAndCheckFileInProject (projectOptions, fileName, source, stale)
        let! fsharpSymbolsUses = results.GetAllUsesOfAllSymbolsInFile ()
      
        let allSymbolsUses =
            fsharpSymbolsUses
            |> Array.map ^ fun symbolUse -> 
                let fullNames = 
                    match symbolUse.Symbol with
                    // Make sure that unsafe manipulation isn't executed if unused opens are disabled
                    | _ when not checkForUnusedOpens -> None
                    | MemberFunctionOrValue func when func.IsExtensionMember ->
                        if func.IsProperty then
                            let fullNames =
                                [|  if func.HasGetterMethod then
                                        yield func.GetterMethod.EnclosingEntity.TryGetFullName()
                                    if func.HasSetterMethod then
                                        yield func.SetterMethod.EnclosingEntity.TryGetFullName()
                                |] |> Array.choose id
                            match fullNames with
                            | [||] -> None 
                            | _ -> Some fullNames
                        else 
                            match func.EnclosingEntity with
                            // C# extension method
                            | FSharpEntity Class ->
                                let fullName = symbolUse.Symbol.FullName.Split '.'
                                if fullName.Length > 2 then
                                    (* For C# extension methods FCS returns full name including the class name, like:
                                        Namespace.StaticClass.ExtensionMethod
                                        So, in order to properly detect that "open Namespace" actually opens ExtensionMethod,
                                        we remove "StaticClass" part. This makes C# extension methods looks identically 
                                        with F# extension members.
                                    *)
                                    let fullNameWithoutClassName =
                                        Array.append fullName.[0..fullName.Length - 3] fullName.[fullName.Length - 1..]
                                    Some [|String.Join (".", fullNameWithoutClassName)|]
                                else None
                            | _ -> None
                    // Operators
                    | MemberFunctionOrValue func ->
                        match func with
                        | Constructor _ ->
                            // full name of a constructor looks like "UnusedSymbolClassifierTests.PrivateClass.( .ctor )"
                            // to make well formed full name parts we cut "( .ctor )" from the tail.
                            let fullName = func.FullName
                            let ctorSuffix = ".( .ctor )"
                            let fullName =
                                if fullName.EndsWith ctorSuffix then 
                                    fullName.[0..fullName.Length - ctorSuffix.Length - 1]
                                else fullName
                            Some [| fullName |]
                        | _ -> 
                            Some [| yield func.FullName 
                                    match func.TryGetFullCompiledOperatorNameIdents() with
                                    | Some idents -> yield String.concat "." idents
                                    | None -> ()
                                |]
                    | FSharpEntity e ->
                        match e with
                        | e, TypedAstPatterns.Attribute, _ ->
                            e.TryGetFullName ()
                            |> Option.map ^ fun fullName -> 
                                [|  fullName 
                                    fullName.Substring (0, fullName.Length - "Attribute".Length) 
                                |]
                        | e, _, _ -> 
                            e.TryGetFullName () |> Option.map ^ fun fullName -> [| fullName |]
                    | RecordField _
                    | UnionCase _ as symbol ->
                        Some [| let fullName = symbol.FullName
                                yield fullName
                                let idents = fullName.Split '.'
                                // Union cases/Record fields can be accessible without mentioning the enclosing type. 
                                // So we add a FullName without having the type part.
                                if idents.Length > 1 then
                                    yield String.Join (".", Array.append idents.[0..idents.Length - 3] idents.[idents.Length - 1..])
                            |]
                    |  _ -> None
                    |> Option.getOrElse [|symbolUse.Symbol.FullName|]
                    |> Array.map ^ fun fullName -> fullName.Split '.'
                  
                {   SymbolUse = symbolUse
                    IsUsed = true
                    FullNames = fullNames 
                }
        return allSymbolsUses 
    }

    /// Get all the uses in the project of a symbol in the given file (using 'source' as the source for the file)
    member __.IsSymbolUsedInProjects(symbol: FSharpSymbol, currentProjectName: string, projectsOptions: FSharpProjectOptions seq) =
        projectsOptions
        |> Seq.toArray
        |> Async.Array.exists ^ fun opts ->
            async {
                let! projectResults = checkerAsync ^ fun x -> x.ParseAndCheckProject opts
                let! refs = projectResults.GetUsesOfSymbol symbol
                return
                    if opts.ProjectFileName = currentProjectName then refs.Length > 1
                    else refs.Length > 0 
            }

    member x.GetUnusedDeclarations (symbolsUses, projectOptions, getSymbolDeclProjects) : SymbolUse[] Async = async {
        let singleDefs = UnusedDeclarations.getSingleDeclarations symbolsUses
        let! notUsedSymbols =
            singleDefs 
            |> Async.Array.map ^ fun fsSymbol -> async {
                let! opts = getSymbolDeclProjects fsSymbol
                match opts with
                | Some projects ->
                    let! isSymbolUsed = x.IsSymbolUsedInProjects (fsSymbol, projectOptions.ProjectFileName, projects) 
                    if isSymbolUsed then return None
                    else return Some fsSymbol
                | None -> return None 
            } |> Async.map ^ Array.choose id

        return 
            match notUsedSymbols with
            | [||] -> symbolsUses
            | _ ->
                symbolsUses |> Array.map ^ fun su -> 
                    { su with 
                        IsUsed = notUsedSymbols |> Array.forall ^ fun s -> 
                            not ^ s.IsEffectivelySameAs su.SymbolUse.Symbol
                    }
    }

    member x.GetAllEntitiesInProjectAndReferencedAssemblies (projectOptions: FSharpProjectOptions, fileName, source, ?withCache) =
        async {
            let! checkResults = x.ParseAndCheckFileInProject (projectOptions, fileName, source, AllowStaleResults.No)
            return Some [ 
                match checkResults.PartialAssemblySignature with
                | Some signature -> 
                    yield! AssemblyContent.getAssemblySignatureContent AssemblyContentType.Full signature
                | None -> ()

                match checkResults.ProjectContext with
                | Some ctx ->
                    // FCS sometimes returns several FSharpAssembly for single referenced assembly. 
                    // For example, it returns two different ones for Swensen.Unquote; the first one 
                    // contains no useful entities, the second one does. Our cache prevents to process
                    // the second FSharpAssembly which results with the entities containing in it to be 
                    // not discovered.
                    let assembliesByFileName =
                        ctx.GetReferencedAssemblies ()
                        |> Seq.groupBy ^ fun asm -> asm.FileName
                        |> Seq.map ^ fun (fileName, asms) -> fileName, List.ofSeq asms
                        |> Seq.toList
                        |> List.rev // if mscorlib.dll is the first then FSC raises exception when we try to
                                    // get Content.Entities from it.

                    for fileName, signatures in assembliesByFileName do
                        let contentType = Public // it's always Public for now since we don't support InternalsVisibleTo attribute yet
                        yield! AssemblyContent.getAssemblyContent withCache contentType fileName signatures
                | None -> () 
             ]
        }

    member x.GetIdentTooltip (line, colAtEndOfNames, lineStr, names, project: FSharpProjectOptions, file, source) = async {
        let! checkResults = x.ParseAndCheckFileInProject (project, file, source, AllowStaleResults.No)
        return! checkResults.GetIdentTooltip (line, colAtEndOfNames, lineStr, names)
    }

    member __.SetCriticalErrorHandler func = errorHandler <- Some func