namespace FSharp.Editing.ProjectSystem

open System
open System.Linq
open System.Threading
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open FSharp.Control
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing



(*  WORKSPACE REFERENCE MATERIAL

    - MonoDevelop.Ide.TypeSystem/MonoDevelopWorkspace.cs
    - https://github.com/mono/monodevelop/blob/master/main/src/core/MonoDevelop.Ide/MonoDevelop.Ide.TypeSystem/MonoDevelopWorkspace.cs






*)







type LastWriteTime = DateTime

    // TODO -
    //  What additional data caches does the workspace need?
    //  * FSharpProjectOptions dictionary that updates on projectInfo change events?
    //  *


//    let _checkProjectId (projectId:ProjectId) =
//        match self.CurrentSolution.ContainsProject projectId with
//        | false -> failwithf "The workspace does not contain a project with the id '%s'" ^ string projectId.Id
//        | true -> ()
//
//    let _projectOptions = ConcurrentDictionary<ProjectId,FSharpProjectOptions>()

type FSharpWorkspace (hostServices:Host.HostServices) as self =
    inherit Workspace (hostServices,Constants.FSharpLanguageName)
    let subscriptions = ResizeArray<IDisposable> ()
    let workspaceChange = Event<_> () 
    let eventStream = workspaceChange.Publish
    let documentTimeStamps = ConcurrentDictionary<DocumentId, LastWriteTime> ()
    let transientDocuments = ConcurrentDictionary<FilePath, DocumentId list> (StringComparer.OrdinalIgnoreCase)
    let _documentIdCache = Dictionary<FilePath, DocumentId> (StringComparer.OrdinalIgnoreCase)
    let transientDocumentIds = HashSet<DocumentId> ()
    let lockObj = obj ()

    do 
        eventStream.Subscribe self.OnWorkspaceChanged |> subscriptions.Add

    let tryAddTransientDocument (fileName:string) (fileContent : string) =
        if String.IsNullOrWhiteSpace fileName then false else
        let projects = self.CurrentSolution.Projects
        if projects.Count () = 0 then false else
        let sourceText = SourceText.From fileContent
        let documents : DocumentInfo list =
            (projects, []) ||> Seq.foldBack ^ fun project docs ->
                let docId = DocumentId.CreateNewId project.Id
                let version = VersionStamp.Create ()
                let docInfo =
                    DocumentInfo.Create
                        (docId, fileName, filePath = fileName,
                        loader = TextLoader.From (TextAndVersion.Create(sourceText, version)))
                docInfo :: docs
        lock lockObj ^ fun () ->
            let docIds = documents |> List.map ^ fun doc -> doc.Id
            transientDocuments.TryAdd (fileName, docIds) |> ignore
            transientDocumentIds.UnionWith docIds
        documents |> List.iter ^ fun doc -> self.AddDocument doc |> ignore
        true

    // new () = new FSharpWorkspace (Host.Mef.DesktopMefHostServices.DefaultServices)
    new () = new FSharpWorkspace (Host.Mef.MefHostServices.DefaultHost )

//    new (aggregator:HostServicesAggregator) = new FSharpWorkspace (aggregator.CreateHostServices())
//    new () = new FSharpWorkspace(FSharpHostService())

    member __.WorkspaceEvents = eventStream

    member self.ApplyTextChanges (docId:DocumentId) (textChanges:LinePositionSpanTextChange seq) = async {
        if Seq.isEmpty textChanges then return () else
        match self.CurrentSolution.TryGetDocument docId with
        | Some doc ->
            let! (sourceText:SourceText) = doc.GetTextAsync ()
            let sourceText =
                (sourceText, textChanges) ||> Seq.fold ^ fun sourceText change ->
                    let startOffset =
                        sourceText.Lines.GetPosition ^ LinePosition (change.StartLine, change.StartLine)
                    let endOffset =
                        sourceText.Lines.GetPosition ^ LinePosition (change.EndLine, change.EndColumn)
                    sourceText.WithChanges [| 
                        TextChange (TextSpan (startOffset, endOffset - startOffset), change.NewText) 
                    |]
            self.OnDocumentChanged (docId, sourceText)
        | None -> ()
    }

    member self.UpdateBuffer (bufferChange:BufferUpdate) = async {
        if isNull bufferChange.FileName then () else
        let documentIds = self.CurrentSolution.GetDocumentIdsWithFilePath bufferChange.FileName
        if not documentIds.IsEmpty then
            for docId in documentIds do
                let doc = self.CurrentSolution.GetDocument docId
                let! sourceText = doc.GetTextAsync () |> Async.AwaitTaskCorrect
                let startOffset =
                    sourceText.Lines.GetPosition ^ LinePosition (bufferChange.StartLine, bufferChange.StartLine)
                let endOffset = sourceText.Lines.GetPosition ^ LinePosition (bufferChange.EndLine, bufferChange.EndColumn)
                let sourceText =
                    sourceText.WithChanges [| 
                        TextChange (TextSpan (startOffset, endOffset - startOffset), bufferChange.NewText) 
                    |]
                self.OnDocumentChanged (docId, sourceText)
        else
            tryAddTransientDocument bufferChange.FileName bufferChange.NewText |> ignore
    }


//    member self.OnAddDocument (args:WorkspaceChangeEventArgs) =
//        match args.Kind with
//        || WorkspaceChangeKind.DocumentAdded -> 
//            let docId = args.DocumentId 
//            let doc = self.TryGetDocument
//
//            if documentIdCache.ContainsKey 
//            
//            documentIdCache.TryAdd 
//            (   doc.Name
//            ,   docId
//            ,   (fun _name docId -> docId)
//            )


    member self.OnWorkspaceChanged (args:WorkspaceChangeEventArgs) =
        let filename =
            match args.Kind with
            | WorkspaceChangeKind.DocumentAdded   -> (args.NewSolution.GetDocument args.DocumentId).FilePath
            | WorkspaceChangeKind.DocumentRemoved -> (args.OldSolution.GetDocument args.DocumentId).FilePath
            | _ -> String.Empty
        if String.IsNullOrEmpty filename then ()
        else
        lock lockObj ^ fun () ->
            match Dict.tryFind filename transientDocuments with
            | None -> ()
            | Some docIds ->
                transientDocuments.TryRemove filename |> ignore
                for docId in docIds do
                    self.RemoveDocument docId |> ignore
                    transientDocumentIds.Remove docId |> ignore


    member self.FindProjectsByFileName (fileName:string) =
        let dirInfo = (FileInfo fileName).Directory
        let candidates =
            self.CurrentSolution.Projects
                .GroupBy(fun project -> (FileInfo project.FilePath).Directory.FullName)
                .ToDictionary
                    (   (fun grouping -> grouping.Key)
                    ,   (fun grouping -> List.ofSeq grouping)
                    )
        let rec loop (dirInfo:DirectoryInfo) =
            if isNull dirInfo then [] else
            match candidates.TryGetValue dirInfo.FullName with
            | true, ps -> ps
            | _, _ -> loop dirInfo.Parent
        loop dirInfo :> _ seq

    override __.CanOpenDocuments = true
    override __.CanApplyChange _ = true


    member self.AddProject projectInfo =
        checkNullArg projectInfo "projectInfo"
        base.OnProjectAdded projectInfo
        base.UpdateReferencesAfterAdd ()
        self.CurrentSolution.GetProject projectInfo.Id

//    /// Adds a project to the workspace. All previous projects remain intact.
//    member self.AddProject(name : string) =
//        ProjectInfo.Create(ProjectId.CreateNewId(), VersionStamp.Create(), name, name, "FSharp")
//        |> self.AddProject

    /// Adds multiple projects to the workspace at once. All existing projects remain intact.
    member self.AddProjects (projectInfos: _ seq) =
        checkNullArg projectInfos "projectInfos"
        for info in projectInfos do
            self.OnProjectAdded info
        base.UpdateReferencesAfterAdd ()


    /// Adds an entire solution to the workspace, replacing any existing solution.
    member self.AddSolutionInfo (solutionInfo:SolutionInfo) =
        checkNullArg solutionInfo "solutionInfo"
        base.OnSolutionAdded solutionInfo
        base.UpdateReferencesAfterAdd()
        self.CurrentSolution


//    /// Adds an entire solution to the workspace, replacing any existing solution.
//    member self.AddSolution (solution:Solution) =
//        checkNullArg solution "solution"
//        let fn (arg:WorkspaceChangeEventArgs) =
//            arg.
//        base.OnSolutionAdded solution
//        base.UpdateReferencesAfterAdd()
//        self.CurrentSolution

//    member self.GetProjectOptionsFromFileName (filePath:string) =
//        documentIdCache.GetOrAdd(filePath, fun filePath -> filePath)
//        documentIdCache.
//        self.CurrentSolution



    member __.AddProjectReference (projectId, projectReference) =
        base.OnProjectReferenceAdded (projectId, projectReference)


    member __.AddMetadataReference (projectId, metadataReference) =
        base.OnMetadataReferenceAdded (projectId, metadataReference)


    member __.RemoveMetadataReference (projectId, metadataReference) =
        base.OnMetadataReferenceRemoved (projectId, metadataReference)


    member __.RemoveProject projectId = base.OnProjectRemoved projectId

    member __.SetCompilationOptions (projectId, options) = base.OnCompilationOptionsChanged(projectId, options)

    member __.SetParseOptions (projectId, parseOptions) = base.OnParseOptionsChanged(projectId, parseOptions)


    /// Adds a document to the workspace.
    member self.AddDocument (documentInfo:DocumentInfo) =
        checkNullArg documentInfo "documentInfo"
        base.OnDocumentAdded documentInfo
        self.CurrentSolution.GetDocument documentInfo.Id

    /// Adds a document to a project in the workspace.
    member __.AddDocument (projectId:ProjectId, name:string, text:SourceText) =
        checkNullArg projectId "projectId"
        checkNullArg name "name"
        checkNullArg text "text"
        let docInfo = 
            DocumentInfo.Create(
                DocumentId.CreateNewId projectId, name,
                loader = TextLoader.From ^ TextAndVersion.Create (text, VersionStamp.Create ())
            )
        base.OnDocumentAdded docInfo


    member __.RemoveDocument documentId = base.OnDocumentRemoved documentId

    /// Puts the specified document into the open state.
    override __.OpenDocument (docId, activate) =
        let doc = base.CurrentSolution.GetDocument docId
        if isNull doc then ()
        else
            let task = doc.GetTextAsync CancellationToken.None
            task.Wait CancellationToken.None
            let text = task.Result
            base.OnDocumentOpened (docId, text.Container, activate)


    

    /// Puts the specified document into the closed state.
    override __.CloseDocument docId =
        let doc = base.CurrentSolution.GetDocument docId
        if isNull doc then ()
        else
            let task = doc.GetTextAsync CancellationToken.None
            task.Wait CancellationToken.None
            let text = task.Result
            let versionTask = doc.GetTextVersionAsync CancellationToken.None
            versionTask.Wait CancellationToken.None
            let version = versionTask.Result
            let loader = TextLoader.From ^ TextAndVersion.Create(text, version, doc.FilePath)
            base.OnDocumentClosed (docId, loader)


    override __.OnDocumentTextChanged (doc:Document) =
        let time = DateTime.UtcNow
        documentTimeStamps.AddOrUpdate(doc.Id,time,(fun _docId _lastTime -> time)) |> ignore
        base.OnDocumentTextChanged doc
        

    member __.OnDocumentChanged (documentId, text) =
        base.OnDocumentTextChanged (documentId, text, PreservationMode.PreserveIdentity)


    member __.TryGetDocumentId filePath =
        let documentIds = base.CurrentSolution.GetDocumentIdsWithFilePath filePath
        match documentIds.FirstOrDefault () with
        | null -> None
        | docId -> Some docId


    member self.GetDocuments filePath =
        base.CurrentSolution.GetDocumentIdsWithFilePath(filePath)
            .Select(fun docId -> self.CurrentSolution.GetDocument docId)


    member self.TryGetDocument filePath =
        self.TryGetDocumentId filePath |> Option.map (fun docId -> self.CurrentSolution.GetDocument docId)


    member self.GetLastWriteTime docId =
        match documentTimeStamps.TryGetValue docId with
        | true, time -> Some time | _ -> None

    member self.GetDocumentProjectOptions (docId:DocumentId) = async {
        match  self.CurrentSolution.TryGetProject docId.ProjectId with
        | None -> 
            return None
        | Some proj -> 
            let! options =  proj.ToFSharpProjectOptionsAsync self
            return Some options
    }

    interface IDisposable with
        member __.Dispose () =
            subscriptions |> Seq.iter dispose
            subscriptions.Clear ()


