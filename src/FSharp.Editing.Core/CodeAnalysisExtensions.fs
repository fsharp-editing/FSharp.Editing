[<AutoOpen>]
/// Type Extensions and Additional Module Functions
module FSharp.Editing.CodeAnalysisExtensions

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.FSharp.Control
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing


(*  =================================== *)
(*   Microsoft.CodeAnalysis Extensions  *)
(*  =================================== *)


type SourceText with
    
    member self.Text with get () = self.ToString()

    member self.GetNthLine  (lineNum:int) : TextLine =
        self.Lines.[lineNum]

    member self.TryGetNthLine (lineNum:int) : TextLine option =
        if lineNum < 0 || lineNum >= self.Lines.Count then None else Some self.Lines.[lineNum]

    member self.GetLineAtPosition (position:LinePosition) =
        self.Lines.GetPosition position
        |> self.Lines.GetLineFromPosition

    member self.GetBytes () = 
        self.ToString() |> self.Encoding.GetBytes    


    member self.SubTextBetween(start,finish)  =
        self.GetSubText (TextSpan(start,finish-start))



type TextSpan with
    /// Compares two instances of Microsoft.CodeAnalysis.Text.TextSpan
    static member CompareTo (a:TextSpan,b:TextSpan) = a.CompareTo b




(*  ====================================== *)
(*   Microsoft.CodeAnalysis.Text.TextLine  *)
(*  ====================================== *)



type TextLine with

    static member isEmpty (tl:TextLine) = 
        TextLine() = tl


    static member From (text:string) =
        TextLine.FromSpan(SourceText.From(text),TextSpan(0,text.Length))

    

    member self.Length = self.End - self.Start

    member self.GetPosition (c:char) =
        let text = self.Text.ToString()
        match text.IndexOf c with
        | -1 -> None
        | x ->  Some (LinePosition (self.LineNumber,x))

    member __.Contains (pos:int)  =
        __.Start <= pos && pos <= __.End

        
    member self.InsertString (pos, str:string) =
        let chg = TextChange(TextSpan(pos,0),str)
        let moddedLine = self.Text.WithChanges(chg)
        TextLine.FromSpan(moddedLine,TextSpan.FromBounds(self.Start,self.Start+ moddedLine.Length))


    /// Returns the first non-whitespace position on the given line as an offset
    /// from the start of the line, or null if the line is empty or contains only
    /// whitespace.
    member  self.GetFirstNonWhitespaceOffset () : int=
          self.ToString () |> String.getfirstNonWhitespaceOffset
 
    
    /// returns -1 if there is not whitespace
    member self.GetFirstNonWhitespacePosition () =
        let pos = self.GetFirstNonWhitespaceOffset()
        if pos <> -1 then self.Start + pos else -1


    member self.GetLeadingWhitespace () =
        self.ToString() |> String.getLeadingWhitespace



    member self.GetLastNonWhitespaceOffset () : int =
        let text = self.ToString()
        [text.Length-1 ..  0]
        |> Seq.tryFind (fun idx -> Char.IsWhiteSpace text.[idx])
        |> Option.getOrElse -1            




    member self.TrimStart () =
        let idx = self.GetFirstNonWhitespaceOffset() 
        TextLine.FromSpan(self.Text,TextSpan(idx,self.Length-idx))



    member self.TrimEnd () =
        let idx = self.GetLastNonWhitespaceOffset() 
        TextLine.FromSpan(self.Text,TextSpan(0,idx))




    member self.StartsWith (prefix:string) =
        let idx = self.GetFirstNonWhitespaceOffset()
        if idx = -1 then false else
        let slice = self.Text.SubTextBetween(idx, prefix.Length-1)
        slice.ToString() = prefix


    member self.StartsWith (prefix:TextLine) =
        let prefix = prefix.Text.ToString() 
        let body = self.Text.ToString() 
        body.StartsWith prefix



    member self.SubSection (start,length) =
        TextLine.FromSpan(self.Text,TextSpan(start,length))

    member self.SubSection start =
        TextLine.FromSpan(self.Text,TextSpan(start,self.Length-start))





type Document with

    member self.ToDocumentInfoAsync () = async {
        let! src = self.GetTextAsync()
        let! version = self.GetTextVersionAsync()
        let textLoader = TextLoader.From (TextAndVersion.Create (src,version,self.FilePath))
        return DocumentInfo.Create
            (   id=self.Id
            ,   name=self.Name
            ,   folders=self.Folders
            ,   sourceCodeKind=self.SourceCodeKind
            ,   loader=textLoader
            ,   filePath=self.FilePath
            )
    }
    
    /// Retrieves the 'SourceText' for the document Synchronously
    member self.GetText () = 
        self.GetTextAsync () |> Async.AwaitTaskCorrect |> Async.RunSynchronously
    
    /// Retrieves the 'VersionStamp' for the document Synchronously
    member self.GetTextVerison () =
        self.GetTextVersionAsync () |> Async.AwaitTaskCorrect |> Async.RunSynchronously
    
    member self.GetBytesAsync () = async {
        let! src = self.GetTextAsync ()
        return src.GetBytes ()
    }


type TextDocument with

    member self.ToDocumentInfoAsync () = async {
        let! src = self.GetTextAsync()
        let! version = self.GetTextVersionAsync()
        let textLoader = TextLoader.From (TextAndVersion.Create (src,version,self.FilePath))
        return DocumentInfo.Create
            (   id=self.Id
            ,   name=self.Name
            ,   folders=self.Folders
            ,   loader=textLoader
            ,   filePath=self.FilePath
            )
    }

type Solution with 

    member self.GetDocumentFilePath (docId:DocumentId) = 
        let target =
            self.Projects
            |> Seq.collect ^ fun proj -> proj.Documents
            |> Seq.tryFind ^ fun doc -> doc.Id = docId 
        target |> Option.map ^ fun doc -> doc.FilePath    
        

    member self.GetProjectsContainingDocument (docId:DocumentId) =
        self.Projects |> Seq.filter (fun proj -> proj.ContainsDocument docId)

    /// Try to get a document inside the solution using the DocumentId
    member self.TryGetDocument (docId:DocumentId) =
        if self.ContainsDocument docId then Some (self.GetDocument docId) else None

    /// Try to get a document inside the solution using the document's name
    member self.TryGetDocument docName =
        self.Projects |> Seq.tryPick ^ fun proj ->
            proj.Documents |> Seq.tryFind ^ fun doc -> doc.Name = docName 

    /// Get a project inside the solution using the project's name
    member self.GetProject projectName =
        self.Projects |> Seq.find ^ fun proj -> proj.Name = projectName

    /// Try to get a project inside the solution using the project's name
    member self.TryGetProject projectName =
        self.Projects |> Seq.tryFind ^ fun proj -> proj.Name = projectName

    /// Try to get a project inside the solution using the project's id
    member self.TryGetProject (projId:ProjectId) =
        if self.ContainsProject projId then Some (self.GetProject projId) else None

    /// Sequence of DocumentInfo for all source files and addtional documents
    /// from all of the projects within the solution
    member self.GetAllDocumentsAsync () = async {
        let docInfoAsyncs = 
            self.Projects |> Seq.collect ^ fun proj ->
                Seq.append
                    (proj.Documents |> Seq.map ^ fun doc -> doc.ToDocumentInfoAsync ())
                    (proj.AdditionalDocuments |> Seq.map ^ fun doc -> doc.ToDocumentInfoAsync ())
        let! docInfo = Async.Parallel docInfoAsyncs
        return  docInfo
    }

    member self.GetAllDocuments () = 
        self.GetAllDocumentsAsync () |> Async.RunSynchronously


    member self.GetProjectsThatThisProjectTransitivelyDependsOn (projId:ProjectId) =
        let graph = self.GetProjectDependencyGraph ()
        graph.GetProjectsThatThisProjectTransitivelyDependsOn projId



type Project with

    /// The list of all other projects within the same solution that reference this project.
    member this.GetDependentProjects () =
        this.Solution.GetProjectDependencyGraph().GetProjectsThatDirectlyDependOnThisProject(this.Id)
        |> Seq.map this.Solution.GetProject

    member self.GetDocumentFilePaths () = 
        self.Documents |> Seq.map ^ fun doc -> doc.FilePath

    member self.GetDocument docName =
        self.Documents |> Seq.find ^ fun doc -> doc.Name = docName

    member self.TryGetDocument docName =
        self.Documents |> Seq.tryFind ^ fun doc -> doc.Name = docName

        
    member self.ToProjectInfoAsync () = async {
        let! docInfos = 
            self.Documents |> Seq.map ^ fun doc -> doc.ToDocumentInfoAsync () 
            |> Async.Parallel
        let! additionalInfos = 
            self.AdditionalDocuments |> Seq.map ^ fun doc -> doc.ToDocumentInfoAsync ()  
            |> Async.Parallel
        return
            ProjectInfo.Create
                (   self.Id
                ,   self.Version
                ,   self.Name
                ,   self.AssemblyName
                ,   self.Language
                ,   self.FilePath
                ,   outputFilePath = self.OutputFilePath
                ,   projectReferences = self.ProjectReferences
                ,   metadataReferences = self.MetadataReferences
                ,   analyzerReferences = self.AnalyzerReferences
                ,   documents = docInfos
                ,   additionalDocuments = additionalInfos
                ,   compilationOptions = self.CompilationOptions
                ,   parseOptions = self.ParseOptions
                ,   isSubmission = self.IsSubmission
                )
    }


    member self.ToProjectInfo () = self.ToProjectInfoAsync() |> Async.RunSynchronously


let internal toFSharpProjectOptionsAsync (workspace:'a :> Workspace) (projInfo:ProjectInfo) : FSharpProjectOptions Async = async {
    let projectStore = Dictionary<ProjectId,FSharpProjectOptions>()
    let loadTime = System.DateTime.Now
    let rec generate (projInfo:ProjectInfo) : FSharpProjectOptions Async = async {
        let getProjectRefs (projInfo:ProjectInfo): Async<(string * FSharpProjectOptions)[]> = async {
            let pathsAndOptions =
                projInfo.ProjectReferences
                |> Seq.choose ^ fun pref -> workspace.CurrentSolution.TryGetProject pref.ProjectId
                |> Seq.map ^ fun proj -> async {
                    let! proj = proj.ToProjectInfoAsync ()
                    if projectStore.ContainsKey proj.Id then
                        return (proj.OutputFilePath, projectStore.[proj.Id])
                    else
                        let! fsinfo = generate proj
                        projectStore.Add( proj.Id,fsinfo)
                        return (proj.OutputFilePath, fsinfo)
                } 
            let! fsharpOptionArray = pathsAndOptions |> Async.Parallel
            return fsharpOptionArray
        }
        let! referencedProjects =  getProjectRefs projInfo
        return
            {   ProjectFileName = projInfo.FilePath
                ProjectFileNames = projInfo.Documents |> Seq.map (fun doc -> doc.FilePath) |> Array.ofSeq
                OtherOptions = [||]
                ReferencedProjects = referencedProjects
                IsIncompleteTypeCheckEnvironment = false
                UseScriptResolutionRules = false
                LoadTime = loadTime
                UnresolvedReferences = None
                OriginalLoadReferences = []
    //            ExtraProjectInfo = None
                // I think this is supposed to hold the workspace?
                ExtraProjectInfo = Some (workspace :> _)
            }
    }
    let! fsprojOptions = generate projInfo
    projectStore.Clear ()
    return fsprojOptions
}

let internal toFSharpProjectOptions (workspace:'a :> Workspace) (projInfo:ProjectInfo): FSharpProjectOptions =
    let projectStore = Dictionary<ProjectId,FSharpProjectOptions>()
    let loadTime = System.DateTime.Now
    let rec generate (projInfo:ProjectInfo) : FSharpProjectOptions =
        let getProjectRefs (projInfo:ProjectInfo): (string * FSharpProjectOptions)[] =
            projInfo.ProjectReferences
            |> Seq.choose ^ fun pref -> workspace.CurrentSolution.TryGetProject pref.ProjectId
            |> Seq.map ^ fun proj ->
                let proj = proj.ToProjectInfo ()
                if projectStore.ContainsKey proj.Id then
                    (proj.OutputFilePath, projectStore.[proj.Id])
                else
                    let fsinfo = generate proj
                    projectStore.Add( proj.Id,fsinfo)
                    (proj.OutputFilePath, fsinfo)
            |> Array.ofSeq
        {   ProjectFileName = projInfo.FilePath
            ProjectFileNames = projInfo.Documents |> Seq.map (fun doc -> doc.FilePath) |> Array.ofSeq
            OtherOptions = [||]
            ReferencedProjects =  getProjectRefs projInfo
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = loadTime
            UnresolvedReferences = None
            OriginalLoadReferences = []
//            ExtraProjectInfo = None
            // I think this is supposed to hold the workspace?
            ExtraProjectInfo = Some (workspace :> _)
        }
    let fsprojOptions = generate projInfo
    projectStore.Clear ()
    fsprojOptions




type ProjectInfo with
    /// Converts a CodeAnalysis ProjectInfo into FSharpProjectOptions for the FSharp.Compiler.Service
    member self.ToFSharpProjectOptions (workspace:'a :> Workspace) : FSharpProjectOptions =
        toFSharpProjectOptions workspace self



    /// Converts a CodeAnalysis Project into FSharpProjectOptions for the FSharp.Compiler.Service
    member self.ToFSharpProjectOptionsAsync (workspace:'a :> Workspace) = async {
        return! toFSharpProjectOptionsAsync workspace self 
    }
    

type Project with
    /// Converts a CodeAnalysis Project into FSharpProjectOptions for the FSharp.Compiler.Service
    member self.ToFSharpProjectOptions (workspace:'a :> Workspace) : FSharpProjectOptions =
        self.ToProjectInfo() |>  toFSharpProjectOptions workspace 

    
    
    /// Converts a CodeAnalysis Project into FSharpProjectOptions for the FSharp.Compiler.Service
    member self.ToFSharpProjectOptionsAsync (workspace:'a :> Workspace) = async {
        let! projectInfo = self.ToProjectInfoAsync()
        return! 
            toFSharpProjectOptionsAsync workspace projectInfo 
    }
    




type Workspace with

    member self.ProjectDictionary () =
        let dict = Dictionary<_,_>()
        self.CurrentSolution.Projects
        |> Seq.iter ^ fun proj -> dict.Add(proj.FilePath,proj.Id)
        dict


    member self.ProjectPaths () =
        self.CurrentSolution.Projects |> Seq.map ^ fun proj -> proj.FilePath


    member self.GetProjectIdFromPath path : ProjectId option =
        let dict = self.ProjectDictionary ()
        Dict.tryFind path dict


    /// checks the workspace for projects located at the provided paths.
    /// returns a mapping of the projectId and path of projects inside the workspace
    /// and a list of the paths to projects the workspace doesn't include
    member self.GetProjectIdsFromPaths paths =
        let dict = self.ProjectDictionary ()
        let pathsInside,pathsOutside = paths |> List.ofSeq |> List.partition ^ fun path -> dict.ContainsKey path
        let idmap = pathsInside |> Seq.map ^ fun path -> dict.[path]
        idmap, pathsOutside


