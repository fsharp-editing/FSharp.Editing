module FSharp.Editing.Documentation.XmlDocGenerator


// A command filter for the editor. 
// Command filters get an opportunity to observe and handle commands before and after the editor acts on them.

open System
open System.Diagnostics
open System.Runtime.InteropServices
open FSharp.Editing
open FSharp.Editing.Documentation
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text

//
//let generateXmlDocStub (languageService:FSharpLanguageService) (typedChar:char) (position:LinePosition) (source:SourceText) (filePath:string) =
//    match typedChar with
//    | ('/' | '<') as lastChar ->
//        let curLine = source.GetLineAtPosition position
//        let lineWithLastCharInserted = curLine.InsertString(position.Character,string typedChar)
//
//        match XmlDocComment.isBlank lineWithLastCharInserted with
//        | Some i when i = position.Character ->
//            asyncMaybe {
//
//                let! doc = languageService.Workspace.TryGetDocument filePath
//                let! options languageService.Workspace.Pr
//                // XmlDocable line #1 are 1-based, editor is 0-based
//                let! parseResults = languageService.ParseFileInProject (fileName, project)
////                let! source = languageService.Workspace.get
//                let! xmlDocables = XmlDocParser.getXmlDocables (source, parseResults.ParseTree) |> liftAsync
//                let xmlDocablesBelowThisLine = 
//                    // +1 because looking below current line for e.g. a 'member'
//                    xmlDocables |> List.filter (fun (XmlDocable(line,_indent,_paramNames)) -> line = curLineNum+1) 
//                match xmlDocablesBelowThisLine with
//                | [] -> ()
//                | XmlDocable(_line,indent,paramNames)::_t ->
//                    // delete the slashes the user typed (they may be indented wrong)
//                    wpfTextView.TextBuffer.Delete(wpfTextView.Caret.Position.BufferPosition.GetContainingLine().Extent.Span) |> ignore
//                    // add the new xmldoc comment
//                    let toInsert = new System.Text.StringBuilder()
//                    toInsert.Append(' ', indent).AppendLine("/// <summary>")
//                            .Append(' ', indent).AppendLine("/// ")
//                            .Append(' ', indent).Append("/// </summary>") |> ignore
//                    paramNames
//                    |> List.iter (fun p ->
//                        toInsert.AppendLine().Append(' ', indent).Append(sprintf "/// <param name=\"%s\"></param>" p) |> ignore)
//                    let _newSS = wpfTextView.TextBuffer.Insert(wpfTextView.Caret.Position.BufferPosition.Position, toInsert.ToString())
//                    // move the caret to between the summary tags
//                    let lastLine = wpfTextView.Caret.Position.BufferPosition.GetContainingLine()
//                    let middleSummaryLine = wpfTextView.TextSnapshot.GetLineFromLineNumber(lastLine.LineNumber - 1 - paramNames.Length)
//                    wpfTextView.Caret.MoveTo(wpfTextView.GetTextViewLineContainingBufferPosition(middleSummaryLine.Start)) |> ignore
//            } 
//            |> Async.Ignore 
//            |> Async.StartImmediateSafe
//        | Some _ | None -> ()
//    | _ -> ()
//passThruToEditor.Exec(&pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut)
//
//member __.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD [], pCmdText: IntPtr) =
//passThruToEditor.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText)
//
//
