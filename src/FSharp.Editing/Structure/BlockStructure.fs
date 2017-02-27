namespace FSharp.Editing.Structure

open System.Collections.Immutable
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Structure

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.UntypedAstUtils
open FSharp.Editing.UntypedAstUtils.Outlining

//module internal BlockStructure =
//    let scopeToBlockType = function
//    | Scope.Open -> BlockTypes.Imports
//    | Scope.Namespace
//    | Scope.Module -> BlockTypes.Namespace 
//    | Scope.Record
//    | Scope.Interface
//    | Scope.TypeExtension
//    | Scope.RecordDefn
//    | Scope.CompExpr
//    | Scope.ObjExpr
//    | Scope.UnionDefn
//    | Scope.Attribute
//    | Scope.Type -> BlockTypes.Type
//    | Scope.New
//    | Scope.RecordField
//    | Scope.Member -> BlockTypes.Member
//    | Scope.LetOrUse
//    | Scope.Match
//    | Scope.MatchClause
//    | Scope.EnumCase
//    | Scope.UnionCase
//    | Scope.MatchLambda
//    | Scope.ThenInIfThenElse
//    | Scope.ElseInIfThenElse
//    | Scope.TryWith
//    | Scope.TryInTryWith
//    | Scope.WithInTryWith
//    | Scope.TryFinally
//    | Scope.TryInTryFinally
//    | Scope.FinallyInTryFinally
//    | Scope.IfThenElse-> BlockTypes.Conditional
//    | Scope.Tuple
//    | Scope.ArrayOrList
//    | Scope.CompExprInternal
//    | Scope.Quote
//    | Scope.SpecialFunc
//    | Scope.Lambda
//    | Scope.LetOrUseBang
//    | Scope.Val
//    | Scope.YieldOrReturn
//    | Scope.YieldOrReturnBang
//    | Scope.TryWith -> BlockTypes.Expression
//    | Scope.Do -> BlockTypes.Statement
//    | Scope.While
//    | Scope.For -> BlockTypes.Loop
//    | Scope.HashDirective -> BlockTypes.PreprocessorRegion
//    | Scope.Comment
//    | Scope.XmlDocComment -> BlockTypes.Comment

//    let createBlockSpans (sourceText:SourceText) (parsedInput:Ast.ParsedInput) =
//        let linetext = sourceText.Lines |> Seq.map (fun x -> x.ToString()) |> Seq.toArray
        
//        getOutliningRanges linetext parsedInput
//        |> Seq.distinctBy (fun x -> x.Range.StartLine)
//        |> Seq.choose (fun scopeRange -> 
//            // the range of text to collapse
//            let textSpan = CommonRoslynHelpers.TryFSharpRangeToTextSpan(sourceText, scopeRange.CollapseRange)
//            // the range of the entire expression
//            let hintSpan = CommonRoslynHelpers.TryFSharpRangeToTextSpan(sourceText, scopeRange.Range)
//            match textSpan,hintSpan with
//            | Some textSpan, Some hintSpan ->
//                let line = sourceText.Lines.GetLineFromPosition  textSpan.Start
//                let bannerText =
//                    match Option.ofNullable (line.Span.Intersection textSpan) with
//                    | Some span -> sourceText.GetSubText(span).ToString()+"..."
//                    | None -> "..."

//                Some <| (BlockSpan(scopeToBlockType scopeRange.Scope, true, textSpan,hintSpan,bannerText):BlockSpan)
//            | _, _ -> None
//        )

