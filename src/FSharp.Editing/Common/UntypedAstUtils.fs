module FSharp.Editing.UntypedAstUtils

open Microsoft.FSharp.Compiler.Ast
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range

type Range.range with
    member inline x.IsEmpty = x.StartColumn = x.EndColumn && x.StartLine = x.EndLine 

type internal ShortIdent = string
type internal Idents = ShortIdent[]

let internal longIdentToArray (longIdent: LongIdent): Idents =
    longIdent |> Seq.map string |> Seq.toArray

    /// An recursive pattern that collect all sequential expressions to avoid StackOverflowException
let rec (|Sequentials|_|) = function
    | SynExpr.Sequential(_, _, e, Sequentials es, _) ->
        Some(e::es)
    | SynExpr.Sequential(_, _, e1, e2, _) ->
        Some [e1; e2]
    | _ -> None

let (|ConstructorPats|) = function
    | SynConstructorArgs.Pats ps -> ps
    | SynConstructorArgs.NamePatPairs(xs, _) -> List.map snd xs

/// Returns all Idents and LongIdents found in an untyped AST.
let internal getLongIdents (input: ParsedInput option) : IDictionary<Range.pos, Idents> =
    let identsByEndPos = Dictionary<Range.pos, Idents>()

    let addLongIdent (longIdent: LongIdent) =
        let idents = longIdentToArray longIdent
        for ident in longIdent do
            identsByEndPos.[ident.idRange.End] <- idents

    let addLongIdentWithDots (LongIdentWithDots (longIdent, lids) as value) =
        match longIdentToArray longIdent with
        | [||] -> ()
        | [|_|] as idents -> identsByEndPos.[value.Range.End] <- idents
        | idents ->
            for dotRange in lids do
                identsByEndPos.[Range.mkPos dotRange.EndLine (dotRange.EndColumn - 1)] <- idents
            identsByEndPos.[value.Range.End] <- idents

    let addIdent (ident: Ident) =
        identsByEndPos.[ident.idRange.End] <- [|ident.idText|]

    let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
        List.iter walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, _, decls, _, attrs, _, _)) =
        List.iter walkAttribute attrs
        List.iter walkSynModuleDecl decls

    and walkAttribute (attr: SynAttribute) =
        addLongIdentWithDots attr.TypeName
        walkExpr attr.ArgExpr

    and walkTyparDecl (SynTyparDecl.TyparDecl (attrs, typar)) =
        List.iter walkAttribute attrs
        walkTypar typar

    and walkTypeConstraint = function
        | SynTypeConstraint.WhereTyparIsValueType (t, _)
        | SynTypeConstraint.WhereTyparIsReferenceType (t, _)
        | SynTypeConstraint.WhereTyparIsUnmanaged (t, _)
        | SynTypeConstraint.WhereTyparSupportsNull (t, _)
        | SynTypeConstraint.WhereTyparIsComparable (t, _)
        | SynTypeConstraint.WhereTyparIsEquatable (t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, _)
        | SynTypeConstraint.WhereTyparSubtypeOfType (t, ty, _) -> walkTypar t; walkType ty
        | SynTypeConstraint.WhereTyparIsEnum (t, ts, _)
        | SynTypeConstraint.WhereTyparIsDelegate (t, ts, _) -> walkTypar t; List.iter walkType ts
        | SynTypeConstraint.WhereTyparSupportsMember (ts, sign, _) -> List.iter walkType ts; walkMemberSig sign

    and walkPat = function
        | SynPat.Tuple (pats, _)
        | SynPat.ArrayOrList (_, pats, _)
        | SynPat.Ands (pats, _) -> List.iter walkPat pats
        | SynPat.Named (pat, ident, _, _, _) ->
            walkPat pat
            addIdent ident
        | SynPat.Typed (pat, t, _) ->
            walkPat pat
            walkType t
        | SynPat.Attrib (pat, attrs, _) ->
            walkPat pat
            List.iter walkAttribute attrs
        | SynPat.Or (pat1, pat2, _) -> List.iter walkPat [pat1; pat2]
        | SynPat.LongIdent (ident, _, typars, ConstructorPats pats, _, _) ->
            addLongIdentWithDots ident
            typars
            |> Option.iter (fun (SynValTyparDecls (typars, _, constraints)) ->
                 List.iter walkTyparDecl typars
                 List.iter walkTypeConstraint constraints)
            List.iter walkPat pats
        | SynPat.Paren (pat, _) -> walkPat pat
        | SynPat.IsInst (t, _) -> walkType t
        | SynPat.QuoteExpr(e, _) -> walkExpr e
        | _ -> ()

    and walkTypar (Typar (_, _, _)) = ()

    and walkBinding (SynBinding.Binding (_, _, _, _, attrs, _, _, pat, returnInfo, e, _, _)) =
        List.iter walkAttribute attrs
        walkPat pat
        walkExpr e
        returnInfo |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

    and walkInterfaceImpl (InterfaceImpl(_, bindings, _)) = List.iter walkBinding bindings

    and walkIndexerArg = function
        | SynIndexerArg.One e -> walkExpr e
        | SynIndexerArg.Two (e1, e2) -> List.iter walkExpr [e1; e2]

    and walkType = function
        | SynType.Array (_, t, _)
        | SynType.HashConstraint (t, _)
        | SynType.MeasurePower (t, _, _) -> walkType t
        | SynType.Fun (t1, t2, _)
        | SynType.MeasureDivide (t1, t2, _) -> walkType t1; walkType t2
        | SynType.LongIdent ident -> addLongIdentWithDots ident
        | SynType.App (ty, _, types, _, _, _, _) -> walkType ty; List.iter walkType types
        | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
        | SynType.Tuple (ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
        | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
            walkType t; List.iter walkTypeConstraint typeConstraints
        | _ -> ()

    and walkClause (Clause (pat, e1, e2, _, _)) =
        walkPat pat
        walkExpr e2
        e1 |> Option.iter walkExpr

    and walkSimplePats = function
        | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
        | SynSimplePats.Typed (pats, ty, _) -> 
            walkSimplePats pats
            walkType ty

    and walkExpr = function
        | SynExpr.Paren (e, _, _, _)
        | SynExpr.Quote (_, _, e, _, _)
        | SynExpr.Typed (e, _, _)
        | SynExpr.InferredUpcast (e, _)
        | SynExpr.InferredDowncast (e, _)
        | SynExpr.AddressOf (_, e, _, _)
        | SynExpr.DoBang (e, _)
        | SynExpr.YieldOrReturn (_, e, _)
        | SynExpr.ArrayOrListOfSeqExpr (_, e, _)
        | SynExpr.CompExpr (_, _, e, _)
        | SynExpr.Do (e, _)
        | SynExpr.Assert (e, _)
        | SynExpr.Lazy (e, _)
        | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
        | SynExpr.Lambda (_, _, pats, e, _) ->
            walkSimplePats pats
            walkExpr e
        | SynExpr.New (_, t, e, _)
        | SynExpr.TypeTest (e, t, _)
        | SynExpr.Upcast (e, t, _)
        | SynExpr.Downcast (e, t, _) -> walkExpr e; walkType t
        | SynExpr.Tuple (es, _, _)
        | Sequentials es
        | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
        | SynExpr.App (_, _, e1, e2, _)
        | SynExpr.TryFinally (e1, e2, _, _, _)
        | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.iter (fun ((ident, _), e, _) ->
                        addLongIdentWithDots ident
                        e |> Option.iter walkExpr)
        | SynExpr.Ident ident -> addIdent ident
        | SynExpr.ObjExpr(ty, argOpt, bindings, ifaces, _, _) ->
            argOpt |> Option.iter (fun (e, ident) ->
                walkExpr e
                ident |> Option.iter addIdent)
            walkType ty
            List.iter walkBinding bindings
            List.iter walkInterfaceImpl ifaces
        | SynExpr.LongIdent (_, ident, _, _) -> addLongIdentWithDots ident
        | SynExpr.For (_, ident, e1, _, e2, e3, _) ->
            addIdent ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.ForEach (_, _, _, pat, e1, e2, _) ->
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) ->
            List.iter walkClause synMatchClauseList
        | SynExpr.Match (_, e, synMatchClauseList, _, _) ->
            walkExpr e
            List.iter walkClause synMatchClauseList
        | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
            List.iter walkType tys; walkExpr e
        | SynExpr.LetOrUse (_, _, bindings, e, _) ->
            List.iter walkBinding bindings; walkExpr e
        | SynExpr.TryWith (e, _, clauses, _, _, _, _) ->
            List.iter walkClause clauses;  walkExpr e
        | SynExpr.IfThenElse (e1, e2, e3, _, _, _, _) ->
            List.iter walkExpr [e1; e2]
            e3 |> Option.iter walkExpr
        | SynExpr.LongIdentSet (ident, e, _)
        | SynExpr.DotGet (e, _, ident, _) ->
            addLongIdentWithDots ident
            walkExpr e
        | SynExpr.DotSet (e1, idents, e2, _) ->
            walkExpr e1
            addLongIdentWithDots idents
            walkExpr e2
        | SynExpr.DotIndexedGet (e, args, _, _) ->
            walkExpr e
            List.iter walkIndexerArg args
        | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
            walkExpr e1
            List.iter walkIndexerArg args
            walkExpr e2
        | SynExpr.NamedIndexedPropertySet (ident, e1, e2, _) ->
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2]
        | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, _) ->
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.LetOrUseBang (_, _, _, pat, e1, e2, _) ->
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.TraitCall (ts, sign, e, _) ->
            List.iter walkTypar ts
            walkMemberSig sign
            walkExpr e
        | SynExpr.Const (SynConst.Measure(_, m), _) -> walkMeasure m
        | _ -> ()

    and walkMeasure = function
        | SynMeasure.Product (m1, m2, _)
        | SynMeasure.Divide (m1, m2, _) -> walkMeasure m1; walkMeasure m2
        | SynMeasure.Named (longIdent, _) -> addLongIdent longIdent
        | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
        | SynMeasure.Power (m, _, _) -> walkMeasure m
        | SynMeasure.Var (ty, _) -> walkTypar ty
        | SynMeasure.One
        | SynMeasure.Anon _ -> ()

    and walkSimplePat = function
        | SynSimplePat.Attrib (pat, attrs, _) ->
            walkSimplePat pat
            List.iter walkAttribute attrs
        | SynSimplePat.Typed(pat, t, _) ->
            walkSimplePat pat
            walkType t
        | _ -> ()

    and walkField (SynField.Field(attrs, _, _, t, _, _, _, _)) =
        List.iter walkAttribute attrs
        walkType t

    and walkValSig (SynValSig.ValSpfn(attrs, _, _, t, SynValInfo(argInfos, argInfo), _, _, _, _, _, _)) =
        List.iter walkAttribute attrs
        walkType t
        argInfo :: (argInfos |> List.concat)
        |> List.map (fun (SynArgInfo(attrs, _, _)) -> attrs)
        |> List.concat
        |> List.iter walkAttribute

    and walkMemberSig = function
        | SynMemberSig.Inherit (t, _)
        | SynMemberSig.Interface(t, _) -> walkType t
        | SynMemberSig.Member(vs, _, _) -> walkValSig vs
        | SynMemberSig.ValField(f, _) -> walkField f
        | SynMemberSig.NestedType(SynTypeDefnSig.TypeDefnSig (info, repr, memberSigs, _), _) ->
            let isTypeExtensionOrAlias =
                match repr with
                | SynTypeDefnSigRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev _, _)
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.TyconAbbrev, _, _)
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _) -> true
                | _ -> false
            walkComponentInfo isTypeExtensionOrAlias info
            walkTypeDefnSigRepr repr
            List.iter walkMemberSig memberSigs

    and walkMember = function
        | SynMemberDefn.AbstractSlot (valSig, _, _) -> walkValSig valSig
        | SynMemberDefn.Member (binding, _) -> walkBinding binding
        | SynMemberDefn.ImplicitCtor (_, attrs, pats, _, _) ->
            List.iter walkAttribute attrs
            List.iter walkSimplePat pats
        | SynMemberDefn.ImplicitInherit (t, e, _, _) -> walkType t; walkExpr e
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
        | SynMemberDefn.Interface (t, members, _) ->
            walkType t
            members |> Option.iter (List.iter walkMember)
        | SynMemberDefn.Inherit (t, _, _) -> walkType t
        | SynMemberDefn.ValField (field, _) -> walkField field
        | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
        | SynMemberDefn.AutoProperty (attrs, _, _, t, _, _, _, _, e, _, _) ->
            List.iter walkAttribute attrs
            Option.iter walkType t
            walkExpr e
        | _ -> ()

    and walkEnumCase (EnumCase(attrs, _, _, _, _)) = List.iter walkAttribute attrs

    and walkUnionCaseType = function
        | SynUnionCaseType.UnionCaseFields fields -> List.iter walkField fields
        | SynUnionCaseType.UnionCaseFullType (t, _) -> walkType t

    and walkUnionCase (SynUnionCase.UnionCase (attrs, _, t, _, _, _)) =
        List.iter walkAttribute attrs
        walkUnionCaseType t

    and walkTypeDefnSimple = function
        | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.iter walkEnumCase cases
        | SynTypeDefnSimpleRepr.Union (_, cases, _) -> List.iter walkUnionCase cases
        | SynTypeDefnSimpleRepr.Record (_, fields, _) -> List.iter walkField fields
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> walkType t
        | _ -> ()

    and walkComponentInfo isTypeExtensionOrAlias (ComponentInfo(attrs, typars, constraints, longIdent, _, _, _, _)) =
        List.iter walkAttribute attrs
        List.iter walkTyparDecl typars
        List.iter walkTypeConstraint constraints
        if isTypeExtensionOrAlias then
            addLongIdent longIdent

    and walkTypeDefnRepr = function
        | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
        | SynTypeDefnRepr.Simple(defn, _) -> walkTypeDefnSimple defn
        | SynTypeDefnRepr.Exception _ -> ()

    and walkTypeDefnSigRepr = function
        | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
        | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn
        | SynTypeDefnSigRepr.Exception _ -> ()

    and walkTypeDefn (TypeDefn (info, repr, members, _)) =
        let isTypeExtensionOrAlias =
            match repr with
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _)
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
            | _ -> false
        walkComponentInfo isTypeExtensionOrAlias info
        walkTypeDefnRepr repr
        List.iter walkMember members

    and walkSynModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
        | SynModuleDecl.NestedModule (info, _, modules, _, _) ->
            walkComponentInfo false info
            List.iter walkSynModuleDecl modules
        | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
        | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
        | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
        | SynModuleDecl.Attributes (attrs, _) -> List.iter walkAttribute attrs
        | _ -> ()

    match input with
    | Some (ParsedInput.ImplFile input) ->
         walkImplFileInput input
    | _ -> ()
    //debug "%A" idents
    identsByEndPos :> _

let getLongIdentAt ast pos =
    let idents = getLongIdents (Some ast)
    match idents.TryGetValue pos with
    | true, idents -> Some idents
    | _ -> None

/// Returns ranges of all quotations found in an untyped AST
let getQuotationRanges ast =
    let quotationRanges = ResizeArray()

    let rec visitExpr = function
        | SynExpr.LongIdentSet (_, expr, _)
        | SynExpr.Typed (expr, _, _)
        | SynExpr.Paren (expr, _, _, _)
        | SynExpr.New (_, _, expr, _)
        | SynExpr.ArrayOrListOfSeqExpr (_, expr, _)
        | SynExpr.CompExpr (_, _, expr, _)
        | SynExpr.ForEach (_, _, _, _, _, expr(*body*), _)
        | SynExpr.YieldOrReturn (_, expr, _)
        | SynExpr.YieldOrReturnFrom (_, expr, _)
        | SynExpr.Do (expr, _)
        | SynExpr.DoBang (expr, _)
        | SynExpr.Downcast (expr, _, _)
        | SynExpr.For (_, _, _, _, _, expr, _)
        | SynExpr.Lazy (expr, _)
        | SynExpr.Assert (expr, _)
        | SynExpr.TypeApp (expr, _, _, _, _, _, _)
        | SynExpr.DotSet (_, _, expr, _)
        | SynExpr.DotIndexedSet (_, _, expr, _, _, _)
        | SynExpr.NamedIndexedPropertySet (_, _, expr, _)
        | SynExpr.DotNamedIndexedPropertySet (_, _, _, expr, _)
        | SynExpr.TypeTest (expr, _, _)
        | SynExpr.Upcast (expr, _, _)
        | SynExpr.InferredUpcast (expr, _)
        | SynExpr.InferredDowncast (expr, _)
        | SynExpr.Lambda (_, _, _, expr, _)
        | SynExpr.AddressOf (_, expr, _, _) ->
            visitExpr expr
        | SynExpr.App (_,_, expr1(*funcExpr*),expr2(*argExpr*), _)
        | SynExpr.LetOrUseBang (_, _, _, _,expr1(*rhsExpr*),expr2(*body*), _)
        | SynExpr.TryFinally (expr1, expr2, _, _, _)
        | SynExpr.While (_, expr1, expr2, _) ->
            visitExpr expr1; visitExpr expr2
        | SynExpr.Tuple (exprs, _, _)
        | SynExpr.ArrayOrList (_, exprs, _)
        | Sequentials  exprs ->
            List.iter visitExpr exprs
        | SynExpr.TryWith (expr, _, clauses, _, _, _, _)
        | SynExpr.Match (_, expr, clauses, _, _) ->
            visitExpr expr; visitMatches clauses
        | SynExpr.IfThenElse (cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            visitExpr cond; visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr
        | SynExpr.LetOrUse (_, _, bindings, body, _) -> visitBindindgs bindings; visitExpr body
        | SynExpr.Quote (_, _isRaw, _quotedExpr, _, range) -> quotationRanges.Add range
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (_, _, bindings, _, _ , _) -> visitBindindgs bindings
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (_, expr, _) -> expr) |> List.iter visitExpr
        | _ -> ()

    and visitBinding (Binding(_, _, _, _, _, _, _, _, _, body, _, _)) = visitExpr body
    and visitBindindgs = List.iter visitBinding

    and visitPattern = function
        | SynPat.QuoteExpr (expr, _) -> visitExpr expr
        | SynPat.Named (pat, _, _, _, _)
        | SynPat.Paren (pat, _)
        | SynPat.Typed (pat, _, _) -> visitPattern pat
        | SynPat.Ands (pats, _)
        | SynPat.Tuple (pats, _)
        | SynPat.ArrayOrList (_, pats, _) -> List.iter visitPattern pats
        | SynPat.Or (pat1, pat2, _) -> visitPattern pat1; visitPattern pat2
        | SynPat.LongIdent (_, _, _, ctorArgs, _, _) ->
            match ctorArgs with
            | SynConstructorArgs.Pats pats -> List.iter visitPattern pats
            | SynConstructorArgs.NamePatPairs(xs, _) ->
                xs |> List.map snd |> List.iter visitPattern
        | SynPat.Record(xs, _) -> xs |> List.map snd |> List.iter visitPattern
        | _ -> ()

    and visitMatch (SynMatchClause.Clause (pat, _, expr, _, _)) = visitPattern pat; visitExpr expr

    and visitMatches = List.iter visitMatch

    let visitMember = function
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
        | SynMemberDefn.Member (binding, _) -> visitBinding binding
        | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpr expr
        | _ -> ()

    let visitType ty =
        let (SynTypeDefn.TypeDefn (_, repr, defns, _)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, objDefns, _) ->
            for d in objDefns do visitMember d
        | _ -> ()
        for d in defns do visitMember d

    let rec visitDeclarations decls =
        decls |> List.iter
           (function
            | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
            | SynModuleDecl.Types (types, _) -> List.iter visitType types
            | SynModuleDecl.NestedModule (_, _, decls, _, _) -> visitDeclarations decls
            | _ -> () )

    let visitModulesAndNamespaces modulesOrNss =
        modulesOrNss
        |> Seq.iter (fun (SynModuleOrNamespace(_, _, _, decls, _, _, _, _)) -> visitDeclarations decls)
    ast
    |> Option.iter (function
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
        | _ -> ())
    quotationRanges

/// Returns all string literal ranges
let internal getStringLiterals ast : Range.range list =
    let result = ResizeArray()

    let visitType ty =
        match ty with
        | SynType.StaticConstant (SynConst.String(_, r), _) -> result.Add r
        | _ -> ()

    let rec visitExpr = function
        | SynExpr.ArrayOrListOfSeqExpr (_, expr, _)
        | SynExpr.CompExpr (_, _, expr, _)
        | SynExpr.Lambda (_, _, _, expr, _)
        | SynExpr.YieldOrReturn (_, expr, _)
        | SynExpr.YieldOrReturnFrom (_, expr, _)
        | SynExpr.New (_, _, expr, _)
        | SynExpr.Assert (expr, _)
        | SynExpr.Do (expr, _)
        | SynExpr.Typed (expr, _, _)
        | SynExpr.Paren (expr, _, _, _)
        | SynExpr.DoBang (expr, _)
        | SynExpr.Downcast (expr, _, _)
        | SynExpr.For (_, _, _, _, _, expr, _)
        | SynExpr.Lazy (expr, _)
        | SynExpr.TypeTest(expr, _, _)
        | SynExpr.Upcast(expr, _, _)
        | SynExpr.InferredUpcast(expr, _)
        | SynExpr.InferredDowncast(expr, _)
        | SynExpr.LongIdentSet (_, expr, _)
        | SynExpr.DotGet (expr, _, _, _)
        | SynExpr.ForEach (_, _, _, _, _,expr(*body*), _) -> visitExpr expr
        | SynExpr.App (_,_, expr1(*funcExpr*), expr2(*argExpr*), _)
        | SynExpr.TryFinally (expr1, expr2, _, _, _)
        | SynExpr.NamedIndexedPropertySet (_, expr1, expr2, _)
        | SynExpr.DotNamedIndexedPropertySet (_, _, expr1, expr2, _)
        | SynExpr.LetOrUseBang (_, _, _, _,expr1(*rhsExpr*), expr2(*body*), _)
        | SynExpr.While (_, expr1, expr2, _) ->
            visitExpr expr1; visitExpr expr2
        | Sequentials exprs
        | SynExpr.Tuple (exprs, _, _)
        | SynExpr.ArrayOrList(_, exprs, _) -> List.iter visitExpr exprs
        | SynExpr.Match (_, expr, clauses, _, _)
        | SynExpr.TryWith(expr, _, clauses, _, _, _, _) ->
            visitExpr expr; visitMatches clauses
        | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            visitExpr cond
            visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr
        | SynExpr.LetOrUse (_, _, bindings, body, _) ->
            visitBindindgs bindings
            visitExpr body
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (_, expr, _) -> expr) |> List.iter visitExpr
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (_, _, bindings, _, _ , _) -> visitBindindgs bindings
        | SynExpr.Const (SynConst.String (_, r), _) -> result.Add r
        | SynExpr.TypeApp(_, _, tys, _, _, _, _) -> List.iter visitType tys
        | _ -> ()

    and visitBinding (Binding(_, _, _, _, _, _, _, _, _, body, _, _)) = visitExpr body
    and visitBindindgs = List.iter visitBinding
    and visitMatch (SynMatchClause.Clause (_, _, expr, _, _)) = visitExpr expr
    and visitMatches = List.iter visitMatch

    let visitMember = function
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
        | SynMemberDefn.Member (binding, _) -> visitBinding binding
        | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpr expr
        | _ -> ()

    let visitTypeDefn ty =
        let (SynTypeDefn.TypeDefn (_, repr, memberDefns, _)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, defns, _) ->
            for d in defns do visitMember d
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(_, SynType.App(_, _, tys, _,_ , _, _), _), _) ->
            List.iter visitType tys
        | _ -> ()
        List.iter visitMember memberDefns

    let rec visitDeclarations decls =
        for declaration in decls do
            match declaration with
            | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
            | SynModuleDecl.Types (types, _) -> for ty in types do visitTypeDefn ty
            | SynModuleDecl.NestedModule (_, _, decls, _, _) -> visitDeclarations decls
            | _ -> ()

    let visitModulesAndNamespaces modulesOrNss =
        Seq.iter (fun (SynModuleOrNamespace(_, _, _, decls, _, _, _, _)) -> visitDeclarations decls) modulesOrNss

    ast
    |> Option.iter (function
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
        | _ -> ())

    List.ofSeq result

/// Get path to containing module/namespace of a given position
let getModuleOrNamespacePath (pos: pos) (ast: ParsedInput) =
    let idents =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) ->
            let rec walkModuleOrNamespace idents (decls, moduleRange) =
                decls
                |> List.fold (fun acc ->
                    function
                    | SynModuleDecl.NestedModule (componentInfo, _, nestedModuleDecls, _, nestedModuleRange) ->
                        if rangeContainsPos moduleRange pos then
                            let (ComponentInfo(_,_,_,longIdent,_,_,_,_)) = componentInfo
                            walkModuleOrNamespace (longIdent::acc) (nestedModuleDecls, nestedModuleRange)
                        else acc
                    | _ -> acc) idents

            modules
            |> List.fold (fun acc (SynModuleOrNamespace(longIdent, _, _, decls, _, _, _, moduleRange)) ->
                    if rangeContainsPos moduleRange pos then
                        walkModuleOrNamespace (longIdent::acc) (decls, moduleRange) @ acc
                    else acc) []
        | ParsedInput.SigFile(ParsedSigFileInput(_, _, _, _, modules)) ->
            let rec walkModuleOrNamespaceSig idents (decls, moduleRange) =
                decls
                |> List.fold (fun acc ->
                    function
                    | SynModuleSigDecl.NestedModule (componentInfo, _, nestedModuleDecls, nestedModuleRange) ->
                        if rangeContainsPos moduleRange pos then
                            let (ComponentInfo(_,_,_,longIdent,_,_,_,_)) = componentInfo
                            walkModuleOrNamespaceSig (longIdent::acc) (nestedModuleDecls, nestedModuleRange)
                        else acc
                    | _ -> acc) idents

            modules
            |> List.fold (fun acc (SynModuleOrNamespaceSig(longIdent, _, _, decls, _, _, _, moduleRange)) ->
                    if rangeContainsPos moduleRange pos then
                        walkModuleOrNamespaceSig (longIdent::acc) (decls, moduleRange) @ acc
                    else acc) []
    idents
    |> List.rev
    |> Seq.concat
    |> Seq.map (fun ident -> ident.idText)
    |> String.concat "."


module HashDirectiveInfo =
    open System.IO

    type IncludeDirective =
        | ResolvedDirectory of string

    type LoadDirective =
        | ExistingFile of string
        | UnresolvableFile of string * previousIncludes : string array

    [<NoComparison>]
    type Directive =
        | Include of IncludeDirective * range
        | Load of LoadDirective * range

    /// returns an array of LoadScriptResolutionEntries
    /// based on #I and #load directives
    let getIncludeAndLoadDirectives ast =
        // the Load items are resolved using fallback resolution relying on previously parsed #I directives
        // (this behaviour is undocumented in F# but it seems to be how it works).

        // list of #I directives so far (populated while encountering those in order)
        // TODO: replace by List.fold if possible
        let includesSoFar = new System.Collections.Generic.List<_>()
        let pushInclude = includesSoFar.Add

        // those might need to be abstracted away from real filesystem operations
        let fileExists = File.Exists
        let directoryExists = Directory.Exists
        let isPathRooted = Path.IsPathRooted
        let getDirectoryOfFile = Path.GetFullPathSafe >> Path.GetDirectoryName
        let getRootedDirectory = Path.GetFullPathSafe
        let makeRootedDirectoryIfNecessary baseDirectory directory =
            if not (isPathRooted directory) then
                getRootedDirectory (baseDirectory </> directory)
            else
                directory

        // separate function to reduce nesting one level
        let parseDirectives modules file =
            [|
            let baseDirectory = getDirectoryOfFile file
            for (SynModuleOrNamespace (_, _, _, declarations, _, _, _, _)) in modules do
                for decl in declarations do
                    match decl with
                    | SynModuleDecl.HashDirective (ParsedHashDirective("I",[directory],range),_) ->
                        let directory = makeRootedDirectoryIfNecessary (getDirectoryOfFile file) directory

                        if directoryExists directory then
                            let includeDirective = ResolvedDirectory(directory)
                            pushInclude includeDirective
                            yield Include (includeDirective, range)

                    | SynModuleDecl.HashDirective (ParsedHashDirective ("load",files,range),_) ->
                        for f in files do
                            if isPathRooted f && fileExists f then

                                // this is absolute reference to an existing script, easiest case
                                yield Load (ExistingFile f, range)

                            else
                                // I'm not sure if the order is correct, first checking relative to file containing the #load directive
                                // then checking for undocumented resolution using previously parsed #I directives
                                let fileRelativeToCurrentFile = baseDirectory </> f
                                if fileExists fileRelativeToCurrentFile then
                                    // this is existing file relative to current file
                                    yield Load (ExistingFile fileRelativeToCurrentFile, range)

                                else
                                    // match file against first include which seemingly have it found
                                    let maybeFile =
                                        includesSoFar
                                        |> Seq.tryPick (function
                                            | (ResolvedDirectory d) ->
                                                let filePath = d </> f
                                                if fileExists filePath then Some filePath else None
                                        )
                                    match maybeFile with
                                    | None -> () // can't load this file even using any of the #I directives...
                                    | Some f ->
                                        yield Load (ExistingFile f,range)
                    | _ -> ()
            |]

        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput(fn,_,_,_,_,modules,_)) -> parseDirectives modules fn
        | _ -> [||]

    /// returns the Some (complete file name of a resolved #load directive at position) or None
    let getHashLoadDirectiveResolvedPathAtPosition (pos: pos) (ast: ParsedInput) : string option =
        getIncludeAndLoadDirectives ast
        |> Array.tryPick (
            function
            | Load (ExistingFile f,range)
                // check the line is within the range
                // (doesn't work when there are multiple files given to a single #load directive)
                when rangeContainsPos range pos
                    -> Some f
            | _     -> None
        )


module Outlining =
    /// Set of visitor utilities, designed for the express purpose of fetching ranges
    /// from an untyped AST for the purposes of block structure.
    [<RequireQualifiedAccess>]
    module private Range =

        let unionOpts (r1:range option) (r2:range option) =
            match r1 , r2 with
            | None, None -> None
            | Some r, None -> Some r
            | None , Some r -> Some r
            | Some r1, Some r2 -> unionRanges r1 r2 |> Some

        /// Create a range starting at the end of r1 and finishing at the end of r2
        let inline endToEnd (r1: range) (r2: range) = mkFileIndexRange r1.FileIndex r1.End r2.End

        /// Create a range starting at the end of r1 and finishing at the start of r2
        let inline endToStart (r1: range) (r2: range) = mkFileIndexRange r1.FileIndex r1.End r2.Start

        /// Create a range beginning at the start of r1 and finishing at the end of r2
        let inline startToEnd (r1: range) (r2: range) = mkFileIndexRange r1.FileIndex r1.Start r2.End

        /// Create a range beginning at the start of r1 and finishing at the start of r2
        let inline startToStart (r1: range) (r2: range) = mkFileIndexRange r1.FileIndex r1.Start r2.Start

        /// Create a new range from r by shifting the starting column by m
        let inline modStart  (m:int) (r: range) =
            let modstart = mkPos r.StartLine (r.StartColumn+m)
            mkFileIndexRange r.FileIndex modstart r.End

        /// Create a new range from r by shifting the ending column by m
        let inline modEnd (m:int) (r: range) =
            let modend = mkPos r.EndLine (r.EndColumn+m)
            mkFileIndexRange r.FileIndex r.Start modend


        /// Produce a new range by adding modStart to the StartColumn of `r`
        /// and subtracting modEnd from the EndColumn of `r`
        let inline modBoth modStart modEnd (r:range) =
            let rStart = Range.mkPos r.StartLine (r.StartColumn+modStart)
            let rEnd   = Range.mkPos r.EndLine   (r.EndColumn - modEnd)
            mkFileIndexRange r.FileIndex rStart rEnd

    let longIdentRange (longId:LongIdent) =
        match longId with 
        | [] -> Range.range0
        | head::_ -> Range.startToEnd head.idRange (List.last longId).idRange

    /// Caclulate the range of the provided type arguments (<'a,...,'z>) 
    /// or return the range `other` when `typeArgs` = []
    let rangeOfTypeArgsElse other (typeArgs:SynTyparDecl list) =
        match typeArgs with
        | [] -> other
        | ls ->
            ls
            |> List.map (fun (TyparDecl (_,typarg)) -> typarg.Range)
            |> List.reduce Range.unionRanges

    let rangeOfSynPatsElse other (synPats:SynSimplePat list) =
        match synPats with
        | [] -> other
        | ls ->
            ls 
            |> List.map (fun x ->
                    match x with
                    | SynSimplePat.Attrib(range = r)
                    | SynSimplePat.Id(range = r)
                    | SynSimplePat.Typed(range = r) -> r)
            |> List.reduce Range.unionRanges


    /// Collapse indicates the way a range/snapshot should be collapsed. `Same` is for a scope inside
    /// some kind of scope delimiter, e.g. `[| ... |]`, `[ ... ]`, `{ ... }`, etc.  `Below` is for expressions
    /// following a binding or the right hand side of a pattern, e.g. `let x = ...`
    type Collapse =
        | Below
        | Same

    /// Tag to identify the constuct that can be stored alongside its associated ranges
    type Scope =
        | Open
        | Namespace
        | Module
        | Type
        | Member
        | LetOrUse
        | Val
        | CompExpr
        | IfThenElse
        | ThenInIfThenElse
        | ElseInIfThenElse
        | TryWith
        | TryInTryWith
        | WithInTryWith
        | TryFinally
        | TryInTryFinally
        | FinallyInTryFinally
        | ArrayOrList
        | ObjExpr
        | For
        | While
        | Match
        | MatchLambda
        | MatchClause
        | Lambda
        | CompExprInternal
        | Quote
        | Record
        | SpecialFunc
        | Do
        | New
        | Attribute
        | Interface
        | HashDirective
        | LetOrUseBang
        | TypeExtension
        | YieldOrReturn
        | YieldOrReturnBang
        | Tuple
        | UnionCase
        | EnumCase
        | RecordField
        | RecordDefn
        | UnionDefn
        | Comment
        | XmlDocComment
        override self.ToString() = self |> function
            | Open                  -> "Open"
            | Namespace             -> "Namespace"
            | Module                -> "Module"
            | Type                  -> "Type"
            | Member                -> "Member"
            | LetOrUse              -> "LetOrUse"
            | Val                   -> "Val"
            | CompExpr              -> "CompExpr"
            | IfThenElse            -> "IfThenElse"
            | ThenInIfThenElse      -> "ThenInIfThenElse"
            | ElseInIfThenElse      -> "ElseInIfThenElse"
            | TryWith               -> "TryWith"
            | TryInTryWith          -> "TryInTryWith"
            | WithInTryWith         -> "WithInTryWith"
            | TryFinally            -> "TryFinally"
            | TryInTryFinally       -> "TryInTryFinally"
            | FinallyInTryFinally   -> "FinallyInTryFinally"
            | ArrayOrList           -> "ArrayOrList"
            | ObjExpr               -> "ObjExpr"
            | For                   -> "For"
            | While                 -> "While"
            | Match                 -> "Match"
            | MatchLambda           -> "MatchLambda"
            | MatchClause           -> "MatchClause"
            | Lambda                -> "Lambda"
            | CompExprInternal      -> "CompExprInternal"
            | Quote                 -> "Quote"
            | Record                -> "Record"
            | SpecialFunc           -> "SpecialFunc"
            | Do                    -> "Do"
            | New                   -> "New"
            | Attribute             -> "Attribute"
            | Interface             -> "Interface"
            | HashDirective         -> "HashDirective"
            | LetOrUseBang          -> "LetOrUseBang"
            | TypeExtension         -> "TypeExtension"
            | YieldOrReturn         -> "YieldOrReturn"
            | YieldOrReturnBang     -> "YieldOrReturnBang"
            | Tuple                 -> "Tuple"
            | UnionCase             -> "UnionCase"
            | EnumCase              -> "EnumCase"
            | RecordField           -> "RecordField"
            | RecordDefn            -> "RecordDefn"
            | UnionDefn             -> "UnionDefn"
            | Comment               -> "Comment"
            | XmlDocComment         -> "XmlDocComment"

    /// Stores the range for a construct, the sub-range that should be collapsed for outlinging,
    /// a tag for the construct type, and a tag for the collapse style
    [<NoComparison>]
    type ScopeRange = {
        Scope: Scope
        Collapse: Collapse
        /// HintSpan in BlockSpan
        Range: range
        /// TextSpan in BlockSpan
        CollapseRange:range
    }

    /// Validation function to ensure that ranges yielded for outlinging span 2 or more lines
    let inline private rcheck scope collapse (fullRange:range) (collapseRange:range)  = seq {
        if fullRange.StartLine <> fullRange.EndLine then yield {
            Scope = scope
            Collapse = collapse
            Range = fullRange
            CollapseRange = collapseRange
        }
    }

      //============================================//
     //     Implementation File AST Traversal      //
    //============================================//


    let rec private parseExpr expression =
        seq {
            match expression with
            | SynExpr.Upcast (e,_,_)
            | SynExpr.Downcast (e,_,_)
            | SynExpr.AddressOf(_,e,_,_)
            | SynExpr.InferredDowncast (e,_)
            | SynExpr.InferredUpcast (e,_)
            | SynExpr.DotGet (e,_,_,_)
            | SynExpr.Do (e,_)
            | SynExpr.DotSet (e,_,_,_)
            | SynExpr.Typed (e,_,_)
            | SynExpr.DotIndexedGet (e,_,_,_)
            | SynExpr.DotIndexedSet (e,_,_,_,_,_) -> yield! parseExpr e
            | SynExpr.New (_,_,expr,r) ->
                let collapse = Range.endToEnd expr.Range r
                yield! rcheck Scope.New Collapse.Below r collapse
                yield! parseExpr expr
            | SynExpr.YieldOrReturn (_,e,r) ->
                yield! rcheck Scope.YieldOrReturn Collapse.Below r r
                yield! parseExpr e
            | SynExpr.YieldOrReturnFrom (_,e,r) ->
                yield! rcheck Scope.YieldOrReturnBang Collapse.Below r r
                yield! parseExpr e
            | SynExpr.DoBang (e,r) ->
                yield! rcheck Scope.Do Collapse.Below r <| Range.modStart 3 r
                yield! parseExpr e
            | SynExpr.LetOrUseBang (_,_,_,pat,e1,e2,_) ->
                // for `let!` or `use!` the pattern begins at the end of the keyword so that
                // this scope can be used without adjustment if there is no `=` on the same line
                // if there is an `=` the range will be adjusted during the tooltip creation
                let r = Range.endToEnd pat.Range e1.Range
                yield! rcheck Scope.LetOrUseBang Collapse.Below r r
                yield! parseExpr e1
                yield! parseExpr e2
            | SynExpr.For (_,_,_,_,_,e,r)
            | SynExpr.ForEach (_,_,_,_,_,e,r) ->
                yield! rcheck Scope.For Collapse.Below r r
                yield! parseExpr e
            | SynExpr.LetOrUse (_,_,bindings, body, _) ->
                yield! parseBindings bindings
                yield! parseExpr body
            | SynExpr.Match (seqPointAtBinding,_expr,clauses,_,r) ->
                match seqPointAtBinding with
                | SequencePointAtBinding sr ->
                    let collapse = Range.endToEnd sr r
                    yield! rcheck Scope.Match Collapse.Same r collapse
                | _ -> ()
                yield! parseMatchClauses clauses
            | SynExpr.MatchLambda (_,caseRange,clauses,matchSeqPoint,r) ->
                let caseRange =
                    match matchSeqPoint with
                    | SequencePointAtBinding r -> r
                    | _ -> caseRange
                let collapse = Range.endToEnd caseRange r
                yield! rcheck Scope.MatchLambda Collapse.Same r collapse
                yield! parseMatchClauses clauses
            | SynExpr.App (atomicFlag,isInfix,funcExpr,argExpr,r) ->
                // seq exprs, custom operators, etc
                if ExprAtomicFlag.NonAtomic=atomicFlag && (not isInfix)
                   && (function | SynExpr.Ident _    -> true  | _ -> false) funcExpr
                   && (function | SynExpr.CompExpr _ -> false | _ -> true ) argExpr then
                   // if the argExrp is a computation expression another match will handle the outlining
                   // these cases must be removed to prevent creating unnecessary tags for the same scope
                    let collapse = Range.endToEnd funcExpr.Range r
                    yield! rcheck Scope.SpecialFunc Collapse.Below r collapse
                elif ExprAtomicFlag.NonAtomic=atomicFlag && (not isInfix)
                   && (function | SynExpr.CompExpr _ -> true | _ -> false) argExpr then
                        let collapse = Range.startToEnd argExpr.Range r
                        yield! rcheck Scope.CompExpr Collapse.Same r <| Range.modBoth 1 1 collapse
                yield! parseExpr argExpr
                yield! parseExpr funcExpr
            | SynExpr.Sequential (_,_,e1,e2,_) ->
                yield! parseExpr e1
                yield! parseExpr e2
            | SynExpr.ArrayOrListOfSeqExpr (isArray,e,r) ->
                yield! rcheck  Scope.ArrayOrList Collapse.Same r <| Range.modBoth (if isArray then 2 else 1) (if isArray then 2 else 1) r
                yield! parseExpr e
            | SynExpr.CompExpr (_arrayOrList,_,e,_r) as _c ->
                yield! parseExpr e
            | SynExpr.ObjExpr (_,argOpt,bindings,extraImpls,newRange,wholeRange) as _objExpr ->
                match argOpt with
                | Some (args,_) ->
                    let collapse = Range.endToEnd args.Range wholeRange
                    yield! rcheck Scope.ObjExpr Collapse.Below wholeRange collapse
                | None ->
                    let collapse = Range.endToEnd newRange wholeRange
                    yield! rcheck Scope.ObjExpr Collapse.Below wholeRange collapse
                yield! parseBindings bindings
                yield! parseExprInterfaces extraImpls
            | SynExpr.TryWith (e,_tryRange,matchClauses,_withRange,wholeRange,tryPoint,withPoint) ->
                match tryPoint, withPoint with
                | SequencePointAtTry tryRange,  SequencePointAtWith withRange ->
                    let fullrange = Range.startToEnd tryRange wholeRange
                    let collapse = Range.endToEnd tryRange wholeRange
                    let collapseTry = Range.endToStart tryRange withRange
                    let fullrangeTry = Range.startToStart tryRange withRange
                    let collapseWith = Range.endToEnd withRange wholeRange
                    let fullrangeWith = Range.startToEnd withRange wholeRange
                    yield! rcheck Scope.TryWith Collapse.Below fullrange collapse
                    yield! rcheck Scope.TryInTryWith Collapse.Below fullrangeTry collapseTry
                    yield! rcheck Scope.WithInTryWith Collapse.Below fullrangeWith collapseWith
                | _ -> ()
                yield! parseExpr e
                yield! parseMatchClauses matchClauses
            | SynExpr.TryFinally (tryExpr,finallyExpr,r,tryPoint,finallyPoint) as _tryFinally ->
                match tryPoint, finallyPoint with
                | SequencePointAtTry tryRange, SequencePointAtFinally finallyRange ->
                    let collapse = Range.endToEnd tryRange finallyExpr.Range
                    let fullrange = Range.startToEnd tryRange finallyExpr.Range
                    let collapseFinally = Range.endToEnd finallyRange r
                    let fullrangeFinally = Range.startToEnd finallyRange r
                    yield! rcheck Scope.TryFinally Collapse.Below fullrange collapse
                    yield! rcheck  Scope.FinallyInTryFinally Collapse.Below fullrangeFinally collapseFinally
                | _ -> ()
                yield! parseExpr tryExpr
                yield! parseExpr finallyExpr
            | SynExpr.IfThenElse (ifExpr,thenExpr,elseExprOpt,spIfToThen,_,ifToThenRange,r) as _ifThenElse->
                match spIfToThen with
                | SequencePointInfoForBinding.SequencePointAtBinding rt ->
                    // Outline the entire IfThenElse
                    let fullrange = Range.startToEnd rt r
                    let collapse = Range.endToEnd  ifExpr.Range r
                    yield! rcheck Scope.IfThenElse Collapse.Below fullrange collapse
                    // Outline the `then` scope
                    let thenRange = Range.endToEnd (Range.modEnd -4  ifToThenRange)   thenExpr.Range
                    let thenCollapse = Range.endToEnd ifToThenRange thenExpr.Range
                    yield! rcheck Scope.ThenInIfThenElse Collapse.Below thenRange thenCollapse
                | _ -> ()
                yield! parseExpr ifExpr
                yield! parseExpr thenExpr
                match elseExprOpt with
                | Some elseExpr ->
                    match elseExpr with // prevent double collapsing on elifs
                    | SynExpr. IfThenElse (_,_,_,_,_,_,_) ->
                        yield! parseExpr elseExpr
                    | _ ->
                        // This is not the best way to establish the position of `else`
                        // the AST doesn't provide an easy way to find the position of the keyword
                        // as such `else` will be left out of block structuring and outlining until a
                        // a suitable approach is determined
                        yield! parseExpr elseExpr
                | None -> ()
            | SynExpr.While (_,_,e,r) ->
                yield! rcheck Scope.While Collapse.Below r r
                yield! parseExpr e
            | SynExpr.Lambda (_,_,pats,e,r) ->
                match pats with
                | SynSimplePats.SimplePats (_,pr)
                | SynSimplePats.Typed (_,_,pr) ->
                    yield! rcheck Scope.Lambda Collapse.Below r <| Range.endToEnd pr r
                yield! parseExpr e
            | SynExpr.Lazy (e,r) ->
                yield! rcheck Scope.SpecialFunc Collapse.Below r r
                yield! parseExpr e
            | SynExpr.Quote (_,isRaw,e,_,r) ->
                // subtract columns so the @@> or @> is not collapsed
                yield! rcheck Scope.Quote Collapse.Same r <| Range.modBoth (if isRaw then 3 else 2) (if isRaw then 3 else 2) r
                yield! parseExpr e
            | SynExpr.Tuple (es,_,r)
            | SynExpr.StructTuple(es,_,r) ->
                yield! rcheck Scope.Tuple Collapse.Same r r
                yield! Seq.collect parseExpr es
            | SynExpr.Paren (e,_,_,_) ->
                yield! parseExpr e
            | SynExpr.Record (recCtor,recCopy,recordFields,r) ->
                if recCtor.IsSome then
                    let (_,ctorArgs,_,_,_) = recCtor.Value
                    yield! parseExpr ctorArgs
                if recCopy.IsSome then
                    let (e,_) = recCopy.Value
                    yield! parseExpr e
                yield! recordFields |> (Seq.choose (fun (_,e,_) -> e) >> Seq.collect parseExpr)
                // exclude the opening `{` and closing `}` of the record from collapsing
                yield! rcheck Scope.Record Collapse.Same r <| Range.modBoth 1 1 r
            | _ -> ()
        }

    and private parseMatchClause (SynMatchClause.Clause(synPat,_,e,_r,_) as clause) =
        let rec getLastPat = function
            | SynPat.Or(_, pat, _) -> getLastPat pat
            | x -> x

        seq {
            let synPat = getLastPat synPat
            let collapse  = Range.endToEnd synPat.Range clause.Range // Collapse the scope starting with `->`
            yield! rcheck Scope.MatchClause Collapse.Same e.Range collapse
            yield! parseExpr e
        }

    and private parseMatchClauses = Seq.collect parseMatchClause

    and private parseAttributes (attrs: SynAttributes) =
        seq{
            let attrListRange =
                if List.isEmpty attrs then Seq.empty else
                let range = Range.startToEnd (attrs.[0].Range) (attrs.[attrs.Length-1].ArgExpr.Range)
                rcheck Scope.Attribute Collapse.Same  range range
            match  attrs with
            | [] -> ()
            | [_] -> yield! attrListRange
            | hd::tl ->
                yield! attrListRange
                yield! parseExpr hd.ArgExpr
                // If there are more than 2 attributes only add tags to the 2nd and beyond, to avoid double collapsing on the first attribute
                yield! tl |> Seq.collect (fun attr ->
                    let range = Range.startToEnd attr.Range attr.ArgExpr.Range
                    rcheck Scope.Attribute Collapse.Same range range
                )
                // visit the expressions inside each attribute
                yield! attrs |> Seq.collect (fun attr -> parseExpr attr.ArgExpr)
        }

    and private parseBinding (SynBinding.Binding (_,kind,_,_,attrs,_,SynValData(memberFlags,_,_),_,_,expr,br,_) as binding) =
        seq {
            match kind with
            | SynBindingKind.NormalBinding ->
                let collapse = Range.endToEnd binding.RangeOfBindingSansRhs binding.RangeOfBindingAndRhs
                match memberFlags with
                | Some ({MemberKind=MemberKind.Constructor}) ->
                    let collapse = Range.startToEnd expr.Range br
                    yield! rcheck Scope.New Collapse.Below br collapse
                | Some _ ->
                    yield! rcheck Scope.Member Collapse.Below binding.RangeOfBindingAndRhs collapse
                | None ->
                    yield! rcheck Scope.LetOrUse Collapse.Below binding.RangeOfBindingAndRhs collapse
            | SynBindingKind.DoBinding ->
                let r = Range.modStart 2 br
                yield! rcheck Scope.Do Collapse.Below br r
            | _ -> ()
            yield! parseAttributes attrs
            yield! parseExpr expr
        }

    and private parseBindings sqs = sqs |> Seq.collect parseBinding

    and private parseExprInterface (InterfaceImpl(synType,bindings,range)) = seq{
        let collapse = Range.endToEnd synType.Range range |> Range.modEnd -1
        yield! rcheck Scope.Interface Collapse.Below range collapse
        yield! parseBindings bindings
     }

    and private parseExprInterfaces (intfs:SynInterfaceImpl list) = Seq.collect parseExprInterface intfs

    and private parseSynMemberDefn (objectModelRange: range) d =
        seq {
            match d with
            | SynMemberDefn.Member(SynBinding.Binding (attrs=attrs; valData=valData; headPat=synPat; range=bindingRange) as binding,_) ->
               match valData with
               | SynValData (Some { MemberKind=MemberKind.Constructor },_,_) ->
                  let collapse = Range.endToEnd synPat.Range d.Range
                  yield! rcheck Scope.New Collapse.Below d.Range collapse
               | SynValData (Some { MemberKind=MemberKind.PropertyGet | MemberKind.PropertySet },_,_) ->
                  let range = 
                    Range.mkRange 
                        d.Range.FileName 
                        (Range.mkPos d.Range.StartLine objectModelRange.StartColumn)
                        d.Range.End
                  
                  let collapse =
                    match synPat with
                    | SynPat.LongIdent(longDotId=longIdent) ->
                       Range.endToEnd longIdent.Range d.Range
                    | _ -> Range.endToEnd bindingRange d.Range

                  yield! rcheck Scope.Member Collapse.Below range collapse
               | _ ->
                  let collapse = Range.endToEnd bindingRange d.Range
                  yield! rcheck Scope.Member Collapse.Below d.Range collapse
               yield! parseAttributes attrs
               yield! parseBinding binding
            | SynMemberDefn.LetBindings (bindings,_,_,_) ->
                yield! parseBindings bindings
            | SynMemberDefn.Interface (tp, iMembers, r) ->
                yield! rcheck Scope.Interface Collapse.Below d.Range <| Range.endToEnd tp.Range d.Range
                match iMembers with
                | Some members ->
                    yield! Seq.collect (parseSynMemberDefn r) members
                | None -> ()
            | SynMemberDefn.NestedType (td, _, _) ->
                yield! parseTypeDefn td 
            | SynMemberDefn.AbstractSlot (ValSpfn(synType=synt), _, r) ->
                yield! rcheck Scope.Member Collapse.Below d.Range (Range.startToEnd synt.Range r)
            | SynMemberDefn.AutoProperty (synExpr=e; range=r) ->
                yield! rcheck Scope.Member Collapse.Below d.Range r
                yield! parseExpr e
            | _ -> ()
        }

    (*  For Cases like
        --------------
            type JsonDocument =
                private {   Json : string
                            Path : string   }
        Or
             type JsonDocument =
                internal |  Json of string
                         |  Path of string
    *)
    and private parseSimpleRepr simple =
        seq {
            match simple with
            | SynTypeDefnSimpleRepr.Enum (cases,_er) ->
                for SynEnumCase.EnumCase (attrs, _, _, _, cr) in cases do
                    yield! rcheck Scope.EnumCase Collapse.Below cr cr
                    yield! parseAttributes attrs
            | SynTypeDefnSimpleRepr.Record (_,fields,rr) ->
                yield! rcheck Scope.RecordDefn Collapse.Same rr rr 
                for SynField.Field (attrs,_,_,_,_,_,_,fr) in fields do
                    yield! rcheck Scope.RecordField Collapse.Below fr fr
                    yield! parseAttributes attrs
            | SynTypeDefnSimpleRepr.Union (_,cases,ur) ->
                yield! rcheck Scope.UnionDefn Collapse.Same ur ur
                for SynUnionCase.UnionCase (attrs,_,_,_,_,cr) in cases do
                    yield! rcheck Scope.UnionCase Collapse.Below cr cr
                    yield! parseAttributes attrs
            | _ -> ()
        }

    and private parseTypeDefn (TypeDefn(SynComponentInfo.ComponentInfo(_,typeArgs,_,_,_,_,_,r), objectModel, members, fullrange)) = 
        seq {
           let typeArgsRange = rangeOfTypeArgsElse r typeArgs
           let collapse = Range.endToEnd (Range.modEnd 1 typeArgsRange) fullrange
           match objectModel with
           | SynTypeDefnRepr.ObjectModel (defnKind, objMembers, r) ->
               match defnKind with
               | SynTypeDefnKind.TyconAugmentation ->
                   yield! rcheck Scope.TypeExtension Collapse.Below fullrange collapse
               | _ ->
                   yield! rcheck Scope.Type Collapse.Below fullrange collapse
               yield! Seq.collect (parseSynMemberDefn r) objMembers
               // visit the members of a type extension
               yield! Seq.collect (parseSynMemberDefn r) members
           | SynTypeDefnRepr.Simple (simpleRepr, r) ->
               yield! rcheck Scope.Type Collapse.Below fullrange collapse
               yield! parseSimpleRepr simpleRepr
               yield! Seq.collect (parseSynMemberDefn r) members
           | SynTypeDefnRepr.Exception _ -> ()
        }

    let private getConsecutiveModuleDecls (predicate: SynModuleDecl -> range option) (scope: Scope) (decls: SynModuleDecls) =
        let groupConsecutiveDecls input =
            let rec loop (input: range list) (res: range list list) currentBulk =
                match input, currentBulk with
                | [], [] -> List.rev res
                | [], _ -> List.rev (currentBulk::res)
                | r :: rest, [] -> loop rest res [r]
                | r :: rest, last :: _ when r.StartLine = last.EndLine + 1 ->
                    loop rest res (r::currentBulk)
                | r :: rest, _ -> loop rest (currentBulk::res) [r]
            loop input [] []

        let selectRanges (ranges: range list) =
            match ranges with
            | [] -> None
            | [r] when r.StartLine = r.EndLine -> None
            | [r] ->
                let range = Range.mkRange "" r.Start r.End
                Some { Scope = scope; Collapse = Collapse.Same; Range = range ; CollapseRange = range }
            | lastRange :: rest ->
                let firstRange = Seq.last rest
                let range = Range.mkRange "" firstRange.Start lastRange.End
                Some { Scope = scope; Collapse = Collapse.Same; Range = range; CollapseRange = range }

        decls |> (List.choose predicate >> groupConsecutiveDecls >> List.choose selectRanges)

    let collectOpens = getConsecutiveModuleDecls (function SynModuleDecl.Open (_, r) -> Some r | _ -> None) Scope.Open

    let collectHashDirectives =
         getConsecutiveModuleDecls(
            function
            | SynModuleDecl.HashDirective (ParsedHashDirective (directive, _, _),r) ->
                let prefixLength = "#".Length + directive.Length + " ".Length
                Some (Range.mkRange "" (Range.mkPos r.StartLine prefixLength) r.End)
            | _ -> None) Scope.HashDirective

    let rec private parseDeclaration (decl: SynModuleDecl) =
        seq {
            match decl with
            | SynModuleDecl.Let (_,bindings,r) ->
                for binding in bindings do
                    let collapse = Range.endToEnd binding.RangeOfBindingSansRhs r
                    yield! rcheck Scope.LetOrUse Collapse.Below r collapse
                yield! parseBindings bindings
            | SynModuleDecl.Types (types,_r) ->
                for t in types do
                    yield! parseTypeDefn t
            // Fold the attributes above a module
            | SynModuleDecl.NestedModule (SynComponentInfo.ComponentInfo (attrs,_,_,_,_,_,_,cmpRange),_, decls,_,_) ->                
                // Outline the full scope of the module
                let r = Range.endToEnd cmpRange decl.Range
                yield! rcheck Scope.Module Collapse.Below decl.Range r
                // A module's component info stores the ranges of its attributes
                yield! parseAttributes attrs
                yield! collectOpens decls
                yield! Seq.collect parseDeclaration decls
            | SynModuleDecl.DoExpr (_,e,_) ->
                yield! parseExpr e
            | SynModuleDecl.Attributes (attrs,_) ->
                yield! parseAttributes attrs
            | _ -> ()
        }

    let private parseModuleOrNamespace (SynModuleOrNamespace.SynModuleOrNamespace (longId,_,isModule,decls,_,attribs,_,r)) =
        seq {
            yield! parseAttributes attribs
            let idRange = longIdentRange longId
            let fullrange = Range.startToEnd idRange r  
            let collapse = Range.endToEnd idRange r 
            if isModule then
                yield! rcheck Scope.Module Collapse.Below fullrange collapse

            yield! collectHashDirectives decls
            yield! collectOpens decls
            yield! Seq.collect parseDeclaration decls
        }

    type private LineNum = int
    type private LineStr = string
    type private CommentType = SingleLine | XmlDoc

    [<NoComparison>]
    type private CommentList =
        { Lines: ResizeArray<LineNum * LineStr>
          Type: CommentType }
        static member New ty lineStr =
            { Type = ty; Lines = ResizeArray [| lineStr |] }

    /// Determine if a line is a single line or xml docummentation comment
    let private (|Comment|_|) (line: string) =
        if line.StartsWith "///" then Some XmlDoc
        elif line.StartsWith "//" then Some SingleLine
        else None

    let getCommentRanges (lines: string[]) =
        let rec loop ((lastLineNum, currentComment: CommentList option, result) as state) (lines: string list) lineNum =
            match lines with
            | [] -> state
            | lineStr :: rest ->
                match lineStr.TrimStart(), currentComment with
                | Comment commentType, Some comment ->
                    loop(
                        if comment.Type = commentType && lineNum = lastLineNum + 1 then
                            comment.Lines.Add (lineNum, lineStr)
                            lineNum, currentComment, result
                        else lineNum, Some (CommentList.New commentType (lineNum, lineStr)), comment :: result) rest (lineNum + 1)
                | Comment commentType, None ->
                    loop(lineNum, Some (CommentList.New commentType (lineNum, lineStr)), result) rest (lineNum + 1)
                | _, Some comment ->
                    loop(lineNum, None, comment :: result) rest (lineNum + 1)
                | _ -> loop(lineNum, None, result) rest (lineNum + 1)

        let comments: CommentList list =
            loop (-1, None, []) (List.ofArray lines) 0
            |> fun (_, lastComment, comments) ->
                match lastComment with
                | Some comment ->
                    comment :: comments
                | _ -> comments
                |> List.rev

        comments
        |> List.filter (fun comment -> comment.Lines.Count > 1)
        |> List.map (fun comment ->
            let lines = comment.Lines
            let startLine, startStr = lines.[0]
            let endLine, endStr = lines.[lines.Count - 1]
            let startCol = startStr.IndexOf '/'
            let endCol = endStr.TrimEnd().Length

            let scopeType =
                match comment.Type with
                | SingleLine -> Scope.Comment
                | XmlDoc -> Scope.XmlDocComment

            let range = Range.mkRange "" (Range.mkPos (startLine + 1) startCol) (Range.mkPos (endLine + 1) endCol)

            { Scope = scopeType
              Collapse = Collapse.Same
              Range = range
              CollapseRange = range })


      //=======================================//
     //     Signature File AST Traversal      //
    //=======================================//

    (*
        The following helper functions are necessary due to a bug in the Parsed UAST within a 
        signature file that causes the scopes to extend past the end of the construct and overlap
        with the following construct. This necessitates inspecting the children of the construct and
        finding the end of the last child's range to use instead.

        Detailed further in - https://github.com/Microsoft/visualfsharp/issues/2094
    *)

    let lastMemberSigRangeElse r memberSigs =
        match memberSigs with
        | [] -> r
        | ls ->
            match List.last ls with
            | SynMemberSig.Inherit (range=r)
            | SynMemberSig.Interface (range=r)
            | SynMemberSig.Member (range=r)
            | SynMemberSig.NestedType (range=r)
            | SynMemberSig.ValField (range=r) -> r

    let lastTypeDefnSigRangeElse range (typeSigs:SynTypeDefnSig list) =
        match typeSigs with
        | [] -> range
        | ls ->
            let (SynTypeDefnSig.TypeDefnSig(_,_,memberSigs,r)) = List.last ls
            lastMemberSigRangeElse r memberSigs

    let lastModuleSigDeclRangeElse range (sigDecls:SynModuleSigDecls) =
        match sigDecls with
        | [] -> range
        | ls -> match List.last ls with
                | SynModuleSigDecl.Types (typeSigs,r) -> lastTypeDefnSigRangeElse r typeSigs
                | SynModuleSigDecl.Val (ValSpfn(range=r),_) -> r
                | SynModuleSigDecl.Exception(_,r) -> r
                | SynModuleSigDecl.Open(_,r)-> r
                | SynModuleSigDecl.ModuleAbbrev(_,_,r)-> r
                | _ -> range


    let rec private parseSynMemberDefnSig (dsig:SynMemberSig) =
        seq {
            match dsig with
            | SynMemberSig.Member(valSigs,_flags,r) ->
                let collapse = Range.endToEnd valSigs.RangeOfId r
                yield! rcheck Scope.Member Collapse.Below r collapse
            | SynMemberSig.ValField(Field(attrs,_a,_b,_c,_d,_e,_,fr),fullrange) ->
                let collapse = Range.endToEnd fr fullrange
                yield! rcheck Scope.Val Collapse.Below fullrange collapse
                yield! parseAttributes attrs
            | SynMemberSig.Interface(tp,r) ->
                yield! rcheck Scope.Interface Collapse.Below r <| Range.endToEnd tp.Range r
            | SynMemberSig.NestedType (typeDefSig, _r) ->
                yield! parseTypeDefnSig typeDefSig
            | _ -> ()
        }


    and private parseTypeDefnSig
      (TypeDefnSig (SynComponentInfo.ComponentInfo(attribs,typeArgs,_constraints,longId,_doc,_b,_access,r)
                as _componentInfo, objectModel,  memberSigs, _)) = seq {
            yield! parseAttributes attribs

            let makeRanges memberSigs =
                let typeArgsRange = rangeOfTypeArgsElse r typeArgs
                let rangeEnd = lastMemberSigRangeElse r memberSigs
                let collapse = Range.endToEnd (Range.modEnd 1 typeArgsRange) rangeEnd
                let fullrange = Range.startToEnd (longIdentRange longId) rangeEnd
                fullrange, collapse

            yield! Seq.collect parseSynMemberDefnSig memberSigs
            match objectModel with
            // matches against a type declaration with <'T,...> and (args,...)
            | SynTypeDefnSigRepr.ObjectModel
                (SynTypeDefnKind.TyconUnspecified,objMembers,_r) ->
                    yield! Seq.collect parseSynMemberDefnSig objMembers
                    let fullrange,collapse = makeRanges objMembers
                    yield! rcheck Scope.Type Collapse.Below fullrange collapse
            | SynTypeDefnSigRepr.ObjectModel
                (SynTypeDefnKind.TyconAugmentation , objMembers, _) ->
                    let fullrange,collapse = makeRanges objMembers
                    yield! rcheck Scope.TypeExtension Collapse.Below fullrange collapse
                    yield! Seq.collect parseSynMemberDefnSig objMembers
            | SynTypeDefnSigRepr.ObjectModel
                (_ , objMembers, _) ->
                    let fullrange,collapse = makeRanges objMembers
                    yield! rcheck Scope.Type Collapse.Below fullrange collapse
                    yield! Seq.collect parseSynMemberDefnSig objMembers
                // visit the members of a type extension
            | SynTypeDefnSigRepr.Simple (simpleRepr,_r) ->
                let fullrange,collapse = makeRanges memberSigs
                yield! rcheck Scope.Type Collapse.Below fullrange collapse
                yield! parseSimpleRepr simpleRepr
            | SynTypeDefnSigRepr.Exception _ -> ()
        }


    let private getConsecutiveSigModuleDecls (predicate: SynModuleSigDecl -> range option) (scope:Scope) (decls: SynModuleSigDecls) =
        let groupConsecutiveSigDecls input =
            let rec loop (input: range list) (res: range list list) currentBulk =
                match input, currentBulk with
                | [], [] -> List.rev res
                | [], _ -> List.rev (currentBulk::res)
                | r :: rest, [] -> loop rest res [r]
                | r :: rest, last :: _ when r.StartLine = last.EndLine + 1 ->
                    loop rest res (r::currentBulk)
                | r :: rest, _ -> loop rest (currentBulk::res) [r]
            loop input [] []


        let selectSigRanges (ranges: range list) =
            match ranges with
            | [] -> None
            | [r] when r.StartLine = r.EndLine -> None
            | [r] ->
                let range = Range.mkRange "" r.Start r.End
                Some { Scope = scope; Collapse = Collapse.Same; Range = range ; CollapseRange = range }
            | lastRange :: rest ->
                let firstRange = Seq.last rest
                let range = Range.mkRange "" firstRange.Start lastRange.End
                Some { Scope = scope; Collapse = Collapse.Same; Range = range; CollapseRange = range }

        decls |> (List.choose predicate >> groupConsecutiveSigDecls >> List.choose selectSigRanges)


    let collectSigHashDirectives (decls: SynModuleSigDecls) =
        decls
        |> getConsecutiveSigModuleDecls(
            function
            | SynModuleSigDecl.HashDirective (ParsedHashDirective (directive, _, _),r) ->
                let prefixLength = "#".Length + directive.Length + " ".Length
                Some (Range.mkRange "" (Range.mkPos r.StartLine prefixLength) r.End)
            | _ -> None) Scope.HashDirective


    let collectSigOpens = getConsecutiveSigModuleDecls (function SynModuleSigDecl.Open (_,r) -> Some r | _ -> None) Scope.Open

    let rec private parseModuleSigDeclaration (decl: SynModuleSigDecl) =
        seq {
            match decl with
            | SynModuleSigDecl.Val ((ValSpfn(attrs,ident,_valdecls,_,_,_,_,_,_,_,valrange)),_r) ->
                let collapse = Range.endToEnd  ident.idRange valrange
                yield! rcheck Scope.Val Collapse.Below _r collapse
                yield! parseAttributes attrs
            | SynModuleSigDecl.Types (typeSigs,_r) ->
                yield! Seq.collect parseTypeDefnSig typeSigs
            // Fold the attributes above a module
            | SynModuleSigDecl.NestedModule (SynComponentInfo.ComponentInfo (attrs,_,_,_,_,_,_,cmpRange),_, decls,moduleRange) ->
                let rangeEnd = lastModuleSigDeclRangeElse moduleRange decls
                // Outline the full scope of the module
                let collapse = Range.endToEnd cmpRange rangeEnd
                let fullrange = Range.startToEnd moduleRange rangeEnd
                yield! rcheck Scope.Module Collapse.Below fullrange collapse
                // A module's component info stores the ranges of its attributes
                yield! parseAttributes attrs
                yield! collectSigOpens decls
                yield! Seq.collect parseModuleSigDeclaration decls
            | _ -> ()
        }

    let private parseModuleOrNamespaceSigs moduleOrNamespaceSig =
        seq {
            let (SynModuleOrNamespaceSig.SynModuleOrNamespaceSig(longId,_,isModule,decls,_,attribs,_,r)) =  moduleOrNamespaceSig
            yield! parseAttributes attribs
            let rangeEnd = lastModuleSigDeclRangeElse r decls
            let idrange = longIdentRange longId
            let fullrange = Range.startToEnd idrange rangeEnd
            let collapse = Range.endToEnd idrange rangeEnd
            if isModule then
                yield! rcheck Scope.Module Collapse.Below fullrange collapse

            yield! collectSigHashDirectives decls
            yield! collectSigOpens decls
            yield! Seq.collect parseModuleSigDeclaration decls
        }

    let getOutliningRanges (sourceLines: string []) (parsedInput: ParsedInput) =
        match parsedInput with
        | ParsedInput.ImplFile (ParsedImplFileInput (_,_,_,_,_,modules,_)) ->
            let astBasedRanges = Seq.collect parseModuleOrNamespace modules
            let commentRanges = getCommentRanges sourceLines
            Seq.append astBasedRanges commentRanges
        | ParsedInput.SigFile (ParsedSigFileInput (_,_,_,_,moduleSigs)) ->
            let astBasedRanges = Seq.collect parseModuleOrNamespaceSigs moduleSigs
            let commentRanges = getCommentRanges sourceLines
            Seq.append astBasedRanges commentRanges

module Printf =
    [<NoComparison>]
    type PrintfFunction = 
        { FormatString: Range.range
          Args: Range.range[] }
    
    [<NoComparison>]
    type private AppWithArg = 
        { Range: Range.range
          Arg: Range.range }

    let internal getAll (input: ParsedInput option) : PrintfFunction[] =
        let result = ResizeArray()
        let appStack: AppWithArg list ref = ref []

        let addAppWithArg appWithArg =
            match !appStack with
            | lastApp :: _ when not (Range.rangeContainsRange lastApp.Range appWithArg.Range) ->
                appStack := [appWithArg]
            | _ -> appStack := appWithArg :: !appStack

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
            List.iter walkSynModuleOrNamespace moduleOrNamespaceList

        and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, _, decls, _, _, _, _)) =
            List.iter walkSynModuleDecl decls

        and walkTypeConstraint = function
            | SynTypeConstraint.WhereTyparDefaultsToType (_, ty, _)
            | SynTypeConstraint.WhereTyparSubtypeOfType (_, ty, _) -> walkType ty
            | SynTypeConstraint.WhereTyparIsEnum (_, ts, _)
            | SynTypeConstraint.WhereTyparIsDelegate (_, ts, _) -> List.iter walkType ts
            | SynTypeConstraint.WhereTyparSupportsMember (_, sign, _) -> walkMemberSig sign
            | _ -> ()

        and walkBinding (SynBinding.Binding (_, _, _, _, _, _, _, _, returnInfo, e, _, _)) =
            walkExpr e
            returnInfo |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

        and walkInterfaceImpl (InterfaceImpl(_, bindings, _)) = List.iter walkBinding bindings

        and walkIndexerArg = function
            | SynIndexerArg.One e -> walkExpr e
            | SynIndexerArg.Two (e1, e2) -> List.iter walkExpr [e1; e2]

        and walkType = function
            | SynType.Array (_, t, _)
            | SynType.HashConstraint (t, _)
            | SynType.MeasurePower (t, _, _) -> walkType t
            | SynType.Fun (t1, t2, _)
            | SynType.MeasureDivide (t1, t2, _) -> walkType t1; walkType t2
            | SynType.App (ty, _, types, _, _, _, _) -> walkType ty; List.iter walkType types
            | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
            | SynType.Tuple (ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
            | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
                walkType t; List.iter walkTypeConstraint typeConstraints
            | _ -> ()

        and walkClause (Clause (_, e1, e2, _, _)) =
            walkExpr e2
            e1 |> Option.iter walkExpr

        and walkSimplePats = function
            | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
            | SynSimplePats.Typed (pats, ty, _) -> 
                walkSimplePats pats
                walkType ty

        and walkExpr e =
            match e with
            | SynExpr.App (_, _, SynExpr.Ident _, SynExpr.Const (SynConst.String (_, stringRange), _), r) ->
                match !appStack with
                | (lastApp :: _) as apps when Range.rangeContainsRange lastApp.Range e.Range ->
                    let intersectsWithFuncOrString (arg: Range.range) =
                        Range.rangeContainsRange arg stringRange
                        || arg = stringRange
                        || Range.rangeContainsRange arg r
                        || arg = r 

                    let rec loop acc (apps: AppWithArg list) =
                        match acc, apps with
                        | _, [] -> acc
                        | [], h :: t -> 
                            if not (intersectsWithFuncOrString h.Arg) then
                                loop [h] t
                            else loop [] t
                        | prev :: _, curr :: rest -> 
                            if Range.rangeContainsRange curr.Range prev.Range 
                               && not (intersectsWithFuncOrString curr.Arg) then
                                loop (curr :: acc) rest 
                            else acc

                    let args = 
                        apps 
                        |> loop []
                        |> List.rev
                        |> List.map (fun x -> x.Arg)
                        |> List.toArray
                    let res = { FormatString = stringRange
                                Args = args }
                    result.Add res
                | _ -> ()
                appStack := []
            | SynExpr.App (_, _, SynExpr.App(_, true, SynExpr.Ident op, e1, _), e2, _) ->
                let rec deconstruct = function
                    | SynExpr.Paren (exp, _, _, _) -> deconstruct exp
                    | SynExpr.Tuple (exps, _, _) -> 
                        exps |> List.iter (fun exp -> addAppWithArg { Range = e.Range; Arg = exp.Range})
                        ()
                    | _ -> ()

                addAppWithArg { Range = e.Range; Arg = e2.Range }
                if op.idText = (PrettyNaming.CompileOpName "||>") 
                        || op.idText = (PrettyNaming.CompileOpName "|||>") then
                    deconstruct e1
                    walkExpr e2
                else
                    if op.idText = (PrettyNaming.CompileOpName "|>") then
                        addAppWithArg { Range = e.Range; Arg = e1.Range }
                    walkExpr e2
                    walkExpr e1
            | SynExpr.App (_, _, SynExpr.App(_, true, _, e1, _), e2, _) ->
                addAppWithArg { Range = e.Range; Arg = e2.Range }
                addAppWithArg { Range = e.Range; Arg = e1.Range }
                walkExpr e1
                walkExpr e2
            | SynExpr.App (_, _, e1, e2, _) ->
                addAppWithArg { Range = e.Range; Arg = e2.Range }
                walkExpr e1
                walkExpr e2
            | _ ->
                match e with
                | SynExpr.Paren (e, _, _, _)
                | SynExpr.Quote (_, _, e, _, _)
                | SynExpr.Typed (e, _, _)
                | SynExpr.InferredUpcast (e, _)
                | SynExpr.InferredDowncast (e, _)
                | SynExpr.AddressOf (_, e, _, _)
                | SynExpr.DoBang (e, _)
                | SynExpr.YieldOrReturn (_, e, _)
                | SynExpr.ArrayOrListOfSeqExpr (_, e, _)
                | SynExpr.CompExpr (_, _, e, _)
                | SynExpr.Do (e, _)
                | SynExpr.Assert (e, _)
                | SynExpr.Lazy (e, _)
                | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
                | SynExpr.Lambda (_, _, pats, e, _) ->
                    walkSimplePats pats
                    walkExpr e
                | SynExpr.New (_, t, e, _)
                | SynExpr.TypeTest (e, t, _)
                | SynExpr.Upcast (e, t, _)
                | SynExpr.Downcast (e, t, _) -> walkExpr e; walkType t
                | SynExpr.Tuple (es, _, _)
                | Sequentials es
                | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
                | SynExpr.TryFinally (e1, e2, _, _, _)
                | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.Record (_, _, fields, _) ->
                    fields |> List.iter (fun (_, e, _) -> e |> Option.iter walkExpr)
                | SynExpr.ObjExpr(ty, argOpt, bindings, ifaces, _, _) ->
                    argOpt |> Option.iter (fun (e, _) -> walkExpr e)
                    walkType ty
                    List.iter walkBinding bindings
                    List.iter walkInterfaceImpl ifaces
                | SynExpr.For (_, _, e1, _, e2, e3, _) -> List.iter walkExpr [e1; e2; e3]
                | SynExpr.ForEach (_, _, _, _, e1, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) ->
                    List.iter walkClause synMatchClauseList
                | SynExpr.Match (_, e, synMatchClauseList, _, _) ->
                    walkExpr e
                    List.iter walkClause synMatchClauseList
                | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
                    List.iter walkType tys; walkExpr e
                | SynExpr.LetOrUse (_, _, bindings, e, _) ->
                    List.iter walkBinding bindings; walkExpr e
                | SynExpr.TryWith (e, _, clauses, _, _, _, _) ->
                    List.iter walkClause clauses;  walkExpr e
                | SynExpr.IfThenElse (e1, e2, e3, _, _, _, _) ->
                    List.iter walkExpr [e1; e2]
                    e3 |> Option.iter walkExpr
                | SynExpr.LongIdentSet (_, e, _)
                | SynExpr.DotGet (e, _, _, _) -> walkExpr e
                | SynExpr.DotSet (e1, _, e2, _) ->
                    walkExpr e1
                    walkExpr e2
                | SynExpr.DotIndexedGet (e, args, _, _) ->
                    walkExpr e
                    List.iter walkIndexerArg args
                | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
                    walkExpr e1
                    List.iter walkIndexerArg args
                    walkExpr e2
                | SynExpr.NamedIndexedPropertySet (_, e1, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.DotNamedIndexedPropertySet (e1, _, e2, e3, _) -> List.iter walkExpr [e1; e2; e3]
                | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.LetOrUseBang (_, _, _, _, e1, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.TraitCall (_, sign, e, _) ->
                    walkMemberSig sign
                    walkExpr e
                | SynExpr.Const (SynConst.Measure(_, m), _) -> walkMeasure m
                | _ -> ()

        and walkMeasure = function
            | SynMeasure.Product (m1, m2, _)
            | SynMeasure.Divide (m1, m2, _) -> walkMeasure m1; walkMeasure m2
            | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
            | SynMeasure.Power (m, _, _) -> walkMeasure m
            | SynMeasure.One
            | SynMeasure.Anon _
            | SynMeasure.Named _
            | SynMeasure.Var _ -> ()

        and walkSimplePat = function
            | SynSimplePat.Attrib (pat, _, _) -> walkSimplePat pat
            | SynSimplePat.Typed(_, t, _) -> walkType t
            | _ -> ()

        and walkField (SynField.Field(_, _, _, t, _, _, _, _)) = walkType t

        and walkMemberSig = function
            | SynMemberSig.Inherit (t, _)
            | SynMemberSig.Interface(t, _) -> walkType t
            | SynMemberSig.ValField(f, _) -> walkField f
            | SynMemberSig.NestedType(SynTypeDefnSig.TypeDefnSig (_, repr, memberSigs, _), _) ->
                walkTypeDefnSigRepr repr
                List.iter walkMemberSig memberSigs
            | SynMemberSig.Member _ -> ()

        and walkMember = function
            | SynMemberDefn.Member (binding, _) -> walkBinding binding
            | SynMemberDefn.ImplicitCtor (_, _, pats, _, _) -> List.iter walkSimplePat pats
            | SynMemberDefn.ImplicitInherit (t, e, _, _) -> walkType t; walkExpr e
            | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
            | SynMemberDefn.Interface (t, members, _) ->
                walkType t
                members |> Option.iter (List.iter walkMember)
            | SynMemberDefn.Inherit (t, _, _) -> walkType t
            | SynMemberDefn.ValField (field, _) -> walkField field
            | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
            | SynMemberDefn.AutoProperty (_, _, _, t, _, _, _, _, e, _, _) ->
                Option.iter walkType t
                walkExpr e
            | _ -> ()

        and walkTypeDefnRepr = function
            | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
            | SynTypeDefnRepr.Simple _ -> ()
            | SynTypeDefnRepr.Exception _ -> ()

        and walkTypeDefnSigRepr = function
            | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
            | SynTypeDefnSigRepr.Simple _ -> ()
            | SynTypeDefnSigRepr.Exception _ -> ()

        and walkTypeDefn (TypeDefn (_, repr, members, _)) =
            walkTypeDefnRepr repr
            List.iter walkMember members

        and walkSynModuleDecl (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule (_, _, modules, _, _) ->
                List.iter walkSynModuleDecl modules
            | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
            | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
            | _ -> ()

        match input with
        | Some (ParsedInput.ImplFile input) ->
             walkImplFileInput input
        | _ -> ()
        //debug "%A" idents
        result.ToArray()