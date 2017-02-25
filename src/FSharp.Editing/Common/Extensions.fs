[<AutoOpen>]
/// Type Extensions and Additional Module Functions
module FSharp.Editing.Extensions

open System
open Microsoft.FSharp.Control

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =

    let inline orElseWith ifNoneThunk option = match option with None -> ifNoneThunk () | Some _ -> option

    let inline ofNull value =
        if obj.ReferenceEquals(value, null) then None else Some value

    let inline ofNullable (value: Nullable<'T>) =
        if value.HasValue then Some value.Value else None

    let inline toNullable (value: 'T option) =
        match value with
        | Some x -> Nullable<_> x
        | None -> Nullable<_> ()

    let inline attempt (f: unit -> 'T) = try Some <| f() with _ -> None

    /// Gets the value associated with the option or the supplied default value.
    let inline getOrElse v =
        function
        | Some x -> x
        | None -> v

    /// Gets the option if Some x, otherwise the supplied default value.
    let inline orElse v =
        function
        | Some x -> Some x
        | None -> v

    /// Gets the value if Some x, otherwise try to get another value by calling a function
    let inline getOrTry f =
        function
        | Some x -> x
        | None -> f()

    /// Gets the option if Some x, otherwise try to get another value
    let inline orTry f =
        function
        | Some x -> Some x
        | None -> f()

    /// Some(Some x) -> Some x | None -> None
    let inline flatten x =
        match x with
        | Some x -> x
        | None -> None

    let inline toList x =
        match x with
        | Some x -> [x]
        | None -> []

    let inline iterElse someAction noneAction opt =
        match opt with
        | Some x -> someAction x
        | None   -> noneAction ()
    


// Async helper functions copied from https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/ControlCollections.Async.fs
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    /// Transforms an Async value using the specified function.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : Async<'T>) : Async<'U> =
        async {
            // Get the input value.
            let! x = value
            // Apply the mapping function and return the result.
            return mapping x
        }

    // Transforms an Async value using the specified Async function.
    [<CompiledName("Bind")>]
    let bind (binding : 'T -> Async<'U>) (value : Async<'T>) : Async<'U> =
        async {
            // Get the input value.
            let! x = value
            // Apply the binding function and return the result.
            return! binding x
        }

    [<RequireQualifiedAccess>]    
    module Array =
        /// Async implementation of Array.map.
        let map (mapping : 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            let len = Array.length array
            let result = Array.zeroCreate len

            async { // Apply the mapping function to each array element.
                for i in 0 .. len - 1 do
                    let! mappedValue = mapping array.[i]
                    result.[i] <- mappedValue

                // Return the completed results.
                return result
            }

        /// Async implementation of Array.mapi.
        let mapi (mapping : int -> 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            let len = Array.length array
            let result = Array.zeroCreate len

            async {
                // Apply the mapping function to each array element.
                for i in 0 .. len - 1 do
                    let! mappedValue = mapping i array.[i]
                    result.[i] <- mappedValue

                // Return the completed results.
                return result
            }

        /// Async implementation of Array.exists.
        let exists (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
            let len = Array.length array
            let rec loop i =
                async {
                    if i >= len then
                        return false
                    else
                        let! found = predicate array.[i]
                        if found then
                            return true
                        else
                            return! loop (i + 1)
                }
            loop 0

    [<RequireQualifiedAccess>]
    module List =
        let rec private mapImpl (mapping, mapped : 'U list, pending : 'T list) =
            async {
                match pending with
                | [] ->
                    // Reverse the list of mapped values before returning it.
                    return List.rev mapped

                | el :: pending ->
                    // Apply the current list element to the mapping function.
                    let! mappedEl = mapping el

                    // Cons the result to the list of mapped values, then continue
                    // mapping the rest of the pending list elements.
                    return! mapImpl (mapping, mappedEl :: mapped, pending)
                }

        /// Async implementation of List.map.
        let map (mapping : 'T -> Async<'U>) (list : 'T list) : Async<'U list> =
            mapImpl (mapping, [], list)





type Async with
    static member EitherEvent(ev1: IObservable<'T>, ev2: IObservable<'U>) = 
        synchronize ^ fun f -> 
            Async.FromContinuations ^ fun (cont, _econt, _ccont) -> 
                let rec callback1 = fun value -> 
                    dispose remover1
                    dispose remover2
                    f cont ^ Choice1Of2 value
                    
                and callback2 = fun value -> 
                    dispose remover1
                    dispose remover2
                    f cont ^ Choice2Of2 value
                    
                and remover1: IDisposable = ev1.Subscribe callback1
                and remover2: IDisposable = ev2.Subscribe callback2
                ()

open System.IO

type Path with

    static member GetFullPathSafe path = try Path.GetFullPath path with _ -> path

    static member GetFileNameSafe path = try Path.GetFileName path with _ -> path


open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text

//
//type System.IServiceProvider with
//    member x.GetService<'T>() = x.GetService(typeof<'T>) :?> 'T
//    member x.GetService<'T, 'S>() = x.GetService(typeof<'S>) :?> 'T




type Solution with 
    member self.GetDocumentFilePath (docId:DocumentId) = 
        let target =
            self.Projects
            |> Seq.collect ^ fun proj -> proj.Documents
            |> Seq.tryFind ^ fun doc -> doc.Id = docId 
        target |> Option.map ^ fun doc -> doc.FilePath    

type Project with
    member self.GetDocumentFilePaths () = self.Documents |> Seq.map ^ fun doc -> doc.FilePath

[<NoComparison>][<NoEquality>]
type CheckResults =
    | Ready of (FSharpParseFileResults * FSharpCheckFileResults) option
    | StillRunning of Async<(FSharpParseFileResults * FSharpCheckFileResults) option>

let getFreshFileCheckResultsTimeoutMillis = getEnvInteger "FSE_GetFreshFileCheckResultsTimeoutMillis" 1000
    
type FSharpChecker with
    member this.ParseDocument(document: Document, options: FSharpProjectOptions, sourceText: string) :  Async<ParsedInput option> = asyncMaybe {
        let! (fileParseResults:FSharpParseFileResults) = this.ParseFileInProject(document.FilePath, sourceText, options) |> liftAsync
        return! fileParseResults.ParseTree
    }

    member this.ParseDocument(document: Document, options: FSharpProjectOptions, ?sourceText: SourceText) = asyncMaybe {
        let! sourceText =
            match sourceText with
            | Some x -> Task.FromResult x
            | None -> document.GetTextAsync()
        return! this.ParseDocument(document, options, sourceText.ToString())
    }

    member this.ParseAndCheckDocument(filePath: string, textVersionHash: int, sourceText: string, options: FSharpProjectOptions, allowStaleResults: bool) : Async<(FSharpParseFileResults * Ast.ParsedInput * FSharpCheckFileResults) option> =
        let parseAndCheckFile = async {
            let! parseResults, checkFileAnswer = this.ParseAndCheckFileInProject(filePath, textVersionHash, sourceText, options)
            return
                match checkFileAnswer with
                | FSharpCheckFileAnswer.Aborted -> 
                    None
                | FSharpCheckFileAnswer.Succeeded(checkFileResults) ->
                    Some (parseResults, checkFileResults)
        }

        let tryGetFreshResultsWithTimeout() : Async<CheckResults> = async {
            try let! worker = Async.StartChild(parseAndCheckFile, getFreshFileCheckResultsTimeoutMillis)
                let! result = worker 
                return Ready result
            with :? TimeoutException -> return StillRunning parseAndCheckFile
        }

        let bindParsedInput(results: (FSharpParseFileResults * FSharpCheckFileResults) option) =
            match results with
            | Some(parseResults, checkResults) ->
                match parseResults.ParseTree with
                | Some parsedInput -> Some (parseResults, parsedInput, checkResults)
                | None -> None
            | None -> None

        if allowStaleResults then
            async {
                let! freshResults = tryGetFreshResultsWithTimeout()
                    
                let! results =
                    match freshResults with
                    | Ready x -> async.Return x
                    | StillRunning worker ->
                        async {
                            match allowStaleResults, this.TryGetRecentCheckResultsForFile(filePath, options) with
                            | true, Some (parseResults, checkFileResults, _) ->
                                return Some (parseResults, checkFileResults)
                            | _ ->
                                return! worker
                        }
                return bindParsedInput results
            }
        else parseAndCheckFile |> Async.map bindParsedInput

    member this.ParseAndCheckDocument(document: Document, options: FSharpProjectOptions, allowStaleResults: bool, ?sourceText: SourceText) : Async<(FSharpParseFileResults * Ast.ParsedInput * FSharpCheckFileResults) option> =
        async {
            let! cancellationToken = Async.CancellationToken
            let! sourceText =
                match sourceText with
                | Some x -> Task.FromResult x |> Async.AwaitTask
                | None -> document.GetTextAsync() |> Async.AwaitTask
            let! textVersion = document.GetTextVersionAsync(cancellationToken) |> Async.AwaitTask
            return! this.ParseAndCheckDocument(document.FilePath, textVersion.GetHashCode(), sourceText.ToString(), options, allowStaleResults)
        }

type FSharpSymbol with
    member this.IsInternalToProject =
        match this with 
        | :? FSharpParameter -> true
        | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember || not m.Accessibility.IsPublic
        | :? FSharpEntity as m -> not m.Accessibility.IsPublic
        | :? FSharpGenericParameter -> true
        | :? FSharpUnionCase as m -> not m.Accessibility.IsPublic
        | :? FSharpField as m -> not m.Accessibility.IsPublic
        | _ -> false


[<RequireQualifiedAccess; NoComparison>] 
type SymbolDeclarationLocation = 
    | CurrentDocument
    | Projects of Project list * isLocalForProject: bool




type FSharpSymbolUse with

    member this.GetDeclarationLocation (currentDocument: Document) : SymbolDeclarationLocation option =
        if this.IsPrivateToFile then Some SymbolDeclarationLocation.CurrentDocument else
        let isSymbolLocalForProject = this.Symbol.IsInternalToProject
                
        let declarationLocation = 
            match this.Symbol.ImplementationLocation with
            | Some x -> Some x
            | None -> this.Symbol.DeclarationLocation
                
        match declarationLocation with
        | Some loc ->
            let filePath = Path.GetFullPathSafe loc.FileName
            let isScript = String.Equals(Path.GetExtension(filePath), ".fsx", StringComparison.OrdinalIgnoreCase)
            if isScript && filePath = currentDocument.FilePath then 
                Some SymbolDeclarationLocation.CurrentDocument
            elif isScript then
                // The standalone script might include other files via '#load'
                // These files appear in project options and the standalone file 
                // should be treated as an individual project
                Some (SymbolDeclarationLocation.Projects ([currentDocument.Project], isSymbolLocalForProject))
            else
                let projects =
                    currentDocument.Project.Solution.GetDocumentIdsWithFilePath(filePath)
                    |> Seq.map (fun x -> x.ProjectId)
                    |> Seq.distinct
                    |> Seq.map currentDocument.Project.Solution.GetProject
                    |> Seq.toList
                match projects with
                | [] -> None
                | projects -> Some (SymbolDeclarationLocation.Projects (projects, isSymbolLocalForProject))
        | None -> None


    member this.IsPrivateToFile = 
        let isPrivate =
            match this.Symbol with
            | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember || m.Accessibility.IsPrivate
            | :? FSharpEntity as m -> m.Accessibility.IsPrivate
            | :? FSharpGenericParameter -> true
            | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
            | :? FSharpField as m -> m.Accessibility.IsPrivate
            | _ -> false
            
        let declarationLocation =
            match this.Symbol.SignatureLocation with
            | Some x -> Some x
            | _ ->
                match this.Symbol.DeclarationLocation with
                | Some x -> Some x
                | _ -> this.Symbol.ImplementationLocation
            
        let declaredInTheFile = 
            match declarationLocation with
            | Some declRange -> declRange.FileName = this.RangeAlternate.FileName
            | _ -> false
            
        isPrivate && declaredInTheFile   

type FSharpMemberOrFunctionOrValue with
    // FullType may raise exceptions (see https://github.com/fsharp/fsharp/issues/307).
    member x.FullTypeSafe = Option.attempt (fun _ -> x.FullType)
        
    member x.IsConstructor = x.CompiledName = ".ctor"
        
    member x.IsOperatorOrActivePattern =
        let name = x.DisplayName
        if name.StartsWith "( " && name.EndsWith " )" && name.Length > 4
        then name.Substring (2, name.Length - 4) |> String.forall (fun c -> c <> ' ')
        else false

    member x.EnclosingEntitySafe =
        try Some x.EnclosingEntity
        with :? InvalidOperationException -> None

type FSharpEntity with
    member x.AllBaseTypes =
        let rec allBaseTypes (entity:FSharpEntity) =
            [   match entity.TryFullName with
                | Some _ ->
                    match entity.BaseType with
                    | Some bt -> yield bt; if bt.HasTypeDefinition then yield! allBaseTypes bt.TypeDefinition
                    | _ -> ()
                | _ -> ()
            ]
        allBaseTypes x

//type FSharpNavigationDeclarationItem with
//    member x.RoslynGlyph : Glyph =
//        match x.GlyphMajor with
//        | GlyphMajor.Class
//        | GlyphMajor.Typedef
//        | GlyphMajor.Type
//        | GlyphMajor.Exception ->
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.ClassPrivate
//            | Some SynAccess.Internal -> Glyph.ClassInternal
//            | _ -> Glyph.ClassPublic
//        | GlyphMajor.Constant -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.ConstantPrivate
//            | Some SynAccess.Internal -> Glyph.ConstantInternal
//            | _ -> Glyph.ConstantPublic
//        | GlyphMajor.Delegate -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.DelegatePrivate
//            | Some SynAccess.Internal -> Glyph.DelegateInternal
//            | _ -> Glyph.DelegatePublic
//        | GlyphMajor.Union
//        | GlyphMajor.Enum -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.EnumPrivate
//            | Some SynAccess.Internal -> Glyph.EnumInternal
//            | _ -> Glyph.EnumPublic
//        | GlyphMajor.EnumMember
//        | GlyphMajor.Variable
//        | GlyphMajor.FieldBlue -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.FieldPrivate
//            | Some SynAccess.Internal -> Glyph.FieldInternal
//            | _ -> Glyph.FieldPublic
//        | GlyphMajor.Event -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.EventPrivate
//            | Some SynAccess.Internal -> Glyph.EventInternal
//            | _ -> Glyph.EventPublic
//        | GlyphMajor.Interface -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.InterfacePrivate
//            | Some SynAccess.Internal -> Glyph.InterfaceInternal
//            | _ -> Glyph.InterfacePublic
//        | GlyphMajor.Method
//        | GlyphMajor.Method2 -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.MethodPrivate
//            | Some SynAccess.Internal -> Glyph.MethodInternal
//            | _ -> Glyph.MethodPublic
//        | GlyphMajor.Module -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.ModulePrivate
//            | Some SynAccess.Internal -> Glyph.ModuleInternal
//            | _ -> Glyph.ModulePublic
//        | GlyphMajor.NameSpace -> Glyph.Namespace
//        | GlyphMajor.Property -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.PropertyPrivate
//            | Some SynAccess.Internal -> Glyph.PropertyInternal
//            | _ -> Glyph.PropertyPublic
//        | GlyphMajor.Struct
//        | GlyphMajor.ValueType -> 
//            match x.Access with
//            | Some SynAccess.Private -> Glyph.StructurePrivate
//            | Some SynAccess.Internal -> Glyph.StructureInternal
//            | _ -> Glyph.StructurePublic
//        | GlyphMajor.Error -> Glyph.Error
//        | _ -> Glyph.None


 
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    let toReadOnlyCollection (xs: _ seq) = ResizeArray(xs).AsReadOnly()
    

[<RequireQualifiedAccess>]
module List =
    /// Fold over the list passing the index and element at that index to a folding function
    let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (xs: 'T list) =
        match xs with 
        | [] -> state
        | _ -> 
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
            let rec loop idx s xs = 
                match xs with 
                | [] -> s
                | h::t -> loop (idx+1) (f.Invoke(s,idx,h)) t
            loop 0 state xs

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    let inline private checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()

    /// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
    /// ~2x faster for floats
    /// ~0.8x slower for ints
    let inline areEqual (xs: 'T []) (ys: 'T []) =
        match xs, ys with
        | null, null -> true
        | [||], [||] -> true
        | null, _ | _, null -> false
        | _ when xs.Length <> ys.Length -> false
        | _ ->
            let mutable break' = false
            let mutable i = 0
            let mutable result = true
            while i < xs.Length && not break' do
                if xs.[i] <> ys.[i] then 
                    break' <- true
                    result <- false
                i <- i + 1
            result

    /// check if subArray is found in the wholeArray starting 
    /// at the provided index
    let inline isSubArray (subArray: 'T []) (wholeArray:'T []) index = 
        if isNull subArray || isNull wholeArray then false
        elif subArray.Length = 0 then true
        elif subArray.Length > wholeArray.Length then false
        elif subArray.Length = wholeArray.Length then areEqual subArray wholeArray else
        let rec loop subidx idx =
            if subidx = subArray.Length then true 
            elif subArray.[subidx] = wholeArray.[idx] then loop (subidx+1) (idx+1) 
            else false
        loop 0 index

    /// Returns true if one array has another as its subset from index 0.
    let startsWith (prefix: _ []) (whole: _ []) =
        isSubArray prefix whole 0

    /// Returns true if one array has trailing elements equal to another's.
    let endsWith (suffix: _ []) (whole: _ []) =
        isSubArray suffix whole (whole.Length-suffix.Length)

    /// Returns a new array with an element replaced with a given value.
    let replace index value (array: _ []) =
        checkNonNull "array" array
        if index >= array.Length then raise (IndexOutOfRangeException "index")
        let res = Array.copy array
        res.[index] <- value
        res

    /// Returns all heads of a given array.
    /// For [|1;2;3|] it returns [|[|1; 2; 3|]; [|1; 2|]; [|1|]|]
    let heads (array: 'T []) =
        checkNonNull "array" array
        let res = Array.zeroCreate<'T[]> array.Length
        for i = array.Length - 1 downto 0 do
            res.[i] <- array.[0..i]
        res

    /// Fold over the array passing the index and element at that index to a folding function
    let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (array: 'T []) =
        checkNonNull "array" array
        if array.Length = 0 then state else
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state:'State = state
        let len = array.Length
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, i, array.[i])
        state

    /// pass an array byref to reverse it in place
    let revInPlace (array: 'T []) =
        checkNonNull "array" array
        if areEqual array [||] then () else
        let arrlen, revlen = array.Length-1, array.Length/2 - 1
        for idx in 0 .. revlen do
            let t1 = array.[idx] 
            let t2 = array.[arrlen-idx]
            array.[idx] <- t2
            array.[arrlen-idx] <- t1

    /// Map all elements of the array that satisfy the predicate
    let filterMap predicate mapfn (array: 'T [])  =
        checkNonNull "array" array
        if array.Length = 0 then [||] else
        let result = Array.zeroCreate array.Length
        let mutable count = 0
        for elm in array do
            if predicate elm then 
               result.[count] <- mapfn elm
               count <- count + 1
        if count = 0 then [||] else
        result.[0..count-1]

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the given function returns
    /// <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <remarks>
    /// This function is similar to Array.partition, but it allows the returned collections to have different types.
    /// </remarks>
    let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) array : 'U1[] * 'U2[] =
        // Preconditions
        checkNonNull "array" array
        
        // OPTIMIZATION : If the input array is empty, immediately return empty results.
        if Array.isEmpty array then
            Array.empty, Array.empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()
    
            // Partition the array, adding each element to the ResizeArray
            // specific by the partition function.
            array
            |> Array.iter (fun el ->
                match partitioner el with
                | Choice1Of2 value ->
                    resultList1.Add value
                | Choice2Of2 value ->
                    resultList2.Add value)
    
            // Convert the ResizeArrays to arrays and return them.
            resultList1.ToArray (),
            resultList2.ToArray ()

    let splitByChunks (chunkSizes : int[]) (arr : 'T[]) =
        let rec loop (chunks : int[]) (arr : 'T[]) acc =
            match chunks, arr with
            | [||], _ 
            | _, [||] -> acc
            | _ ->
                let chunk = min chunks.[0] arr.Length
                loop chunks.[1 .. ] arr.[chunk .. ] (arr.[0..(chunk-1)] :: acc)

        loop chunkSizes arr []
        |> Array.ofList
        |> Array.rev

    let toShortHexString (bytes: byte[]) =
        let length = bytes.Length
        let chars = Array.zeroCreate length
        for i in 0..length/2-1 do
            let b1 = byte (bytes.[i] >>> 4)
            chars.[i * 2] <- if b1 > 9uy then char (b1 + 87uy) else char (b1 + 0x30uy)
            let b2 = byte (bytes.[i] &&& 0xFuy)
            chars.[i * 2 + 1] <- if b2 > 9uy then char (b2 + 87uy) else char (b2 + 0x30uy)
        String chars




[<RequireQualifiedAccess>]
module Dict = 
    open System.Collections.Generic

    let add key value (dict: #IDictionary<_,_>) =
        dict.[key] <- value
        dict

    let remove (key: 'k) (dict: #IDictionary<'k,_>) =
        dict.Remove key |> ignore
        dict

    let tryFind key (dict: #IDictionary<'k, 'v>) = 
        let mutable value = Unchecked.defaultof<_>
        if dict.TryGetValue (key, &value) then Some value
        else None

    let ofSeq (xs: ('k * 'v) seq) = 
        let dict = Dictionary()
        for k, v in xs do dict.[k] <- v
        dict

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    let inline toCharArray (str:string) = str.ToCharArray()

    let lowerCaseFirstChar (str: string) =
        if String.IsNullOrEmpty str 
         || Char.IsLower(str, 0) then str else 
        let strArr = toCharArray str
        match Array.tryHead strArr with
        | None -> str
        | Some c  -> 
            strArr.[0] <- Char.ToLower c
            String (strArr)


    let extractTrailingIndex (str: string) =
        match str with
        | null -> null, None
        | _ ->
            let charr = str.ToCharArray() 
            Array.revInPlace charr
            let digits = Array.takeWhile Char.IsDigit charr
            Array.revInPlace digits
            String digits
            |> function
               | "" -> str, None
               | index -> str.Substring (0, str.Length - index.Length), Some (int index)

    /// Remove all trailing and leading whitespace from the string
    /// return null if the string is null
    let trim (value: string) = if isNull value then null else value.Trim()
    
    /// Splits a string into substrings based on the strings in the array separators
    let split options (separator: string []) (value: string) = 
        if isNull value  then null else value.Split(separator, options)

    let (|StartsWith|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.StartsWith pattern then
            Some()
        else None

    let (|Contains|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.Contains pattern then
            Some()
        else None
    
    open System.IO

    let getLines (str: string) =
        use reader = new StringReader(str)
        [|
        let line = ref (reader.ReadLine())
        while isNotNull (!line) do
            yield !line
            line := reader.ReadLine()
        if str.EndsWith("\n") then
            // last trailing space not returned
            // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
            yield String.Empty
        |]

    let getNonEmptyLines (str: string) =
        use reader = new StringReader(str)
        [|
        let line = ref (reader.ReadLine())
        while isNotNull (!line) do
            if (!line).Length > 0 then
                yield !line
            line := reader.ReadLine()
        |]

    /// Parse a string to find the first nonempty line
    /// Return null if the string was null or only contained empty lines
    let firstNonEmptyLine (str: string) =
        use reader = new StringReader (str)
        let rec loop (line:string) =
            if isNull line then None 
            elif  line.Length > 0 then Some line
            else loop (reader.ReadLine())
        loop (reader.ReadLine())


open System.Text
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StringBuilder =
    /// Pipelining function for appending a string to a stringbuilder
    let inline append (str:string) (sb:StringBuilder) = sb.Append str

    /// Pipelining function for appending a string with a '\n' to a stringbuilder
    let inline appendLine (str:string) (sb:StringBuilder) = sb.AppendLine str
    
    /// SideEffecting function for appending a string to a stringbuilder
    let inline appendi (str:string) (sb:StringBuilder) = sb.Append str |> ignore

    /// SideEffecting function for appending a string with a '\n' to a stringbuilder
    let inline appendLinei (str:string) (sb:StringBuilder) = sb.AppendLine str |> ignore


module Reflection =
    open System.Reflection
    open Microsoft.FSharp.Reflection

    type private Expr = System.Linq.Expressions.Expression
    let instanceNonPublic = BindingFlags.Instance ||| BindingFlags.NonPublic
    
    let precompileFieldGet<'R>(f : FieldInfo) =
        let p = Expr.Parameter(typeof<obj>)
        let lambda = Expr.Lambda<Func<obj, 'R>>(Expr.Field(Expr.Convert(p, f.DeclaringType) :> Expr, f) :> Expr, p)
        lambda.Compile().Invoke

    // Various flags configurations for Reflection
    let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
    let instanceFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
    let ctorFlags = instanceFlags
    let inline asMethodBase (a : #MethodBase) = a :> MethodBase
    
    let (?) (o : obj) name : 'R = 
        // The return type is a function, which means that we want to invoke a method
        if FSharpType.IsFunction(typeof<'R>) then 
            let argType, _resType = FSharpType.GetFunctionElements(typeof<'R>)
            FSharpValue.MakeFunction(typeof<'R>, 
                fun args -> 
                    // We treat elements of a tuple passed as argument as a list of arguments
                    // When the 'o' object is 'System.Type', we call static methods
                    let methods, instance, args = 
                        let typeInfo = o.GetType()
                        let args = 
                            if argType = typeof<unit> then [||]
                            elif not (FSharpType.IsTuple(argType)) then [| args |]
                            else FSharpValue.GetTupleFields(args)
                        if (typeof<System.Type>).IsAssignableFrom(typeInfo) then 
                            let methods = (unbox<Type> o).GetMethods(staticFlags) |> Array.map asMethodBase
                            let ctors = 
                                (unbox<Type> o).GetConstructors(ctorFlags) 
                                |> Array.map asMethodBase
                            Array.concat [ methods; ctors ], null, args
                        else 
                            typeInfo.GetMethods(instanceFlags) |> Array.map asMethodBase, o, 
                            args
                                         
                    // A simple overload resolution based on the name and number of parameters only
                    let methods = 
                        [ for m in methods do
                            if m.Name = name && m.GetParameters().Length = args.Length then 
                                yield m ]
                                         
                    match methods with
                    | [] -> failwithf "No method '%s' with %d arguments found" name args.Length
                    | _ :: _ :: _ -> 
                        failwithf "Multiple methods '%s' with %d arguments found" name args.Length
                    | [ :? ConstructorInfo as c ] -> c.Invoke(args)
                    | [ m ] -> m.Invoke(instance, args))
            |> unbox<'R>
        else 
            // When the 'o' object is 'System.Type', we access static properties
            let typ, flags, instance = 
                if (typeof<System.Type>).IsAssignableFrom(o.GetType()) then unbox o, staticFlags, null
                else o.GetType(), instanceFlags, o
            
            // Find a property that we can call and get the value
            let prop = typ.GetProperty(name, flags)
            if prop = null then failwithf "Property '%s' not found in '%s' using flags '%A'." name typ.Name flags
            let meth = prop.GetGetMethod(true)
            if prop = null then failwithf "Property '%s' found, but doesn't have 'get' method." name
            meth.Invoke(instance, [||]) |> unbox<'R>

    let (?<-) (o: obj) name value = o.GetType().GetProperty(name).SetValue(o, value, null)

module File =
    open System.IO

    let tryGetLastWriteTime file = Option.attempt (fun _ -> FileInfo(file).LastWriteTimeUtc)
