module FSharp.Editing.ProjectSystem.SolutionFileInfo


open System
open System.IO
open System.Text
open Microsoft.CodeAnalysis
open FSharp.Editing


type LineScanner(line : string) =
    let mutable line = line
    let mutable currentPosition = 0

    /// Return the text following the scanner's current position
    member __.ReadRest() =
        let rest = line.Substring currentPosition
        currentPosition <- line.Length
        rest

    /// Return the text between the scanner's current position and
    /// the provided delimiter.
    member self.ReadUpToAndEat(delimiter : string) =
        match line.IndexOf(delimiter, currentPosition) with
        | -1 -> self.ReadRest()
        | index ->
            let upToDelimiter = line.Substring(currentPosition, index - currentPosition)
            currentPosition <- index + delimiter.Length
            upToDelimiter

type SectionBlock = {
    BlockType : string
    ParenthesizedName : string
    Value : string
    KeyMap : (string * string) []
}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module SectionBlock =
    let parse (reader : TextReader) =
        let rec findStart() =
            match reader.ReadLine() with
            | null -> failwith "found null while parsing a section block"
            | line ->
                let startline = line.TrimStart [||]
                if startline <> String.Empty then startline
                else findStart()

        let startline = findStart()
        let scanner = LineScanner startline
        let blockType = scanner.ReadUpToAndEat "("
        let parenthesizedName = scanner.ReadUpToAndEat ") = "
        let sectionValue = scanner.ReadRest()
        printfn "section blocktype is - %s" blockType
        printfn "End%s" blockType
        let rec findPairs() = [|
            match reader.ReadLine() with
            | null -> ()
            | line ->
                match line.TrimStart [||] with
                | txt when txt = "End" + blockType -> ()
                | "" -> yield! findPairs()
                | line ->
                    let scanner = LineScanner line
                    let key = scanner.ReadUpToAndEat " = "
                    let value = scanner.ReadRest()
                    yield (key, value)
                    yield! findPairs()
        |]

        let keyMap = findPairs()
        {   BlockType = blockType
            ParenthesizedName = parenthesizedName
            Value = sectionValue
            KeyMap = keyMap
        }

    let getText indent (block : SectionBlock) =
        let builder =
            StringBuilder().Append('\t', indent).Append(sprintf "%s(%s) = " block.BlockType block.ParenthesizedName)
                .AppendLine block.Value
        for (key, value) in block.KeyMap do
            builder.Append('\t', indent + 1).Append(key).Append(" = ").AppendLine value |> ignore
        builder.Append('\t', indent).Append(sprintf "End%s" block.BlockType).AppendLine() |> string

type ProjectBlock =
    { ProjectTypeGuid : Guid
      ProjectName : string
      ProjectPath : string
      ProjectGuid : Guid
      ProjectSections : SectionBlock [] }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module ProjectBlock =
    let parse (reader : TextReader) : ProjectBlock =
        let startLine = reader.ReadLine().TrimStart [||]
        let scanner = LineScanner startLine
        let invalid str = raise <| exn (sprintf "Invalid Project Block in Solution - expected %s" str)
        if scanner.ReadUpToAndEat "(\"" <> "Project" then invalid "Project"
        let projectTypeGuid = Guid.Parse <| scanner.ReadUpToAndEat "\")"
        if scanner.ReadUpToAndEat("\"").Trim() <> "=" then invalid "="
        let projectName = scanner.ReadUpToAndEat "\""
        if scanner.ReadUpToAndEat("\"").Trim() <> "," then invalid ","
        let projectPath = scanner.ReadUpToAndEat "\""
        if scanner.ReadUpToAndEat("\"").Trim() <> "," then invalid ","
        let projectGuid = Guid.Parse <| scanner.ReadUpToAndEat "\""

        let rec getSections() =
            [| if not (Char.IsWhiteSpace <| char (reader.Peek())) then ()
               else
                   yield SectionBlock.parse reader
                   yield! getSections() |]

        let projectSections = getSections()
        let peekCharNot c = (reader.Peek() |> char) <> c
        // Expect to see "EndProject" but be tolerant with missing tags as in Dev12.
        // Instead, we may see either P' for "Project" or 'G' for "Global", which will be handled next.
        if peekCharNot 'P' && peekCharNot 'G' then
            if reader.ReadLine() <> "EndProject" then invalid "EndProject"
        { ProjectTypeGuid = projectTypeGuid
          ProjectName = projectName
          ProjectPath = projectPath
          ProjectGuid = projectGuid
          ProjectSections = projectSections }



    let getText (projectBlock : ProjectBlock) =
        let typeGuid = projectBlock.ProjectTypeGuid.ToString("B").ToUpper()
        let projGuid = projectBlock.ProjectGuid.ToString("B").ToUpper()
        let builder =
            StringBuilder()
                .Append(sprintf "Project(\"%s\") = \"%s\", \"%s\", \"%s\"" typeGuid projectBlock.ProjectName
                            projectBlock.ProjectPath projGuid).AppendLine()
        for section in projectBlock.ProjectSections do
            SectionBlock.getText 1 section
            |> builder.Append
            |> ignore
        builder.AppendLine "EndProject" |> string

//    let toProjectInfo (workspace:'a when 'a :> Workspace) (slnDirectory:string) (block: ProjectBlock) : ProjectInfo =
//        let path = Path.Combine(slnDirectory,block.ProjectPath)
////        let fileinfo = ProjectFileInfo.fromXDoc (block.ProjectPath |> Path.GetFullPath)
//        let fileinfo = ProjectFileInfo.create (block.ProjectPath |> Path.GetFullPath)
//        ProjectFileInfo.toProjectInfo workspace fileinfo
//        ProjectInfo.Create
//            (   id = ProjectId.CreateFromSerialized block.ProjectGuid
//            ,   version = VersionStamp.Create()
//            ,   name = block.ProjectName
//            ,   assemblyName = "fake assembly name"
//            ,   language = "en-us"
//            ,   filePath = (block.ProjectPath |> Path.GetFullPath)
            // ?outputFilePath:string
            // ?compilationOptions:CompilationOptions
            // ?parseOptions:ParseOptions
            // ?documents:IEnumerable<DocumentInfo>
            // ?projectReferences:IEnumerable<ProjectReference>
            // ?metadataReferences:IEnumerable<MetadataReference>
            // ?analyzerReferences:IEnumerable<Diagnostics.AnalyzerReference>
            // ?additionalDocuments:IEnumerable<DocumentInfo>
            // ?isSubmission:bool
            // ?hostObjectType:Type
//        )

//    let toProjectInfo (projectBlock:ProjectBlock) =
//        ProjectInfo.
//        let p = projectBlock
type SolutionFileInfo =
    { Path : string
      HeaderLines : string []
      VSVersionLineOpt : string
      MinVSVersionLineOpt : string
      ProjectBlocks : ProjectBlock []
      GlobalSectionBlocks : SectionBlock [] }
    member self.Directory = (FileInfo self.Path).Directory.FullName
// NOTE - I'm not a fan of this parsing approach,

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module SolutionFileInfo =
    let getText (sln : SolutionFileInfo) =
        let builder = StringBuilder().AppendLine()
        for line in sln.HeaderLines do
            builder.AppendLine line |> ignore
        for block in sln.ProjectBlocks do
            block
            |> ProjectBlock.getText
            |> builder.Append
            |> ignore
        builder.AppendLine "Global" |> ignore
        for section in sln.GlobalSectionBlocks do
            section
            |> SectionBlock.getText 1
            |> builder.Append
            |> ignore
        builder.AppendLine "EndGlobal" |> string

    let private getNextNonEmptyLine (reader : TextReader) =
        let rec getLine (line : string) =
            if isNull line || line.Trim() <> String.Empty then line
            else getLine <| reader.ReadLine()
        getLine <| reader.ReadLine()

    let private consumeEmptyLines (reader : TextReader) =
        while reader.Peek() <> -1 && "\r\n".Contains(reader.Peek()|> char|> string) do
            reader.ReadLine() |> ignore

    let inline private parseError expected actual =
        raise ^ exn ^ sprintf "invalid global section - expected '%s', was - '%s'" expected actual

    let private parseCheck expected actual =
        if expected <> actual then parseError expected actual

    let private parseGlobal (reader : TextReader) : SectionBlock [] =
        if reader.Peek() = -1 then [||]
        else
        let firstline = getNextNonEmptyLine reader
//        printfn "%s" firstline
        parseCheck "Global" firstline
        let rec getBlocks() = [|
            if reader.Peek() = -1 || not (Char.IsWhiteSpace(reader.Peek() |> char)) then ()
            else
            yield SectionBlock.parse reader
            yield! getBlocks()
        |]

        let globalSectionBlocks = getBlocks()
//        printfn "%A" globalSectionBlocks
        parseCheck "EndGlobal" (getNextNonEmptyLine reader)
        consumeEmptyLines reader
        globalSectionBlocks

    let private parse path (reader:TextReader) =
        let headerLine1 = getNextNonEmptyLine reader
        if isNull headerLine1 || not (headerLine1.StartsWith ^ "Microsoft Visual Studio Solution File") then
            parseError "Microsoft Visual Studio Solution File" headerLine1
        /// skip comment lines and empty lines
        let rec getLines () =
            [| // finish if not a commentline, empty line, or if it's the end of the file
                if reader.Peek() = -1 || not (Array.contains (reader.Peek() |> char) [| '#'; '\r'; '\n' |]) then ()
                else if reader.Peek() = -1 then ()
                else
                yield reader.ReadLine()
                yield! getLines() |]

        let hl = getLines()
        let headerLines = Array.append [| headerLine1 |] hl

        let visualStudioVersionLineOpt =
            if char (reader.Peek()) = 'V' then
                let line = getNextNonEmptyLine reader
                if not (line.StartsWith "VisualStudioVersion") then parseError "VisualStudioVersion" line
                line
            else String.Empty // should this be null?

        let minimumVisualStudioVersionLineOpt =
            if char (reader.Peek()) = 'M' then
                let line = getNextNonEmptyLine reader
                if not ^ line.StartsWith "MinimumVisualStudioVersion" then
                    raise ^ exn "invalid global section - didn't start with 'MinimumVisualStudioVersion'"
                line
            else String.Empty // should this be null?

        // Parse project blocks while we have them
        let rec getBlocks() =
            [|  if char (reader.Peek()) <> 'P' then ()
                else
                yield ProjectBlock.parse reader
                // Comments and Empty Lines between the Project Blocks are skipped
                getLines() |> ignore
                yield! getBlocks() |]

        // Parse project blocks while we have them
        let projectBlocks =
            getBlocks() |> Array.map ^ fun blk ->
                { blk with ProjectPath = Path.Combine(Directory.fromPath path,blk.ProjectPath) }

//        projectBlocks |> Array.iter (printfn "%A")
        // We now have a global block
        let globalSectionBlocks = parseGlobal reader
        if reader.Peek() <> -1 then raise ^ exn "Should be at the end of file"
        {   Path = path
            HeaderLines = headerLines
            VSVersionLineOpt = visualStudioVersionLineOpt
            MinVSVersionLineOpt = minimumVisualStudioVersionLineOpt
            ProjectBlocks = projectBlocks
            GlobalSectionBlocks = globalSectionBlocks
        }

    /// Loads the solution file at path as a filestream
    let loadFile (path : string) =
        use stream = File.OpenRead path
        use reader = new StreamReader(stream)
        parse path reader

    /// Loads the solution file at path as a string closes file and reads the string
    let load (path : string) =
        let path = Path.GetFullPath path
        use reader = new StringReader(File.ReadAllText path)
        parse path reader

//    let toSolutionInfo (workspace:'a when 'a :> Workspace) (solutionFile: SolutionFileInfo) : SolutionInfo =
//
//        let projectInfos =
//            solutionFile.ProjectBlocks
//            |> Array.filter ^ fun block -> not(block.ProjectTypeGuid = Constants.SolutionFolderGuid)
//            |> Array.map ^ ProjectBlock.toProjectInfo workspace solutionFile.Directory
//
//        SolutionInfo.Create(
//            SolutionId.CreateNewId(),
//            VersionStamp.Create(),
//            solutionFile.Path,
//            projectInfos
//        )

