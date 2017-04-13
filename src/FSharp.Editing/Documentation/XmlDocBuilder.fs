namespace FSharp.Editing.Documentation


//open System
//open System.Text
//open System.Collections.Generic
//open System.Text.RegularExpressions
//open Internal.Utilities.Collections
//open Microsoft.CodeAnalysis
//open Microsoft.CodeAnalysis.Classification
//open Microsoft.CodeAnalysis.Completion
//open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
//open Microsoft.FSharp.Compiler.SourceCodeServices
//open Microsoft.FSharp.Compiler.SourceCodeServices
//open Microsoft.FSharp.Compiler.Layout
//open Microsoft.FSharp.Compiler.Layout.TaggedTextOps
//open FSharp.Editing


//type ITaggedTextCollector =
//    abstract Add: text: TaggedText -> unit
//    abstract EndsWithLineBreak: bool
//    abstract IsEmpty: bool
//    abstract StartXMLDoc: unit -> unit

//type TextSanitizingCollector(collector, ?lineLimit: int) =
//    let mutable isEmpty = true 
//    let mutable endsWithLineBreak = false
//    let mutable count = 0
//    let mutable startXmlDoc = false

//    let addTaggedTextEntry text =
//        match lineLimit with
//        | Some lineLimit when lineLimit = count ->
//            // add ... when line limit is reached
//            collector (tagText "...")
//            count <- count + 1
//        | _ ->
//            isEmpty <- false
//            endsWithLineBreak <- match text with LayoutTag.LineBreak _ -> true | _ -> false
//            if endsWithLineBreak then count <- count + 1
//            collector text
    
//    static let splitTextRegex = Regex(@"\s*\n\s*\n\s*", RegexOptions.Compiled ||| RegexOptions.ExplicitCapture)
//    static let normalizeSpacesRegex = Regex(@"\s+", RegexOptions.Compiled ||| RegexOptions.ExplicitCapture)

//    let reportTextLines (s: string) =
//        // treat _double_ newlines as line breaks and remove all \n after that
//        let paragraphs = splitTextRegex.Split(s.Replace("\r", "")) |> Array.filter (not << String.IsNullOrWhiteSpace)
//        paragraphs
//        |> Array.iteri (fun i paragraph ->
//            let paragraph = normalizeSpacesRegex.Replace(paragraph, " ")
//            let paragraph = 
//                // it's the first line of XML Doc. It often has heading '\n' and spaces, we should remove it.
//                // We should not remove them from subsequent lines, because spaces may be proper delimiters 
//                // between plane text and formatted code.
//                if startXmlDoc then 
//                    startXmlDoc <- false
//                    paragraph.TrimStart() 
//                else paragraph
    
         
//            addTaggedTextEntry (tagText paragraph)
//            if i < paragraphs.Length - 1 then
//                // insert two line breaks to separate paragraphs
//                addTaggedTextEntry Literals.lineBreak
//                addTaggedTextEntry Literals.lineBreak)

//    member __.TextLines = reportTextLines
    
    
//    interface ITaggedTextCollector with
//        member __.Add (_text: TaggedText ) = ()
//        member __.EndsWithLineBreak = true
//        member __.IsEmpty = false
//        member __.StartXMLDoc () = ()
     



////    interface ITaggedTextCollector with
////        member this.Add text = 
////            // TODO: bail out early if line limit is already hit
////            match text with
////            | TaggedText.Text t -> reportTextLines t
////            | addTaggedTextEntry t
////            | TaggedText.ActivePatternCase(_) -> Unchecked.defautOf<_>
////            | TaggedText.ActivePatternResult(_) -> Unchecked.defautOf<_>
////            | TaggedText.Alias(_) -> Unchecked.defautOf<_>
////            | TaggedText.Class(_) -> Unchecked.defautOf<_>
////            | TaggedText.Union(_) -> Unchecked.defautOf<_>
////            | TaggedText.UnionCase(_) -> Unchecked.defautOf<_>
////            | TaggedText.Delegate(_) -> Unchecked.defautOf<_>
////            | TaggedText.Enum(_) -> Unchecked.defautOf<_>
////            | TaggedText.Event(_) -> Unchecked.defautOf<_>
////            | TaggedText.Field(_) -> Unchecked.defautOf<_>
////            | TaggedText.Interface(_) -> Unchecked.defautOf<_>
////            | TaggedText.Keyword(_) -> Unchecked.defautOf<_>
////            | TaggedText.LineBreak(_) -> Unchecked.defautOf<_>
////            | TaggedText.Local(_) -> Unchecked.defautOf<_>
////            | TaggedText.Record(_) -> Unchecked.defautOf<_>
////            | TaggedText.RecordField(_) -> Unchecked.defautOf<_>
////            | TaggedText.Method(_) -> Unchecked.defautOf<_>
////            | TaggedText.Member(_) -> Unchecked.defautOf<_>
////            | TaggedText.ModuleBinding(_) -> Unchecked.defautOf<_>
////            | TaggedText.Module(_) -> Unchecked.defautOf<_>
////            | TaggedText.Namespace(_) -> Unchecked.defautOf<_>
////            | TaggedText.NumericLiteral(_) -> Unchecked.defautOf<_>
////            | TaggedText.Operator(_) -> Unchecked.defautOf<_>
////            | TaggedText.Parameter(_) -> Unchecked.defautOf<_>
////            | TaggedText.Property(_) -> Unchecked.defautOf<_>
////            | TaggedText.Space(_) -> Unchecked.defautOf<_>
////            | TaggedText.StringLiteral(_) -> Unchecked.defautOf<_>
////            | TaggedText.Struct(_) -> Unchecked.defautOf<_>
////            | TaggedText.TypeParameter(_) -> Unchecked.defautOf<_>
////            | TaggedText.Punctuation(_) -> Unchecked.defautOf<_>
////            | TaggedText.UnknownType(_) -> Unchecked.defautOf<_>
////            | TaggedText.UnknownEntity(_) -> Unchecked.defautOf<_>

//    member this.IsEmpty = isEmpty
//    member this.EndsWithLineBreak = isEmpty || endsWithLineBreak
//    member this.StartXMLDoc() = startXmlDoc <- true



///// XmlDocumentation builder, using the VS interfaces to build documentation.  An interface is used
///// to allow unit testing to give an alternative implementation which captures the documentation.
//type IDocumentationBuilder =

//    /// Append the given raw XML formatted into the string builder
//    abstract AppendDocumentationFromProcessedXML : collector: ITaggedTextCollector * processedXml:string * showExceptions:bool * showParameters:bool * paramName:string option-> unit

//    /// Appends text for the given filename and signature into the StringBuilder
//    abstract AppendDocumentation : collector: ITaggedTextCollector * filename: string * signature: string * showExceptions: bool * showParameters: bool * paramName: string option-> unit

///// Documentation helpers.
//module XmlDocumentation =

//    /// If the XML comment starts with '<' not counting whitespace then treat it as a literal XML comment.
//    /// Otherwise, escape it and surround it with <summary></summary>
//    let ProcessXml (xml:string) =
//        if String.IsNullOrEmpty xml then xml else
//        let trimmedXml = xml.TrimStart([|' ';'\r';'\n'|])
//        if trimmedXml.Length > 0 then
//            if trimmedXml.[0] <> '<' then 
//            #if !NETCORE
//                // This code runs for local/within-project xmldoc tooltips, but not for cross-project or .XML - for that see ast.fs in the compiler
//                let escapedXml = System.Security.SecurityElement.Escape(xml)
//                "<summary>" + escapedXml + "</summary>"
//            #else
//                "<summary>" + xml + "</summary>"
//            #endif
//            else 
//                "<root>" + xml + "</root>"
//        else xml

//    let AppendHardLine(collector: ITaggedTextCollector) =
//        collector.Add Literals.lineBreak
       
//    let EnsureHardLine(collector: ITaggedTextCollector) =
//        if not collector.EndsWithLineBreak then AppendHardLine collector
        
//    let AppendOnNewLine (collector: ITaggedTextCollector) (line:string) =
//        if line.Length > 0 then 
//            EnsureHardLine collector
//            collector.Add(TaggedTextOps.tagText line)

//    open System.Xml
//    open System.Xml.Linq

//    let rec private WriteElement (collector: ITaggedTextCollector) (n: XNode) = 
//        match n.NodeType with
//        | XmlNodeType.Text -> 
//            WriteText collector (n :?> XText)
//        | XmlNodeType.Element ->
//            let el = n :?> XElement
//            match el.Name.LocalName with
//            | "see" | "seealso" -> 
//                for attr in el.Attributes() do
//                    WriteAttribute collector attr "cref" (WriteTypeName collector)
//            | "paramref" | "typeref" ->
//                for attr in el.Attributes() do
//                    WriteAttribute collector attr "name" (tagParameter >> collector.Add)
//            | _ -> 
//                WriteNodes collector (el.Nodes())
//        | _ -> ()
                
//    and WriteNodes (collector: ITaggedTextCollector) (nodes: seq<XNode>) = 
//        for n in nodes do
//            WriteElement collector n

//    and WriteText (collector: ITaggedTextCollector) (n: XText) = 
//        collector.Add(tagText n.Value)

//    and WriteAttribute (collector: ITaggedTextCollector) (attr: XAttribute) (taggedName: string) tagger = 
//        if attr.Name.LocalName = taggedName then
//            tagger attr.Value
//        else
//            collector.Add(tagText attr.Value)

//    and WriteTypeName (collector: ITaggedTextCollector) (typeName: string) =
//        let typeName = if typeName.StartsWith("T:") then typeName.Substring(2) else typeName
//        let parts = typeName.Split([|'.'|])
//        for i = 0 to parts.Length - 2 do
//            collector.Add(tagNamespace parts.[i])
//            collector.Add(Literals.dot)
//        collector.Add(tagClass parts.[parts.Length - 1])

//    type XmlDocReader(s: string) = 
//        let doc = XElement.Parse(ProcessXml(s))
//        let tryFindParameter name = 
//            doc.Descendants (XName.op_Implicit "param")
//            |> Seq.tryFind (fun el -> 
//                match el.Attribute(XName.op_Implicit "name") with
//                | null -> false
//                | attr -> attr.Value = name)

//        member __.CollectSummary(collector: ITaggedTextCollector) = 
//            match Seq.tryHead (doc.Descendants(XName.op_Implicit "summary")) with
//            | None -> ()
//            | Some el ->
//                EnsureHardLine collector
//                WriteElement collector el

//        member this.CollectParameter(collector: ITaggedTextCollector, paramName: string) =
//            match tryFindParameter paramName with
//            | None -> ()
//            | Some el ->
//                EnsureHardLine collector
//                WriteNodes collector (el.Nodes())
           
//        member this.CollectParameters(collector: ITaggedTextCollector) =
//            for p in doc.Descendants(XName.op_Implicit "param") do
//                match p.Attribute(XName.op_Implicit "name") with
//                | null -> ()
//                | name ->
//                    EnsureHardLine collector
//                    collector.Add(tagParameter name.Value)
//                    collector.Add(Literals.colon)
//                    collector.Add(Literals.space)
//                    WriteNodes collector (p.Nodes())

//        member this.CollectExceptions(collector: ITaggedTextCollector) =
//            let mutable started = false;
//            for p in doc.Descendants(XName.op_Implicit "exception") do
//                match p.Attribute(XName.op_Implicit "cref") with
//                | null -> ()
//                | exnType ->
//                    if not started then
//                        started <- true
//                        AppendHardLine collector
//                        AppendHardLine collector
//                        AppendOnNewLine collector "ExceptionsHeader"
//                    EnsureHardLine collector
//                    collector.Add(tagSpace "    ")
//                    WriteTypeName collector exnType.Value
//                    if not (Seq.isEmpty (p.Nodes())) then
//                        collector.Add Literals.space
//                        collector.Add Literals.minus
//                        collector.Add Literals.space
//                        WriteNodes collector (p.Nodes())


//    open System.Collections.Concurrent

////    type VsThreadToken() = class end
////    let vsToken = VsThreadToken()
////    
//    /// Provide Xml Documentation             
////    type Provider(xmlIndexService:IVsXMLMemberIndexService, dte: DTE) = 
//    type Provider () =
//        /// Index of assembly name to xml member index.
////        let mutable xmlCache = new AgedLookup<VsThreadToken,string,IVsXMLMemberIndex>(10,areSame=(fun (x,y) -> x = y))
//        let mutable xmlCache = new ConcurrentDictionary<_,_>()
        
////        let events = dte.Events :?> Events2
////        let solutionEvents = events.SolutionEvents
////        do solutionEvents.add_AfterClosing(fun () -> 
////            xmlCache.Clear(vsToken))

//    // #if DEBUG // Keep under DEBUG so that it can keep building.

//    //     let _AppendTypeParameters (collector: ITaggedTextCollector) (memberData:IVsXMLMemberData3) = 
//    //         let ok,count = memberData.GetTypeParamCount()
//    //         if Com.Succeeded(ok) && count > 0 then 
//    //             for param in 0..count do
//    //                 let ok,name,text = memberData.GetTypeParamTextAt(param)
//    //                 if Com.Succeeded(ok) then
//    //                     EnsureHardLine collector
//    //                     collector.Add(tagTypeParameter name)
//    //                     collector.Add(Literals.space)
//    //                     collector.Add(tagPunctuation "-")
//    //                     collector.Add(Literals.space)
//    //                     collector.Add(tagText text)

//    //     let _AppendRemarks (collector: ITaggedTextCollector) (memberData:IVsXMLMemberData3) = 
//    //         let ok,remarksText = memberData.GetRemarksText()
//    //         if Com.Succeeded(ok) then 
//    //             AppendOnNewLine collector remarksText            
//    // #endif

////        let _AppendReturns (collector: ITaggedTextCollector) (memberData:IVsXMLMemberData3) = 
////            let ok,returnsText = memberData.GetReturnsText()
////            if Com.Succeeded(ok) then 
////                if not collector.EndsWithLineBreak then 
////                    AppendHardLine(collector)
////                    AppendHardLine(collector)
////                AppendOnNewLine collector returnsText
////
//        /// Retrieve the pre-existing xml index or None
//        let GetMemberIndexOfAssembly (assemblyName) =
////            match xmlCache.TryGet(vsToken, assemblyName) with 
//            match xmlCache.TryGetValue (assemblyName) with 
//            | true , memberIndex -> Some memberIndex
//            | _ -> 
//                None
////                let ok,memberIndex = xmlIndexService.CreateXMLMemberIndex(assemblyName)
////                if Com.Succeeded(ok) then 
////                    let ok = memberIndex.BuildMemberIndex()
////                    if Com.Succeeded(ok) then 
////                        xmlCache.Put(vsToken, assemblyName,memberIndex)
////                        Some(memberIndex)
////                    else None
////                else None

//        let AppendMemberData(collector: ITaggedTextCollector, xmlDocReader: XmlDocReader,showExceptions:bool,showParameters:bool) =
//            AppendHardLine collector
//            collector.StartXMLDoc()
//            xmlDocReader.CollectSummary(collector)
////          AppendParameters appendTo memberData
////          AppendTypeParameters appendTo memberData
//            if (showParameters) then
//                xmlDocReader.CollectParameters collector
//                // Not showing returns because there's no resource localization in language service to place the "returns:" text
//                // AppendReturns appendTo memberData
//            if (showExceptions) then 
//                xmlDocReader.CollectExceptions collector
////          AppendRemarks appendTo memberData

//        interface IDocumentationBuilder with 
//            /// Append the given processed XML formatted into the string builder
//            override this.AppendDocumentationFromProcessedXML(appendTo, processedXml, showExceptions, showParameters, paramName) = 
//                let xmlDocReader = XmlDocReader(processedXml)
//                if paramName.IsSome then
//                    xmlDocReader.CollectParameter(appendTo, paramName.Value)
//                else
//                    AppendMemberData(appendTo, xmlDocReader, showExceptions,showParameters)

//            /// Append Xml documentation contents into the StringBuilder
//            override this.AppendDocumentation
//                            ( /// ITaggedTextCollector to add to
//                              _sink: ITaggedTextCollector,
//                              /// Name of the library file
//                              filename:string,
//                              /// Signature of the comment
//                              _signature:string,
//                              /// Whether to show exceptions
//                              _showExceptions:bool,
//                              /// Whether to show parameters and return
//                              _showParameters:bool,
//                              /// Name of parameter
//                              _paramName:string option                            
//                             ) = 
//                try     
//                    match GetMemberIndexOfAssembly(filename) with
//                    | Some(_index) ->
////                        let _,idx = index.ParseMemberSignature(signature)
////                        if idx <> 0u then
////                            let ok,xml = index.GetMemberXML(idx)
////                            if Com.Succeeded(ok) then 
////                                (this:>IDocumentationBuilder).AppendDocumentationFromProcessedXML(sink, xml, showExceptions, showParameters, paramName)
//                            ()
//                    | None -> ()
//                with e-> 
//                    Assert.Exception(e)
//                    reraise()    
 
//    /// Append an XmlCommnet to the segment.
//    let AppendXmlComment(documentationProvider:IDocumentationBuilder, sink: ITaggedTextCollector, xml, showExceptions, showParameters, paramName) =
//        match xml with
//        | FSharpXmlDoc.None -> ()
//        | FSharpXmlDoc.XmlDocFileSignature(filename,signature) -> 
//            documentationProvider.AppendDocumentation(sink, filename, signature, showExceptions, showParameters, paramName)
//        | FSharpXmlDoc.Text(rawXml) ->
//            let processedXml = ProcessXml(rawXml)
//            documentationProvider.AppendDocumentationFromProcessedXML(sink, processedXml, showExceptions, showParameters, paramName)

//    let private AddSeparator (collector: ITaggedTextCollector) =
//        if not collector.IsEmpty then
//            EnsureHardLine collector
//            collector.Add (tagText "-------------")
//            AppendHardLine collector

//    /// Build a data tip text string with xml comments injected.
//    let BuildTipText(_documentationProvider:IDocumentationBuilder, _ataTipText: FSharpToolTipText , textCollector, xmlCollector, _showText, _showExceptions, _showParameters, _showOverloadText) = 
//        let textCollector: ITaggedTextCollector = TextSanitizingCollector(textCollector, lineLimit = 45) :> _
//        let xmlCollector: ITaggedTextCollector = TextSanitizingCollector(xmlCollector, lineLimit = 45) :> _

//        let _addSeparatorIfNecessary add =
//            if add then
//                AddSeparator textCollector
//                AddSeparator xmlCollector

//        //let Process add (dataTipElement:  FSharpToolTipElement) = dataTipElement
//        ()
////            match dataTipElement with 
////            | FSharpStructuredToolTipElement.None -> false
////            | FSharpStructuredToolTipElement.Single (text, xml) ->
////                addSeparatorIfNecessary add
////                if showText then 
////                    renderL (taggedTextListR textCollector.Add) text |> ignore
////                AppendXmlComment(documentationProvider, xmlCollector, xml, showExceptions, showParameters, None)
////                true
////            | FSharpStructuredToolTipElement.SingleParameter(text, xml, paramName) ->
////                addSeparatorIfNecessary add
////                if showText then
////                    renderL (taggedTextListR textCollector.Add) text |> ignore
////                AppendXmlComment(documentationProvider, xmlCollector, xml, showExceptions, showParameters, Some paramName)
////                true
////            | FSharpStructuredToolTipElement.Group (overloads) -> 
////                let overloads = Array.ofList overloads
////                let len = Array.length overloads
////                if len >= 1 then
////                    addSeparatorIfNecessary add
////                    if showOverloadText then 
////                        let AppendOverload(text,_) = 
////                            if not(Microsoft.FSharp.Compiler.Layout.isEmptyL text) then
////                                if not textCollector.IsEmpty then textCollector.Add Literals.lineBreak
////                                renderL (taggedTextListR textCollector.Add) text |> ignore
////
////                        AppendOverload(overloads.[0])
////                        if len >= 2 then AppendOverload(overloads.[1])
////                        if len >= 3 then AppendOverload(overloads.[2])
////                        if len >= 4 then AppendOverload(overloads.[3])
////                        if len >= 5 then AppendOverload(overloads.[4])
////                        if len >= 6 then 
////                            textCollector.Add Literals.lineBreak
////                            textCollector.Add (tagText(PrettyNaming.FormatAndOtherOverloadsString(len-5)))
////
////                    let _,xml = overloads.[0]
////                    AppendXmlComment(documentationProvider, textCollector, xml, showExceptions, showParameters, None)
////                    true
////                else
////                    false
////
////            | FSharpStructuredToolTipElement.CompositionError(errText) -> 
////                textCollector.Add(tagText errText)
////                true
////            dataTipElement

////        List.fold Process false dataTipText |> ignore

//    let BuildDataTipText(documentationProvider:IDocumentationBuilder, textCollector:TaggedText->_, xmlCollector, dataTipText:FSharpToolTipText) =         
//        BuildTipText(documentationProvider,dataTipText , textCollector, xmlCollector, true, true, false, true) 

//    let BuildMethodOverloadTipText(documentationProvider, textCollector, xmlCollector,(dataTipText), showParams) = 
//        BuildTipText(documentationProvider, dataTipText, textCollector, xmlCollector, false, false, showParams, false) 

//    let BuildMethodParamText(documentationProvider, xmlCollector, xml, paramName) =
//        AppendXmlComment(documentationProvider, TextSanitizingCollector(xmlCollector), xml, false, true, Some paramName)

