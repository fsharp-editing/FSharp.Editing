#r "System.Xml"
#r "System.Xml.Linq"

open System.Runtime.CompilerServices
open System.Xml.Linq

let (^) = (<|)

let xattr (name:string) value    = XAttribute (XName.Get name, value)


let inline localName x = (^a:(member Name:XName) x).LocalName

let inline private matchName (name:string) x = name = (localName x)

/// Helper function to filter a seq of XElements by matching their local name against the provided string
let inline private nameFilter name sqs = sqs |> Seq.filter ^ matchName name

let inline private hasNamed name sqs = sqs |> Seq.exists ^ matchName name

let inline private getNamed name sqs = sqs |> Seq.find ^ matchName name

let  inline private tryGetNamed name sqs = 
    sqs |> Seq.tryPick  ^ fun elm -> if matchName name elm then Some elm else None
    

[<RequireQualifiedAccess>]
module XAttr =
    let value (xattr:XAttribute) = xattr.Value
    let parent (xattr:XAttribute) = xattr.Parent
    let previous (xattr:XAttribute) = xattr.PreviousAttribute
    let next (xattr:XAttribute) = xattr.NextAttribute

[<RequireQualifiedAccess>]
/// Functions for operating on XElements
module XElem =
    
    let inline getName (xelem:#XElement) = xelem.Name.LocalName

    let inline isNamed name (xelem:#XElement) = 
        matchName name xelem
       

    let inline notNamed name (xelem:#XElement) = 
        not ^ matchName name xelem

    let create (name:string) (content:seq<'a>) = 
        XElement (XName.Get name, Seq.toArray content)

    let make (name:string) (value:'a) = 
        XElement (XName.Get name, [value])



   
    let value (xelem:#XElement) = xelem.Value

    let nodes (xelem:#XElement) = xelem.Nodes()
    
    let descendants (xelem:#XElement) = xelem.Descendants()
    
    let descendantNamed name (xelem:#XElement) =
        descendants xelem |> tryGetNamed name

    let descendantsNamed name (xelem:#XElement) = 
        descendants xelem |> nameFilter name
    
    let elements (xelem:#XElement) = xelem.Elements()

    let hasElement name (xelem:#XElement) =
        elements xelem |> hasNamed name

    let getElement name (xelem:#XElement) =
        elements xelem |> getNamed name

    let getElementValue name (xelem:#XElement) =
        elements xelem |> getNamed name |> value

    let tryGetElement name (xelem:#XElement) =
        elements xelem |> tryGetNamed name

    let tryGetElementValue name (xelem:#XElement) =
        elements xelem |> tryGetNamed name |> Option.map value

    let getElements name (xelem:#XElement) =
        elements xelem |> nameFilter name

    let attributes (xelem:#XElement) =
        xelem.Attributes()
    
    let hasAttribute name (xelem:#XElement) =
        attributes xelem |> hasNamed name

    let getAttribute name (xelem:#XElement) =
        xelem.Attribute ^ XName.Get name

    let getAttributeValue name (xelem:#XElement) =
        xelem.Attribute ^ XName.Get name |> XAttr.value

    let tryGetAttribute name (xelem:#XElement) =
        attributes xelem |> tryGetNamed name

    let tryGetAttributeValue name (xelem:#XElement) =
        tryGetAttribute name xelem |> Option.map XAttr.value

    let setAttribute name value (xelem:#XElement) =
        xelem.SetAttributeValue(XName.Get name, value)
        xelem

    let singleAttr elemName attrName value  =
        create elemName []
        |> setAttribute attrName value
       
    let withAttrs elemName (attrList:(string*string)list) =
        (create elemName [],attrList)
        ||> List.fold ^ fun xelem (name,value) ->
            xelem |> setAttribute name value
        

    
    let addAttribute (xattr:#XAttribute) (xelem:#XElement) =
        xelem.Add xattr
        xelem

    let setElement name value (xelem:#XElement) =
        xelem.SetElementValue(XName.Get name, value)
        xelem

    let addElement (child:XElement) (parent:XElement) =
        parent.Add child
        parent

    let addSingleAttr (name:string) (attrName:string) value parent =
        addElement  (singleAttr name attrName value) parent

    let addElements (children:#seq<XElement>) (parent:XElement) =
        parent.Add children
        parent

    /// Creates a new XElement and adds it as a child
    let inline addElem elmName value xelem =
        addElement (create elmName [value]) xelem

