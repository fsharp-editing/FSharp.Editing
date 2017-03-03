module FSharp.Editing.Documentation.XmlDocCache



open System
open System.Collections.Generic
open System.Collections.Specialized

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Differencing

open FSharp.Editing


(*  The XML Sig info path generators are in ServiceDeclarations.fs

    - Processing the XML File (C# Programming Guide)
    - https://msdn.microsoft.com/en-us/library/fsbx0t7x.aspx

    - Recommended Tags for Documentation Comments (C# Programming Guide)
    - https://msdn.microsoft.com/en-us/library/5ast78ax.aspx


    - Mono.Documentation/monodocer.cs
    - https://github.com/mono/api-doc-tools/blob/master/mdoc/Mono.Documentation/monodocer.cs



*)