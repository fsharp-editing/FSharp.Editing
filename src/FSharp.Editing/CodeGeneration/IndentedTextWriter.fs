namespace FSharp.Editing.CodeGeneration

open System.Diagnostics
open System
open System.IO
open System.Text
open System.Globalization

module IndentationTab =
    let [<Literal>] DefaultTabString = "    "

type internal Indentation (writer:IndentedTextWriter, indent: int ) =
    let mutable str = None
    
    let makeTabString () =
        let tabstring:string = writer.TabString
        let sb = StringBuilder (indent * tabstring.Length)
        for i in 0 .. indent-1 do
            sb.Append tabstring |> ignore
        sb.ToString ()

    member __.IndentationString 
        with get () =
            match str with
            | None -> let s = makeTabString () in str <- Some s; s
            | Some s -> s
        

and IndentedTextWriter (writer:TextWriter, tabString:string) =
    inherit TextWriter (CultureInfo.InvariantCulture)
    let mutable indentLevel = 0
    let mutable tabsPending = false

    new (writer) = new IndentedTextWriter (writer, IndentationTab.DefaultTabString)

    override __.Encoding = writer.Encoding
    member   __.InnerWriter = writer
    member   __.TabString = tabString

    override __.NewLine 
        with get () = writer.NewLine
        and  set v  = writer.NewLine <- v

    member __.Indent
        with get () = indentLevel
        and  set  v = 
            Debug.Assert (v >= 0, "Bogus Indent... probably caused by mismatched Indent++ and Indent--");
            indentLevel <- if v < 0 then 0 else v


    // override __.Close () = writer.Close ()
    override __.Flush () = writer.Flush ()

    member __.OutputTabs () =
        if tabsPending then 
            for i in 0 .. indentLevel-1 do
                writer.Write tabString
            tabsPending <- false

    override self.Write (value:bool) = 
        self.OutputTabs (); writer.Write value

    override self.Write (value:single) = 
        self.OutputTabs (); writer.Write value

    override self.Write (value:double) = 
        self.OutputTabs (); writer.Write value

     override self.Write (value:int) = 
        self.OutputTabs (); writer.Write value

     override self.Write (value:int64) = 
        self.OutputTabs (); writer.Write value
 
    override self.Write (value:char) = 
        self.OutputTabs (); writer.Write value
    
    override self.Write (buffer:char[]) = 
        self.OutputTabs (); writer.Write buffer
    
    override self.Write (buffer:char[],index:int,count:int) = 
        self.OutputTabs (); writer.Write (buffer,index,count)

    override self.Write (text:string) = 
        self.OutputTabs (); writer.Write text

    override self.Write (value:obj) = 
        self.OutputTabs (); writer.Write value

    override self.Write (format:string, arg0:obj) = 
        self.OutputTabs (); writer.Write (format,arg0)

    override self.Write (format:string, arg0:obj, arg1:obj) = 
        self.OutputTabs (); writer.Write (format,arg0,arg1)

    override self.Write (format:string, [<ParamArray>] args:obj[]) = 
        self.OutputTabs (); writer.Write (format, args)

    member __.WriteLineNoTabs (text:string) = 
        writer.Write text

    override self.WriteLine () =
        self.OutputTabs  () 
        writer.WriteLine ()
        tabsPending <- true

    override self.WriteLine (text:string) =
        self.OutputTabs () 
        writer.WriteLine text
        tabsPending <- true

    override self.WriteLine (value:bool) = 
        self.OutputTabs () 
        writer.Write value
        tabsPending <- true

    override self.WriteLine (value:single) = 
        self.OutputTabs () 
        writer.Write value
        tabsPending <- true
        
    override self.WriteLine (value:double) = 
        self.OutputTabs () 
        writer.Write value
        tabsPending <- true
        
    override self.WriteLine (value:int) = 
        self.OutputTabs () 
        writer.Write value
        tabsPending <- true

    override self.WriteLine (value:int64) = 
        self.OutputTabs () 
        writer.Write value
        tabsPending <- true
 
    override self.WriteLine (value:char) = 
        self.OutputTabs () 
        writer.Write value
        tabsPending <- true
    
    override self.WriteLine (buffer:char[]) = 
        self.OutputTabs () 
        writer.Write buffer
        tabsPending <- true
    
    override self.WriteLine (buffer:char[],index:int,count:int) = 
        self.OutputTabs () 
        writer.Write (buffer,index,count)
        tabsPending <- true

    override self.WriteLine (value:obj) = 
        self.OutputTabs ()
        writer.Write value
        tabsPending <- true

    override self.WriteLine (format:string, arg0:obj) = 
        self.OutputTabs () 
        writer.Write (format,arg0)
        tabsPending <- true

    override self.WriteLine (format:string, arg0:obj, arg1:obj) = 
        self.OutputTabs () 
        writer.Write (format,arg0,arg1)
        tabsPending <- true

    override self.WriteLine (format:string, [<ParamArray>] args:obj[]) = 
        self.OutputTabs () 
        writer.Write (format, args)
        tabsPending <- true

    member self.InternalOutputTabs () =
        for x in 0 .. indentLevel-1 do writer.Write tabString

