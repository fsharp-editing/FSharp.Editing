module FSharp.Editing.Logging 

open System
open System.IO

[<RequireQualifiedAccess>]
type LogType =
    | Info
    | Warn
    | Error
    | Message
    override x.ToString () =
        match x with
        | Message   -> "Message"
        | Info      -> "Information"
        | Warn      -> "Warning"
        | Error     -> "Error"

/// Stores the data that structures the log message
type Trace = {
    Level: LogType
    Text: string
    NewLine: bool 
}

let LogEvent = Event<Trace>()

let LogStream = LogEvent.Publish

let private writeText toStdErr color newLine text = 
    let curColor = Console.ForegroundColor
    if curColor <> color then Console.ForegroundColor <- color
    let printer =
        match toStdErr, newLine with
        | true, true -> eprintfn
        | true, false -> eprintf
        | false, true -> printfn
        | false, false -> printf
    printer "%s" text
    if curColor <> color then Console.ForegroundColor <- curColor

let private monitor = new Object()

let logToConsole (trace:Trace) =
    lock monitor ^ fun () ->
        let time = DateTime.Now.ToString "hh:mm:ss tt"
        match trace.Level with
        | LogType.Message -> sprintf "[F#][%s%s] %s" ""       time trace.Text |> writeText false ConsoleColor.Gray true
        | LogType.Info    -> sprintf "[F#][%s%s] %s" "INFO "  time trace.Text |> writeText false ConsoleColor.DarkCyan true
        | LogType.Warn    -> sprintf "[F#][%s%s] %s" "WARN "  time trace.Text |> writeText false ConsoleColor.Yellow true
        | LogType.Error   -> sprintf "[F#][%s%s] %s" "ERROR " time trace.Text |> writeText true  ConsoleColor.Red true

/// post time stamped formatted message to the console regardless of LoggingState (mode)
let trace msg = logToConsole { Level = LogType.Message; Text = msg; NewLine = true }

/// Stores the path to the file used for logging
let mutable logFile : string option = None

/// If file logging is enabled, log to the globally set logfile
let logToFile (trace:Trace) =
    match logFile with
    | Some fileName -> 
        let time = DateTime.Now.ToString "{0:u}"
        let text =
            match trace.Level with
            | LogType.Message -> sprintf "[%s] %s - %s" ""       time trace.Text 
            | LogType.Info    -> sprintf "[%s] %s - %s" "INFO "  time trace.Text 
            | LogType.Warn    -> sprintf "[%s] %s - %s" "WARN "  time trace.Text 
            | LogType.Error   -> sprintf "[%s] %s - %s" "ERROR " time trace.Text 
        try File.AppendAllLines(fileName,[text]) with _ -> ()
    | _ -> ()

/// Specify the path to the global log file
let setLogFile fileName =
    let fi = FileInfo fileName
    logFile <- Some fi.FullName
    if fi.Exists then fi.Delete ()
    elif not ^ fi.Directory.Exists then fi.Directory.Create ()
    LogStream |> Observable.subscribe logToFile

[<RequireQualifiedAccess>]
type LoggingState =
    | Disabled
    | Console
    | File
    | Both

/// Stores the global setting for which type of logging to enable (console, file, both, none)
let mutable Mode = LoggingState.Console

let private log logType msg = LogEvent.Trigger { Level = logType; Text = msg; NewLine = true }

let private processTrace trace = 
    match Mode with 
    | LoggingState.Disabled -> ()
    | LoggingState.Console ->  logToConsole trace
    | LoggingState.File ->  logToFile trace
    | LoggingState.Both ->  logToConsole trace; logToFile trace

// permanently subscribe to the logstream
LogStream.Add processTrace
            
let logMsg      msg = log LogType.Message   msg
let logInfo     msg = log LogType.Info      msg
let logWarning  msg = log LogType.Warn      msg
let logError    msg = log LogType.Error     msg

let logMsgf     msg = Printf.kprintf (log LogType.Message)  msg
let logInfof    msg = Printf.kprintf (log LogType.Info)  msg
let logWarningf msg = Printf.kprintf (log LogType.Warn)  msg
let logErrorf   msg = Printf.kprintf (log LogType.Error) msg

let logException (ex: Exception) =
    logErrorf "Exception Message: %s\nStack Trace: %s" ex.Message ex.StackTrace

let logExceptionWithContext(ex: Exception, context) =
    logErrorf "Context: %s\nException Message: %s\nStack Trace: %s" context ex.Message ex.StackTrace