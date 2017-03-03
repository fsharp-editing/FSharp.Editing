System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#r "../../packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "../../packages/Microsoft.CodeAnalysis.Common/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r "../../packages/Microsoft.CodeAnalysis.Features/lib/netstandard1.3/Microsoft.CodeAnalysis.Features.dll"
#r "../../packages/Microsoft.CodeAnalysis.Workspaces.Common/lib/net46/Microsoft.CodeAnalysis.Workspaces.Desktop.dll"
#r "../../packages/Microsoft.CodeAnalysis.Workspaces.Common/lib/net46/Microsoft.CodeAnalysis.Workspaces.dll"
#r "../../packages/Wire/lib/net45/Wire.dll"
#r "../FSharp.Editing.Core/bin/Release/FSharp.Editing.Core.dll"
#r "bin/release/FSharp.Editing.dll"

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open FSharp.Editing
open System.IO
open FSharp.Editing.ProjectSystem
open Wire


let inspectorPath = "../Fsharp.Editing.ProjectInspector/bin/Release/Fsharp.Editing.ProjectInspector.exe"
let clientFsproj = "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj"
let fsprojPath = Path.GetFullPath clientFsproj
let inspectorTool = Path.GetFullPath inspectorPath


let p = new System.Diagnostics.Process()
p.StartInfo.FileName <- inspectorPath
p.StartInfo.Arguments <-  fsprojPath
p.StartInfo.UseShellExecute <- false
p.StartInfo.CreateNoWindow <- true
p.StartInfo.RedirectStandardOutput <- true
ignore <| p.Start()

let ser = Serializer()
let info = ser.Deserialize<ProjectInfo>(p.StandardOutput.BaseStream)
let fsls = FSharpLanguageService()
//let fsproj = ProjectFileInfo.create "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj"
//let fsproj = ProjectFileInfo.create "../Fsharp.Editing.Client/FSharp.Editing.Client.fsproj"

let proj = fsls.Workspace.AddProject info

fsls.Workspace.CurrentSolution.Projects
|> Seq.iter (fun x -> printfn "%s - %s" x.Name x.AssemblyName)


