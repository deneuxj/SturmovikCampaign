// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "Ast.fs"
#load "Parsing.fs"
#load "AutoSchema.fs"

open SturmovikMission.DataProvider

let dir = @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission"
let conquest = @"data\Conquest\StalingradConquest.Mission"
let path = System.IO.Path.Combine(dir, conquest)

let kinds, _ = AutoSchema.getTopTypes (Parsing.Stream.FromFile(path))
match Map.tryFind "Block" kinds with
| Some kind ->
    printfn "%A" kind
| None ->
    printfn "Not found"