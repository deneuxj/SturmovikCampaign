// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open SturmovikMission.DataProvider

[<EntryPoint>]
let main argv =

    let dir = @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission"
    let conquest = @"data\Conquest\StalingradConquest.Mission"
    let path = System.IO.Path.Combine(dir, conquest)

    AutoSchema.getTopTypes (Parsing.Stream.FromFile(path)) |> ignore
    0
 
