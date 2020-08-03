#I "../bin/Debug/netstandard2.0"
#I @"C:\Users\johann\.nuget\packages\FSharp.Control.AsyncSeq\2.0.24\lib\netstandard2.0"
#I @"C:\Users\johann\.nuget\packages\SturmovikMission.DataProvider\8.0.0\typeproviders\fsharp41\netstandard2.0"
#I @"C:\Users\johann\.nuget\packages\FSharp.Data\3.3.3\lib\netstandard2.0"
#r "FSharp.Control.AsyncSeq"
#r "Campaign"
#r "Blocks"

open FSharp.Control
open Campaign
open Campaign.WarState.IO
open Campaign.NewWorldDescription.IO
open Campaign.WarStateUpdate.CommandExecution

// Load world and state
let world = NewWorldDescription.World.LoadFromFile(@"C:\Users\johann\AppData\Local\CoconutCampaign\Current\world.xml")
let state = WarState.WarState.LoadFromFile(@"C:\Users\johann\AppData\Local\CoconutCampaign\Current\000-state.xml", world)
let firstLogFile = @"G:\dserver\data\logs\missionReport(2020-07-25_12-01-29)[0].txt"

System.Diagnostics.Debugger.Launch()

// Find initial log file created right after the mission was started
async {
    let dir = System.IO.Path.GetDirectoryName(firstLogFile)
    let filename = System.IO.Path.GetFileNameWithoutExtension(firstLogFile)
    // Replace final [0] by * in the pattern, and add .txt extension
    let pattern = filename.Substring(0, filename.Length - "[0]".Length) + "*.txt"
    let lines =
        asyncSeq {
            for file in System.IO.Directory.EnumerateFiles(dir, pattern) |> Seq.sortBy (fun file -> System.IO.File.GetCreationTimeUtc(file)) do
                yield! AsyncSeq.ofSeq (System.IO.File.ReadAllLines(file))
        }
    let commands = MissionResults.commandsFromLogs state lines
    let! effects =
        asyncSeq {
            for command in commands do
                let effects = command.Execute(state)
                yield (command, effects)
        }
        |> AsyncSeq.toArrayAsync
    printfn "%A" effects
    return ()
}
|> Async.RunSynchronously