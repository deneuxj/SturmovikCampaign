// Parse the mission log and produce a new world state.

#I @"..\bin\Debug"

#r "Campaign.dll"
#r "System.Numerics.Vectors.dll"
#r "McuLibrary"
#r "DataProvider.dll"
#r "ploggy"

#load "Configuration.fsx" 

open Configuration
open Campaign.WorldState

let entries = Campaign.Run.MissionLogParsing.stage0alt(config)

let missionResults = Campaign.Run.MissionLogParsing.stage1(config, entries)

Campaign.Run.MissionLogParsing.backupFiles config
let date = Campaign.Run.WeatherComputation.getNextDateFromState config
let weather = Campaign.Run.WeatherComputation.run(config, date)

Campaign.Run.MissionLogParsing.updateHangars(config, missionResults, entries)

let newProduction, battleResults, ((oldState, newState) as states) = Campaign.Run.MissionLogParsing.updateState(config, missionResults)
let axisAAR, alliesAAR = Campaign.Run.MissionLogParsing.buildAfterActionReports(config, oldState, newState, missionResults.TakeOffs, missionResults.Landings, missionResults.StaticDamages @ missionResults.VehicleDamages, newProduction, battleResults)

Campaign.Run.MissionLogParsing.stage2 config (oldState, newState, axisAAR, alliesAAR, battleResults)

let totalHealth (state : WorldState) =
    let regionStorage =
        state.Regions
        |> Seq.sumBy (fun reg -> reg.StorageHealth |> Seq.concat |> Seq.sum)
    let regionProduction =
        state.Regions
        |> Seq.sumBy (fun reg -> reg.ProductionHealth |> Seq.concat |> Seq.sum)
    let airfieldStorage =
        state.Airfields
        |> Seq.sumBy (fun af -> af.StorageHealth |> Seq.concat |> Seq.sum )
    regionStorage + regionProduction + airfieldStorage

printfn "Old health: %0.0f" (totalHealth oldState)
printfn "New health: %0.0f" (totalHealth newState)