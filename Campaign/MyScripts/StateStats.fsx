#I @"..\bin\Debug"

#r "Campaign.dll"
#r "System.Numerics.Vectors.dll"
#r "DataProvider.dll"
#r "FsPickler.dll"
#r "ploggy"

#load "Configuration.fsx" 

open Configuration
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Run
open System.IO
open MBrace.FsPickler

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

let serializer = FsPickler.CreateXmlSerializer(indent = true)

let stateFile = @"state.xml"

let world, state =
    try
        use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
        use stateFile = File.OpenText(Path.Combine(config.OutputDir, stateFile))
        serializer.Deserialize<World>(worldFile),
        serializer.Deserialize<WorldState>(stateFile)
    with
    | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message
 
printfn "health: %0.0f" (totalHealth state)