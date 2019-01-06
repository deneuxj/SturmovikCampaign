// Create the world description data and the initial state

#I "../bin/Debug"
#r "Campaign.dll"
#r "DataProvider.dll"

#load "Configuration.fsx" 

open SturmovikMission.DataProvider

let startDate, description =
    try
        Campaign.Run.Init.createWorld(Configuration.config, "MoscowWinter")
    with
    | :? Parsing.ParseError as e ->
        Parsing.printParseError e
        |> String.concat "\n"
        |> printfn "%s"
        raise e
printfn "%s" description
Campaign.Run.WeatherComputation.run(Configuration.config, startDate)
let descrState = Campaign.Run.Init.createState Configuration.config
printfn "%s" (String.concat "\n" descrState)
let decision = Campaign.Run.OrderDecision.run Configuration.config
