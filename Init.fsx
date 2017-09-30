// Create the world description data and the initial state

#I "bin/Debug"
#r "Campaign.dll"
#r "DataProvider.dll"

#load "Configuration.fsx" 

open SturmovikMission.DataProvider

let startDate =
    try
        Campaign.Run.Init.createWorld Configuration.config
    with
    | :? Parsing.ParseError as e ->
        Parsing.printParseError e
        |> String.concat "\n"
        |> printfn "%s"
        raise e
Campaign.Run.WeatherComputation.run(Configuration.config, startDate)
Campaign.Run.Init.createState Configuration.config
Campaign.Run.OrderDecision.run Configuration.config
