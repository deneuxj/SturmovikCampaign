// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Campaign.Run

open System.IO

open Campaign.BasicTypes
open Campaign.Configuration
open PlaneModel
open FSharp.Control

module Filenames =
    let axisOrders = "axisOrders.xml"
    let alliesOrders = "alliesOrders.xml"
    let state = "state.xml"
    let world = "world.xml"
    let weather = "weather.xml"
    let axisAAR = "axisAAR.xml"
    let alliesAAR = "alliesAAR.xml"
    let missionResults = "results.xml"
    let battles = "battles.xml"
    let hangars = "hangars.xml"
    let extraLogs = "extraLogs.txt"
    /// Format of date used in backup files
    let dateFormat = "yyyy-MM-dd_HH-mm-ss"

module Init =
    open SturmovikMission.Blocks.BlocksMissionData
    open Campaign.WorldDescription
    open Campaign.WorldState
    open System.IO
    open MBrace.FsPickler
    open System.Numerics
    open VectorExtension

    let private logger = NLog.LogManager.GetCurrentClassLogger()

    let createWorld(config : Configuration, scenario : string) =
        let subBlocksFile = Path.Combine(config.ScriptPath, "Config", "SubBlocks.yaml")
        let world0 = World.Create(scenario, config.PlaneSet scenario, Path.Combine(config.ScriptPath, scenario + ".Mission"), 1.0f<E/H> * config.PlaneProduction, subBlocksFile)
        let totalProduction =
            world0.Regions
            |> Seq.sumBy (fun region -> region.Production |> Seq.sumBy (fun grp -> grp.Production(world0.SubBlockSpecs, 1.0f)))
        let desiredProduction = config.DesiredProduction * 1.0f<E/H> * float32 world0.Regions.Length
        let factor = desiredProduction / totalProduction
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()
        let world = { world0 with WeatherDaysOffset = (float config.WeatherDayMaxOffset) * (random.NextDouble() - 0.5); ProductionFactor = factor }

        let capacity =
            world.Regions
            |> Seq.map (fun region -> region.RegionId, region.Storage |> Seq.sumBy (fun sto -> sto.Storage world.SubBlockSpecs))
            |> Map.ofSeq

        let production =
            world.Regions
            |> Seq.map (fun region -> region.RegionId, region.Production |> Seq.sumBy (fun prod -> prod.Production(world.SubBlockSpecs, world.ProductionFactor)))
            |> Seq.map (fun (region, production) -> region, production)
            |> Map.ofSeq

        let antiAirUsage =
            world.AntiAirDefenses
            |> Seq.map (fun def -> def.Home, def.MaxNumGuns)
            |> Seq.groupBy fst
            |> Seq.map (fun (region, canons) -> region, canons |> Seq.sumBy snd)
            |> Map.ofSeq

        let antiTankUsage =
            world.AntiTankDefenses
            |> Seq.map (fun def -> def.Home, def.MaxNumGuns)
            |> Seq.groupBy fst
            |> Seq.map (fun (region, canons) -> region, canons |> Seq.maxBy snd |> snd)
            |> Map.ofSeq

        let description =
            [
                yield sprintf "%20s | %13s | %12s | %12s" "region" "Prod - cap" "AA" "AT"
                for region in world.Regions do
                    let (RegionId regionName) = region.RegionId
                    let aa = Map.tryFind region.RegionId antiAirUsage |> fun x -> defaultArg x 0
                    let at = Map.tryFind region.RegionId antiTankUsage |> fun x -> defaultArg x 0
                    let cap = Map.tryFind region.RegionId capacity |> fun x -> defaultArg x 0.0f<E>
                    let prod = world.ProductionFactor * (Map.tryFind region.RegionId production |> fun x -> defaultArg x 0.0f<E/H>)
                    yield sprintf "%20s | %6.1f - %3.0f | %4d - %5.1f | %4d - %5.1f" regionName prod cap aa (100.0f * float32 aa / (cap / cannonCost)) at (100.0f * float32 at / (cap / cannonCost))
            ]
            |> String.concat "\n"

        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let outputDir = config.OutputDir
        use worldFile = File.CreateText(Path.Combine(outputDir, Filenames.world))
        serializer.Serialize(worldFile, world)
        world.StartDate, description

    let createState(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
        let world = serializer.Deserialize<World>(worldFile)
        use weatherFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.weather))
        let weather = serializer.Deserialize<Weather.WeatherState>(weatherFile)

        let state = WorldState.Create(config, world, float32 weather.Wind.Direction)
        let outputDir = config.OutputDir
        use stateFile = File.CreateText(Path.Combine(outputDir, Filenames.state))
        serializer.Serialize(stateFile, state)

        let description =
            [
                for coalition in [Axis; Allies] do
                    let prod =
                        Seq.zip world.Regions state.Regions
                        |> Seq.filter(fun (_, reg) -> reg.Owner = Some coalition)
                        |> Seq.sumBy(fun (reg, regState) -> regState.ProductionCapacity(reg, world.SubBlockSpecs, world.ProductionFactor))
                    yield sprintf "%s: %5.0f" (string coalition) prod
            ]

        description

module PlayChess =
    open MBrace.FsPickler
    open System.IO
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.MinMax
    open System.Diagnostics
    open System.Threading

    let run(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message
        seq {
            let board, neighboursOf = BoardState.Create(world, state, false, 1.0f<H>)
            yield "Initially"
            yield board.DisplayString
            yield sprintf "%f" board.Score.Value
            let minMax cancel n = minMax cancel n (fun (x, y) -> neighboursOf x y)
            let rec timeBound cancel prev n board =
                if n >= 100 then
                    prev
                else
                    printfn "Max depth: %d" n
                    let res = minMax cancel n board
                    if cancel.IsCancellationRequested then
                        prev
                    else
                        timeBound cancel res (n + 1) board
            let minMax board =
                use cancellation = new CancellationTokenSource()
                cancellation.CancelAfter(config.ThinkTime * 1000)
                timeBound cancellation.Token ({ Axis = None; Allies = None }, Ongoing 0.0f) 1 board
            yield! play minMax board
        }

module WeatherComputation =
    open MBrace.FsPickler
    open System.IO
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.NewWorldState

    let getNextDateFromState(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let state =
            try
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read state data. Reason was: '%s'" e.Message
        let date = nextDate config.LongWorkDay (1.0f<H> * float32 config.MissionLength / 60.0f) state.Date
        date

    let run(config : Configuration, date : System.DateTime) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                serializer.Deserialize<World>(worldFile)
            with
            | e -> failwithf "Failed to read world data. Reason was: '%s'" e.Message
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()
        let daysOffset = System.TimeSpan(int64(world.WeatherDaysOffset * 3600.0 * 24.0  * 1.0e7))
        let weather = Weather.getWeather random (date + daysOffset)

        use weatherFile = File.CreateText(Path.Combine(config.OutputDir, Filenames.weather))
        serializer.Serialize(weatherFile, weather)
        weather

module OrderDecision =
    open System.IO
    open MBrace.FsPickler
    open Campaign.AutoOrder
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.Orders
    open Campaign.AiPlanes
    open Util
    open System.Numerics

    let run(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state, weather =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                use weatherFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.weather))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile),
                serializer.Deserialize<Weather.WeatherState>(weatherFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message

        // Decide column movements
        let columnOrders =
            if state.Regions |> List.exists (fun region -> region.HasInvaders) then
                Continue []
            else
                let dt = 1.0f<H> * float32 config.MissionLength / 60.0f
                decideColumnMovements dt config.LongWorkDay world state config.ThinkTime
        match columnOrders with
        | Continue columnOrders ->
            let axisColumns =
                columnOrders
                |> List.filter (fun order -> order.OrderId.Coalition = Axis)
            let alliesColumns =
                columnOrders
                |> List.filter (fun order -> order.OrderId.Coalition = Allies)
            let pickReinforcements coalition =
                // Don't send reinforcements from regions that are already involved in a move picked by minmax
                let noStart =
                    match coalition with
                    | Axis -> axisColumns
                    | Allies -> alliesColumns
                    |> List.map (fun order -> order.Start) |> Set.ofList
                allTankReinforcements world state coalition
                |> Array.filter (fun move -> not(noStart.Contains(move.Start)))
                |> Array.truncate(5) // Pick at random among the 5 best choices
                |> Array.shuffle (System.Random())
                |> function
                    | [||] -> []
                    | arr -> [arr.[0]]
            let axisColumns = axisColumns @ pickReinforcements Axis
            let alliesColumns = alliesColumns @ pickReinforcements Allies

            // Decide resupply convoy movements
            let mkConvoys coalition =
                createAllConvoyOrders config.MissionLengthH coalition (world, state)
                |> List.filter (fun order ->
                    match order.Means with
                    | ByRoad ->
                        // Remove road convoys that take the same road as a tank column
                        let orderEndPoints = order.Convoy.EndPoints
                        match coalition with
                        | Axis -> axisColumns
                        | Allies -> alliesColumns
                        |> List.exists (fun column ->
                            let columnEndPoints =
                                [ column.Start; column.Destination ]
                                |> List.sort
                            orderEndPoints = columnEndPoints)
                        |> not
                    | ByRail | ByAir _ | BySeaShip | ByRiverShip ->
                        true)
                |> List.filter (fun order ->
                    // Remove excessively small convoys. From a player's perspective it's underwhelming to find convoys composed of two trucks.
                    match order.Means with
                    | ByRoad ->
                        order.Convoy.TransportedSupplies >= ResupplyOrder.TruckCapacity * 6.0f
                    | ByRail | ByAir _ | BySeaShip | ByRiverShip ->
                        true)
                |> prioritizeConvoys config.MissionLengthH world state
                |> List.truncate config.MaxConvoys
            let axisConvoys = mkConvoys Axis
            let alliesConvoys = mkConvoys Allies

            // Air patrols
            let mkPatrols coalition =
                mkAllPatrols world state coalition
                |> prioritizeAiPatrols config.MissionLengthH world state
                |> Seq.fold (fun (starts, filtered) (af, patrol) ->
                    if Set.contains af.AirfieldId starts then
                        (starts, filtered)
                    else
                        (Set.add af.AirfieldId starts, patrol :: filtered)
                ) (Set.empty, [])
                |> snd
                |> List.rev
                |> List.truncate config.MaxPatrols
            let axisPatrols, alliesPatrols =
                if not weather.IsOvercast || weather.CloudHeight > 3500.0 then
                    mkPatrols Axis, mkPatrols Allies
                else
                    [], []

            // Air attacks
            let mkAttacks() =
                let random = System.Random()
                let attacks = 
                    mkAllAttackers world state
                    |> Array.ofSeq
                    |> Array.shuffle random
                match attacks with
                | [||] -> []
                | _ ->
                    attacks // Keep attacks up to max number of attacker planes
                    |> Seq.fold (fun (outAttacks, totalPlanes) attack ->
                        if totalPlanes >= config.MaxAttackers then
                            (outAttacks, totalPlanes)
                        else
                            let numPlanes = min attack.NumPlanes (config.MaxAttackers - totalPlanes)
                            ({ attack with NumPlanes = numPlanes } :: outAttacks, totalPlanes + numPlanes)) ([], 0)
                    |> fst
            let axisAttacks, alliesAttacks =
                if not weather.IsOvercast || weather.CloudHeight > 2500.0 then
                    let allAttacks = mkAttacks()
                    allAttacks |> List.filter (fun attack -> attack.Coalition = Axis), allAttacks |> List.filter (fun attack -> attack.Coalition = Allies)
                else
                    [], []

            // Ferry flights
            let axisFerryFlights, alliesFerryFlights =
                if config.PlaneRentalAllowed && weather.Wind.Speed < 7.0 then
                    decidePlaneTransfers world state Axis, decidePlaneTransfers world state Allies
                else
                    [], []

            // Production
            let axisProduction = computeProductionPriorities config.MissionLengthH Axis world state
            let alliesProduction = computeProductionPriorities config.MissionLengthH Allies world state

            // Write orders to disk
            let outputDir = config.OutputDir
            use axisOrderFiles = File.CreateText(Path.Combine(outputDir, Filenames.axisOrders))
            let package = { Resupply = axisConvoys; Columns = axisColumns; Patrols = axisPatrols; Attacks = axisAttacks; Production = axisProduction; PlaneFerries = axisFerryFlights }.Renumber()
            serializer.Serialize(axisOrderFiles, package )
            use alliesOrderFiles = File.CreateText(Path.Combine(outputDir, Filenames.alliesOrders))
            let package = { Resupply = alliesConvoys; Columns = alliesColumns; Patrols = alliesPatrols; Attacks = alliesAttacks; Production = alliesProduction; PlaneFerries = alliesFerryFlights }.Renumber()
            serializer.Serialize(alliesOrderFiles, package)
        | _ ->
            ()
        columnOrders

module MissionFileGeneration =
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.MissionGeneration
    open Campaign.AutoOrder
    open Campaign.Orders
    open Campaign.Weather
    open Util
    open Campaign.NewWorldState
    open Campaign.AfterActionReport
    open SturmovikMission.Blocks.BlocksMissionData
    open System.IO
    open MBrace.FsPickler
    open System.Diagnostics

    let private logger = NLog.LogManager.GetCurrentClassLogger()

    let private retry1s h items = Async.keepTryingPaced 60 1000 h items

    let private handleResult sel msg res =
        match res with
        | Result.Error (_, files) ->
            failwithf "Failed to %s: %s" msg (String.concat "," (files |> List.map sel))
        | Result.Ok () ->
            ()

    let run(config : Configuration) =
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()

        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message

        let weather =
            try
                use weatherFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.weather))
                serializer.Deserialize<WeatherState>(weatherFile)
            with
            | e -> failwithf "Failed to read weather data. Reason was: '%s'" e.Message

        let allAxisOrders, allAlliesOrders =
            try
                use axisOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.axisOrders))
                use alliesOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.alliesOrders))
                serializer.Deserialize<OrderPackage>(axisOrdersFile),
                serializer.Deserialize<OrderPackage>(alliesOrdersFile)
            with
            | e -> failwithf "Failed to read orders. Reason was: '%s'" e.Message

        let battles =
            let path = Path.Combine(config.OutputDir, Filenames.battles)
            if File.Exists(path) then
                try
                    use battleFile = File.OpenText(path)
                    serializer.Deserialize<BattleSummary list>(battleFile)
                with
                | e ->
                    logger.Warn(sprintf "Failed to read battles file because '%s'" e.Message)
                    []
                |> List.map (fun battle -> battle.GetText() |> String.concat "")
                |> String.concat "<br>"
            else
                ""

        let dt = (1.0f<H>/60.0f) * float32 config.MissionLength

        let author = "coconut"
        let preamble = "Dynamic campaign software by coconut. Pledges available at www.patreon.com/coconutside<br><br>"
        let timeAndDate =
            sprintf "%s<br><br>" (state.Date.ToString("d MMM yyyy HH:mm"))
        let weatherDescription =
            let cover = weather.CloudDescription
            let windDirection =
                weather.Wind.Direction
            let windOrigin =
                let descs =
                    [ 1.0, "south"
                      3.0, "south-west"
                      5.0, "west"
                      7.0, "north-west"
                      9.0, "north"
                      11.0, "north-east"
                      13.0, "east"
                      15.0, "south-east" ]
                descs
                |> List.tryFind(fun (k, _) -> windDirection < k * 22.5)
                |> Option.map snd
                |> Option.defaultVal "south"
            sprintf "<b>Weather</b><br>Temperature: %2.0fC, Cloud cover: %s, Wind %3.1f m/s from %s<br><br>" weather.Temperature cover weather.Wind.Speed windOrigin
        let mainText =
            config.Briefing.Replace("\r\n", "\n").Split('\n')
            |> Array.map (fun s -> s.Trim())
            |> String.concat "<br>"
        let commandsHelp =
            if config.ChatLogCommandsEnabled then
                "<b>Commands</b><br>!sp airfield - Show reserved planes at airfield<br>!sa plane - Show airfields with reserved plane<br>!cash - Show cash reserve<br><br>"
            else
                ""
        let spawnRestrictions =
            if config.SpawnsAreRestricted then
                ["You can spawn at any airfield named in ALL CAPS."
                 "You can also spawn at other airfields, if you have landed <b>that</b> plane (undamaged) there earlier"] @
                if config.PlaneRentalAllowed then
                    ["or if you have gathered enough rewards to rent a plane."
                     sprintf "The base cost of a fighter is around %0.0f, an attacker %0.0f and a bomber %0.0f.<br><br>" Bf109f4.Cost Bf110e.Cost Ju88a4.Cost
                     "The cost of rental depends on the number of planes. The fewer they are, the more they cost."]
                else
                    []
                |> String.concat " "
            else
                ""
        let briefing = preamble + timeAndDate + weatherDescription + spawnRestrictions + mainText + "<br><br>" + commandsHelp + battles
        let missionName = config.MissionName
        let missionParams =
            { PlaneSet = config.PlaneSet world.Scenario
              MaxCapturedPlanes = config.MaxCapturedPlanes
              Author = author
              MissionName = config.MissionName
              Briefing = briefing
              MissionLength = config.MissionLength
              ColumnSplitInterval = config.ColumnSplitInterval
              MaxFires = config.MaxFires
              MaxSimultaneousConvoys = config.MaxSimultaneousConvoys
              MaxSimultaneousFerryFlights = config.MaxSimultaneousFerryFlights
              MaxStaticPlanes = config.MaxStaticPlanes
              EnablePlayerTanks = config.EnablePlayerTanks
              StrategyMissionFile = Path.Combine(config.ScriptPath, world.Scenario + ".Mission")
              MaxVehiclesInBattle = config.MaxVehiclesInBattle
              MaxBuildingIcons = config.MaxBuildingIcons
              BattleKillRatio = config.BattleKillRatio
              SpawnsAreRestricted = config.SpawnsAreRestricted
              MaxTanksInParks = config.MaxTanksInParks
              MaxAACannons = config.MaxAACannons
            }
        let missionData =
            { World = world
              State = state
              Weather = weather
              Random = random
              AxisOrders = allAxisOrders
              AlliesOrders = allAlliesOrders
            }
        async {
            // Retry an operation for at most 1 minute at 1s interval
            let copy(x, y) =
                if File.Exists(y) then
                    File.Delete(y)
                File.Copy(x, y)
            // Create Multiplayer/Dogfight directory if it doesn't already exist
            let mkdir d = if not (Directory.Exists(d)) then Directory.CreateDirectory(d) |> ignore
            let! result = retry1s mkdir [Path.Combine(config.OutputDir, "Multiplayer", "Dogfight")]
            handleResult id "make directory" result
            let localMpDir = Path.Combine(config.OutputDir, "Multiplayer", "Dogfight")
            // Write mission file
            let! result = retry1s (writeMissionFile missionParams missionData) [Path.Combine(localMpDir, missionName + "_1.Mission")]
            handleResult id "write mission file" result
            // Duplicate localization from English
            let langs = ["fra"; "rus"; "ger"; "spa"; "pol"]
            let files = [
                for lang in langs do
                    yield (Path.Combine(localMpDir, missionName + "_1.eng"), Path.Combine(localMpDir, missionName + "_1." + lang))
            ]
            let! result = retry1s copy files
            handleResult snd "duplicate localization files" result
            // Copy mission and localization files from _1 to _2
            let copyLoc =
                async {
                    let files = [
                        for lang in "Mission" :: "eng" :: langs do
                            yield (Path.Combine(localMpDir, missionName + "_1." + lang), Path.Combine(localMpDir, missionName + "_2." + lang))
                    ]
                    let! result = retry1s copy files
                    handleResult snd "copy localization files from _1 to _2" result
                }
            // Run resaver
            let resave missionFile =
                let resaverDir = Path.Combine(config.ServerBinDir, "resaver")
                let p = ProcessStartInfo("MissionResaver.exe", sprintf "-d \"%s\" -f \"%s\"" config.OutputDir missionFile)
                p.WorkingDirectory <- resaverDir
                p.UseShellExecute <- true
                let proc = Process.Start(p)
                try
                    // Lower priority to avoid interfering with a running instance of DServer
                    proc.PriorityClass <- ProcessPriorityClass.BelowNormal
                with
                | _ -> logger.Warn("Failed to lower priority of Resaver to BelowNormal")
                proc.WaitForExit()
                logger.Info(sprintf "Resaver exited with code %d" proc.ExitCode)
                if proc.ExitCode <> 0 then
                    failwith "Resaver failed"
            if config.UseTextMissionFile then
                resave (Path.Combine(localMpDir, missionName + "_1.Mission"))
                // Delete msnbin file
                let! result = retry1s File.Delete [Path.Combine(localMpDir, missionName + "_1.msnbin")]
                handleResult id "delete" result
                // Translate list file from _1 to _2
                let handle (file_in, file_out) =
                    let regex = System.Text.RegularExpressions.Regex("(filename=\".*)_1([.].*\",)")
                    let replaced =
                        [
                            for line in File.ReadAllLines(file_in) do
                                yield regex.Replace(line, @"$1_2$2")
                        ]
                    File.WriteAllLines(file_out, replaced)
                let listFile = (Path.Combine(localMpDir, missionName + "_1.list"), Path.Combine(localMpDir, missionName + "_2.list")) 
                let! result = retry1s handle [listFile]
                handleResult snd "translate second list file" result
                // Copy localization files from _1 to _2
                do! copyLoc
            else
                resave (Path.Combine(localMpDir, missionName + "_1.Mission"))
                // Copy localization files from _1 to _2
                do! copyLoc
                resave (Path.Combine(localMpDir, missionName + "_2.Mission"))
        }

    // Publish mission files to DServer
    let publish(config : Configuration) =
        async {
            let missionName = config.MissionName
            let localMpDir = Path.Combine(config.OutputDir, "Multiplayer", "Dogfight")
            let mpDir = Path.Combine(config.ServerDataDir, "Multiplayer", "Dogfight")
            let langs = ["eng"; "fra"; "rus"; "ger"; "spa"; "pol"]
            let suffixes = ["_1"; "_2"]
            // Remove old files from multiplayer directory
            let filesToDelete =
                [
                    for suffix in "" :: suffixes do
                        yield Path.Combine(mpDir, missionName + suffix + ".Mission")
                        yield Path.Combine(mpDir, missionName + suffix + ".msnbin")
                        yield Path.Combine(mpDir, missionName + suffix + ".list")
                        for lang in langs do
                            yield Path.Combine(mpDir, missionName + suffix + "." + lang)
                        yield Path.Combine(mpDir, missionName + suffix + ".msnbin")
                ]
            let! result = retry1s (fun f -> if File.Exists(f) then File.Delete(f)) filesToDelete
            // Files may be locked by server, so it's not always fatal if the operation fails
            match result with
            | Result.Error (_, files) ->
                let msg =
                    files
                    |> String.concat ", "
                logger.Warn(sprintf "Failed to delete the following files: %s" msg)
            | Result.Ok () ->
                ()
            // Copy new files to multiplayer directory
            let filesToCopy =
                [
                    let ext = if config.UseTextMissionFile then ".Mission" else ".msnbin"
                    for suffix in suffixes do
                        yield (Path.Combine(localMpDir, missionName + suffix + ext), Path.Combine(mpDir, missionName + suffix + ext))
                        yield (Path.Combine(localMpDir, missionName + suffix + ".list"), Path.Combine(mpDir, missionName + suffix + ".list"))
                        for lang in langs do
                            yield (Path.Combine(localMpDir, missionName + suffix + "." + lang), Path.Combine(mpDir, missionName + suffix + "." + lang))
                ]
            let! result = retry1s (fun (x, y) -> File.Copy(x, y)) filesToCopy
            match result with
            | Result.Error (_, files) ->
                let msg =
                    files
                    |> List.map (fun (src, dest) -> sprintf "%s -> %s" src dest)
                    |> String.concat ", "
                failwithf "Failed to publish the following files: %s" msg
            | Result.Ok () ->
                ()
        }

module MissionLogParsing =
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.ResultExtraction
    open Campaign.NewWorldState
    open Campaign.Orders
    open Campaign.AfterActionReport
    open Campaign.PlayerHangar
    open Campaign.PlaneAvailabilityChecks
    open MBrace.FsPickler
    open System.IO
    open ploggy
    open System.Text.RegularExpressions
    open SturmovikMission.Blocks.Util.String
    open SturmovikMission.Blocks.Links

    let private logger = NLog.LogManager.GetCurrentClassLogger()

    /// Make a dated copy of all current state files
    let backupFiles config =
        let outputDir = config.OutputDir
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let state =
            try
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message
        let backupFile name =
            let backupName =
                let dateString =
                    state.Date.ToString(Filenames.dateFormat)
                sprintf "%s_%s.xml" name dateString
            let backupDest = Path.Combine(outputDir, backupName)
            if File.Exists backupDest then
                File.Delete(backupDest)
            let infile = Path.Combine(outputDir, sprintf "%s.xml" name)
            if File.Exists(infile) then
                File.Copy(infile, backupDest)
        [ Filenames.state
          Filenames.axisOrders
          Filenames.alliesOrders
          Filenames.axisAAR
          Filenames.alliesAAR
          Filenames.weather
          Filenames.missionResults
          Filenames.battles
          Filenames.hangars
        ]
        |> List.iter (fun filename -> Path.GetFileNameWithoutExtension(filename) |> backupFile)

    /// Delete logs that are older than two days
    let purgeLogs(missionLogsDir : string) =
        let now = System.DateTime.UtcNow
        let old = System.TimeSpan(2, 0, 0, 0) // Two days
        for file in Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt") do
            let created = File.GetCreationTimeUtc(file)
            if now - created > old then
                logger.Info(sprintf "Purging old log '%s'" (Path.GetFileName(file)))
                try
                    File.Delete(file)
                with
                | e ->
                    logger.Warn(sprintf "Failed to purge old log '%s': %s" (Path.GetFileName(file)) e.Message)

    type RecordType =
    | Skip
    | Recording of LogEntry list * startTime:System.DateTime * lastFile:string

    /// Select the logs corresponding to a given mission and return the entries they contain.
    let getEntries(missionLogsDir : string, missionName : string, startDate : System.DateTime, minMissionDurationMinutes) =
        do
            Plogger.Init()

        let entries =
            logger.Info(sprintf "Looking for logs in %s" missionLogsDir)
            // entries to remove from the log
            let timeLessEntryTypes = Set.ofList [ LogEntryType.LogVersion; LogEntryType.PosChanged; LogEntryType.Join; LogEntryType.Leave ]
            let missionIsComplete (startTime : System.DateTime, file : string) =
                let endTime = File.GetLastWriteTimeUtc(file)
                // 90% of the duration to allow for clock skew between real time and mission time
                let duration = 0.9 * float minMissionDurationMinutes
                let actualDuration = endTime - startTime
                logger.Info (sprintf "Mission actual length: %d minutes" (int actualDuration.TotalMinutes))
                actualDuration > System.TimeSpan.FromMinutes(duration)
            seq {
                let unordered = Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt")
                let r = Regex(@"(missionReport\(.*\))\[([0-9]+)\]\.txt")
                let ordered =
                    unordered
                    |> Seq.choose(fun path ->
                        let m = r.Match(Path.GetFileName(path))
                        if not m.Success then
                            None
                        else
                            Some(path, (m.Groups.[1].ToString(), System.Int32.Parse(m.Groups.[2].ToString()))))
                    |> Seq.sortBy snd
                    |> Seq.map fst
                for file in ordered do
                    for line in File.ReadAllLines(file) do
                        let entry = LogEntry.Parse(line)
                        match entry with
                        | null -> ()
                        | entry when entry.IsValid() -> yield entry, file
                        | invalid -> ()
            }
            |> Seq.fold (fun (previous, current) (entry, file) ->  // Keep the latest mission reports with the proper mission start date
                match current, entry with
                | _, (:? MissionStartEntry as start) ->
                    // Move current to previous if it's non empty and has lasted long enough
                    let previous =
                        match current with
                        | Skip -> previous
                        | Recording(currentEntries, startTime, _) ->
                            if missionIsComplete(startTime, file) then
                                logger.Debug(sprintf "Reached start of new mission, moving current to previous")
                                currentEntries
                            else
                                logger.Debug(sprintf "Reached start of new mission, discarding current because too short")
                                previous
                    let expectedMissionFile = missionName.ToLower()
                    let actualMissionFile = Path.GetFileNameWithoutExtension(start.MissionFile.Split('/', '\\') |> Seq.last).ToLower()
                    let actualMissionFile = Regex.Replace(actualMissionFile, "_\d$", "")
                    if start.MissionTime = startDate && actualMissionFile = expectedMissionFile then
                        logger.Info(sprintf "Start collecting from %s" file)
                        previous, Recording([entry], File.GetLastWriteTimeUtc(file), file) // Start new list
                    else
                        let reason =
                            if start.MissionTime <> startDate then
                                "bad start date"
                            elif actualMissionFile.ToLower() <> expectedMissionFile.ToLower() then
                                "bad mission filename"
                            else
                                "???"
                        logger.Warn(sprintf "Stopped collecting at %s because %s" file reason)
                        previous, Skip // Start new skip
                | Skip, _ ->
                    logger.Debug(sprintf "Skipped %s" file)
                    previous, Skip // Skip, looking for next mission start that has the right time
                | Recording(currentEntries, startTime, _), _ ->
                    logger.Debug(sprintf "Collected %s" file)
                    previous, Recording(entry :: currentEntries, startTime, file) // Recording, update current
            ) ([], Skip)
            |> function // Keep latest complete mission
                | previous, Skip ->
                    previous
                | previous, Recording(currentEntries, startDate, lastFile) ->
                    // Current list is not empty, check if mission is complete
                    if missionIsComplete(startDate, lastFile) then
                        // If so use it
                        currentEntries
                    else
                        let actualDuration = File.GetLastWriteTimeUtc(lastFile) - startDate
                        logger.Warn(sprintf "Last set of mission logs is incomplete (%d minutes)" (int actualDuration.TotalMinutes))
                        // Otherwise keep previous
                        logger.Debug("Discarded short sequence")
                        previous
            |> List.filter (fun entry -> not (timeLessEntryTypes.Contains(entry.EntryType)))
            |> List.rev
        entries

    /// Get mission log entries from the game logs
    let stage0(config : Configuration) =
        let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")
        let state =
            let serializer = FsPickler.CreateXmlSerializer(indent = true)
            try
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message
        if config.PurgeLogs then
            purgeLogs(missionLogsDir)
        let regularEntries =
            getEntries(missionLogsDir, config.MissionName, state.Date, config.MissionLength)
        let extraLogEntries =
            try
                File.ReadAllLines(Path.Combine(config.OutputDir, Filenames.extraLogs))
                |> Seq.map (LogEntry.Parse)
                |> Seq.filter (function
                    | null -> false
                    | entry when entry.IsValid() -> true
                    | invalid -> false)
                |> List.ofSeq
            with
            | exc ->
                logger.Error(sprintf "Failed to read extra logs: '%s'" exc.Message)
                []

        regularEntries @ extraLogEntries
        |> List.sortBy (fun entry -> entry.Timestamp)

    /// Retrieve mission log entries from an existing results.xml file
    let stage0alt(config : Configuration) =
        do
            Plogger.Init()
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let state =
            try
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read state data. Reason was: '%s'" e.Message
        let results =
            try
                use resultsFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.missionResults))
                serializer.Deserialize<MissionResults>(resultsFile)
            with
            | e -> failwithf "Failed to read previously existing mission results. Reason was: '%s'" e.Message
        results.Entries
        |> List.map LogEntry.Parse

    /// Extract results from log entries
    let stage1(config : Configuration, entries : LogEntry list) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state, axisOrders, alliesOrders =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                use axisOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.axisOrders))
                use alliesOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.alliesOrders))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile),
                serializer.Deserialize<OrderPackage>(axisOrdersFile),
                serializer.Deserialize<OrderPackage>(alliesOrdersFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message

        if List.isEmpty entries then
            failwith "No entries found suitable for result extraction"
        let entriesAsStrings =
            entries
            |> List.map (fun entry -> entry.OriginalString)
        let entries = AsyncSeq.ofSeq entries
        let shipments =
            let shipmentAxis = extractSuppliesShipped axisOrders.Resupply entries |> AsyncSeq.toList
            let shipmentAllies = extractSuppliesShipped alliesOrders.Resupply entries |> AsyncSeq.toList
            shipmentAxis @ shipmentAllies
        let staticDamages =
            extractStaticDamages world entries
            |> AsyncSeq.toBlockingSeq
            |> Damage.GroupByObjectAndPlayer
            |> List.ofSeq
        let vehicleDamages =
            extractVehicleDamages world (axisOrders.Columns @ alliesOrders.Columns) (axisOrders.Resupply @ alliesOrders.Resupply) entries
            |> AsyncSeq.toBlockingSeq
            |> Damage.GroupByObjectAndPlayer
            |> List.ofSeq
        let takeOffs, landings =
            let both =
                extractTakeOffsAndLandings world state entries
                |> AsyncSeq.toList
            both |> List.choose (function TookOff x -> Some x | _ -> None),
            both |> List.choose (function ResultExtraction.Landed x -> Some x | _ -> None)
        let movements = axisOrders.Columns @ alliesOrders.Columns
        let columnDepartures = extractColumnDepartures movements entries |> AsyncSeq.toList
        let battles =
            Battlefield.identifyBattleAreas world state
            |> Seq.cache
        let paraDrops = extractParaDrops world state battles entries |> AsyncSeq.toList
        let planeFerryEvents = extractFerryPlanes (axisOrders.PlaneFerries @ alliesOrders.PlaneFerries) entries |> AsyncSeq.toList
        let battleKills = extractBattleDamages world state battles entries |> AsyncSeq.toList
        let blocked = extractBlockedVehicles (axisOrders.Columns @ alliesOrders.Columns) (axisOrders.Resupply @ alliesOrders.Resupply) entries |> AsyncSeq.toList
        let results =
            { Entries = entriesAsStrings
              Shipments = shipments
              StaticDamages = staticDamages
              VehicleDamages = vehicleDamages
              TakeOffs = takeOffs
              Landings = landings
              ColumnDepartures = columnDepartures
              ParaDrops = paraDrops
              FerryPlanes = planeFerryEvents
              BattleKills = battleKills
              Blocked = blocked
            }
        use missionFile = File.CreateText(Path.Combine(config.OutputDir, Filenames.missionResults))
        serializer.Serialize(missionFile, results)
        results

    /// Update player reserved planes and rewards
    let updateHangars(config, results : MissionResults, entries) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message
        let hangars =
            match tryLoadHangars (Path.Combine(config.OutputDir, Filenames.hangars)) with
            | Some hangars ->
                hangars
                |> guidToStrings
            | None -> Map.empty
        // Restrict cash reserves to maximum allowed in configuration
        let hangars =
            hangars
            |> Map.map (fun player hangar -> { hangar with Reserve = min hangar.Reserve (1.0f<E> * float32 config.MaxCash) })
        let hangars =
            AsyncSeq.ofSeq entries
            |> checkPlaneAvailability config.MissionLengthH (Limits.FromConfig config) world state hangars
            |> AsyncSeq.toBlockingSeq
            |> Seq.choose (function Status(x, _) -> Some x | _ -> None)
            |> Seq.tryLast
            |> Option.defaultValue hangars
            |> stringsToGuids
        // Refill fresh spawns
        let hangars =
            hangars
            |> Map.map (fun _ h ->
                let freshSpawns =
                    h.FreshSpawns
                    |> Map.map (fun planeType qty ->
                        match planeType with
                        | Fighter -> qty + config.FreshFighterRefill |> min (float32 config.FreshFighterSpawns)
                        | Attacker -> qty + config.FreshAttackerRefill |> min (float32 config.FreshAttackerSpawns)
                        | Bomber -> qty + config.FreshBomberRefill |> min (float32 config.FreshBomberSpawns)
                        | Transport -> qty + config.FreshTransportRefill |> min (float32 config.FreshTransportSpawns)
                    )
                { h with FreshSpawns = freshSpawns }
            )
        saveHangars (Path.Combine(config.OutputDir, Filenames.hangars)) hangars

    /// Compute new campaign state
    let updateState(config, missionResults) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state, axisOrders, alliesOrders, weather =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                use axisOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.axisOrders))
                use alliesOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.alliesOrders))
                use weatherFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.weather))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile),
                serializer.Deserialize<OrderPackage>(axisOrdersFile),
                serializer.Deserialize<OrderPackage>(alliesOrdersFile),
                serializer.Deserialize<Weather.WeatherState>(weatherFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message
        let state2, newlyProduced, battleReports =
            newState
                config
                world state
                axisOrders alliesOrders
                missionResults
                (float32 weather.Wind.Direction)
        newlyProduced, battleReports, (state, state2)

    /// Wipe plane reservations in regions that have changed owners
    let wipeReservations (config : Configuration, world : World, state : WorldState, state2 : WorldState) =
        match tryLoadHangars (Path.Combine(config.OutputDir, Filenames.hangars)) with
        | Some hangars ->
            let wg = world.FastAccess
            let conqueredRegions =
                List.zip state.Regions state2.Regions
                |> Seq.filter (fun (reg1, reg2) -> reg1.Owner <> reg2.Owner)
                |> Seq.map (fun (reg, _) -> reg.RegionId)
                |> Set.ofSeq

            let hangars2 =
                hangars
                |> Map.map (fun _ h ->
                    { h with
                        Airfields =
                            h.Airfields
                            |> Map.filter (fun af _ -> not(conqueredRegions.Contains(wg.GetAirfield(af).Region)))
                    })
            saveHangars (Path.Combine(config.OutputDir, Filenames.hangars)) hangars2
        | None ->
            ()

    /// Build the after-action reports, solely used for presentation to players
    let buildAfterActionReports(config, state1, state2, tookOff, landed, damages, newlyProduced, battleReports) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, axisOrders, alliesOrders =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                use axisOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.axisOrders))
                use alliesOrdersFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.alliesOrders))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<OrderPackage>(axisOrdersFile),
                serializer.Deserialize<OrderPackage>(alliesOrdersFile)
            with
            | e -> failwithf "Failed to read world and state data. Reason was: '%s'" e.Message
        let newSupplies, newAxisVehicles, newAlliesVehicles = newlyProduced
        let aarAxis = buildReport world state1 state2 tookOff landed damages axisOrders.Columns newSupplies newAxisVehicles battleReports Axis
        let aarAllies = buildReport world state1 state2 tookOff landed damages alliesOrders.Columns newSupplies newAlliesVehicles battleReports Allies
        aarAxis, aarAllies

    /// Dump new state to files
    let stage2 config (state, state2, aarAxis, aarAllies, battles) =
        let outputDir = config.OutputDir
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        use stateFile = File.CreateText(Path.Combine(outputDir, Filenames.state))
        serializer.Serialize(stateFile, state2)
        use aarAxisFile = File.CreateText(Path.Combine(outputDir, Filenames.axisAAR))
        serializer.Serialize(aarAxisFile, aarAxis)
        use aarAlliesFile = File.CreateText(Path.Combine(outputDir, Filenames.alliesAAR))
        serializer.Serialize(aarAlliesFile, aarAllies)
        use battlesFile = File.CreateText(Path.Combine(outputDir, Filenames.battles))
        serializer.Serialize(battlesFile, battles)
        File.Delete(Path.Combine(outputDir, Filenames.extraLogs))