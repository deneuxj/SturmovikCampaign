module Campaign.Run

open System.IO

open Campaign.BasicTypes
open Campaign.Configuration
open PlaneModel


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

module Init =
    open SturmovikMission.Blocks.BlocksMissionData
    open Campaign.WorldDescription
    open Campaign.WorldState
    open System.IO
    open MBrace.FsPickler
    open System.Numerics
    open VectorExtension

    let createWorld(config : Configuration) =
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()

        let world0 = World.Create(config.PlaneSet, Path.Combine(config.ScriptPath, config.StrategyFile), config.MaxTankNeeds * GroundAttackVehicle.MediumTankCost, 1.0f<E/H> * config.PlaneProduction)
        let world = { world0 with WeatherDaysOffset = (float config.WeatherDayMaxOffset) * (random.NextDouble() - 0.5); ProductionFactor = config.ProductionFactor }

        let capacity =
            world.Regions
            |> Seq.map (fun region -> region.RegionId, region.Storage |> Seq.sumBy (fun sto -> sto.Storage))
            |> Map.ofSeq

        let production =
            world.Regions
            |> Seq.map (fun region -> region.RegionId, region.Production |> Seq.sumBy (fun prod -> prod.Production(config.ProductionFactor)))
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

        printfn "%20s | %13s | %12s | %12s" "region" "Prod - cap" "AA" "AT"
        for region in world.Regions do
            let (RegionId regionName) = region.RegionId
            let aa = Map.tryFind region.RegionId antiAirUsage |> fun x -> defaultArg x 0
            let at = Map.tryFind region.RegionId antiTankUsage |> fun x -> defaultArg x 0
            let cap = Map.tryFind region.RegionId capacity |> fun x -> defaultArg x 0.0f<E>
            let prod = Map.tryFind region.RegionId production |> fun x -> defaultArg x 0.0f<E/H>
            printfn "%20s | %6.1f - %3.0f | %4d - %5.1f | %4d - %5.1f" regionName prod cap aa (100.0f * float32 aa / (cap / cannonCost)) at (100.0f * float32 at / (cap / cannonCost))

        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let outputDir = config.OutputDir
        use worldFile = File.CreateText(Path.Combine(outputDir, Filenames.world))
        serializer.Serialize(worldFile, world)
        world.StartDate

    let createState(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
        let world = serializer.Deserialize<World>(worldFile)
        use weatherFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.weather))
        let weather = serializer.Deserialize<Weather.WeatherState>(weatherFile)

        let state = WorldState.Create(world, Path.Combine(config.ScriptPath, config.StrategyFile), float32 weather.Wind.Direction)

        let outputDir = config.OutputDir
        use stateFile = File.CreateText(Path.Combine(outputDir, Filenames.state))
        serializer.Serialize(stateFile, state)

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
            let board, neighboursOf = BoardState.Create(world, state)
            yield "Initially"
            yield board.DisplayString
            yield sprintf "%f" board.Score.Value
            let minMax cancel n = minMax cancel n neighboursOf
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
        let date = nextDate (1.0f<H> * float32 config.MissionLength / 60.0f) state.Date
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
    open Campaign.Util
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
                []
            else
                decideColumnMovements world state config.ThinkTime
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
            |> Array.shuffle (System.Random())
            |> function
                | [||] -> []
                | arr -> [arr.[0]]
        let axisColumns = axisColumns @ pickReinforcements Axis
        let alliesColumns = alliesColumns @ pickReinforcements Allies

        // Decide resupply convoy movements
        let mkConvoys coalition =
            createAllConvoyOrders coalition (world, state)
            |> List.filter (fun order ->
                match order.Means with
                | ByRoad ->
                    // Remove road convoys that take the same road as a tank column
                    let orderEndPoints =
                        [ order.Convoy.Start; order.Convoy.Destination ]
                        |> List.sort
                    match coalition with
                    | Axis -> axisColumns
                    | Allies -> alliesColumns
                    |> List.exists (fun column ->
                        let columnEndPoints =
                            [ column.Start; column.Destination ]
                            |> List.sort
                        orderEndPoints <> columnEndPoints)
                | ByRail | ByAir _ ->
                    true)
            |> List.filter (fun order ->
                // Remove excessively small convoys. From a player's perspective it's a but underwhelming to find convoys with composed of two trucks.
                match order.Means with
                | ByRoad ->
                    order.Convoy.TransportedSupplies >= ResupplyOrder.TruckCapacity * 6.0f
                | ByRail | ByAir _ ->
                    true)
            |> prioritizeConvoys world state
            |> List.truncate config.MaxConvoys
        let axisConvoys = mkConvoys Axis
        let alliesConvoys = mkConvoys Allies

        // Air patrols
        let mkPatrols coalition =
            mkAllPatrols world state coalition
            |> prioritizeAiPatrols world state
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
            if weather.CloudDensity < 0.8 || weather.CloudHeight > 3500.0 then
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
                let attack, numPlanes = attacks.[0]
                List.init (min config.MaxAttackers numPlanes) (fun _ ->
                    let offset = 500.0f * Vector2(random.NextDouble() |> float32, random.NextDouble() |> float32)
                    { attack with
                        Start = attack.Start + offset
                    }
                )
        let axisAttacks, alliesAttacks =
            if weather.CloudDensity < 0.8 || weather.CloudHeight > 2500.0 then
                let allAttacks = mkAttacks()
                allAttacks |> List.filter (fun attack -> attack.Coalition = Axis), allAttacks |> List.filter (fun attack -> attack.Coalition = Allies)
            else
                [], []

        // Ferry flights
        let axisFerryFlights, alliesFerryFlights =
            if weather.CloudDensity < 0.8 || weather.CloudHeight > 2500.0 then
                decidePlaneTransfers world state Axis, decidePlaneTransfers world state Allies
            else
                [], []

        // Production
        let axisProduction = computeProductionPriorities Axis world state
        let alliesProduction = computeProductionPriorities Allies world state

        // Write orders to disk
        let outputDir = config.OutputDir
        use axisOrderFiles = File.CreateText(Path.Combine(outputDir, Filenames.axisOrders))
        let package = { Resupply = axisConvoys; Columns = axisColumns; Patrols = axisPatrols; Attacks = axisAttacks; Production = axisProduction; PlaneFerries = axisFerryFlights }.Renumber()
        serializer.Serialize(axisOrderFiles, package )
        use alliesOrderFiles = File.CreateText(Path.Combine(outputDir, Filenames.alliesOrders))
        let package = { Resupply = alliesConvoys; Columns = alliesColumns; Patrols = alliesPatrols; Attacks = alliesAttacks; Production = alliesProduction; PlaneFerries = alliesFerryFlights }.Renumber()
        serializer.Serialize(alliesOrderFiles, package )

module MissionFileGeneration =
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.MissionGeneration
    open Campaign.AutoOrder
    open Campaign.Orders
    open Campaign.Weather
    open Campaign.Util
    open Campaign.NewWorldState
    open Campaign.AfterActionReport
    open SturmovikMission.Blocks.BlocksMissionData
    open System.IO
    open MBrace.FsPickler
    open System.Diagnostics

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
                    printfn "Failed to read battles file because '%s'" e.Message
                    []
                |> List.map (fun battle -> battle.GetText() |> String.concat "")
                |> String.concat "<br>"
            else
                ""

        let dt = (1.0f<H>/60.0f) * float32 config.MissionLength

        let author = "coconut"
        let timeAndDate =
            sprintf "%s<br><br>" (state.Date.ToString("d MMM yyyy HH:mm"))
        let weatherDescription =
            let cover =
                if weather.CloudDensity < 0.2 then
                    "clear"
                elif weather.CloudDensity < 0.4 then
                    "light"
                elif weather.CloudDensity < 0.6 then
                    "medium"
                elif weather.CloudDensity < 0.8 then
                    "heavy"
                else
                    "overcast"
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
            sprintf "<b>Weather<b><br>Temperature: %2.0fC, Cloud cover: %s, Wind %3.1f m/s from %s<br><br>" weather.Temperature cover weather.Wind.Speed windOrigin
        let mainText =
            config.Briefing.Replace("\r\n", "\n").Split('\n')
            |> Array.map (fun s -> s.Trim())
            |> String.concat "<br>"
        let briefing = timeAndDate + weatherDescription + mainText + "<br><br>" + battles
        let missionName = config.MissionName
        let missionParams =
            { PlaneSet = config.PlaneSet
              MaxCapturedPlanes = config.MaxCapturedPlanes
              Author = author
              MissionName = config.MissionName
              Briefing = briefing
              MissionLength = config.MissionLength
              ColumnSplitInterval = config.ColumnSplitInterval
              MaxSimultaneousConvoys = config.MaxSimultaneousConvoys
              MaxSimultaneousFerryFlights = config.MaxSimultaneousFerryFlights
              StrategyMissionFile = Path.Combine(config.ScriptPath, config.StrategyFile)
              MaxVehiclesInBattle = config.MaxVehiclesInBattle
            }
        let missionData =
            { World = world
              State = state
              Weather = weather
              Random = random
              AxisOrders = allAxisOrders
              AlliesOrders = allAlliesOrders
            }
        writeMissionFile missionParams missionData (Path.Combine(config.OutputDir, missionName + ".Mission"))

        let mpDir = Path.Combine(config.ServerDataDir, "Multiplayer")
        let langs = ["eng"; "fra"; "rus"; "ger"; "spa"; "pol"]
        let swallow f = try f() with | _ -> ()
        // Remove old files from multiplayer directory
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".list")))
        for lang in langs do
            swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + "." + lang)))
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".msnbin")))
        // Copy new files to multiplayer directory
        File.Copy(Path.Combine(config.OutputDir, missionName + ".Mission"), Path.Combine(mpDir, missionName + ".Mission"))
        for lang in langs do
            // Mission.eng -> Mission.lang: we use the english text for all languagues. Better than not having any text at all.
            File.Copy(Path.Combine(config.OutputDir, missionName + ".eng"), Path.Combine(mpDir, missionName + "." + lang))
        let resaverDir = Path.Combine(config.ServerBinDir, "resaver")
        let p = ProcessStartInfo(sprintf "\"%s\"" (Path.Combine(resaverDir, "MissionResaver.exe")), sprintf "-d \"%s\" -f \"%s\"" config.ServerDataDir (Path.Combine(mpDir, missionName + ".Mission")))
        p.WorkingDirectory <- resaverDir
        p.UseShellExecute <- false
        let proc = Process.Start(p)
        proc.WaitForExit()
        // Remove text Mission file, it slows down mission loading.
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
        printfn "Resaver exited with code %d" proc.ExitCode
        proc.ExitCode

module MissionLogParsing =
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.ResultExtraction
    open Campaign.NewWorldState
    open Campaign.Orders
    open Campaign.AfterActionReport
    open MBrace.FsPickler
    open System.IO
    open ploggy
    open NLog
    open System.Text.RegularExpressions

    type MissionResults = {
        Entries : string list
        Shipments : SuppliesShipped list
        StaticDamages : Damage list
        VehicleDamages : Damage list
        TakeOffs : TookOff list
        Landings : Landed list
        ColumnDepartures : ColumnLeft list
        ParaDrops : ParaDropResult list
        FerryPlanes : Choice<PlaneFerryOrder, PlaneFerryOrder, PlaneFerryOrder> list
        BattleKills : BattleParticipantKilled list
    }

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
                    state.Date.ToString("yyyy-MM-dd_HH-mm-ss")
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
        ]
        |> List.iter (fun filename -> Path.GetFileNameWithoutExtension(filename) |> backupFile)

    let stage1(config : Configuration) =
        let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")

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

        do
            let config = Config.LoggingConfiguration()
            LogManager.Configuration <- config
            Plogger.Init()

        let entries =
            printfn "Looking for logs in %s" missionLogsDir
            // entries to remove from the log
            let timeLessEntryTypes = Set.ofList [ LogEntryType.LogVersion; LogEntryType.PosChanged; LogEntryType.Join; LogEntryType.Leave ]
            let missionHasPassed30Min (entry : LogEntry) =
                true
                //entry.Timestamp > System.TimeSpan(0, 30, 0)
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
                        yield LogEntry.Parse(line), file
            }
            |> Seq.filter (fun (entry, _) -> not (timeLessEntryTypes.Contains(entry.EntryType)))
            |> Seq.fold (fun (previous, current) (entry, file) ->  // Keep the latest mission reports with the proper mission start date
                match current, entry with
                | _, (:? MissionStartEntry as start) ->
                    // Move current to previous if it's non empty and has lasted long enough (30 minutes)
                    let previous =
                        match current with
                        | [] -> previous
                        | (last : LogEntry) :: _ ->
                            if missionHasPassed30Min last then
                                printfn "Reached start of new mission, moving current to previous"
                                current
                            else
                                printfn "Reached start of new mission, discarding current because too short"
                                previous
                    let expectedMissionFile = sprintf "%s.msnbin" config.MissionName
                    let actualMissionFile = start.MissionFile.Split('/') |> Seq.last
                    if start.MissionTime = state.Date && actualMissionFile.ToLower() = expectedMissionFile.ToLower() then
                        printfn "Start collecting from %s" file
                        previous, [entry] // Start new list
                    else
                        let reason =
                            if start.MissionTime <> state.Date then
                                "bad start date"
                            elif actualMissionFile.ToLower() <> expectedMissionFile.ToLower() then
                                "bad mission filename"
                            else
                                "???"
                        printfn "Stopped collecting at %s because %s" file reason
                        previous, [] // Start new skip
                | [], _ ->
                    printfn "Skipped %s" file
                    previous, [] // Skip, looking for next mission start that has the right time
                | _ :: _, _ ->
                    printfn "Collected %s" file
                    previous, entry :: current // Recording, update current
            ) ([], [])
            |> function // current if it's longer than 30 min, previous otherwise
                | (previous, []) -> previous
                | (previous, ((last :: _) as current)) ->
                    if missionHasPassed30Min last then
                        current
                    else
                        printfn "Discarded short sequence"
                        previous
            |> List.rev

        if List.isEmpty entries then
            failwith "No entries found suitable for result extraction"
        let shipments =
            let shipmentAxis = extractSuppliesShipped axisOrders.Resupply entries |> List.ofSeq
            let shipmentAllies = extractSuppliesShipped alliesOrders.Resupply entries |> List.ofSeq
            shipmentAxis @ shipmentAllies
        let staticDamages = extractStaticDamages world entries |> List.ofSeq
        let vehicleDamages = extractVehicleDamages (axisOrders.Columns @ alliesOrders.Columns) (axisOrders.Resupply @ alliesOrders.Resupply) entries |> List.ofSeq
        let takeOffs, landings =
            let both =
                extractTakeOffsAndLandings world state entries
            both |> List.choose (function TookOff x -> Some x | _ -> None),
            both |> List.choose (function Landed x -> Some x | _ -> None)
        let movements = axisOrders.Columns @ alliesOrders.Columns
        let columnDepartures = extractColumnDepartures movements entries |> List.ofSeq
        let battles =
            Battlefield.identifyBattleAreas world state
            |> Seq.cache
        let paraDrops = extractParaDrops world state battles entries
        let planeFerryEvents = extractFerryPlanes (axisOrders.PlaneFerries @ alliesOrders.PlaneFerries) entries
        let battleKills = extractBattleDamages world state battles entries
        let results =
            { Entries = entries |> List.map (fun entry -> entry.OriginalString)
              Shipments = shipments
              StaticDamages = staticDamages
              VehicleDamages = vehicleDamages
              TakeOffs = takeOffs
              Landings = landings
              ColumnDepartures = columnDepartures
              ParaDrops = paraDrops
              FerryPlanes = planeFerryEvents
              BattleKills = battleKills
            }
        use missionFile = File.CreateText(Path.Combine(config.OutputDir, Filenames.missionResults))
        serializer.Serialize(missionFile, results)
        results

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
        let dt = (1.0f<H>/60.0f) * float32 config.MissionLength
        let movements = axisOrders.Columns @ alliesOrders.Columns
        let state2, newlyProduced, battleReports = newState dt world state axisOrders.Production alliesOrders.Production movements missionResults.Shipments (axisOrders.Resupply @ alliesOrders.Resupply) (missionResults.StaticDamages @ missionResults.VehicleDamages) missionResults.TakeOffs missionResults.Landings missionResults.ColumnDepartures missionResults.ParaDrops missionResults.FerryPlanes missionResults.BattleKills (float32 weather.Wind.Direction)
        newlyProduced, battleReports, (state, state2)

    let buildAfterActionReports(config, state1, state2, tookOff, landed, damages, newlyProduced) =
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
        let aarAxis = buildReport world state1 state2 tookOff landed damages axisOrders.Columns newSupplies newAxisVehicles Axis
        let aarAllies = buildReport world state1 state2 tookOff landed damages alliesOrders.Columns newSupplies newAlliesVehicles Allies
        aarAxis, aarAllies

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