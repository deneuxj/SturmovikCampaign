module Campaign.Commenting

open System
open System.IO
open System.Text.RegularExpressions
open Campaign.BasicTypes
open Campaign.Configuration
open Util
open ploggy
open System.Numerics
open FSharp.Control
open Campaign.ResultExtraction
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes
open Campaign.PlaneModel
open MBrace.FsPickler
open Campaign.Orders

type EventHandlers =
    // player name, coalition, airfield, plane, cargo
    { OnTookOff : string * CoalitionId * AirfieldId * PlaneModel * float32<E> -> Async<unit>
      // playername, coalition, airfield, plane, cargo, health, damages inflicted
      OnLanded : string * CoalitionId * AirfieldId * PlaneModel * float32<E> * float32 * float32<E> -> Async<unit>
    }

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (missionLogsDir : string, handlers : EventHandlers, world : World, state : WorldState, convoys : ResupplyOrder list, columns : ColumnMovement list) =
    // retrieve entries from most recent mission, if it matches the state's mission and start date.
    let files =
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
        ordered
        |> List.ofSeq
    let lines =
        seq {
            for file in files do
                yield! File.ReadAllLines(file)
        }
        |> List.ofSeq
    let initialEntries =
        lines
        |> Seq.map LogEntry.Parse
        |> Seq.split (function :? MissionStartEntry -> true | _ -> false)
        |> Seq.filter (fun group ->
            if Seq.isEmpty group then
                false
            else
                match Seq.head group with
                | :? MissionStartEntry as start ->
                    start.MissionTime = state.Date
                | _ ->
                    false)
        |> Seq.tryLast
        |> Option.defaultVal []
        |> List.ofSeq
    let watcher = new FileSystemWatcher()
    do watcher.Path <- missionLogsDir
       watcher.Filter <- "missionReport*.txt"
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    // An AsyncSeq built from new file notifications: generate log entries
    let rec getEntry (batched, alreadyHandled : Set<string>) = 
        async {
            match batched with
            | [] ->
                let! ev = Async.AwaitEvent watcher.Changed
                if not(alreadyHandled.Contains(ev.FullPath)) then
                    let entries2 =
                        try
                            [
                                for line in File.ReadAllLines(ev.FullPath) do
                                    yield LogEntry.Parse(line)
                            ]
                        with
                        | e ->
                            eprintfn "Failed to parse '%s' because '%s'" ev.FullPath e.Message
                            []
                    return! getEntry (entries2, Set.add  ev.FullPath alreadyHandled)
                else
                    return! getEntry (batched, alreadyHandled)
            | x :: xs ->
                return Some(x, (xs, alreadyHandled))
        }
    let asyncSeqEntries =
        AsyncSeq.unfoldAsync getEntry (initialEntries, Set.ofSeq files)
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    // Notify of interesting take-offs and landings
    do
        let task =
            let takeOffsAndLandings =
                asyncSeqEntries
                |> extractTakeOffsAndLandings world state
            let staticDamages =
                asyncSeqEntries
                |> extractStaticDamages world
            let vehicleDamages =
                asyncSeqEntries
                |> extractVehicleDamages columns convoys
            let damages = AsyncSeq.merge staticDamages vehicleDamages
            asyncSeq {
                let damageInflicted = ref Map.empty
                let coalitionOf = ref Map.empty
                let wg = world.FastAccess
                let sg = state.FastAccess
                let convoys =
                    convoys
                    |> Seq.map (fun convoy -> convoy.OrderId, convoy)
                    |> Map.ofSeq
                let columns =
                    columns
                    |> Seq.map (fun column -> column.OrderId, column)
                    |> Map.ofSeq
                for event in AsyncSeq.mergeChoice takeOffsAndLandings damages do
                    match event with
                    | Choice1Of2((TookOff { PlayerName = Some player; Coalition = coalition }) as tookOff) ->
                        damageInflicted := Map.add player 0.0f<E> damageInflicted.Value
                        coalitionOf := Map.add player coalition coalitionOf.Value
                        yield tookOff, 0.0f<E>
                    | Choice1Of2((Landed { PlayerName = Some player }) as landed) ->
                        let damage = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                        damageInflicted := Map.remove player damageInflicted.Value
                        yield landed, damage
                    | Choice1Of2(TookOff { PlayerName = None }) | Choice1Of2(Landed { PlayerName = None }) -> ()
                    | Choice2Of2 damage ->
                        match damage.Data.ByPlayer with
                        | Some player ->
                            let acc = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                            let coalition = coalitionOf.Value.TryFind player |> Option.bind id
                            let cost =
                                match damage.Object with
                                | Production(region, idx) ->
                                    let pro = wg.GetRegion(region).Production.[idx]
                                    let lostDueToDamage =
                                        0.5f * pro.Production(world.SubBlockSpecs, world.ProductionFactor) * pro.RepairCost(world.SubBlockSpecs) * damage.Data.Amount / healLimit
                                    pro.RepairCost(world.SubBlockSpecs) + pro.Storage world.SubBlockSpecs + lostDueToDamage / damage.Data.Amount
                                | Storage(region, idx) ->
                                    let sto = wg.GetRegion(region).Storage.[idx]
                                    sto.RepairCost(world.SubBlockSpecs) + sto.Storage world.SubBlockSpecs
                                | Airfield(af, idx) ->
                                    let sto = wg.GetAirfield(af).Storage.[idx]
                                    sto.RepairCost(world.SubBlockSpecs) + sto.Storage world.SubBlockSpecs
                                | Convoy vehicle ->
                                    match convoys.TryFind(vehicle.OrderId) with
                                    | Some order ->
                                        match order.Means with
                                        | ByRiverShip
                                        | BySeaShip -> ResupplyOrder.ShipCapacity
                                        | ByAir(_, _) -> order.Convoy.TransportedSupplies
                                        | ByRail -> order.Convoy.TransportedSupplies
                                        | ByRoad -> ResupplyOrder.TruckCapacity
                                    | None ->
                                        0.0f<E>
                                | Column vehicle ->
                                    match columns.TryFind(vehicle.OrderId) with
                                    | Some order ->
                                        match order.TransportType with
                                        | ColByRiverShip
                                        | ColBySeaShip ->
                                            order.Composition
                                            |> Seq.sumBy (fun x -> x.Cost)
                                            |> fun x -> x * 0.5f
                                        | ColByTrain ->
                                            order.Composition
                                            |> Seq.sumBy (fun x -> x.Cost)
                                        | ColByRoad ->
                                            order.Composition.[vehicle.Rank].Cost
                                    | None ->
                                        0.0f<E>
                                | ParkedPlane(_, plane) ->
                                    plane.Cost
                                | Cannon _ ->
                                    cannonCost
                                | LightMachineGun _ ->
                                    lightMachineGunCost
                                | HeavyMachineGun _ ->
                                    heavyMachineGunCost
                                | Vehicle(_, vehicle) ->
                                    vehicle.Cost
                                | ActivePlane(_, plane) ->
                                    plane.Cost
                            // friendly-fire modifier: -1 coefficient
                            let penalty =
                                if coalition = damage.Object.Coalition(wg, sg) then
                                    -1.0f
                                else
                                    1.0f
                            damageInflicted := Map.add player (acc + penalty * damage.Data.Amount * cost) damageInflicted.Value
                        | None ->
                            ()
            }
            |> AsyncSeq.iterAsync (fun (event, damage) ->
                async {
                    match event with
                    | TookOff ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        return! handlers.OnTookOff(player, coalition, x.Airfield, x.Plane, x.Cargo)
                    | Landed ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        return! handlers.OnLanded(player, coalition, x.Airfield, x.Plane, x.Cargo, x.Health, damage)
                    | _ ->
                        return()
                })
        Async.Start(task, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()

/// Monitor state.xml, (re-) starting a commentator whenever the file is modified
type CommentatorRestarter(missionLogsDir : string, campaignDir : string, missionName : string, handlers : EventHandlers, onStateWritten : unit -> unit) =
    let missionFile = missionName + ".mission"
    let watcher = new FileSystemWatcher()
    do watcher.Path <- campaignDir
       watcher.Filter <- missionFile
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    let serializer = FsPickler.CreateXmlSerializer()
    let rec awaitFiles remaining =
        async {
            if Set.isEmpty remaining then
                return ()
            else
                let! ev = Async.AwaitEvent watcher.Changed
                return! awaitFiles (Set.remove ev.Name remaining)
        }
    let rec work world commentator =
        async {
            match commentator with
            | Some (commentator : Commentator) -> commentator.Dispose()
            | None -> ()
            let state =
                try
                    use stateFile = File.OpenText(Path.Combine(campaignDir, "state.xml"))
                    serializer.Deserialize<WorldState>(stateFile) |> Some
                with
                | exc ->
                    eprintfn "Failed to parse state.xml: %s" exc.Message
                    None
            let axisOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "axisOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    eprintfn "Failed to parse axisOrders.xml: %s" exc.Message
                    None
            let alliesOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "alliesOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    eprintfn "Failed to parse alliesOrders.xml: %s" exc.Message
                    None
            match state, axisOrders, alliesOrders with
            | Some state, Some axisOrders, Some alliesOrders ->
                printfn "Starting new commentator"
                let commentator = new Commentator(missionLogsDir, handlers, world, state, axisOrders.Resupply @ alliesOrders.Resupply, axisOrders.Columns @ alliesOrders.Columns)
                do! awaitFiles (Set[missionFile])
                onStateWritten()
                return! work world (Some commentator)
            | _ ->
                printfn "Waiting until next change to %s" missionFile
                do! awaitFiles (Set[missionFile])
                onStateWritten()
                return! work world None
        }
    // Load world, then repeatedly monitor for new state and order files
    let prepare =
        async {
            let worldPath = Path.Combine(campaignDir, "world.xml")
            let missionPath = Path.Combine(campaignDir, missionFile)
            let rec loadWorld() =
                async {
                    // Wait until the mission file exists. When that file exists, we know that world.xml is ready to be read.
                    if not (File.Exists(missionPath)) then
                        do! Async.Sleep 5000
                        return! loadWorld()
                    else
                        use worldFile = File.OpenText(worldPath)
                        let world = serializer.Deserialize<World>(worldFile)
                        return world
                }
            let! world = loadWorld()
            return! work world None
        }
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    do Async.Start(prepare, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()