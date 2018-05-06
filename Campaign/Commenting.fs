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
open Campaign.PlayerDiscipline
open Campaign.PlayerHangar
open Campaign.WatchLogs
open MBrace.FsPickler
open Campaign.Orders
open SturmovikMission.Blocks.Util.String
open Campaign.ChatCommands

let private logger = NLog.LogManager.GetCurrentClassLogger()


type EventHandlers =
    // player name, coalition, airfield, coalition of airfield, plane, cargo
    { OnTookOff : string * CoalitionId * AirfieldId * CoalitionId option * PlaneModel * float32<K> -> Async<unit>
      // playername, coalition of player, airfield, coalition of airfield, plane, cargo, health, damages inflicted
      OnLanded : string * CoalitionId * AirfieldId * CoalitionId option * PlaneModel * float32<K> * float32 * float32<E> -> Async<unit>
      // Region name, attacker coalition
      OnMaxBattleDamageExceeded : string * CoalitionId -> Async<unit>
      // A player was banned or kicked
      OnPlayerPunished : Judgement -> Async<unit>
      // Send messages to a player
      OnMessagesToPlayer : UserIds * string list -> Async<unit>
      // Send messages to a coalition
      OnMessagesToCoalition : CoalitionId * string list -> Async<unit>
      // Update planeset at airfield
      OnPlaneSetChanged : AirfieldId * int -> Async<unit>
    }

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (config : Configuration, handlers : EventHandlers, world : World, state : WorldState, hangars : Map<string, PlayerHangar>, convoys : ResupplyOrder list, columns : ColumnMovement list) =
    let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    // retrieve entries from most recent mission
    let files =
        let unordered = Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt")
        let r = Regex(@"(missionReport\(.*\))\[([0-9]+)\]\.txt")
        let filtered =
            unordered
            |> Seq.choose(fun path ->
                let m = r.Match(Path.GetFileName(path))
                if not m.Success then
                    None
                else
                    Some(path, (m.Groups.[1].ToString(), System.Int32.Parse(m.Groups.[2].ToString()))))
        let lastGroup =
            filtered
            |> Seq.groupBy (snd >> fst)
            |> Seq.tryLast
            |> Option.map snd
            |> Option.defaultVal Seq.empty
        let sorted =
            lastGroup
            |> Seq.sortBy snd
            |> Seq.map fst
        sorted
        |> Array.ofSeq
    let asyncSeqEntries =
        resumeWatchlogs(missionLogsDir, "missionReport*.txt", files, cancelOnDispose.Token)
    let asyncIterNonMuted f xs =
        AsyncSeq.mergeChoice xs asyncSeqEntries
        |> AsyncSeq.map (fun x -> logger.Debug(sprintf "%A" x); x)
        |> AsyncSeq.skipWhile(
            function
            | Choice1Of2 _ -> true
            | Choice2Of2(Old _) -> true
            | Choice2Of2(Fresh _) -> false)
        |> AsyncSeq.choose(
            function
            | Choice1Of2 x -> Some x
            | Choice2Of2 _ -> None)
        |> AsyncSeq.iterAsyncParallelThrottled 8 f
    let asyncSeqEntries =
        asyncSeqEntries
        |> AsyncSeq.choose (fun line ->
            try
                let x = LogEntry.Parse(string line)
                Some x
            with
            | _ ->
                logger.Error(sprintf "Failed to parse '%s'" (string line))
                None)
        |> AsyncSeq.filter (function null -> false | _ -> true)
    let asyncCommands =
        if config.ChatLogCommandsEnabled then
            watchCommands(Path.Combine(config.ServerDataDir, "chatlogs"), cancelOnDispose.Token)
            |> AsyncSeq.map (fun line -> ChatCommand.Parse(world, line))
        else
            AsyncSeq.empty
    // Notify of interesting take-offs and landings
    do
        let battleDamages =
            let battles =
                Battlefield.identifyBattleAreas world state
                |> Seq.cache
            asyncSeqEntries
            |> extractBattleDamages world state battles
            |> AsyncSeq.filter (fun damage -> damage.KilledByPlayer.IsSome)
        let takeOffsAndLandings =
            asyncSeqEntries
            |> extractTakeOffsAndLandings world state
        let staticDamages =
            asyncSeqEntries
            |> extractStaticDamages world
        let vehicleDamages =
            asyncSeqEntries
            |> extractVehicleDamages world columns convoys
        let damages = AsyncSeq.merge staticDamages vehicleDamages
        let wg = world.FastAccess
        let sg = state.FastAccess
        let task =
            asyncSeq {
                let damageInflicted = ref Map.empty
                let coalitionOf = ref Map.empty
                let convoys =
                    convoys
                    |> Seq.map (fun convoy -> convoy.OrderId, convoy)
                    |> Map.ofSeq
                let columns =
                    columns
                    |> Seq.map (fun column -> column.OrderId, column)
                    |> Map.ofSeq
                for event in AsyncSeq.mergeChoice3 takeOffsAndLandings damages battleDamages do
                    match event with
                    | Choice1Of3((TookOff { PlayerName = Some player; Coalition = coalition }) as tookOff) ->
                        logger.Info("Take off event")
                        damageInflicted := Map.add player 0.0f<E> damageInflicted.Value
                        coalitionOf := Map.add player coalition coalitionOf.Value
                        yield tookOff, 0.0f<E>
                    | Choice1Of3((Landed { PlayerName = Some player }) as landed) ->
                        logger.Info("Landed event")
                        let damage = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                        damageInflicted := Map.remove player damageInflicted.Value
                        yield landed, damage
                    | Choice1Of3(TookOff { PlayerName = None }) | Choice1Of3(Landed { PlayerName = None }) -> ()
                    | Choice2Of3 damage ->
                        match damage.Data.ByPlayer with
                        | Some player ->
                            let acc = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                            let coalition = coalitionOf.Value.TryFind player |> Option.bind id
                            let cost =
                                match damage.Object with
                                | Production(region, idx) ->
                                    let pro = wg.GetRegion(region).Production.[idx]
                                    let lostDueToDamage =
                                        0.5f * pro.Production(world.SubBlockSpecs, world.ProductionFactor) * pro.RepairCost(world.SubBlockSpecs) * damage.Data.Amount / world.RepairSpeed
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
                    | Choice3Of3 ({ KilledByPlayer = Some killer } as battleKill) ->
                        match coalitionOf.Value.TryFind(killer) |> Option.bind id with
                        | Some coalition ->
                            let penalty =
                                if coalition = battleKill.Coalition then
                                    -1.0f
                                else
                                    1.0f
                            let acc = damageInflicted.Value.TryFind killer |> Option.defaultVal 0.0f<E>
                            damageInflicted := Map.add killer (acc + penalty * battleKill.Vehicle.Cost / float32 config.BattleKillRatio) damageInflicted.Value
                        | None ->
                            ()
                    | Choice3Of3 { KilledByPlayer = None } -> ()
                }
            |> asyncIterNonMuted(fun (event, damage) ->
                async {
                    match event with
                    | TookOff ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        logger.Info(sprintf "%s took off" player)
                        let afCoalition = sg.GetRegion(wg.GetAirfield(x.Airfield).Region).Owner
                        return! handlers.OnTookOff(player, coalition, x.Airfield, afCoalition, x.Plane, x.Cargo)
                    | Landed ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        logger.Info(sprintf "%s landed" player)
                        let afCoalition = sg.GetRegion(wg.GetAirfield(x.Airfield).Region).Owner
                        return! handlers.OnLanded(player, coalition, x.Airfield, afCoalition, x.Plane, x.Cargo, x.Health, damage)
                    | _ ->
                        return ()
                })
        let task2 =
            asyncSeq {
                let battleDamage = ref Map.empty
                for battleKill in battleDamages do
                    let oldBattleDamage =
                        battleDamage.Value.TryFind (battleKill.BattleId, battleKill.Coalition)
                        |> Option.defaultValue 0.0f<E>
                    let newDamage = oldBattleDamage + battleKill.Vehicle.Cost / (float32 config.BattleKillRatio)
                    battleDamage := Map.add (battleKill.BattleId, battleKill.Coalition) newDamage battleDamage.Value
                    let totalValue =
                        GroundAttackVehicle.AllVehicles
                        |> Seq.sumBy (fun vehicle ->
                            (float32(sg.GetRegion(battleKill.BattleId).GetNumVehicles(battleKill.Coalition, vehicle))) * vehicle.Cost)
                    if newDamage > totalValue * config.MaxBattleKillsRatioByPlayers then
                        yield battleKill.Coalition.Other, battleKill.BattleId
            }
            |> asyncIterNonMuted (fun (coalition, region) -> handlers.OnMaxBattleDamageExceeded(string region, coalition))
        let disciplineTask =
            asyncSeqEntries
            |> disciplinePlayers config world 
            |> asyncIterNonMuted (fun penalty -> handlers.OnPlayerPunished penalty)
        let hangarTask =
            asyncSeqEntries
            |> checkPlaneAvailability world state hangars 
            |> AsyncSeq.mergeChoice asyncCommands
            |> AsyncSeq.scan (fun (_, hangars : Map<string, PlayerHangar>, airfields : Map<AirfieldId, AirfieldState>) item ->
                match item with
                | Choice1Of2 cmd ->
                    let userIds =
                        hangars
                        |> Map.toSeq
                        |> Seq.map snd
                        |> Seq.filter (fun hangar -> hangar.PlayerName = cmd.Player)
                        |> List.ofSeq
                    let uid =
                        match userIds with
                        | [hangar] ->
                            { UserId = string hangar.Player; Name = hangar.PlayerName }
                        | _ ->
                            // Use a fake Guid, OK because the player name can be used instead.
                            { UserId = string (Guid("00000000-0000-0000-0000-000000000000")); Name = cmd.Player }
                    Some(Overview(uid, 0, cmd.Interpret(hangars, airfields))), hangars, airfields
                | Choice2Of2 (Status(hangars, airfields) as x) ->
                    Some x, hangars, airfields
                | Choice2Of2 x ->
                    Some x, hangars, airfields
            ) (None, Map.empty, Map.empty)
            |> AsyncSeq.choose (function (Some x, _, _) -> Some x | _ -> None)
            |> asyncIterNonMuted (
                function
                | Overview(user, delay, messages) ->
                    async {
                        do! Async.Sleep(delay * 1000)
                        return! handlers.OnMessagesToPlayer(user, messages)
                    }
                | Warning(user, delay, messages) ->
                    async {
                        do! Async.Sleep(delay * 1000)
                        return! handlers.OnMessagesToPlayer(user, messages)
                    }
                | Announce(coalition, messages) ->
                    handlers.OnMessagesToCoalition(coalition, messages)
                | Violation(user) ->
                    handlers.OnMessagesToPlayer(user, ["Kicking for stealing planes not enabled yet, consider yourself lucky"])
                    // handlers.OnPlayerPunished({ Player = user; Decision = Kicked })
                | PlanesAtAirfield(afId, numPlanes) ->
                    // State of airfield at start of mission
                    let afs = sg.GetAirfield(afId)
                    match sg.GetRegion(wg.GetAirfield(afId).Region).Owner with
                    | Some coalition ->
                        // Compute plane index: For each plane initially found in the spawn, check if it's now available.
                        // If so, include it in the plane index, which is a bit mask.
                        let planeSetIndex =
                            let origNumPlanes = afs.NumPlanes |> Map.map (fun _ qty -> int(floor(qty)))
                            let spawnPlanes = Airfield.selectPlaneSpawns Airfield.maxPlaneSpawns coalition origNumPlanes
                            spawnPlanes
                            |> Array.fold(fun (res, index) plane ->
                                let qty = numPlanes.TryFind(plane) |> Option.defaultValue 0.0f
                                if qty > 1.0f then
                                    (res ||| index, index <<< 1)
                                else
                                    (res, index <<< 1)) (0, 1)
                            |> fst
                        logger.Debug(sprintf "Change to planeset %d at %s" planeSetIndex afId.AirfieldName)
                        handlers.OnPlaneSetChanged(afId, planeSetIndex)
                    | None ->
                        async.Zero()
                | Status _ ->
                    async.Zero()
                )
        Async.Start(Async.catchLog "live commentator" task, cancelOnDispose.Token)
        Async.Start(Async.catchLog "battle limits notifier" task2, cancelOnDispose.Token)
        Async.Start(Async.catchLog "abuse detector" disciplineTask, cancelOnDispose.Token)
        Async.Start(Async.catchLog "plane availability checker" hangarTask, cancelOnDispose.Token)

    member this.Dispose() =
        cancelOnDispose.Cancel()

/// Monitor state.xml, (re-) starting a commentator whenever the file is modified
type CommentatorRestarter(config : Configuration, handlers : EventHandlers, onStateWritten : unit -> unit) =
    let missionName = config.MissionName
    let campaignDir = config.OutputDir
    let missionDir = Path.Combine(campaignDir, "Multiplayer", "Dogfight")
    let missionFile = missionName + "_1.mission"
    let watcher = new FileSystemWatcher()
    do watcher.Path <- missionDir
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
                    logger.Error(sprintf "Failed to parse state.xml: %s" exc.Message)
                    None
            let axisOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "axisOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    logger.Error(sprintf "Failed to parse axisOrders.xml: %s" exc.Message)
                    None
            let alliesOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "alliesOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    logger.Error(sprintf "Failed to parse alliesOrders.xml: %s" exc.Message)
                    None
            let hangars =
                tryLoadHangars (Path.Combine(campaignDir, "hangars.xml"))
                |> Option.defaultValue Map.empty
                |> guidToStrings
            match state, axisOrders, alliesOrders with
            | Some state, Some axisOrders, Some alliesOrders ->
                logger.Info("Starting new commentator")
                let commentator = new Commentator(config, handlers, world, state, hangars, axisOrders.Resupply @ alliesOrders.Resupply, axisOrders.Columns @ alliesOrders.Columns)
                do! awaitFiles (Set[missionFile])
                onStateWritten()
                return! work world (Some commentator)
            | _ ->
                logger.Info(sprintf "Waiting until next change to %s" missionFile)
                do! awaitFiles (Set[missionFile])
                onStateWritten()
                return! work world None
        }
    // Load world, then repeatedly monitor for new state and order files
    let prepare =
        async {
            let worldPath = Path.Combine(campaignDir, "world.xml")
            let missionPath = Path.Combine(missionDir, missionFile)
            let rec loadWorld() =
                async {
                    // Wait until the mission file exists. When that file exists, we know that world.xml is ready to be read.
                    if not (File.Exists(missionPath)) then
                        do! Async.Sleep 5000
                        return! loadWorld()
                    else
                        use worldFile = File.OpenText(worldPath)
                        let world =
                            try
                                serializer.Deserialize<World>(worldFile)
                                |> Some
                            with
                            | _ -> None
                        match world with
                        | Some world -> return world
                        | None ->
                            do! Async.Sleep(15000)
                            return! loadWorld()
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