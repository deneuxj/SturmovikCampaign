module Campaign.MissionGeneration

open System.Numerics
open System.Collections.Generic

open Util

open SturmovikMission.DataProvider

open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.Train
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open SturmovikMission.Blocks.IO
open SturmovikMission.Blocks.TransportFlight
open SturmovikMission.Blocks.ShipConvoy
open SturmovikMission.Blocks.FireLoop
open SturmovikMission.Blocks.ParaDrop
open SturmovikMission.Blocks.MissionEnd
open SturmovikMission.Blocks.MapGraphics
open SturmovikMission.Blocks.Vehicles

open VectorExtension

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Weather
open Campaign.Orders
open Campaign.MapGraphics
open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.ArtilleryGroup
open Campaign.StaticBlocks
open Campaign.Airfield
open Campaign.Convoys
open Campaign.TankParks


/// Set the country of entity owners in a list of Mcus depending on the region where they are located.
let setCountries (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (group : Mcu.McuBase list) =
    let subst = Mcu.substId <| store.GetIdMapper()
    for mcu in group do
        subst mcu
    for mcu in group do
        match mcu with
        | :? Mcu.HasEntity as flag ->
            let pos = Vector2.FromMcu(flag.Pos)
            let owner =
                List.zip world.Regions state.Regions
                |> List.tryPick (fun (region, regState) ->
                    if pos.IsInConvexPolygon region.Boundary then
                        regState.Owner
                    else
                        None
                )
                |> function
                    | Some Axis -> Mcu.CountryValue.Germany
                    | Some Allies -> Mcu.CountryValue.Russia
                    | None -> Mcu.CountryValue.Russia
            flag.Country <- Some owner
        | _ ->
            ()


let createBuildingFires store (world : World) (state : WorldState) (windDirection : float32) =
    let maxFires = 20
    let fires = state.FirePositions(world, maxFires)
    [
        for pos, alt, size in fires do
            if size >= smallDamage then
                let fireType =
                    if size >= bigDamage then
                        FireType.CityFire
                    elif size >= mediumDamage then
                        FireType.CityFireSmall
                    else
                        FireType.VillageSmoke
                yield FireLoop.Create(store, pos, alt, windDirection, fireType)
    ]


let createParaTrooperDrops (world : World) store lcStore (battlefields : (DefenseAreaId * CoalitionId) seq) =
    let wg = world.FastAccess
    battlefields
    |> Seq.map (fun (bf, defending) ->
        let bf = wg.GetAntiTankDefenses(bf)
        [ ParaDrop.Create(store, lcStore, bf.DefensePos, bf.Position.Rotation, defending.ToCountry, "D-" + string bf.Home)
          ParaDrop.Create(store, lcStore, bf.AttackPos, bf.Position.Rotation + 180.0f, defending.Other.ToCountry, "A-" + string bf.Home) ])
    |> List.concat


let addMultiplayerPlaneConfigs (planeSet : PlaneModel.PlaneSet) (options : T.Options) =
    let configs =
        PlaneModel.AllModels(planeSet)
        |> List.map (fun model -> T.String(model.ScriptModel.Script))
    options.SetMultiplayerPlaneConfig(configs)


type MissionGenerationParameters = {
    PlaneSet : PlaneSet
    MaxCapturedPlanes : int
    Author : string
    MissionName : string
    Briefing : string
    MissionLength : int
    ColumnSplitInterval : int
    MaxSimultaneousConvoys : int
    MaxSimultaneousFerryFlights : int
    MaxVehiclesInBattle : int
    StrategyMissionFile : string
}


type MissionData = {
    World : World
    Random : System.Random
    Weather : WeatherState
    State : WorldState
    AxisOrders : OrderPackage
    AlliesOrders : OrderPackage
}


let writeMissionFile (missionParams : MissionGenerationParameters) (missionData : MissionData) (filename : string) =
    let wg = WorldFastAccess.Create(missionData.World)
    let strategyMissionData = T.GroupData(Parsing.Stream.FromFile missionParams.StrategyMissionFile)
    let options = strategyMissionData.ListOfOptions.Head
    let store = NumericalIdentifiers.IdStore()
    let lcStore = NumericalIdentifiers.IdStore()
    lcStore.SetNextId 3
    let getId = store.GetIdMapper()
    let missionBegin = newMissionBegin (getId 1)
    let includeSearchLights =
        missionData.State.Date.Hour <= 8 || missionData.State.Date.Hour + missionParams.MissionLength / 60 >= 18
    let inAttackArea(pos : Vector2) =
        missionData.AxisOrders.Attacks @ missionData.AlliesOrders.Attacks
        |> List.exists (fun attack -> (attack.Target - pos).Length() < 3000.0f)
    let staticDefenses = ArtilleryGroup.Create(missionData.Random, store, lcStore, includeSearchLights, missionBegin, missionData.World, missionData.State, missionData.AxisOrders.Columns @ missionData.AlliesOrders.Columns)
    let icons = MapIcons.CreateRegions(store, lcStore, missionData.World, missionData.State)
    let icons2 = MapIcons.CreateSupplyLevels(store, lcStore, missionData.World, missionData.State)
    let spotting = createStorageIcons store lcStore missionBegin missionData.World missionData.State
    let blocks =
        let allBlocks = strategyMissionData.ListOfBlock
        let parkedPlanes =
            strategyMissionData.GetGroup("Parked planes").CreateMcuList()
            |> List.map (fun mcu -> mcu.Index)
            |> Set.ofList
        allBlocks
        |> List.filter(fun block -> not(parkedPlanes.Contains(block.GetIndex().Value)))
        |> createBlocks missionData.Random store missionData.World missionData.State inAttackArea
    let bridges =
        strategyMissionData.ListOfBridge
        |> createBridges missionData.Random store missionData.World missionData.State inAttackArea
    let ground =
        strategyMissionData.ListOfGround
        |> createGrounds store
    let spawns = createAirfieldSpawns missionParams.MaxCapturedPlanes store missionData.World missionData.State
    let landingDirections = createLandingDirections store missionData.World missionData.State
    let mkConvoyNodes orders =
        let convoyPrioNodes, convoys = createConvoys store lcStore missionData.World missionData.State orders
        for node, (orderId, convoy) in List.zip convoyPrioNodes.Nodes convoys do
            let start, destroyed, arrived =
                match convoy with
                | Choice1Of4 trucks ->
                    trucks.Api.Start, trucks.Api.Destroyed, trucks.Api.Arrived
                | Choice2Of4 train ->
                    train.TheTrain.Start, train.TheTrain.Killed, train.TheTrain.Arrived
                | Choice3Of4 ships ->
                    ships.Start, ships.Killed, ships.Arrived
                | Choice4Of4 flight ->
                    flight.Start, flight.Killed, flight.Arrived
            Mcu.addTargetLink node.Do start.Index
            Mcu.addTargetLink destroyed convoyPrioNodes.Try.Index
            Mcu.addTargetLink arrived convoyPrioNodes.Try.Index
        for i, node in Seq.indexed convoyPrioNodes.Nodes do
            if i < missionParams.MaxSimultaneousConvoys then
                Mcu.addTargetLink missionBegin node.Do.Index
            else
                Mcu.addTargetLink missionBegin node.Enable.Index
        let convoys : (OrderId * Mcu.McuTrigger * McuUtil.IMcuGroup) list =
            convoys
            |> List.map (
                function
                | orderId, Choice1Of4 x -> orderId, x.StartDelay.Elapsed, x :> McuUtil.IMcuGroup
                | orderId, Choice2Of4 x -> orderId, x.Started.Trigger, x :> McuUtil.IMcuGroup
                | orderId, Choice3Of4 x -> orderId, x.Start, x.All
                | orderId, Choice4Of4 x -> orderId, x.Start, x.All)
        convoyPrioNodes.All, convoys
    let axisPrio, axisConvoys = mkConvoyNodes missionData.AxisOrders.Resupply
    let alliesPrio, alliesConvoys = mkConvoyNodes missionData.AlliesOrders.Resupply
    let mkColumns orders =
        let maxColumnSplit = max 1 (missionParams.MissionLength / missionParams.ColumnSplitInterval - 1)
        orders
        |> createColumns missionData.Random store lcStore missionData.World missionData.State missionBegin (60.0 * float missionParams.ColumnSplitInterval) maxColumnSplit missionParams.MissionLength
    let columns = mkColumns (missionData.AxisOrders.Columns @ missionData.AlliesOrders.Columns)
    let arrows =
        let startOfOrder =
            seq {
                for orderId, start, _ in axisConvoys do
                    yield orderId, start
                for orderId, start, _ in alliesConvoys do
                    yield orderId, start
                for orderId, start, _ in columns do
                    yield orderId, upcast start
            }
            |> dict
        [Axis; Allies]
        |> List.collect (fun coalition -> MapGraphics.MapIcons.CreateArrows(store, lcStore, missionData.World, missionData.State, missionData.AxisOrders, missionData.AlliesOrders, coalition))
        |> List.map (fun (orderId, arrow) ->
            match startOfOrder.TryGetValue(orderId), arrow.Show with
            | (true, start), Some show ->
                Mcu.addTargetLink start show.Index
                arrow.All
            | _, _ ->
                arrow.All)
        |> List.map McuUtil.groupFromList
    let axisConvoys =
        axisConvoys
        |> List.map (fun (_, _, group) -> group)
    let alliesConvoys =
        alliesConvoys
        |> List.map (fun (_, _, group) -> group)
    let columns =
        columns
        |> List.collect (fun (_, start, group) -> (McuUtil.groupFromList [start]) :: group)
    let battles =
        Battlefield.generateBattlefields missionParams.MaxVehiclesInBattle missionData.Random store lcStore missionData.World missionData.State
    for bf in battles do
        for start in bf.Starts do
            Mcu.addTargetLink missionBegin start.Index
    let battles = battles |> List.map (fun bf -> bf.All)
    let paraDrops =
        createParaTrooperDrops missionData.World store lcStore (Battlefield.identifyBattleAreas missionData.World missionData.State)
        |> List.map (fun p -> p.All)
    let parkedPlanes =
        createParkedPlanes store missionData.World missionData.State inAttackArea
        |> McuUtil.groupFromList
    let parkedTanks =
        createParkedTanks store missionData.World missionData.State inAttackArea missionData.AxisOrders Axis @ createParkedTanks store missionData.World missionData.State inAttackArea missionData.AlliesOrders Allies
        |> McuUtil.groupFromList
    let flags = strategyMissionData.GetGroup("Windsocks").CreateMcuList()
    setCountries store missionData.World missionData.State flags
    let ndbs = strategyMissionData.GetGroup("NDBs").CreateMcuList()
    setCountries store missionData.World missionData.State ndbs
    let ndbIcons =
        ndbs
        |> List.choose (fun ndb ->
            match ndb with
            | :? Mcu.HasEntity as ndb ->
                let coalition =
                    match ndb.Country with
                    | Some Mcu.CountryValue.Germany -> Mcu.CoalitionValue.Axis
                    | Some Mcu.CountryValue.Russia -> Mcu.CoalitionValue.Allies
                    | _ -> Mcu.CoalitionValue.Neutral
                let icon =
                    IconDisplay.IconDisplay.Create(store, lcStore, Vector2.FromMcu ndb.Pos, "NDB", coalition, Mcu.IconIdValue.Waypoint)
                Mcu.addTargetLink missionBegin icon.Show.Index
                Some icon.All
            | _ ->
                None)
    let landFires =
        if includeSearchLights then
            strategyMissionData.GetGroup("Land fires").CreateMcuList()
            |> createLandFires store missionData.World missionData.State missionBegin
        else
            []
    let landlights =
        if includeSearchLights then
            strategyMissionData.GetGroup("Land lights").CreateMcuList()
            |> createLandLights store missionData.World missionData.State missionBegin (spawns |> List.map fst)
        else
            []
    let allPatrols =
        let axisPatrols =
            missionData.AxisOrders.Patrols |> List.map (fun patrol -> patrol.ToPatrolBlock(store, lcStore))
        let alliesPatrols =
            missionData.AlliesOrders.Patrols |> List.map (fun patrol -> patrol.ToPatrolBlock(store, lcStore))
        [
            for allMcus, blocks in axisPatrols @ alliesPatrols do
                for block in blocks do
                    Mcu.addTargetLink missionBegin block.Start.Index
                yield allMcus
        ]
    let allAttacks =
        let axisAttacks =
            missionData.AxisOrders.Attacks |> List.map (fun attack -> attack.ToPatrolBlock(store, lcStore))
        let alliesAttacks =
            missionData.AlliesOrders.Attacks |> List.map (fun attack -> attack.ToPatrolBlock(store, lcStore))
        let mkAttackStarts (attacks : (_ * GroundAttack.Attacker list) list) =
            // Spawn "wing" immediately after "leader"
            for (_, blocks) in attacks do
                for b1, b2 in Seq.pairwise blocks do
                    Mcu.addTargetLink b1.Spawned b2.ImmediateStart.Index
            // Spawn pairs one minute after previous pair
            for (_, block1), (_, block2) in Seq.pairwise attacks do
                match block1, block2 with
                | b1 :: _, b2 :: _ ->
                    Mcu.addTargetLink b1.Spawned b2.DelayedStart.Index
                | _, _ ->
                    failwith "Expected at least one block in each attack list"
            // Start first pair one minute after mission start
            match attacks with
            | (_, hd :: _) :: _ ->
                Mcu.addTargetLink missionBegin hd.DelayedStart.Index
            | _ -> ()
        mkAttackStarts axisAttacks
        mkAttackStarts alliesAttacks
        axisAttacks @ alliesAttacks |> List.map fst
    let axisPlaneFerries =
        missionData.AxisOrders.PlaneFerries
        |> PlaneFerry.generatePlaneTransfer store lcStore missionData.World missionData.State missionBegin missionParams.MaxSimultaneousFerryFlights
    let alliesPlaneFerries =
        missionData.AlliesOrders.PlaneFerries
        |> PlaneFerry.generatePlaneTransfer store lcStore missionData.World missionData.State missionBegin missionParams.MaxSimultaneousFerryFlights
    let buildingFires =
        createBuildingFires store missionData.World missionData.State (float32 missionData.Weather.Wind.Direction)
        |> List.map (fun fire -> fire.All)
    let options =
        (Weather.setOptions missionData.Weather missionData.State.Date options)
            .SetMissionType(T.Integer 2) // deathmatch
            |> addMultiplayerPlaneConfigs missionParams.PlaneSet
    let optionStrings =
        { new McuUtil.IMcuGroup with
              member x.Content = []
              member x.LcStrings =
                [ (0, missionParams.MissionName)
                  (1, missionParams.Briefing)
                  (2, missionParams.Author)
                ]
              member x.SubGroups = []
        }
    let serverInputMissionEnd = MissionEnd.Create(store)
    let allGroups =
        [ optionStrings
          McuUtil.groupFromList [missionBegin]
          upcast staticDefenses
          upcast icons
          upcast icons2
          McuUtil.groupFromList blocks
          McuUtil.groupFromList bridges
          McuUtil.groupFromList ground
          McuUtil.groupFromList (spawns |> List.collect snd)
          McuUtil.groupFromList flags
          McuUtil.groupFromList ndbs
          McuUtil.groupFromList landlights
          parkedPlanes
          parkedTanks
          axisPrio
          alliesPrio
          axisPlaneFerries
          alliesPlaneFerries
          serverInputMissionEnd.All ] @ axisConvoys @ alliesConvoys @ spotting @ landFires @ arrows @ allPatrols @ allAttacks @ buildingFires @ columns @ battles @ paraDrops @ ndbIcons @ landingDirections
    writeMissionFiles "eng" filename options allGroups