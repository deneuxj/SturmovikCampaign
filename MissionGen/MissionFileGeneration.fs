// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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

/// Creation of game mission files
module Campaign.MissionGen.MissionFileGeneration

open System.Numerics
open VectorExtension
open Util

open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Battlefield
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks
open SturmovikMission.Blocks.VirtualConvoy
open SturmovikMission.Blocks.Train

open Campaign.Common.Buildings
open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel
open Campaign.Common.AiPlanes

open Campaign.MissionGen.StaticDefenseOptimization
open Campaign.MissionGen.MapGraphics

let private logger = NLog.LogManager.GetCurrentClassLogger()

type GameType =
    | Coop
    | SinglePlayer
    | MultiPlayer

type PlayerSpawnType =
    | Airborne
    | Runway
    | Parking of WarmedUp: bool
with
    member this.IntValue =
        match this with
        | Airborne -> 0
        | Runway -> 1
        | Parking _ -> 2

type PlayerSpawnPlane =
    {
        Model : PlaneModel
        AllowedMods : ModRange list
        AllowedPayloads : ModRange list
        AllowedSkins : ModRange list
        Mods : int64
        Payload : int
        Skin : int
    }
with
    static member Default(plane) =
        {
            Model = plane
            AllowedMods = ModRange.All
            AllowedPayloads = ModRange.All
            AllowedSkins = ModRange.All
            Mods = 0L
            Payload = 0
            Skin = 0
        }

type WaypointAction =
    | TakeOff
    | Land
    | Fly
    | AttackAir of Radius: float32<M> * Duration: float32<H>
    | AttackGround of Radius: float32<M>

type Waypoint =
    { Pos : OrientedPosition
      Action : WaypointAction
    }

type PlayerDirectedFlight =
    {
        Title : string
        Flight : string
        Rank : int
        Waypoints : Waypoint list
    }

type PlayerFlight =
    | Unconstrained of PlayerSpawnPlane list
    | Directed of PlayerDirectedFlight

type Campaign.Common.BasicTypes.IRunway with
    member this.Chart =
        let chart = T.Airfield.Chart.Default
        let getRelativeXZ =
            let x = Vector2.FromYOri(float this.SpawnPos.Rotation)
            let z = x.Rotate(90.0f)
            fun (v : Vector2) ->
                let v2 = v - this.SpawnPos.Pos
                float(Vector2.Dot(v2, x)), float(Vector2.Dot(v2, z))
        let mkPoint(v, t) =
            let x, z = getRelativeXZ v
            T.Airfield.Chart.Point.Default
                .SetX(T.Float.N x)
                .SetY(T.Float.N z)
                .SetType(T.Integer.N t)
        let points =
            [
                match this.PathToRunway with
                | initial :: path ->
                    yield mkPoint(initial, 0)
                    for v in path do
                        yield mkPoint(v, 1)
                | [] ->
                    ()
                yield mkPoint(this.Start, 2)
                yield mkPoint(this.End, 2)
                match List.rev this.PathOffRunway with
                | last :: rpath ->
                    yield! List.rev [
                        yield mkPoint(last, 0)
                        for v in rpath do
                            yield mkPoint(v, 1)
                    ]
                | [] ->
                    ()
            ]
        chart
            .SetPoint(points)

type IAirfieldQuery =
    abstract Airfields : IAirfield list
    abstract GetAirfield : AirfieldId -> IAirfield
    abstract GetOwner : AirfieldId -> CountryId option

type PlayerSpawn =
    {
        Airfield : AirfieldId
        RunwayName : string
        SpawnType : PlayerSpawnType
        Pos : OrientedPosition
        Flight : PlayerFlight
    }
with
    member this.BuildMCUs(store : NumericalIdentifiers.IdStore, airfields : IAirfieldQuery, windDir : Vector2) =
        let subst = Mcu.substId(store.GetIdMapper())
        let af = airfields.GetAirfield(this.Airfield)
        let coalition = airfields.GetOwner(this.Airfield)
        match coalition with
        | None ->
            // No spawning in neutral regions
            McuUtil.groupFromList []
        | Some country ->
            match this.Flight with
            | Unconstrained planes ->
                let runway =
                    try
                        af.Runways
                        |> Seq.maxBy (fun runway -> Vector2.Dot(windDir, (runway.End - runway.Start).Rotate(runway.SpawnPos.Rotation)))
                        |> Some
                    with _ -> None
                let spawn =
                    T.Airfield.Default
                        .SetIndex(T.Integer.N 1)
                        .SetLinkTrId(T.Integer.N 2)
                        .SetName(T.String.N ((this.Airfield.AirfieldName + " " + this.RunwayName).Trim()))
                        .SetReturnPlanes(T.Boolean.N true)
                        .SetRefuelFriendlies(T.Boolean.N true)
                        .SetRearmFriendlies(T.Boolean.N true)
                        .SetRepairFriendlies(T.Boolean.N true)
                        .SetMaintenanceRadius(T.Integer.N 3000)
                        .SetRefuelTime(T.Integer.N 30)
                        .SetRearmTime(T.Integer.N 30)
                        .SetRepairTime(T.Integer.N 30)
                        .SetModel(T.String.N @"graphics\airfields\fakefield.mgm")
                        .SetScript(T.String.N @"LuaScripts\WorldObjects\Airfields\fakefield.txt")
                        .SetCountry(T.Integer.N (int country.ToMcuValue))
                let spawn =
                    match runway with
                    | Some runway ->
                        spawn.SetChart(Some runway.Chart)
                    | None ->
                        spawn
                let spawn =
                    match this.SpawnType with
                    | Airborne ->
                        spawn
                            .SetXPos(T.Float.N(float this.Pos.Pos.X))
                            .SetZPos(T.Float.N(float this.Pos.Pos.Y))
                            .SetYPos(T.Float.N(float this.Pos.Altitude))
                            .SetYOri(T.Float.N(float this.Pos.Rotation))
                    | Runway ->
                        let pos, ori =
                            match runway with
                            | Some runway ->
                                runway.Start, (runway.End - runway.Start).YOri
                            | _ ->
                                this.Pos.Pos, this.Pos.Rotation
                        spawn
                            .SetXPos(T.Float.N(float pos.X))
                            .SetZPos(T.Float.N(float pos.Y))
                            .SetYOri(T.Float.N(float ori))
                    | Parking _ ->
                        let pos, ori =
                            match runway with
                            | Some runway ->
                                runway.SpawnPos.Pos, runway.SpawnPos.Rotation
                            | _ ->
                                this.Pos.Pos, this.Pos.Rotation
                        spawn
                            .SetXPos(T.Float.N(float pos.X))
                            .SetZPos(T.Float.N(float pos.Y))
                            .SetYOri(T.Float.N(float ori))
                let planes =
                    planes
                    |> List.map (fun plane ->
                        let modFilters = ModRange.ModFilters plane.AllowedMods
                        let payloadFilters = ModRange.ModFilters plane.AllowedPayloads
                        let skinFilters = ModRange.ModFilters plane.AllowedSkins
                        let afPlane = newAirfieldPlane(modFilters, payloadFilters, plane.Mods, plane.Payload, skinFilters, plane.Model.Name, -1)
                        afPlane
                            .SetAILevel(T.Integer.N 2)  // Normal
                            .SetStartInAir(T.Integer.N this.SpawnType.IntValue)
                            .SetModel(T.String.N plane.Model.ScriptModel.Model)
                            .SetScript(T.String.N plane.Model.ScriptModel.Script)
                    )
                let spawn =
                    spawn.SetPlanes(Some(T.Airfield.Planes.Default.SetPlane planes))
                let spawnEntity =
                    T.MCU_TR_Entity.Default.SetIndex(T.Integer.N 2).SetMisObjID(T.Integer.N 1).SetEnabled(T.Boolean.N true)
                let mcus =
                    [ spawn.CreateMcu(); spawnEntity.CreateMcu() ]
                for mcu in mcus do subst mcu
                McuUtil.groupFromList mcus
            | Directed _ ->
                failwith "TODO"

type GroundBattleNumbers =
    {
        NumRocketArtillery : int
        NumArtillery : int
        NumAntiTankGuns : int
        NumTanks : int
    }

type private AreaLocation =
    | DefenseBack
    | DefenseMiddle
    | AttackBack
    | AttackMiddle

type GroundBattle =
    {
        Region : RegionId
        Boundary : Vector2 list
        Pos : OrientedPosition
        Defending : CountryId
        Attacking : CountryId
        DefendingCoalition : CoalitionId
        NumDefending : GroundBattleNumbers
        NumAttacking : GroundBattleNumbers
    }
with

    member this.CreateMCUs(random : System.Random, store, lcStore, region, startTrigger) =
        // Get a random position within the bounding rectangle of the boundary
        let getRandomPos(areaLocation) =
            let dir = Vector2.FromYOri(float this.Pos.Rotation)
            let back =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, dir))
                |> Seq.min
            let front =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, dir))
                |> Seq.max
            let side = Vector2.FromYOri (float this.Pos.Rotation + 90.0)
            let left =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, side))
                |> Seq.min
            let right =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, side))
                |> Seq.max
            let r0 =
                let r = random.NextDouble() |> float32
                match areaLocation with
                | AttackBack -> 0.25f * r
                | DefenseBack -> 1.0f - 0.25f * r
                | AttackMiddle -> 0.25f * r + 0.25f
                | DefenseMiddle -> 0.75f - 0.25f * r
            let r1 = random.NextDouble() |> float32
            let dx = (back * (1.0f - r0) + front * r0) * dir
            let dz = (left * (1.0f - r1) + right * r1) * side
            this.Pos.Pos + dx + dz
        // Get a random position within the boundary
        let getRandomPos(areaLocation) =
            Seq.initInfinite (fun _ -> getRandomPos areaLocation)
            |> Seq.find (fun v -> v.IsInConvexPolygon(this.Boundary))
        // Build an attacking tank
        let buildTank (country : CountryId) (model : VehicleTypeData) =
            let tank = RespawningTank.Create(store, getRandomPos(AttackMiddle), getRandomPos(DefenseBack), country.ToMcuValue)
            let role = if country = this.Defending then "D" else "A"
            tank.Tank.Name <- sprintf "B-%s-%s-%s" region role "medium"
            model.AssignTo(tank.Tank)
            tank |> Choice1Of2
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCannon (country : CountryId) (location, model : VehicleTypeData, wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(location), getRandomPos(AttackBack), country.ToMcuValue)
            let role = if country = this.Defending then "D" else "A"
            arty.Canon.Name <- sprintf "B-%s-%s-%s" region role "medium"
            wallModel.AssignTo(arty.Wall)
            model.AssignTo(arty.Canon)
            arty |> Choice2Of2
        // Build moving and fixed tanks/guns
        let buildTanksAndSupport(country, composition : GroundBattleNumbers) =
            let attackers, support =
                let buildCannon = buildCannon country
                let buildTank = buildTank country
                let backPosition =
                    if country = this.Defending then
                        DefenseBack
                    else
                        AttackBack
                seq {
                    for _ in 1 .. composition.NumTanks do
                        if country = this.Defending then
                            match country with
                            | Russia -> (DefenseBack, vehicles.RussianMediumTank, vehicles.AntiTankPosition) |> buildCannon
                            | Italy | Germany -> (DefenseBack, vehicles.GermanMediumTank, vehicles.AntiTankPosition) |> buildCannon
                            | GreatBritain | UnitedStates -> (DefenseBack, vehicles.AmericanMediumTank, vehicles.AntiTankPosition) |> buildCannon
                        else
                            match country with
                            | Russia -> vehicles.RussianMediumTank |> buildTank
                            | Italy | Germany -> vehicles.GermanMediumTank |> buildTank
                            | GreatBritain | UnitedStates -> vehicles.AmericanMediumTank |> buildTank
                    for _ in 1 .. composition.NumAntiTankGuns do
                        match country with
                        | Russia -> (backPosition, vehicles.RussianAntiTankCanon, vehicles.AntiTankPosition) |> buildCannon
                        | Italy | Germany -> (backPosition, vehicles.GermanAntiTankCanon, vehicles.AntiTankPosition) |> buildCannon
                        | GreatBritain | UnitedStates -> (backPosition, vehicles.AmericanAntiTankCanon, vehicles.AntiTankPosition) |> buildCannon
                    for _ in 1 .. composition.NumArtillery do
                        match country with
                        | Russia -> (backPosition, vehicles.RussianArtillery, vehicles.ArtilleryPosition) |> buildCannon
                        | Italy | Germany -> (backPosition, vehicles.GermanArtillery, vehicles.ArtilleryPosition) |> buildCannon
                        | GreatBritain | UnitedStates -> (backPosition, vehicles.AmericanArtillery, vehicles.ArtilleryPosition) |> buildCannon
                    for _ in 1 .. composition.NumRocketArtillery do
                        match country with
                        | Russia -> (backPosition, vehicles.RussianRocketArtillery, vehicles.TankPosition) |> buildCannon
                        | Italy | Germany -> (backPosition, vehicles.GermanRocketArtillery, vehicles.TankPosition) |> buildCannon
                        | GreatBritain | UnitedStates -> (backPosition, vehicles.AmericanArtillery, vehicles.ArtilleryPosition) |> buildCannon
                }
                |> List.ofSeq
                |> List.partition (function Choice1Of2 _ -> true | _ -> false)
            let attackers = attackers |> List.map (function Choice1Of2 x -> x | _ -> failwith "Not a Choice1Of2")
            let support = support |> List.map (function Choice2Of2 x -> x | _ -> failwith "Not a Choice2Of2")
            attackers, support
        // Instantiate attacker blobks
        let attackers, support = buildTanksAndSupport(this.Attacking, this.NumAttacking)
        // Instantiate defender blocks
        let defenders, cannons = buildTanksAndSupport(this.Defending, this.NumDefending)

        // Icons
        let icon1 = BattleIcons.Create(store, lcStore, this.Pos.Pos, this.Pos.Rotation, this.NumDefending.NumTanks, this.NumAttacking.NumTanks, Defenders this.DefendingCoalition.ToCoalition)
        let icon2 = BattleIcons.Create(store, lcStore, this.Pos.Pos, this.Pos.Rotation, this.NumAttacking.NumTanks, this.NumDefending.NumTanks, Attackers this.DefendingCoalition.Other.ToCoalition)

        // Start
        for support in support do
            Mcu.addTargetLink startTrigger support.Start.Index
        for x in attackers do
            Mcu.addTargetLink startTrigger x.Start.Index
        for x in defenders do
            Mcu.addTargetLink startTrigger x.Start.Index
        for x in cannons do
            Mcu.addTargetLink startTrigger x.Start.Index

        // Result
        { new McuUtil.IMcuGroup with
            member x.Content = []
            member x.LcStrings = []
            member x.SubGroups = [
                for s in support do
                    yield s.All
                for a in attackers do
                    yield a.All
                for d in defenders do
                    yield d.All
                for d in cannons do
                    yield d.All
                yield icon1.All
                yield icon2.All
            ]
        }

type ConvoyMember =
    | Train
    | Truck
    | Tank
    | ArmoredCar
    | AntiAirTruck
    | StaffCar
with
    member this.VehicleData(country : CountryId) =
        match country, this with
        | (Russia | UnitedStates | GreatBritain), Train -> vehicles.RussianTrain
        | (Germany | Italy), Train -> vehicles.GermanTrain
        | Russia, Truck -> vehicles.RussianTruck
        | (UnitedStates | GreatBritain), Truck -> vehicles.AmericanTruck
        | (Germany | Italy), Truck -> vehicles.GermanTruck
        | Russia, Tank -> vehicles.RussianMediumTank
        | (UnitedStates | GreatBritain), Tank -> vehicles.AmericanMediumTank
        | (Germany | Italy), Tank -> vehicles.GermanMediumTank
        | (Russia | UnitedStates | GreatBritain), ArmoredCar -> vehicles.RussianLightArmor
        | (Germany | Italy), ArmoredCar -> vehicles.GermanLightArmor
        | Russia, AntiAirTruck -> vehicles.RussianMobileAA
        | (UnitedStates | GreatBritain), AntiAirTruck -> vehicles.AmericanMobileAA
        | (Germany | Italy), AntiAirTruck -> vehicles.GermanMobileAA
        | Russia, StaffCar -> vehicles.RussianCar
        | (UnitedStates | GreatBritain), StaffCar -> vehicles.AmericanCar
        | (Germany | Italy), StaffCar -> vehicles.GermanCar

    member this.StaticVehicleData(country : CountryId) =
        match country, this with
        | _, Train ->
            vehicles.StaticTrain
        | Russia, (Truck | AntiAirTruck) ->
            vehicles.StaticRussianTruck
        | (UnitedStates | GreatBritain), (Truck | AntiAirTruck) ->
            vehicles.StaticAmericanTruck
        | (Germany | Italy), (Truck | AntiAirTruck)->
            vehicles.StaticGermanTruck
        | Russia, Tank ->
            vehicles.RussianStaticMediumTank
        | (UnitedStates | GreatBritain), (Tank | ArmoredCar) ->
            vehicles.AmericanStaticTank
        | (Germany | Italy), Tank ->
            vehicles.GermanStaticMediumTank
        | Russia, ArmoredCar ->
            vehicles.RussianStaticLightArmor
        | (Germany | Italy), ArmoredCar ->
            vehicles.GermanStaticLightArmor
        | Russia, StaffCar ->
            vehicles.StaticRussianCar
        | (UnitedStates | GreatBritain), StaffCar ->
            vehicles.StaticAmericanCar
        | (Germany | Italy), StaffCar ->
            vehicles.StaticGermanCar

    member this.Name =
        match this with
        | Train -> "TRAIN_CAR"
        | Truck -> "TRUCK"
        | Tank -> "TANK"
        | ArmoredCar -> "ARMORED_CAR"
        | AntiAirTruck -> "AATRUCK"
        | StaffCar -> "CAR"

    static member All = [
        Train
        Truck
        Tank
        ArmoredCar
        AntiAirTruck
        StaffCar
    ]

type Convoy =
    {
        Country : CountryId
        Coalition : CoalitionId
        Members : ConvoyMember list
        Path : OrientedPosition list
        StartPositions : OrientedPosition list
    }
with
    member this.CreateMCUs(store, lcStore, columnName, startTrigger) =
        let pathVertices : Factory.PathVertex list=
            this.Path
            |> List.map (fun p ->
                {
                    Factory.Pos = p.Pos
                    Ori = p.Rotation
                    Radius = 50
                    Speed = 50
                    Priority = 1
                    SpawnSide = Types.SpawnSide.Center
                    Role = Factory.PathVertexRole.Intermediate
                }
            )
        match this.Members with
        | [Train] ->
            let train = TrainWithNotification.Create(store, lcStore, true, pathVertices, [], this.Country.ToMcuValue, this.Coalition.ToCoalition, columnName)
            let links = train.CreateLinks()
            links.Apply(McuUtil.deepContentOf train)
            Mcu.addTargetLink startTrigger train.TheTrain.Start.Index
            train :> IMcuGroup
        | _ ->
            let columnContent =
                this.Members
                |> List.map (fun x -> x.VehicleData(this.Country))
            let column = Factory.VirtualConvoy.CreateColumn(store, lcStore, pathVertices, [], columnContent, this.Country.ToMcuValue, this.Coalition.ToCoalition, columnName, 0)
            let links = column.CreateLinks()
            links.Apply(McuUtil.deepContentOf column)
            Mcu.addTargetLink startTrigger column.Api.Start.Index
            column :> IMcuGroup

type MissionGenSettings =
    {
        MaxAntiAirCannons : int
        MaxAiPatrolPlanes : int
        Planes : PlaneModel list
        OutFilename : string
    }

type IBuildingQuery =
    abstract BuildingDamages : (BuildingInstanceId * int * float32) list
    abstract GetBuildingInstance : BuildingInstanceId -> BuildingInstance
    abstract GetOwner : BuildingInstanceId -> CountryId option

/// Create the MCUs for all static blocks or bridges within a convex hull, creating entities for those that need entities (typically objectives for AI ground attackers).
let inline private mkStaticMCUs (store : NumericalIdentifiers.IdStore, buildings : IBuildingQuery, blocks, hull, hasEntity, filterDamages) =
    let blockAt =
        blocks
        |> Seq.map (fun block -> OrientedPosition.FromMission block, block)
        |> Seq.distinctBy fst
        |> Seq.mutableDict
    // Apply damages
    for (bId, part, health) in buildings.BuildingDamages |> Seq.filter filterDamages do
        let building = buildings.GetBuildingInstance(bId)
        match blockAt.TryGetValue(building.Pos) with
        | true, block ->
            let damages = CommonMethods.getDamaged block
            let damages = CommonMethods.setItem part (T.Float.N(float health)) damages
            let block = CommonMethods.setDamaged damages block
            let block = CommonMethods.setDurability (T.Integer.N(building.Properties.Durability)) block
            blockAt.[building.Pos] <- block
        | false, _ ->
            // No block at position. Maybe the mission file was edited after the campaign started. Bad, but not worth dying with an exception.
            logger.Warn(sprintf "Failed to set damage on building or bridge at %s. No building found at that position in the campaign mission file." (string building.Pos))
            ()
    // Cull everything not in the hull
    let blocks =
        blockAt
        |> Seq.filter (fun kvp -> kvp.Key.Pos.IsInConvexPolygon hull)
        |> List.ofSeq
    // Set country
    let blocks =
        blocks
        |> List.map (fun kvp ->
            let pos = kvp.Key
            let country = buildings.GetOwner(BuildingInstanceId pos)
            let block =
                match country with
                | Some country -> CommonMethods.setCountry (T.Integer.N (int country.ToMcuValue)) kvp.Value
                | None -> kvp.Value
            pos, block
        )
    // Create entities when needed, and create unique IDs
    let mcus =
        let subst = Mcu.substId <| store.GetIdMapper()
        [
            for pos, block in blocks do
                if hasEntity pos.Pos then
                    let subst = Mcu.substId <| store.GetIdMapper()
                    let mcu1 = CommonMethods.createMcu block
                    let mcu2 = newEntity (mcu1.Index + 1)
                    Mcu.connectEntity (mcu1 :?> Mcu.HasEntity) mcu2
                    for mcu in [mcu1; upcast mcu2] do
                        subst mcu
                        pos.Pos.AssignTo mcu.Pos
                        mcu.Pos.Y <- float pos.Altitude
                        mcu.Ori.Y <- float pos.Rotation
                    yield mcu1
                    yield upcast mcu2
                else
                    let mcu =
                        block
                        |> CommonMethods.setLinkTrId (T.Integer.N 0)
                        |> CommonMethods.createMcu
                    subst mcu
                    yield mcu
        ]
    // Result
    McuUtil.groupFromList mcus

/// Set the list of planes in the options of a mission file
let private addMultiplayerPlaneConfigs (planes : PlaneModel seq) (options : T.Options) =
    let configs =
        planes
        |> Seq.map (fun model -> T.String.N (model.ScriptModel.Script))
    options.SetMultiplayerPlaneConfig(List.ofSeq configs)

type IMissionBuilderData =
    inherit IRegionQuery
    inherit IBuildingQuery
    inherit IAirfieldQuery
    abstract GetCountryCoalition : CountryId -> CoalitionId
    abstract GetPlaneModel : PlaneModelId -> PlaneModel

/// Data needed to create a multiplayer "dogfight" mission
type MultiplayerMissionContent =
    {
        Date : System.DateTime
        Briefing : string
        Boundary : Vector2 list
        PlayerSpawns : PlayerSpawn list
        AntiAirNests : Nest list
        GroundBattles : GroundBattle list
        AiPatrols : AiPatrol list
        AiAttacks : AiAttack list
        /// List of chains of convoys, convoys in each chain start after the previous one reaches completion.
        Convoys : Convoy list list
        ParkedPlanes : (PlaneModelId * OrientedPosition * CountryId) list
        ParkedVehicles : (ConvoyMember * OrientedPosition * CountryId) list
    }
with
    /// Create the groups suitable for a multiplayer "dogfight" mission
    member this.BuildMission(random, settings : MissionGenSettings, scenario, date, weather, isBridge, data : IMissionBuilderData) =
        let strategyMissionData = T.GroupData.Parse(Parsing.Stream.FromFile (scenario + ".Mission"))
        let options = Seq.head strategyMissionData.ListOfOptions
        let store = NumericalIdentifiers.IdStore()
        let lcStore = NumericalIdentifiers.IdStore()
        lcStore.SetNextId 3
        let getId = store.GetIdMapper()
        let missionBegin = newMissionBegin (getId 1)

        let inTargetedArea(pos : Vector2) =
            this.AiAttacks
            |> Seq.exists(fun attack -> (attack.Target - pos).Length() < 10000.0f)

        // Mission name, briefing, author
        let optionStrings =
            { new McuUtil.IMcuGroup with
                  member x.Content = []
                  member x.LcStrings =
                    [ (0, "Dynamic online campaign " + scenario)
                      (1, this.Briefing)
                      (2, "auto-generated-coconut-campaign")
                    ]
                  member x.SubGroups = []
            }
        // Weather and player planes
        let options =
            (Campaign.Common.Weather.setOptions random weather date options)
                .SetDate(T.Date.FromDate(this.Date.Year, this.Date.Month, this.Date.Day))
                .SetTime(T.Options.Time.Create(T.Integer.N this.Date.Hour, T.Integer.N this.Date.Minute, T.Integer.N this.Date.Second))
                .SetMissionType(T.Integer.N 2) // deathmatch
                |> addMultiplayerPlaneConfigs settings.Planes

        // Static buildings and blocks
        let buildings =
            let isBuilding (bId : BuildingInstanceId, _, _) = not(isBridge bId)
            mkStaticMCUs(store, data, strategyMissionData.GetGroup("Static").ListOfBlock, this.Boundary, inTargetedArea, isBuilding)

        // Bridges
        let bridges =
            let isBridge (bId : BuildingInstanceId, _, _) = isBridge bId
            let bridges =
                [ "BridgesHW" ; "BridgesRW" ]
                |> Seq.map strategyMissionData.GetGroup
                |> Seq.collect (fun g -> g.ListOfBridge)
            mkStaticMCUs(store, data, bridges, this.Boundary, inTargetedArea, isBridge)

        // Spawns
        let spawns =
            let windDir = Vector2.FromYOri(weather.Wind.Direction)
            this.PlayerSpawns
            |> List.map (fun ps -> ps.BuildMCUs(store, data, windDir))

        // AA
        let retainedAA =
            this.AntiAirNests
            |> Campaign.MissionGen.StaticDefenseOptimization.select random settings.MaxAntiAirCannons
            |> Campaign.MissionGen.StaticDefenseOptimization.instantiateAll store lcStore random missionBegin
            |> List.map (fun grp -> grp :> IMcuGroup)

        // Ground battles
        let battles =
            this.GroundBattles
            |> List.map (fun battle -> battle.CreateMCUs(random, store, lcStore, string battle.Region, missionBegin))

        // Patrols, with per-coalition limit on simultaneous active planes
        let allPatrols =
            let groups, patrols =
                this.AiPatrols
                |> List.groupBy (fun patrol -> data.GetCountryCoalition(patrol.Country))
                |> List.fold (fun (groups, allPatrols) (coalition, patrols) ->
                    let group, patrols2 =
                        patrols
                        |> Campaign.Common.AiPlanes.AiPatrol.ToConstrainedPatrolBlocks (settings.MaxAiPatrolPlanes, store, lcStore, Vector2(1000.0f, 0.0f))
                    (group :: groups, patrols2 @ allPatrols)
                ) ([], [])
            for block in patrols do
                Mcu.addTargetLink missionBegin block.Start.Index
            groups

        // Attacks
        let allAttacks =
            let attacksByCoalition =
                this.AiAttacks
                |> List.groupBy (fun attack -> attack.Country)
                |> List.collect (fun (coalition, attacks) ->
                    attacks
                    |> List.map (fun attack -> attack.ToPatrolBlock(store, lcStore)))
            let mkAttackStarts (attacks : (_ * GroundAttack.Attacker list) list) =
                // Spawn "wing" immediately after "leader"
                for (_, blocks) in attacks do
                    for b1, b2 in Seq.pairwise blocks do
                        Mcu.addTargetLink b1.Spawned b2.Start.Index
                // Spawn pairs one minute after previous pair
                for (_, block1), (_, block2) in Seq.pairwise attacks do
                    match block1, block2 with
                    | b1 :: _, b2 :: _ ->
                        b2.StartDelay <- 60.0
                        Mcu.addTargetLink b1.Spawned b2.Start.Index
                    | _, _ ->
                        failwith "Expected at least one block in each attack list"
                // Start first pair one minute after mission start
                match attacks with
                | (_, hd :: _) :: _ ->
                    hd.StartDelay <- 60.0
                    Mcu.addTargetLink missionBegin hd.Start.Index
                | _ -> ()
            mkAttackStarts attacksByCoalition
            attacksByCoalition
            |> List.map fst

        // Convoys
        let convoys : IMcuGroup list =
            this.Convoys
            |> List.mapi(fun i convoys ->
                ((i * 100, missionBegin, None), convoys)
                ||> List.scan (fun (i, trigger, _) convoy ->
                    let mcus = convoy.CreateMCUs(store, lcStore, sprintf "convoy%02d" i, trigger)
                    let completed =
                        match mcus with
                        | :? TrainWithNotification as train ->
                            train.TheTrain.Completed
                        | :? Factory.VirtualConvoy as convoy ->
                            convoy.Api.Completed
                        | _ ->
                            trigger
                    (i + 1, completed, Some mcus)
                )
                |> List.choose (fun (_, _, x) -> x))
            |> List.concat

        // Parked planes
        let mkParkedPlane(model : PlaneModel, pos : OrientedPosition, country) =
            let modelScript = model.StaticScriptModel
            let mcus =
                let durability =
                    match model.Kind with
                    | PlaneType.Fighter -> 1000
                    | PlaneType.Attacker -> 1250
                    | PlaneType.Bomber | PlaneType.Transport -> 1500
                let block, entity = newBlockWithEntityMcu store country modelScript.Model modelScript.Script durability
                match block with
                | :? Mcu.HasEntity as block -> block.Name <- model.Name
                | _ -> ()
                entity.Enabled <- true
                [ block; upcast entity ]
            for mcu in mcus do
                pos.Pos.AssignTo mcu.Pos
                mcu.Ori.Y <- float pos.Rotation
            McuUtil.groupFromList mcus

        let parkedPlanes =
            this.ParkedPlanes
            |> List.map (fun (plane, pos, country) -> mkParkedPlane(data.GetPlaneModel(plane), pos, int country.ToMcuValue))

        // Parked vehicles
        let mkParkedVehicle(model : ConvoyMember, pos : OrientedPosition, country) =
            let modelScript = model.StaticVehicleData country
            let mcus =
                let durability =
                    match model with
                    | Tank -> 2000
                    | _ -> 1250
                let block, entity = newBlockWithEntityMcu store (int country.ToMcuValue) modelScript.Model modelScript.Script durability
                match block with
                | :? Mcu.HasEntity as block -> block.Name <- model.Name
                | _ -> ()
                entity.Enabled <- true
                [ block; upcast entity ]
            for mcu in mcus do
                pos.Pos.AssignTo mcu.Pos
                mcu.Ori.Y <- float pos.Rotation
            McuUtil.groupFromList mcus

        let parkedVehicles =
            this.ParkedVehicles
            |> List.map (fun (vehicle, pos, country) -> mkParkedVehicle(vehicle, pos, country))

        // Mission end triggered by server input
        let serverInputMissionEnd = MissionEnd.MissionEnd.Create(store)

        // Region outlines and capitals
        let borders = MapGraphics.MapIcons.CreateRegions(store, lcStore, data)

        // Result
        let allGroups =
            [
                yield optionStrings
                yield buildings
                yield bridges
                yield! spawns
                yield McuUtil.groupFromList [ missionBegin ]
                yield! retainedAA
                yield! battles
                yield! allPatrols
                yield! allAttacks
                yield! convoys
                yield! parkedPlanes
                yield! parkedVehicles
                yield serverInputMissionEnd.All
                yield upcast borders
            ]

        // Create directories in path to file, if needed
        let outDir = System.IO.Path.GetDirectoryName(settings.OutFilename)
        if not(System.IO.Directory.Exists(outDir)) then
            try
                System.IO.Directory.CreateDirectory(outDir)
                |> ignore
            with _ -> ()

        // Write file
        McuOutput.writeMissionFiles "eng" settings.OutFilename options allGroups

