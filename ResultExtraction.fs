/// Datatypes and function related to extracting data from mission log reports.
/// This data is used to produce an updated WorldState.
module Campaign.ResultExtraction

open System.Numerics

open ploggy
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.ParaDrop
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.Util

/// Match the object type strings in log events with plane models.
let (|PlaneObjectType|_|) (s : string) =
    match s with
    | null -> None
    | s -> Some s
    |> Option.bind (fun s ->
        PlaneModel.AllModels PlaneSet.All
        |> List.tryFind (fun model -> s.ToLower().Contains(model.MissionLogName)))

/// A region shipped supplies
type SuppliesShipped = {
    OrderId : OrderId // Refers to a resupply order
}

let extractSuppliesShipped (orders : ResupplyOrder list) (entries : LogEntry seq) =
    let tryGetOrder(eventName) =
        orders
        |> Seq.tryFind (fun order -> order.MatchesMissionLogDepartureEventName(eventName))
    seq {
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    match tryGetOrder eventName with
                    | Some order ->
                        let energy = order.Convoy.TransportedSupplies
                        yield { OrderId = order.OrderId }
                    | None -> ()
                | None ->
                    ()
            | _ ->
                ()
    }

/// A tank column left its home region
type ColumnLeft = {
    OrderId : OrderId
    Vehicles : Map<GroundAttackVehicle, int>
}

let extractColumnDepartures (orders : ColumnMovement list) (entries : LogEntry seq) =
    let tryGetRank(eventName) =
        orders
        |> Seq.tryPick (fun order -> order.MatchesMissionLogDepartureEventName(eventName) |> Option.map (fun rankOffset -> order, rankOffset))
    seq {
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    match tryGetRank eventName with
                    | Some(order, rankOffset) ->
                        let vehicles =
                            try
                                order.Composition
                                |> Seq.skip (rankOffset - 1)
                            with
                            | _ -> order.Composition |> Seq.ofArray
                            |> Seq.truncate ColumnMovement.MaxColumnSize
                            |> Util.compactSeq
                        yield { OrderId = order.OrderId; Vehicles = vehicles }
                    | None -> ()
                | None ->
                    ()
            | _ ->
                ()
    }

/// Easy handling of result of extractFerryPlanes
let (|PlaneFerrySpawned|PlaneFerryLanded|PlaneFerryKilled|) =
    function
    | Choice1Of3 (x : PlaneFerryOrder) -> PlaneFerrySpawned x
    | Choice2Of3 (x : PlaneFerryOrder) -> PlaneFerryLanded x
    | Choice3Of3 (x : PlaneFerryOrder) -> PlaneFerryKilled x

/// Extract events related to ferry planes.
let extractFerryPlanes (orders : PlaneFerryOrder list) (entries : LogEntry seq) =
    [
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    let spawnedOrder =
                        orders
                        |> List.tryFind (fun order -> order.SpawnedEventName = eventName)
                    let landedOrder =
                        orders
                        |> List.tryFind (fun order -> order.LandedEventName = eventName)
                    let killedOrder =
                        orders
                        |> List.tryFind (fun order -> order.KilledEventName = eventName)
                    match spawnedOrder, landedOrder, killedOrder with
                    | Some order, _, _ ->
                        yield Choice1Of3 order
                    | None, Some order, _ ->
                        yield Choice2Of3 order
                    | None, None, Some order ->
                        yield Choice3Of3 order
                    | None, None, None ->
                        ()
                | None ->
                    ()
            | _ ->
                ()
    ]

/// Precision of successful paratropper drop.
type ParaDropPrecision = Precise | Wide

/// A paratrooper landed alive inside or near a landing zone.
type ParaDropResult = {
    Coalition : CoalitionId
    LandZone : RegionId
    Precision : ParaDropPrecision
}

let extractParaDrops (orders : ColumnMovement list) (entries : LogEntry seq) =
    let checkForParaDrop precision eventName =
        let f =
            match precision with
            | Precise -> ParaDrop.TryGetPreciseDropEventName
            | Wide -> ParaDrop.TryGetWideDropEventName
        match f eventName with
        | Some suffix ->
            match orders |> List.tryFind (fun order -> order.OrderId.AsString() = suffix) with
            | Some order ->
                Some {
                    Coalition = order.OrderId.Coalition
                    LandZone = order.Destination
                    Precision = Precise
                }
            | None ->
                None
        | None ->
            None
    [
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    match checkForParaDrop Precise eventName with
                    | Some result -> yield result
                    | None ->
                        match checkForParaDrop Wide eventName with
                        | Some result -> yield result
                        | None -> ()
                | None ->
                    ()
            | _ ->
                ()
    ]

/// A plane took off, possibly took some damage and then landed/crashed near or at an airfield
type TookOff = {
    PlaneId : int
    Airfield : AirfieldId
    Plane : PlaneModel
    Cargo : float32<E>
    BombLoad : float32<K>
}

type Landed = {
    PlaneId : int
    Airfield : AirfieldId
    Plane : PlaneModel
    Health : float32
    Cargo : float32<E>
}
with
    static member MaxDistanceFromAirfield = 5000.0f

let (|TookOff|Landed|) =
    function
    | Choice1Of2 x -> TookOff x
    | Choice2Of2 x -> Landed x

let extractTakeOffsAndLandings (world : World) (state : WorldState) (entries : LogEntry seq) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let tookOff (x : TookOff) = Choice1Of2 x
    let landed (x : Landed) = Choice2Of2 x
    [
        let planeIds = ref Map.empty
        let damages = ref Map.empty
        let cargo = ref Map.empty
        let bombLoad = ref Map.empty
        // Map object ID to starting airfield
        let ongoingFlight = ref Map.empty
        // Map human player (NickId) to vehicle id
        let playerPilot = ref Map.empty
        // Function to send back a plane to starting airfield. Used for planes still in the air when the round ends or if a player disconnects while flying.
        let sendPlaneBack vehicle af =
            match planeIds.Value.TryFind vehicle with
            | Some plane ->
                let health =
                    match Map.tryFind vehicle !damages with
                    | Some x -> max 0.0f (1.0f - x)
                    | None -> 1.0f
                let cargoAmount =
                    cargo.Value.TryFind vehicle
                    |> Option.defaultVal 0.0f<E>
                cargo := cargo.Value.Remove vehicle
                ongoingFlight := ongoingFlight.Value.Remove vehicle
                Some { PlaneId = vehicle; Airfield = af; Plane = plane; Health = health; Cargo = cargoAmount }
            | None ->
                None
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                match spawned.ObjectType with
                | PlaneObjectType model ->
                    planeIds := Map.add spawned.ObjectId model !planeIds
                    damages := Map.add spawned.ObjectId 0.0f !damages
                | _ -> ()
            | :? DamageEntry as damage ->
                let oldDamage =
                    match Map.tryFind damage.TargetId !damages with
                    | Some x -> x
                    | None -> 0.0f
                let newDamage = oldDamage + damage.Damage
                damages := Map.add damage.TargetId newDamage !damages
            | :? PlayerPlaneEntry as playerPlane ->
                playerPilot := playerPilot.Value.Add(playerPlane.NickId, playerPlane.VehicleId)
                match playerPlane.VehicleType with
                | PlaneObjectType plane when plane.Roles.Contains CargoTransporter ->
                    let modmask, payload = plane.CargoPayload
                    if playerPlane.Payload = payload then
                        cargo := Map.add playerPlane.VehicleId (plane.CargoCapacity * bombCost) cargo.Value
                | _ ->
                    ()
                match playerPlane.VehicleType with
                | PlaneObjectType model ->
                    let weight =
                        model.BombLoads
                        |> List.tryPick (fun (loadout, weight) -> if loadout = playerPlane.Payload then Some weight else None)
                        |> Option.defaultVal 0.0f<K>
                    bombLoad := Map.add playerPlane.VehicleId weight bombLoad.Value
                | _ ->
                    ()
            | :? TakeOffEntry as takeOff ->
                let pos = Vector2(takeOff.Position.X, takeOff.Position.Z)
                let af = world.GetClosestAirfield(pos)
                match Map.tryFind takeOff.VehicleId !planeIds with
                | Some plane ->
                    let cargo =
                        cargo.Value.TryFind takeOff.VehicleId
                        |> Option.defaultVal 0.0f<E>
                    let bombLoad =
                        bombLoad.Value.TryFind takeOff.VehicleId
                        |> Option.defaultVal 0.0f<K>
                    ongoingFlight := ongoingFlight.Value.Add(takeOff.VehicleId, af.AirfieldId)
                    yield tookOff { PlaneId = takeOff.VehicleId; Airfield = af.AirfieldId; Plane = plane; Cargo = cargo; BombLoad = bombLoad }
                | None ->
                    ()
            | :? LandingEntry as landing ->
                let pos = Vector2(landing.Position.X, landing.Position.Z)
                let af = world.GetClosestAirfield(pos)
                let dist = (af.Pos - pos).Length()
                // Close the flight
                ongoingFlight := ongoingFlight.Value.Remove(landing.VehicleId)
                // Handle landing event (repair plane, register landing)
                if dist < Landed.MaxDistanceFromAirfield then
                    match Map.tryFind landing.VehicleId !planeIds with
                    | Some plane ->
                        let health =
                            match Map.tryFind landing.VehicleId !damages with
                            | Some x -> max 0.0f (1.0f - x)
                            | None -> 1.0f
                        let repairedHealth =
                            // Lightly damaged planes are repaired to full health.
                            // The acceptable level of damage depends on who controls the airfield...
                            let repairable =
                                match plane.Coalition, sg.GetRegion(af.Region).Owner with
                                | coalition, Some coalition2 when coalition = coalition2 -> 0.75f // Own airfield
                                | captured, Some capturing -> 0.9f // Enemy airfield, fewer repair options available
                                | _, None -> 1.0f // Neutral airfield, no repairs
                            if health > repairable then
                                1.0f
                            else
                                health
                        let cargoAmount =
                            cargo.Value.TryFind landing.VehicleId
                            |> Option.defaultVal 0.0f<E>
                        cargo := cargo.Value.Remove landing.VehicleId
                        yield landed { PlaneId = landing.VehicleId; Airfield = af.AirfieldId; Plane = plane; Health = health; Cargo = cargoAmount }
                    | None -> ()
            | :? RoundEndEntry as roundEnd ->
                // register all ongoing flights as landed back at starting airfield
                for vehicle, af in ongoingFlight.Value |> Map.toSeq do
                    match planeIds.Value.TryFind vehicle with
                    | Some plane ->
                        match sendPlaneBack vehicle af with
                        | Some landing -> yield landed landing
                        | None -> ()
                    | None ->
                        ()
            | :? LeaveEntry as left ->
                // register an in-flight disconnection as landed back at starting airfield
                match playerPilot.Value.TryFind left.NickId with
                | Some vehicle ->
                    match ongoingFlight.Value.TryFind vehicle with
                    | Some af ->
                        match sendPlaneBack vehicle af with
                        | Some landing -> yield landed landing
                        | None -> ()
                    | None ->
                        ()
                | None ->
                    ()
                playerPilot := playerPilot.Value.Remove left.NickId
            | _ -> ()
    ]

/// <summary>
/// Remove landings of planes that never took off.
/// Used to avoid adding planes from AI flights that returned to base.
/// </summary>
let filterNoTakeOff (takeOffs : TookOff list) (landings : Landed list) =
    let tookOff =
        takeOffs
        |> Seq.map (fun ev -> ev.PlaneId)
        |> Set.ofSeq
    landings
    |> List.filter (fun ev -> tookOff.Contains ev.PlaneId)

type VehicleInColumn = {
    OrderId : OrderId
    Rank : int
}

/// Something got bombed or strafed
type DamagedObject =
    | Production of RegionId * int
    | Storage of RegionId * int
    | Airfield of AirfieldId * int
    | Cannon of DefenseAreaId
    | HeavyMachineGun of DefenseAreaId
    | LightMachineGun of DefenseAreaId
    | Convoy of VehicleInColumn
    | Column of VehicleInColumn
    | Vehicle of RegionId * GroundAttackVehicle
    | ParkedPlane of AirfieldId * PlaneModel

type CommonDamageData = {
    Amount : float32
}

type Damage = {
    Object : DamagedObject
    Data : CommonDamageData
}

let (|BuildingObjectType|_|) (s : string) =
    let low = s.ToLower()
    [ "arf_"; "industrial_"; "vl_" ]
    |> List.exists (fun prefix -> low.StartsWith(prefix))
    |> function true -> Some s | false -> None

let (|StaticPlaneType|_|) (planeSet : PlaneSet) (s : string) =
    PlaneModel.AllModels planeSet
    |> List.tryPick(fun model ->
        if s.Contains(model.StaticScriptModel.ShortName) then
            Some model
        else
            None
    )

let (|StaticVehicleType|_|) (s : string) =
    let s = s.ToLowerInvariant()
    [
        ("t34", GroundAttackVehicle.HeavyTank)
        ("pz_iii", GroundAttackVehicle.HeavyTank)
        ("t70", GroundAttackVehicle.MediumTank)
        ("pz_iv", GroundAttackVehicle.MediumTank)
        ("bt-7m", GroundAttackVehicle.LightArmor)
        ("sdkfz251", GroundAttackVehicle.LightArmor)
    ]
    |> List.tryPick (fun (subs, model) ->
        if s.Contains("static_") && s.Contains(subs) then
            Some model
        else
            None)

let extractStaticDamages (world : World) (entries : LogEntry seq) =
    let wg = WorldFastAccess.Create(world)
    let tryFindContainingRegion (pos : Vector2) =
        world.Regions
        |> List.tryFind(fun r ->
            pos.IsInConvexPolygon(r.Boundary))
    seq {
        let idMapper = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idMapper := Map.add spawned.ObjectId (spawned.ObjectType, spawned.ObjectName, spawned.SubGroup) !idMapper
            | :? DamageEntry as damage ->
                let damagePos = Vector2(damage.Position.X, damage.Position.Z)
                match Map.tryFind damage.TargetId !idMapper with
                | Some(BuildingObjectType buildingType, _, subGroup) ->
                    // Damage to buildings: storage or production
                    match tryFindContainingRegion damagePos with
                    | Some region ->
                        let airfields =
                            world.Airfields
                            |> List.filter (fun af -> af.Pos.IsInConvexPolygon(region.Boundary))
                        let matchingAirfieldBuildings =
                            airfields
                            |> List.map (fun af ->
                                af.Storage
                                |> List.mapi (fun i sto -> Airfield(af.AirfieldId, i), sto))
                            |> List.concat
                        let matchingStorageBuildings =
                            region.Storage
                            |> List.mapi (fun i sto -> Storage(region.RegionId, i), sto)
                        let matchingProductionBuildings =
                            region.Production
                            |> List.mapi (fun i pro -> Production(region.RegionId, i), pro)
                        let closest =
                            try
                                matchingAirfieldBuildings @ matchingProductionBuildings @ matchingStorageBuildings
                                |> Seq.map (fun (x, building) -> x, (building.Pos.Pos - damagePos).Length(), building)
                                |> Seq.filter (fun (_, dist, _) -> dist < 100.0f)
                                |> Seq.minBy (fun (_, dist, _) -> dist)
                                |> Some
                            with
                            | _ -> None
                        match closest with
                        | Some(damaged, _, building) ->
                            let significantSubBlocks = building.SubBlocks
                            if  List.contains subGroup significantSubBlocks then
                                let damageAmount =
                                    damage.Damage / float32 (List.length significantSubBlocks)
                                yield { Object = damaged; Data = { Amount = damage.Damage } }
                        | None -> () // No known building nearby
                    | None -> () // Outside of know regions
                | Some(StaticPlaneType world.PlaneSet planeModel, _, _) ->
                    let closestAirfield =
                        world.Airfields
                        |> List.minBy (fun af -> (af.Pos - damagePos).LengthSquared())
                    let distance = (closestAirfield.Pos - damagePos).Length()
                    if distance < 3000.0f then
                        yield { Object = ParkedPlane(closestAirfield.AirfieldId, planeModel); Data = { Amount = damage.Damage } }
                | Some(StaticVehicleType vehicleModel, _, _) ->
                    match tryFindContainingRegion damagePos with
                    | Some region ->
                        yield { Object = Vehicle(region.RegionId, vehicleModel); Data = { Amount = damage.Damage } }
                    | None ->
                        ()
                | Some(_, (CannonObjectName as gunType), _) | Some (_, (HeavyMachineGunAAName as gunType), _) | Some (_, (LightMachineGunAAName as gunType), _) ->
                    let defenseArea =
                        world.AntiAirDefenses @ world.AntiTankDefenses
                        |> List.tryFind (fun area -> damagePos.IsInConvexPolygon(area.Boundary))
                    match defenseArea with
                    | Some area ->
                        let objType =
                            match gunType with
                            | CannonObjectName -> Some(Cannon(area.DefenseAreaId))
                            | LightMachineGunAAName -> Some(LightMachineGun(area.DefenseAreaId))
                            | HeavyMachineGunAAName -> Some(HeavyMachineGun(area.DefenseAreaId))
                            | _ ->
                                None
                        match objType with
                        | Some objType ->
                            yield { Object = objType; Data = { Amount = damage.Damage } }
                        | None ->
                            ()
                    | None ->
                        ()
                | _ -> () // Ignored object type
            | _ -> () // Ignored log entry
    }
    |> Seq.groupBy (fun damage -> damage.Object)
    |> Seq.map (fun (damageObject, damages) ->
        { Object = damageObject
          Data = { Amount = damages |> Seq.sumBy (fun dam -> dam.Data.Amount) } })

let extractVehicleDamages (tanks : ColumnMovement list) (convoys : ResupplyOrder list) (entries : LogEntry seq) =
    seq {
        let idMapper = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idMapper := Map.add spawned.ObjectId spawned.ObjectName !idMapper
            | :? KillEntry as kill ->
                match Map.tryFind kill.TargetId !idMapper with
                | Some name ->
                    let tankDamage =
                        tanks
                        |> List.tryPick (fun order ->
                            order.MatchesMissionLogVehicleKilledEventName(name)
                            |> Option.map (fun rank -> order, rank))
                    let truckDamage =
                        lazy
                            convoys
                            |> List.tryPick (fun order ->
                                order.MatchesMissionLogVehicleKilledEventName(name)
                                |> Option.map (fun rank -> order, rank))
                    match tankDamage with
                    | Some(order, rank) -> yield { Object = Column { OrderId = order.OrderId; Rank = rank }; Data = { Amount = 1.0f } }
                    | None ->
                        match truckDamage.Value with
                        | Some(order, rank) -> yield { Object = Convoy { OrderId = order.OrderId; Rank = rank }; Data = { Amount = 1.0f } }
                        | None -> ()
                | None ->
                    ()
            | _ -> ()
    }
    |> Seq.groupBy (fun damage -> damage.Object)
    |> Seq.map (fun (damageObject, damages) ->
        { Object = damageObject
          Data = { Amount = damages |> Seq.sumBy (fun dam -> dam.Data.Amount) } })
