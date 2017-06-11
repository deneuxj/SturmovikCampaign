/// Datatypes and function related to extracting data from mission log reports.
/// This data is used to produce an updated WorldState.
module Campaign.ResultExtraction

open System.Numerics

open ploggy
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.Blocks.VirtualConvoy.Factory
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
        match s.ToLower() with
        | "i-16 type 24" -> Some PlaneModel.I16
        | "il-2 mod.1941" -> Some PlaneModel.IL2M41
        | "il-2 mod.1942" -> Some PlaneModel.IL2M42
        | "il-2 mod.1943" -> Some PlaneModel.IL2M43
        | "ju 52 3mg4e" -> Some PlaneModel.Ju52
        | "mc.202 ser.viii" -> Some PlaneModel.Mc202
        | "p-40e-1" -> Some PlaneModel.P40
        | "pe-2 ser.35" -> Some PlaneModel.Pe2s35
        | "pe-2 ser.87" -> Some PlaneModel.Pe2s87
        | "bf 109 e-7" -> Some PlaneModel.Bf109e7
        | "bf 109 f-2" -> Some PlaneModel.Bf109f2
        | "bf 109 f-4" -> Some PlaneModel.Bf109f4
        | "bf 109 g-2" -> Some PlaneModel.Bf109g2
        | "bf 109 g-4" -> Some PlaneModel.Bf109g4
        | "mig-3 ser.24" -> Some PlaneModel.Mig3
        | "bf 110 e-2" -> Some PlaneModel.Bf110e
        | "bf 110 g-2" -> Some PlaneModel.Bf110g
        | "ju 88 a-4" -> Some PlaneModel.Ju88a4
        | "ju 87 d-3" -> Some PlaneModel.Ju87
        | "he 111 h-6" -> Some PlaneModel.He111h6
        | "he 111 h-16" -> Some PlaneModel.He111h16
        | "yak-1 ser.69" -> Some PlaneModel.Yak1s69
        | "yak-1 ser.127" -> Some PlaneModel.Yak1s127
        | "la-5 ser.8" -> Some PlaneModel.La5
        | "lagg-3 ser.29" -> Some PlaneModel.Lagg3s29
        | _ -> None)

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
                match playerPlane.VehicleType with
                | PlaneObjectType PlaneModel.Ju52 ->
                    if playerPlane.Payload = 0 then
                        cargo := Map.add playerPlane.VehicleId (2300.0f<K> * bombCost) cargo.Value
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
                    yield tookOff { PlaneId = takeOff.VehicleId; Airfield = af.AirfieldId; Plane = plane; Cargo = cargo; BombLoad = bombLoad }
                | None ->
                    ()
            | :? LandingEntry as landing ->
                let pos = Vector2(landing.Position.X, landing.Position.Z)
                let af = world.GetClosestAirfield(pos)
                let dist = (af.Pos - pos).Length()
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
                        let cargo =
                            cargo.Value.TryFind landing.VehicleId
                            |> Option.defaultVal 0.0f<E>
                        yield landed { PlaneId = landing.VehicleId; Airfield = af.AirfieldId; Plane = plane; Health = health; Cargo = cargo }
                    | None -> ()
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
    | Canon of DefenseAreaId
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

let (|AirfieldObjectType|_|) (s : string) =
    if s.StartsWith "arf_" || s.StartsWith "Arf_" then
        Some s
    else
        None

let (|IndustrialObjectType|_|) (s : string) =
    if s.StartsWith "industrial_" then
        Some s
    else
        None

let (|StaticObjectType|_|) (s : string) =
    if s.StartsWith "static_" then
        Some s
    else
        None

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
                | Some(IndustrialObjectType buildingType, _, subGroup)
                | Some(AirfieldObjectType buildingType, _, subGroup) ->
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
                | Some(_, "CANON", _) ->
                    let defenseArea =
                        world.AntiAirDefenses @ world.AntiTankDefenses
                        |> List.tryFind (fun area -> damagePos.IsInConvexPolygon(area.Boundary))
                    match defenseArea with
                    | Some area ->
                        yield { Object = Canon(area.DefenseAreaId); Data = { Amount = damage.Damage } }
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
