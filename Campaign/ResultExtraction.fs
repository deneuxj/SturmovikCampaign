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

/// Match the object type strings in log events with plane models.
let (|PlaneObjectType|_|) (s : string) =
    match s.ToLower() with
    | "i-16 type 24" -> Some PlaneModel.I16
    | "il-2 mod.1941" -> Some PlaneModel.IL2M41
    | "ju 52 3mg4e" -> Some PlaneModel.Ju52
    | "mc.202 ser.viii" -> Some PlaneModel.Mc202
    | "p-40e-1" -> Some PlaneModel.P40
    | "pe-2 ser.35" -> Some PlaneModel.Pe2s35
    | "bf 109 e-7" -> Some PlaneModel.Bf109e7
    | "bf 109 f-2" -> Some PlaneModel.Bf109f2
    | "mig-3 ser.24" -> Some PlaneModel.Mig3
    | "bf 110 e-2" -> Some PlaneModel.Bf110e
    | "ju 88 a-4" -> Some PlaneModel.Ju88a4
    | _ -> None

/// A region received truck convoys or trains.
type Resupplied = {
    Region : RegionId
    Energy : float32<E>
}

let extractResupplies (world : World) (state : WorldState) (entries : LogEntry seq) =
    let wg = WorldFastAccess.Create(world)
    let sg = WorldStateFastAccess.Create(state)
    let tryFindContainingRegion (pos : Vector2) =
        world.Regions
        |> List.tryFind(fun r ->
            pos.IsInConvexPolygon(r.Boundary))
    seq {
        for entry in entries do
            match entry with
            | :? MissionObjectiveEntry as objective ->
                let pos = Vector2(objective.Position.X, objective.Position.Z)
                match tryFindContainingRegion pos with
                | Some region ->
                    let energy =
                        match objective.IconType with
                        | x when x = VirtualConvoy.CoverTrain -> ResupplyOrder.TrainCapacity
                        | x when x = VirtualConvoy.CoverTransportColumn -> ResupplyOrder.TruckCapacity
                        | _ -> 0.0f<E>
                    yield (region.RegionId, objective.Coalition), { Region = region.RegionId; Energy = energy }
                | None -> ()
            | _ ->
                ()
    }
    |> Seq.groupBy fst
    |> Seq.map (fun (k, amounts) -> k, amounts |> Seq.sumBy (fun (_, sup) -> sup.Energy))
    |> Seq.map (fun ((region, _), amount) -> { Region = region; Energy = amount })

/// A plane took off, possibly took some damage and then landed/crashed near or at an airfield
type TookOff = {
    Airfield : AirfieldId
    Plane : PlaneModel
}

type Landed = {
    Airfield : AirfieldId
    Plane : PlaneModel
    Health : float32
}
with
    static member MaxDistanceFromAirfield = 5000.0f

let extractTakeOffs (world : World) (entries : LogEntry seq) =
    seq {
        let planeIds = ref Map.empty
        let damages = ref Map.empty
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
            | :? TakeOffEntry as takeOff ->
                let pos = Vector2(takeOff.Position.X, takeOff.Position.Z)
                let af = world.GetClosestAirfield(pos)
                match Map.tryFind takeOff.VehicleId !planeIds with
                | Some plane -> yield Choice1Of2({ Airfield = af.AirfieldId; Plane = plane } : TookOff)
                | None -> ()
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
                        yield Choice2Of2({ Airfield = af.AirfieldId; Plane = plane; Health = health })
                    | None -> ()
            | _ -> ()
    }

/// Something got bombed or strafed
type DamagedObject =
    | Production of RegionId * int
    | Storage of RegionId * int
    | Airfield of AirfieldId * int
    | AntiAir of DefenseAreaId
    | AntiTank of DefenseAreaId
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

let (|StaticPlaneType|_|) (s : string) =
    [
        ("bf109_e7", PlaneModel.Bf109e7)
        ("bf109_net", PlaneModel.Mc202) // No static model for the mc.202
        ("bf109", PlaneModel.Bf109f2)
        ("bf110", PlaneModel.Bf110e)
        ("ju88", PlaneModel.Ju88a4)
        ("ju52", PlaneModel.Ju52)
        ("i16", PlaneModel.I16)
        ("mig3", PlaneModel.Mig3)
        ("mig3_net", PlaneModel.P40) // No static model for the P40
        ("il2", PlaneModel.IL2M41)
        ("pe2", PlaneModel.Pe2s35)
    ]
    |> List.tryPick (fun (subs, model) ->
        if s.Contains(subs) then
            Some model
        else
            None)


let extractStaticDamages (world : World) (state : WorldState) (entries : LogEntry seq) =
    let wg = WorldFastAccess.Create(world)
    let sg = WorldStateFastAccess.Create(state)
    let tryFindContainingRegion (pos : Vector2) =
        world.Regions
        |> List.tryFind(fun r ->
            pos.IsInConvexPolygon(r.Boundary))
    seq {
        let idMapper = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idMapper := Map.add spawned.ObjectId spawned.ObjectType !idMapper
            | :? DamageEntry as damage ->
                let damagePos = Vector2(damage.Position.X, damage.Position.Z)
                match Map.tryFind damage.TargetId !idMapper with
                | Some(IndustrialObjectType buildingType)
                | Some(AirfieldObjectType buildingType) ->
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
                                |> List.mapi (fun i sto -> Airfield(af.AirfieldId, i), sto.Pos))
                            |> List.concat
                        let matchingStorageBuildings =
                            region.Storage
                            |> List.mapi (fun i sto -> Storage(region.RegionId, i), sto.Pos)
                        let matchingProductionBuildings =
                            region.Production
                            |> List.mapi (fun i pro -> Production(region.RegionId, i), pro.Pos)
                        let closest =
                            try
                                matchingAirfieldBuildings @ matchingProductionBuildings @ matchingStorageBuildings
                                |> Seq.map (fun (x, pos2) -> x, (pos2.Pos - damagePos).Length())
                                |> Seq.filter (fun (_, dist) -> dist < 100.0f)
                                |> Seq.minBy snd
                                |> Some
                            with
                            | _ -> None
                        match closest with
                        | Some(damaged, _) -> yield { Object = damaged; Data = { Amount = damage.Damage } }
                        | None -> () // No known building nearby
                    | None -> () // Outside of know regions
                | Some(StaticPlaneType planeModel) ->
                    let closestAirfield =
                        world.Airfields
                        |> List.minBy (fun af -> (af.Pos - damagePos).LengthSquared())
                    let distance = (closestAirfield.Pos - damagePos).Length()
                    if distance < 3000.0f then
                        yield { Object = ParkedPlane(closestAirfield.AirfieldId, planeModel); Data = { Amount = damage.Damage } }
                | _ -> () // Ignored object type
            | _ -> () // Ignored log entry
    }
    |> Seq.groupBy (fun damage -> damage.Object)
    |> Seq.map (fun (damageObject, damages) ->
        { Object = damageObject
          Data = { Amount = damages |> Seq.sumBy (fun dam -> dam.Data.Amount) } })
