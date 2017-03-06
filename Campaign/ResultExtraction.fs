module Campaign.ResultExtraction

open ploggy
open Campaign.WorldDescription
open Campaign.WorldState
open System.Numerics
open Vector

type Resupplied = {
    Region : RegionId
    Weight : float32
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
                | Some region -> yield { Region = region.RegionId; Weight = 1000.0f }
                | None -> ()
            | _ ->
                ()
    }
    |> Seq.groupBy (fun r -> r.Region)
    |> Seq.map (fun (region, amounts) -> region, amounts |> Seq.sumBy (fun sup -> sup.Weight))
    |> Seq.map (fun (region, amount) -> { Region = region; Weight = amount })

type DamagedObject =
    | Production of RegionId * int
    | Storage of RegionId * int
    | Airfield of AirfieldId * int
    | AntiAir of DefenseAreaId
    | AntiTank of DefenseAreaId
    | Vehicle of RegionId * GroundAttackVehicle
    | ParkedPlane of AirfieldId
    | Plane of AirfieldId * PlaneModel

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

let extractBuildingDamages (world : World) (state : WorldState) (entries : LogEntry seq) =
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
                match Map.tryFind damage.TargetId !idMapper with
                | Some(AirfieldObjectType buildingType) ->
                    // Damage to buildings: storage or production
                    let pos = Vector2(damage.Position.X, damage.Position.Z)
                    match tryFindContainingRegion pos with
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
                                |> Seq.map (fun (x, pos2) -> x, (pos2.Pos - pos).Length())
                                |> Seq.filter (fun (_, dist) -> dist < 100.0f)
                                |> Seq.minBy snd
                                |> Some
                            with
                            | _ -> None
                        match closest with
                        | Some(damaged, _) -> yield { Object = damaged; Data = { Amount = damage.Damage } }
                        | None -> () // No known building nearby
                    | None -> () // Outside of know regions
                | _ -> () // Ignored object type
            | _ -> () // Ignored log entry
    }
    |> Seq.groupBy (fun damage -> damage.Object)
    |> Seq.map (fun (damageObject, damages) ->
        { Object = damageObject
          Data = { Amount = damages |> Seq.sumBy (fun dam -> dam.Data.Amount) } })
