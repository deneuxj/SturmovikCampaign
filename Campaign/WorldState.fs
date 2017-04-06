/// The state of the world: Amount of supplies, number of units, who controls each region.
module Campaign.WorldState

open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.Vehicles

open Campaign.WorldDescription
open Campaign.Util

/// Types of ground attack vehicles.
type GroundAttackVehicle =
    | HeavyTank
    | MediumTank
    | LightArmor
with
    member this.GetModel(coalition) =
        match coalition with
        | Axis ->
            match this with
            | HeavyTank -> germanHeavyTank
            | MediumTank -> germanMediumTank
            | LightArmor -> germanLightArmor
        | Allies ->
            match this with
            | HeavyTank -> russianHeavyTank
            | MediumTank -> russianMediumTank
            | LightArmor -> russianLightArmor

    static member LightArmorCost = 600.0f<E>
    static member MediumTankCost = GroundAttackVehicle.LightArmorCost * 2.0f
    static member HeavyTankCost = GroundAttackVehicle.LightArmorCost * 5.0f

    member this.Cost =
        match this with
        | HeavyTank -> GroundAttackVehicle.HeavyTankCost
        | MediumTank -> GroundAttackVehicle.MediumTankCost
        | LightArmor -> GroundAttackVehicle.LightArmorCost

    static member AllVehicles = [ HeavyTank; MediumTank; LightArmor ]

/// How much various production goals have accumulated.
type ProductionAssignment = {
    Supplies : float32<E>
    Planes : Map<PlaneModel, float32<E>>
    Vehicles : Map<GroundAttackVehicle, float32<E>>
}

/// State of a region.
type RegionState = {
    RegionId : RegionId
    Owner : CoalitionId option
    StorageHealth : float32 list
    ProductionHealth : float32 list
    Products : ProductionAssignment
    Supplies : float32<E>
    NumVehicles : Map<GroundAttackVehicle, int>
}
with
    member this.GetNumVehicles(vehicle : GroundAttackVehicle) =
        this.NumVehicles
        |> Map.tryFind vehicle
        |> Option.defaultVal 0

    member this.StorageCapacity(region : WorldDescription.Region) =
        List.zip region.Storage this.StorageHealth
        |> List.sumBy (fun (sto, health) -> health * sto.Storage)

    member this.ProductionCapacity(region : WorldDescription.Region) =
        List.zip region.Production this.ProductionHealth
        |> List.sumBy (fun (prod, health) -> health * prod.Production)

/// State of a defense area within a region.
type DefenseAreaState = {
    DefenseAreaId : DefenseAreaId
    NumUnits : int
}

/// State of an airfield.
type AirfieldState = {
    AirfieldId : AirfieldId
    NumPlanes : Map<PlaneModel, float32> // float because we are talking airplane damages into account. Two half-damaged planes make one usable one.
    StorageHealth : float32 list
    Supplies : float32<E>
}
with
    member this.StorageCapacity(af : WorldDescription.Airfield) =
        List.zip af.Storage this.StorageHealth
        |> List.sumBy (fun (sto, health) -> health * sto.Storage)

    /// Get total value of all planes at this airfield.
    member this.TotalPlaneValue =
        this.NumPlanes
        |> Map.map (fun plane qty -> plane.Cost * qty)
        |> Map.toSeq
        |> Seq.sumBy snd

/// Packages all state data.
type WorldState = {
    Regions : RegionState list
    AntiAirDefenses : DefenseAreaState list
    AntiTankDefenses : DefenseAreaState list
    Airfields : AirfieldState list
    Date : System.DateTime
}

/// Provide fast access to state data using indexes.
type WorldStateFastAccess = {
    GetRegion : RegionId -> RegionState
    GetAntiAirDefenses : DefenseAreaId -> DefenseAreaState
    GetAntiTankDefenses : DefenseAreaId -> DefenseAreaState
    GetAirfield : AirfieldId -> AirfieldState
}
with
    static member Create(state : WorldState) =
        { GetRegion = mkGetStuffFast state.Regions (fun r -> r.RegionId)
          GetAntiAirDefenses = mkGetStuffFast state.AntiAirDefenses (fun area -> area.DefenseAreaId)
          GetAntiTankDefenses = mkGetStuffFast state.AntiTankDefenses (fun area -> area.DefenseAreaId)
          GetAirfield = mkGetStuffFast state.Airfields (fun af -> af.AirfieldId)
        }

type WorldState
with
    member this.FastAccess = WorldStateFastAccess.Create(this)

    member this.TotalPlaneValueOfCoalition(world : World, coalition : CoalitionId) =
        let sg = this.FastAccess
        List.zip world.Airfields this.Airfields
        |> Seq.filter (fun (af, _) -> sg.GetRegion(af.Region).Owner = Some coalition)
        |> Seq.sumBy (fun (_, afs) -> afs.TotalPlaneValue)

open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Mcu
open System.Numerics
open Vector

/// Maximum number of anti-air canons in an area. Depends on the area's size.
let getAntiAirCanonsForArea (area : DefenseArea) =
    let refArea = 500000.0f
    let area = Vector2.ConvexPolygonArea(area.Boundary)
    max (5.0f * area / refArea) 2.0f
    |> ceil
    |> int

/// Maximum number of anti-tank canons in an area. Depends on the area's size.
let getAntiTankCanonsForArea (area : DefenseArea) =
    let refArea = 130.0e3f
    let area = Vector2.ConvexPolygonArea(area.Boundary)
    max (5.0f * area / refArea) 2.0f
    |> ceil
    |> int

/// <summary>
/// Compute the number of regions from any region to its nearest reachable source region.
/// </summary>
/// <param name="requirePathsInOwnedRegions">If true, regions under the control of different coalitions are never considered to be connected.</param>
/// <param name="getPaths">Get paths, if any, between any two regions</param>
/// <param name="getOwner">Get the owner of a region</param>
/// <param name="isSource">Indicate which regions are the source ones, i.e. regions with distance 0</param>
/// <param name="world">Contains definitions of regions</param>
let computeDistance requirePathsInOwnedRegions (getPaths : World -> Path list) (getOwner : RegionId -> CoalitionId option) (isSource : RegionId -> bool) (world : World) =
    let areConnectedByPath(start, destination) =
        getPaths world
        |> List.exists (fun path ->
            path.StartId = start && path.EndId = destination || path.StartId = destination && path.EndId = start
        )
    let filterInOwnedRegions =
        if requirePathsInOwnedRegions then
            fun coalition ->
                match coalition with
                | Some coalition ->
                    Seq.filter (fun ngh -> Some coalition = getOwner ngh)
                | None ->
                    fun _ -> Seq.empty
        else
            fun _ ->
                id
    let wg = WorldFastAccess.Create(world)
    let rec work (distances : Map<RegionId, int>) (working : RegionId list) =
        match working with
        | [] -> distances
        | current :: rest ->
            let distance = distances.[current]
            let region = wg.GetRegion current
            let nghs =
                region.Neighbours
                |> filterInOwnedRegions (getOwner region.RegionId)
                |> Seq.filter (fun ngh -> areConnectedByPath(region.RegionId, ngh))
                |> List.ofSeq
            let distances, working =
                nghs
                |> Seq.fold (fun (distances, working) ngh ->
                    match Map.tryFind ngh distances with
                    | Some oldDist when oldDist > distance + 1 ->
                        (Map.add ngh (distance + 1) distances, ngh :: working)
                    | Some _ -> (distances, working)
                    | None ->
                        (Map.add ngh (distance + 1) distances, ngh :: working)
                ) (distances, rest)
            work distances working
    let sources =
        world.Regions
        |> Seq.filter (fun r -> isSource r.RegionId)
        |> Seq.map (fun region -> region.RegionId)
        |> List.ofSeq
    let distances0 =
        sources
        |> Seq.map (fun region -> region, 0)
        |> Map.ofSeq
    let distances = work distances0 sources
    distances

/// Compute the number of regions from a region to the nearest region with factories.
let computeDistanceFromFactories requirePathsInOwnedRegions getPaths getOwner (world : World) (coalition : CoalitionId) =
    computeDistance requirePathsInOwnedRegions getPaths getOwner (
        fun regionId ->
            getOwner regionId = Some coalition &&
            world.Regions
            |> List.exists (fun r ->
                r.RegionId = regionId &&
                not(List.isEmpty r.Production))) world

/// <summary>
/// Set the number of units in the state of a defense area, not taking into account supplies.
/// </summary>
/// <param name="getOwner">return the owner of a region</param>
/// <param name="inFrontLine">indicate whether a ground defense area needs to be equipped with anti-tank canons</param>
/// <param name="baseNumUnits">get the number of units that will fit in a certain area</param>
/// <param name="area">the description of the defense area</param>
let setNumUnitsAsIfFullySupplied (getOwner : RegionId -> CoalitionId Option) inFrontLine (baseNumUnits : DefenseArea -> int) (area : DefenseArea, areaState : DefenseAreaState) =
    let owner = getOwner area.Home.Home
    let numUnits =
        match owner with
        | None -> 0
        | Some _ ->
            match area.Home with
            | Central _ -> baseNumUnits area
            | FrontLine(home, other) ->
                if inFrontLine(home, other) then
                    baseNumUnits area
                else
                    0
    { areaState with
        NumUnits = numUnits
    }

let getNumCanonsPerRegion (areas : (DefenseArea * DefenseAreaState) seq) =
    areas
    |> Seq.groupBy (fun (desc, state) -> desc.Home.Home)
    |> Seq.map (fun (region, areas) -> region, areas |> Seq.map snd |> Seq.sumBy (fun area -> area.NumUnits))

/// Compute set of pairs of regions that are neighbours and are controlled by different coalitions.
let computeFrontLine (includeNeutral : bool) (world : World) (regions : RegionState list) =
    let wg = WorldFastAccess.Create world
    let stateOfRegion =
        let m =
            regions
            |> List.map(fun state -> state.RegionId, state)
            |> dict
        fun x -> m.[x]
    let filter owner =
        if includeNeutral then
            fun region -> region.Owner <> Some owner
        else
            fun region -> region.Owner.IsSome && region.Owner.Value <> owner
    seq {
        for region in regions do
            match region.Owner with
            | None -> ()
            | Some owner ->
                let neighbours = (wg.GetRegion region.RegionId).Neighbours
                let enemies =
                    neighbours
                    |> List.map stateOfRegion
                    |> List.filter (filter owner)
                for enemy in enemies do
                    yield (region.RegionId, enemy.RegionId)
    }
    |> Set.ofSeq

/// Set number of canons in each defense area
let updateNumCanons (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let frontLine = computeFrontLine false world state.Regions
    let updateArea getCanonsForArea =
        setNumUnitsAsIfFullySupplied
            (fun region -> sg.GetRegion(region).Owner)
            (fun x -> Set.contains x frontLine)
            getCanonsForArea
    // Number of anti-air canons in each anti-air defense area, assuming unlimited supplies
    let antiAir =
        List.zip world.AntiAirDefenses state.AntiAirDefenses
        |> List.map (updateArea getAntiAirCanonsForArea)
    // Number of anti-tank canons in each anti-tank defense area, assuming unlimited supplies
    let antiTank =
        List.zip world.AntiTankDefenses state.AntiTankDefenses
        |> List.map (updateArea getAntiTankCanonsForArea)
    // Total amount of anti-air and anti-tank canons in each region
    let canonsPerRegion =
        let m1 =
            Seq.zip world.AntiAirDefenses antiAir
            |> getNumCanonsPerRegion
            |> dict
        let m2 =
            Seq.zip world.AntiTankDefenses antiTank
            |> getNumCanonsPerRegion
            |> dict
        fun x -> m1.[x] + m2.[x]
    // Correct number of defensive units depending on actual shell count.
    let adjustNumCanons (area : DefenseArea, state : DefenseAreaState) =
        let region = area.Home.Home
        let totalUnits =
            canonsPerRegion(region)
        let supportedUnits =
            int(ceil(sg.GetRegion(region).Supplies / canonCost))
        if totalUnits > supportedUnits then
            let factor = float32 supportedUnits / float32 totalUnits
            { state with
                NumUnits = int(ceil(factor * float32 state.NumUnits))
            }
        else
            state
    { state with
        AntiTankDefenses = List.zip world.AntiTankDefenses antiTank |> List.map adjustNumCanons
        AntiAirDefenses = List.zip world.AntiAirDefenses antiAir |> List.map adjustNumCanons
    }

/// Build the initial state
let mkInitialState(world : World, strategyFile : string) =
    let data = T.GroupData(Stream.FromFile strategyFile)
    
    let wg = WorldFastAccess.Create(world)
    
    let getOwner =
        let ownedByRussia =
            data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea
            |> Seq.filter (fun region -> region.GetCountry().Value = int CountryValue.Russia)
            |> Seq.map (fun region -> RegionId(region.GetName().Value))
            |> Set.ofSeq
        let ownedByGermany =
            data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea
            |> Seq.filter (fun region -> region.GetCountry().Value = int CountryValue.Germany)
            |> Seq.map (fun region -> RegionId(region.GetName().Value))
            |> Set.ofSeq
        fun x ->
            match ownedByRussia.Contains(x), ownedByGermany.Contains(x) with
            | true, _ -> Some Allies
            | _, true -> Some Axis
            | false, false -> None
    
    let getRegion = wg.GetRegion

    let regions =
        world.Regions
        |> List.map (fun region ->
            let owner = getOwner region.RegionId
            let production =
                (getRegion region.RegionId).Production
                |> List.sumBy (fun pro -> pro.Production)
            // Regions with factories are supplied and defended, rest is empty.
            let supplies, vehicles =
                match owner, production > 0.0f<E/H> with
                | _, false
                | None, _ -> 0.0f<E>, Map.empty
                | Some owner, true ->
                    let supplies =
                        region.Storage
                        |> Seq.sumBy (fun storage -> storage.Storage)
                    let scale (n : int) =
                        int(ceil(float32 n * production / 500.0f<E/H>))
                    let vehicles =
                        [(HeavyTank, scale 3); (MediumTank, scale 9); (LightArmor, scale 3)]
                        |> Map.ofList
                    supplies, vehicles
            { RegionId = region.RegionId
              Owner = owner
              StorageHealth = region.Storage |> List.map (fun _ -> 1.0f)
              ProductionHealth = region.Production |> List.map (fun _ -> 1.0f)
              Products = { Supplies = 0.0f<E>; Vehicles = Map.empty; Planes = Map.empty }
              Supplies = supplies
              NumVehicles = vehicles
            }
        )
    // Airfields with factories have ammo and plane.
    let mkAirfield (airfield : Airfield) =
        let hasFactories =
            //true // For now, put planes everywhere. This makes it easier to test the mission, flight times are shorter.
            not <| List.isEmpty (getRegion airfield.Region).Production
        let owner =
            getOwner airfield.Region
        let numPlanes =
            let numFighters = 5 * List.length airfield.ParkedFighters |> float32
            let numF1, numF2 = 2.0f * numFighters / 5.0f, numFighters / 5.0f
            let numAttackers = 5 * List.length airfield.ParkedAttackers |> float32
            let numBombers = 5 * List.length airfield.ParkedBombers |> float32
            let numJu52 = if numBombers >= 25.0f then 10.0f else 0.0f
            if hasFactories then
                match owner with
                | None -> Map.empty
                | Some Allies -> [ (I16, numF1); (IL2M41, numAttackers); (Mig3, numF1); (P40, numF2); (Pe2s35, numBombers) ] |> Map.ofList
                | Some Axis -> [ (Bf109e7, numF1); (Bf110e, numAttackers); (Bf109f2, numF1); (Mc202, numF2); (Ju88a4, numBombers - numJu52); (Ju52, numJu52) ] |> Map.ofList
            else
                Map.empty
        let supplies =
            if hasFactories then
                match owner with
                | None -> 0.0f<E>
                | Some _ -> airfield.Storage |> Seq.sumBy (fun gr -> gr.Storage)
            else
                0.0f<E>
        { AirfieldId = airfield.AirfieldId
          NumPlanes = numPlanes
          StorageHealth = airfield.Storage |> List.map (fun _ -> 1.0f)
          Supplies = supplies
        }
    let airfields = world.Airfields |> List.map mkAirfield
    { Airfields = airfields
      Regions = regions
      AntiAirDefenses = world.AntiAirDefenses |> List.map (fun area -> { DefenseAreaId = area.DefenseAreaId; NumUnits = 0})
      AntiTankDefenses = world.AntiTankDefenses |> List.map (fun area -> { DefenseAreaId = area.DefenseAreaId; NumUnits = 0})
      Date = world.StartDate
    }
    |> updateNumCanons world

type WorldState with
    static member Create(world : World, strategyFile : string) = mkInitialState(world, strategyFile)