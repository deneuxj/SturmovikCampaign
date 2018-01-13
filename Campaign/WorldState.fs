/// The state of the world: Amount of supplies, number of units, who controls each region.
module Campaign.WorldState

open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.Vehicles

open Campaign.WorldDescription
open Util
open Campaign.BasicTypes
open Campaign.PlaneModel
open System.Numerics
open VectorExtension

/// Types of ground attack vehicles.
type GroundAttackVehicle =
    | HeavyTank
    | MediumTank
    | LightArmor
with
    member this.GetModel(coalition, lightArmorIsAA) =
        match coalition with
        | Axis ->
            match this with
            | HeavyTank -> vehicles.GermanHeavyTank
            | MediumTank -> vehicles.GermanMediumTank
            | LightArmor ->
                if lightArmorIsAA then
                    vehicles.GermanMobileAA
                else
                    vehicles.GermanLightArmor
        | Allies ->
            match this with
            | HeavyTank -> vehicles.RussianHeavyTank
            | MediumTank -> vehicles.RussianMediumTank
            | LightArmor ->
                if lightArmorIsAA then
                    vehicles.RussianMobileAA
                else
                    vehicles.RussianLightArmor

    member this.Description =
        match this with
        | HeavyTank -> "heavy"
        | MediumTank -> "medium"
        | LightArmor -> "light"

    static member LightArmorCost = 600.0f<E>
    static member MediumTankCost = GroundAttackVehicle.LightArmorCost * 2.0f
    static member HeavyTankCost = GroundAttackVehicle.LightArmorCost * 5.0f

    member this.Cost =
        match this with
        | HeavyTank -> GroundAttackVehicle.HeavyTankCost
        | MediumTank -> GroundAttackVehicle.MediumTankCost
        | LightArmor -> GroundAttackVehicle.LightArmorCost

    member this.Durability =
        match this with
        | HeavyTank -> 12500
        | MediumTank -> 10000
        | LightArmor -> 5000

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
    NumInvadingVehicles : Map<GroundAttackVehicle, int>
}
with
    member this.GetNumVehicles(vehicle : GroundAttackVehicle) =
        this.NumVehicles
        |> Map.tryFind vehicle
        |> Option.defaultVal 0

    member this.GetNumInvadingVehicles(vehicle : GroundAttackVehicle) =
        this.NumInvadingVehicles
        |> Map.tryFind vehicle
        |> Option.defaultVal 0

    member this.GetNumVehicles(coalition: CoalitionId, vehicle : GroundAttackVehicle) =
        match this.Owner with
        | None -> 0
        | Some x when x = coalition -> this.GetNumVehicles(vehicle)
        | Some _ -> this.GetNumInvadingVehicles(vehicle)

    member this.HasInvaders =
        this.NumInvadingVehicles
        |> Map.exists (fun _ qty -> qty > 0)

    member this.TotalVehicleValue =
        this.NumVehicles
        |> Map.toSeq
        |> Seq.sumBy (fun (vehicle, qty) -> vehicle.Cost * float32 qty)

    member this.StorageCapacity(region : WorldDescription.Region, subBlocksSpecs) =
        List.zip region.Storage this.StorageHealth
        |> List.sumBy (fun (sto, health) -> health * sto.Storage subBlocksSpecs)

    member this.ProductionCapacity(region : WorldDescription.Region, subBlockSpecs, factor) =
        List.zip region.Production this.ProductionHealth
        |> List.sumBy (fun (prod, health) -> health * prod.Production(subBlockSpecs, factor))


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
    Runway : Vector2 * float32
    AiSpawnPos : Vector2 * float32
}
with
    member this.StorageCapacity(af : WorldDescription.Airfield, subBlocksSpecs) =
        List.zip af.Storage this.StorageHealth
        |> List.sumBy (fun (sto, health) -> health * sto.Storage subBlocksSpecs)

    /// Get total value of all planes at this airfield.
    member this.TotalPlaneValue =
        this.NumPlanes
        |> Map.map (fun plane qty -> plane.Cost * qty)
        |> Map.toSeq
        |> Seq.sumBy snd

    /// Get total amount of bombs for all planes at this airfield.
    member this.BombNeeds =
        this.NumPlanes
        |> Map.toSeq
        |> Seq.sumBy (fun (plane, qty) -> plane.BombCapacity * (ceil qty))

    /// Damage supplies and planes by a specified amount.
    member this.ApplyDamage(damage : float32<E>) =
        let random = System.Random()
        let planes = this.NumPlanes |> Map.toArray
        let rec apply (planes : (PlaneModel * float32)[]) (damage : float32<E>) =
            if damage <= 0.0f<E> then
                planes
            elif planes.Length = 0 then
                planes
            else
                let i = random.Next(planes.Length)
                let plane, health = planes.[i]
                let availableToDamage = health * plane.Cost
                let damageInflicted = min damage availableToDamage
                let damage = damage - damageInflicted
                let health = health - damageInflicted / plane.Cost
                planes.[i] <- (plane, health)
                if health <= 0.0f then
                    let planes = Array.filter (fun (_, v) -> v > 0.0f) planes
                    apply planes damage
                else
                    apply planes damage
        let planes = apply planes damage
        { this with
            NumPlanes = Map.ofArray planes
            Supplies = this.Supplies - damage |> max 0.0f<E>
        }

    /// Set runway and AI spawn position according to wind direction.
    member this.SetRunway(windDirection : float32, runways : T.Airfield list) =
        let upwind =
            runways
            |> List.maxBy (fun spawn ->
                match runwayOfAirfieldSpawn spawn with
                | Some(_, yori) ->
                    let angleDiff =
                        (windDirection - yori) * float32 System.Math.PI / 180.0f
                    -cos(angleDiff)
                | None ->
                    System.Single.NegativeInfinity)
        match runwayOfAirfieldSpawn upwind, parkingOfAirfieldSpawn upwind with
        | Some runway, Some parking ->
            { this with Runway = runway; AiSpawnPos = parking }
        | _ ->
            this

/// Packages all state data.
type WorldState = {
    Date : System.DateTime
    AttackingSide : CoalitionId
    Regions : RegionState list
    Airfields : AirfieldState list
}

/// Provide fast access to state data using indexes.
type WorldStateFastAccess = {
    GetRegion : RegionId -> RegionState
    GetAirfield : AirfieldId -> AirfieldState
}
with
    static member Create(state : WorldState) =
        { GetRegion = mkGetStuffFast state.Regions (fun r -> r.RegionId)
          GetAirfield = mkGetStuffFast state.Airfields (fun af -> af.AirfieldId)
        }

type WorldState
with
    member this.FastAccess = WorldStateFastAccess.Create(this)

    member this.GetRegion(region) =
        this.Regions
        |> List.find (fun r -> r.RegionId = region)

    member this.TotalPlaneValueOfCoalition(world : World, coalition : CoalitionId) =
        let sg = this.FastAccess
        List.zip world.Airfields this.Airfields
        |> Seq.filter (fun (af, _) -> sg.GetRegion(af.Region).Owner = Some coalition)
        |> Seq.sumBy (fun (_, afs) -> afs.TotalPlaneValue)

    member this.HasCoalitionFactories(coalition : CoalitionId) =
        this.Regions
        |> List.exists (fun region -> region.Owner = Some coalition && not region.ProductionHealth.IsEmpty)

    member this.GetAmmoCostPerRegion(world : World) =
        let sg = this.FastAccess
        let aaCosts =
            seq {
                for area in world.AntiAirDefenses do
                    yield area.Home, area.AmmoCost
            }
            |> Seq.groupBy fst
            |> Seq.map (fun (reg, costs) -> reg, costs |> Seq.sumBy snd)
            |> Map.ofSeq
        let atCosts =
            seq {
                for region, regState in List.zip world.Regions this.Regions do
                    let areas =
                        region.Neighbours
                        |> Seq.filter (fun ngh ->
                            match sg.GetRegion(ngh).Owner, regState.Owner with
                            | Some x, Some y when x <> y -> true
                            | _ -> false
                        )
                        |> Seq.map (fun ngh ->
                            let area = world.GetBattlefield(Some ngh, region.RegionId)
                            area.DefenseAreaId, area)
                        |> Map.ofSeq
                    let cost =
                        areas
                        |> Map.toSeq
                        |> Seq.sumBy (fun (_, area) -> area.AmmoCost)
                    yield region.RegionId, cost
            }
            |> Map.ofSeq
        Map.sumUnion aaCosts atCosts

    member this.GetAmmoFillLevel(world : World, bf : DefenseArea) =
        let region = bf.Home
        let regState = this.GetRegion(region)
        let aaCost =
            seq {
                for area in world.AntiAirDefenses do
                    if area.Home = region then
                        yield area.AmmoCost
            }
            |> Seq.sum
        let atCost = bf.AmmoCost
        regState.Supplies / (aaCost + atCost)
        |> max 0.0f
        |> min 1.0f

    /// <summary>
    /// Check if a side is victorious.
    /// </summary>
    member this.VictoriousSide(world : World) =
        let getNumRegions coalition =
            this.Regions
            |> Seq.filter (fun reg -> reg.Owner = Some coalition)
            |> Seq.length
        let getTanks coalition =
            this.Regions
            |> List.sumBy (fun regState ->
                if regState.Owner = Some coalition then
                    regState.TotalVehicleValue
                elif regState.Owner = Some coalition.Other then
                    regState.NumInvadingVehicles
                    |> Map.toSeq
                    |> Seq.sumBy (fun (tank, qty) -> tank.Cost * float32 qty)
                else
                    0.0f<E>)
        let axisRegions, alliesRegions = getNumRegions Axis, getNumRegions Allies
        let axisTanks, alliesTanks = getTanks Axis, getTanks Allies
        let k = 2.0f
        // If one side has twice as many tanks as the other, and it controls 3x more regions, it has won
        if axisRegions >= 3 * alliesRegions && axisTanks > k * alliesTanks then
            Some Axis
        elif 3 * axisRegions <= alliesRegions && k * axisTanks < alliesTanks then
            Some Allies
        else
            None


open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Mcu

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
let computeDistanceFromFactories getPaths getOwner (world : World) (coalition : CoalitionId) =
    computeDistance true getPaths getOwner (
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
    let owner = getOwner area.Home
    let numUnits =
        match owner with
        | None -> 0
        | Some _ -> baseNumUnits area
    { areaState with
        NumUnits = numUnits
    }

let getNumCanonsPerRegion (areas : (DefenseArea * DefenseAreaState) seq) =
    areas
    |> Seq.groupBy (fun (desc, state) -> desc.Home)
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

/// Compute amounts of supplies to have anti-tank and anti-air canons fully operational.
let computeFullDefenseNeeds (world : World) =
    [
        for area in world.AntiTankDefenses @ world.AntiAirDefenses do
            yield area.Home, area.AmmoCost
    ]
    |> List.groupBy fst
    |> List.map (fun (region, costs) -> region, costs |> Seq.sumBy snd)

/// Build the initial state
let mkInitialState(world : World, strategyFile : string, windDirection : float32) =
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

    // Set of regions explicitly marked as strong, i.e. they have full supplies and vehicles
    let strongRegions =
        data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea
        |> Seq.filter (fun region -> region.GetDesc().Value.Contains "***")
        |> Seq.map (fun region -> RegionId(region.GetName().Value))
        |> Set.ofSeq

    let getRegion = wg.GetRegion

    let distanceFromStrongRegions =
        if strongRegions.IsEmpty then
            // No region explicitly marked as strong, use factories instead.
            let distanceFromAxisFactories = computeDistanceFromFactories (fun world -> world.Roads @ world.Rails) getOwner world Axis
            let distanceFromAlliesFactories = computeDistanceFromFactories (fun world -> world.Roads @ world.Rails) getOwner world Allies
            function
            | Axis -> distanceFromAxisFactories
            | Allies -> distanceFromAlliesFactories
        else
            let distanceFromAxisStrong = computeDistance true (fun world -> world.Roads @ world.Rails) getOwner (fun r -> getOwner r = Some Axis && strongRegions.Contains r) world
            let distanceFromAlliesStrong = computeDistance true (fun world -> world.Roads @ world.Rails) getOwner (fun r -> getOwner r = Some Allies && strongRegions.Contains r) world
            function
            | Axis -> distanceFromAxisStrong
            | Allies -> distanceFromAlliesStrong

    // regions further from factories than this value are unsupplied.
    let cutoffHops = 4
    let regions =
        let supplyNeeds = computeFullDefenseNeeds world
        List.zip world.Regions supplyNeeds
        |> List.map (fun (region, (_, needs)) ->
            let owner = getOwner region.RegionId
            let supplies, vehicles =
                match owner with
                | None -> 0.0f<E>, Map.empty
                | Some owner ->
                    let hops =
                        Map.tryFind region.RegionId (distanceFromStrongRegions owner)
                        |> Option.defaultVal 10
                        |> min 10
                    let scale x =
                        x * (1.0f - (float32 hops) / (float32 cutoffHops))
                        |> max 0.0f
                    let supplies =
                        region.Storage
                        |> Seq.sumBy (fun storage -> storage.Storage world.SubBlockSpecs)
                        |> if region.Production.IsEmpty then min needs else id
                        |> float32
                        |> scale
                    let ceilint = ceil >> int
                    let scale =
                        let numAirfields =
                            world.Airfields
                            |> Seq.filter (fun af -> af.Region = region.RegionId)
                            |> Seq.length
                            |> max 1
                            |> float32
                        fun x ->
                            (scale x) * numAirfields
                    let vehicles =
                        [(HeavyTank, scale 3.0f |> ceilint); (MediumTank, scale 9.0f |> ceilint); (LightArmor, scale 3.0f |> ceilint)]
                        |> Map.ofList
                    1.0f<E> * supplies, vehicles
            { RegionId = region.RegionId
              Owner = owner
              StorageHealth = region.Storage |> List.map (fun _ -> 1.0f)
              ProductionHealth = region.Production |> List.map (fun _ -> 1.0f)
              Products = { Supplies = 0.0f<E>; Vehicles = Map.empty; Planes = Map.empty }
              Supplies = supplies
              NumVehicles = vehicles
              NumInvadingVehicles = Map.empty
            }
        )
    let mkAirfield (airfield : Airfield) =
        let owner =
            getOwner airfield.Region
        let numPlanes, supplies =
            match owner with
            | Some owner ->
                let hops =
                    Map.tryFind airfield.Region (distanceFromStrongRegions owner)
                    |> Option.defaultVal cutoffHops
                    |> min cutoffHops
                let scale (x : float32) =
                    x * (1.0f - (float32 hops) / (float32 cutoffHops)) |> max 0.0f
                let maxPlanes =
                    airfield.ParkedAttackers.Length + airfield.ParkedBombers.Length + airfield.ParkedFighters.Length
                    |> float32
                let planeTypeShares = PlaneModel.PlaneTypeShares(owner)
                let numFighters = maxPlanes * planeTypeShares.[PlaneType.Fighter] |> scale |> round
                let numAttackers = maxPlanes * planeTypeShares.[PlaneType.Attacker] |> scale |> round
                let numBombers = maxPlanes * planeTypeShares.[PlaneType.Bomber] |> scale |> round
                let numTransports = maxPlanes * planeTypeShares.[PlaneType.Transport] |> scale |> round
                let getNumPlanes =
                    function
                    | PlaneType.Attacker -> numAttackers
                    | PlaneType.Bomber -> numBombers
                    | PlaneType.Fighter -> numFighters
                    | PlaneType.Transport -> numTransports
                let random = new System.Random()
                let numPlanes =
                    PlaneModel.AllModels world.PlaneSet
                    |> Seq.filter (fun plane -> plane.Coalition = owner)
                    |> Seq.map (fun plane -> plane.PlaneType, Array.init (int <| getNumPlanes plane.PlaneType) (fun _ -> plane))
                    |> Seq.groupBy fst
                    |> Seq.map (fun (planeType, planes) -> planeType, planes |> Seq.map snd |> Array.concat |> Array.shuffle random |> Array.take (int <| getNumPlanes planeType))
                    |> Seq.map snd
                    |> Array.concat
                    |> Util.compactSeq
                    |> Map.map (fun k v -> float32 v)
                let supplies =
                    airfield.Storage |> Seq.sumBy (fun gr -> gr.Storage world.SubBlockSpecs)
                    |> float32
                    |> scale
                numPlanes, 1.0f<E> * supplies
            | None ->
                Map.empty, 0.0f<E>
        { AirfieldId = airfield.AirfieldId
          NumPlanes = numPlanes
          StorageHealth = airfield.Storage |> List.map (fun _ -> 1.0f)
          Supplies = supplies
          Runway = Vector2.Zero, 0.0f
          AiSpawnPos = Vector2.Zero, 0.0f
        }.SetRunway(windDirection, airfield.Spawn)
    let airfields = world.Airfields |> List.map mkAirfield
    { Airfields = airfields
      Regions = regions
      Date = world.StartDate
      AttackingSide = Axis
    }

type WorldState with
    static member Create(world : World, strategyFile : string, windDirection : float32) = mkInitialState(world, strategyFile, windDirection)

    /// <summary>
    /// Get positions of the most damaged buildings (storage and production).
    /// </summary>
    /// <param name="world">Description of buildings in regions and airfields.</param>
    /// <param name="maxNumFires">Maximum number of positions to return.</param>
    member this.FirePositions(world : World, maxNumFires : int) =
        let damagedRegionStorage =
            List.zip (world.Regions |> List.map (fun region -> region.Storage)) (this.Regions |> List.map (fun region -> region.StorageHealth))
            |> List.collect (fun (buildings, healths) -> List.zip buildings healths)
            |> List.filter (fun (_, health) -> health < 0.5f)
            |> List.map (fun (building, health) -> building.Storage world.SubBlockSpecs * (1.0f - health), (building, health))
        let damagedRegionProduction =
            List.zip (world.Regions |> List.map (fun region -> region.Production)) (this.Regions |> List.map (fun region -> region.ProductionHealth))
            |> List.collect (fun (buildings, healths) -> List.zip buildings healths)
            |> List.filter (fun (_, health) -> health < 0.5f)
            |> List.map (fun (building, health) -> 24.0f<H> * (building.Production(world.SubBlockSpecs, productionFactor world)) * (1.0f - health), (building, health))
        let damagedAirfieldStorage =
            List.zip (world.Airfields |> List.map (fun airfield -> airfield.Storage)) (this.Airfields |> List.map (fun airfield -> airfield.StorageHealth))
            |> List.collect (fun (buildings, healths) -> List.zip buildings healths)
            |> List.filter (fun (_, health) -> health < 0.5f)
            |> List.map (fun (building, health) -> building.Storage world.SubBlockSpecs * (1.0f - health), (building, health))
        List.concat [ damagedRegionStorage; damagedRegionProduction; damagedAirfieldStorage ]
        |> List.sortByDescending fst
        |> List.truncate maxNumFires
        |> List.map (fun (damage, (grp, _)) -> grp.Pos.Pos, grp.Pos.Altitude, damage)
