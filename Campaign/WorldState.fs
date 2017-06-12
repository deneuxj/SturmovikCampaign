/// The state of the world: Amount of supplies, number of units, who controls each region.
module Campaign.WorldState

open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.Vehicles

open Campaign.WorldDescription
open Campaign.Util
open Campaign.BasicTypes
open Campaign.PlaneModel

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
            | HeavyTank -> vehicles.GermanHeavyTank
            | MediumTank -> vehicles.GermanMediumTank
            | LightArmor -> vehicles.GermanLightArmor
        | Allies ->
            match this with
            | HeavyTank -> vehicles.RussianHeavyTank
            | MediumTank -> vehicles.RussianMediumTank
            | LightArmor -> vehicles.RussianLightArmor

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

    member this.HasCoalitionFactories(coalition : CoalitionId) =
        this.Regions
        |> List.exists (fun region -> region.Owner = Some coalition && not region.ProductionHealth.IsEmpty)

open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Mcu
open System.Numerics
open Vector

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
    let updateArea =
        setNumUnitsAsIfFullySupplied
            (fun region -> sg.GetRegion(region).Owner)
            (fun x -> Set.contains x frontLine)
            (fun area -> area.MaxNumGuns)
    // Number of anti-air canons in each anti-air defense area, assuming unlimited supplies
    let antiAir =
        List.zip world.AntiAirDefenses state.AntiAirDefenses
        |> List.map updateArea
    // Number of anti-tank canons in each anti-tank defense area, assuming unlimited supplies
    let antiTank =
        List.zip world.AntiTankDefenses state.AntiTankDefenses
        |> List.map updateArea
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
        fun x ->
            match m1.TryGetValue(x) with
            | true, y -> y
            | false, _ -> 0
            +
            match m2.TryGetValue(x) with
            | true, y -> y
            | false, _ -> 0
    // Correct number of defensive units depending on actual shell count.
    let adjustNumCanons (area : DefenseArea, state : DefenseAreaState) =
        let region = area.Home.Home
        let factor =
            sg.GetRegion(region).Supplies / area.AmmoCost
            |> min 1.0f
        { state with
            NumUnits = int(ceil(factor * float32 state.NumUnits))
        }

    { state with
        AntiTankDefenses = List.zip world.AntiTankDefenses antiTank |> List.map adjustNumCanons
        AntiAirDefenses = List.zip world.AntiAirDefenses antiAir |> List.map adjustNumCanons
    }

/// Compute amounts of supplies to have anti-tank and anti-air canons fully operational.
let computeDefenseNeeds (world : World) =
    [
        for antiTank in world.AntiTankDefenses do
            match antiTank.Home with
            | FrontLine(home, ngh) ->
                yield home, float32(antiTank.MaxNumGuns) * cannonCost
            | _ ->
                ()
        for antiAir in world.AntiAirDefenses do
            match antiAir.Home with
            | Central(home) ->
                yield home, float32(antiAir.MaxNumGuns) * cannonCost
            | _ ->
                ()
    ]
    |> List.groupBy fst
    |> List.map (fun (region, costs) -> region, costs |> Seq.sumBy snd)

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

    let distanceFromAxisFactories = computeDistanceFromFactories true (fun world -> world.Roads @ world.Rails) getOwner world Axis
    let distanceFromAlliesFactories = computeDistanceFromFactories true (fun world -> world.Roads @ world.Rails) getOwner world Allies
    let distanceFromFactories =
        function
        | Axis -> distanceFromAxisFactories
        | Allies -> distanceFromAlliesFactories
    // regions further from factories than this value are unsupplied.
    let cutoffHops = 4
    let regions =
        let supplyNeeds = computeDefenseNeeds world
        List.zip world.Regions supplyNeeds
        |> List.map (fun (region, (_, needs)) ->
            let owner = getOwner region.RegionId
            let supplies, vehicles =
                match owner with
                | None -> 0.0f<E>, Map.empty
                | Some owner ->
                    let hops =
                        Map.tryFind region.RegionId (distanceFromFactories owner)
                        |> Option.defaultVal 10
                        |> min 10
                    let scale x =
                        x * (1.0f - (float32 hops) / (float32 cutoffHops))
                        |> max 0.0f
                    let supplies =
                        region.Storage
                        |> Seq.sumBy (fun storage -> storage.Storage)
                        |> min needs
                        |> float32
                        |> scale
                    let ceilint = ceil >> int
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
            }
        )
    let mkAirfield (airfield : Airfield) =
        let owner =
            getOwner airfield.Region
        let numPlanes, supplies =
            match owner with
            | Some owner ->
                let hops =
                    Map.tryFind airfield.Region (distanceFromFactories owner)
                    |> Option.defaultVal cutoffHops
                    |> min cutoffHops
                let scale (x : float32) =
                    x * (1.0f - (float32 hops) / (float32 cutoffHops)) |> max 0.0f
                let numFighters = List.length airfield.ParkedFighters |> float32 |> scale
                let numAttackers = List.length airfield.ParkedAttackers |> float32 |> scale
                let numBombers = List.length airfield.ParkedBombers |> float32 |> scale
                let getNumPlanes =
                    function
                    | PlaneType.Attacker -> numAttackers
                    | PlaneType.Bomber -> numBombers
                    | PlaneType.Fighter -> numFighters
                    | PlaneType.Transport -> 0.0f
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
                    airfield.Storage |> Seq.sumBy (fun gr -> gr.Storage)
                    |> float32
                    |> scale
                numPlanes, 1.0f<E> * supplies
            | None ->
                Map.empty, 0.0f<E>
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