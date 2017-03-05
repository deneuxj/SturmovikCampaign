namespace Campaign.WorldState

open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.Vehicles

open Campaign.WorldDescription
open Campaign.Util

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
            | LightArmor -> germanMobileAA
        | Allies ->
            match this with
            | HeavyTank -> russianHeavyTank
            | MediumTank -> russianMediumTank
            | LightArmor -> russianMobileAA

type RegionState = {
    RegionId : RegionId
    Owner : CoalitionId option
    StorageHealth : float32 list
    ProductionHealth : float32 list
    ShellCount : float32
    NumVehicles : Map<GroundAttackVehicle, int>
}
with
    static member ShellWeight = 5.0f

type DefenseAreaState = {
    DefenseAreaId : DefenseAreaId
    NumUnits : int
}

type AirfieldState = {
    AirfieldId : AirfieldId
    NumPlanes : Map<PlaneModel, int>
    StorageHealth : float32 list
    BombWeight : float32
    NumRockets : int
}
with
    static member RocketWeight = 20.0f

type WorldState = {
    Regions : RegionState list
    DefenseAreas : DefenseAreaState list
    Airfields : AirfieldState list
    Date : System.DateTime
}

type WorldStateFastAccess = {
    GetRegion : RegionId -> RegionState
    GetDefenseArea : DefenseAreaId -> DefenseAreaState
    GetAirfield : AirfieldId -> AirfieldState
}
with
    static member Create(state : WorldState) =
        { GetRegion = mkGetStuffFast state.Regions (fun r -> r.RegionId)
          GetDefenseArea = mkGetStuffFast state.DefenseAreas (fun area -> area.DefenseAreaId)
          GetAirfield = mkGetStuffFast state.Airfields (fun af -> af.AirfieldId)
        }

type WorldState
with
    member this.FastAccess = WorldStateFastAccess.Create(this)


module Functions =
    open SturmovikMission.DataProvider.Parsing
    open SturmovikMission.DataProvider.Mcu
    open System.Numerics
    open Vector

    let (|Contains|_|) suffix (s : string) =
        if s.Contains(suffix) then
            Some()
        else
            None
    let getWeightCapacityPerBuilding (model : string) = 5000.0f
    let getShellsPerBuilding (model : string) = (getWeightCapacityPerBuilding model) / RegionState.ShellWeight
    let getDurabilityForBuilding (model : string) =
        match model with
        | Contains "arf_net" -> 1000
        | Contains "arf_dugout" -> 15000
        | Contains "arf_barak" -> 10000
        | Contains "arf_hangar" -> 10000
        | Contains "industrial" -> 10000
        | Contains "static_" -> 2500
        | _ -> 10000
    let getAntiAirCanonsForArea (area : DefenseArea) =
        let refArea = 1.0e6f
        let area = Vector2.ConvexPolygonArea(area.Boundary)
        min (5.0f * area / refArea) 2.0f
        |> ceil
        |> int
    let getAntiTankCanonsForArea (area : DefenseArea) =
        let refArea = 130.0e3f
        let area = Vector2.ConvexPolygonArea(area.Boundary)
        min (5.0f * area / refArea) 2.0f
        |> ceil
        |> int

    /// Compute the number of regions from a region to the neaest region with factories.
    let computeRegionDistances (getPaths : World -> Path list) (getOwner : RegionId -> CoalitionId option) (coalition : CoalitionId, world : World) =
        let areConnectedByRoad(start, destination) =
            getPaths world
            |> List.exists (fun path ->
                path.StartId = start && path.EndId = destination || path.StartId = destination && path.EndId = start
            )
        let wg = WorldFastAccess.Create(world)
        let rec work (distances : Map<RegionId, int>) (working : RegionId list) =
            match working with
            | [] -> distances
            | current :: rest ->
                let distance = distances.[current]
                let region = wg.GetRegion current
                let nghs =
                    region.Neighbours
                    |> Seq.filter (fun ngh -> Some coalition = getOwner ngh) // It belongs to the coalition
                    |> Seq.filter (fun ngh -> areConnectedByRoad(region.RegionId, ngh))
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
            |> Seq.filter (fun region -> not <| List.isEmpty region.Production)
            |> Seq.filter (fun region -> Some coalition = getOwner region.RegionId)
            |> Seq.map (fun region -> region.RegionId)
            |> List.ofSeq
        let distances0 =
            sources
            |> Seq.map (fun region -> region, 0)
            |> Map.ofSeq
        let distances = work distances0 sources
        distances


    let mkInitialState(world : World, strategyFile : string) =
        let data = T.GroupData(Stream.FromFile strategyFile)
        let wg = WorldFastAccess.Create(world)
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
        let getOwner x =
            match ownedByRussia.Contains(x), ownedByGermany.Contains(x) with
            | true, _ -> Some Allies
            | _, true -> Some Axis
            | false, false -> None
        let getRegion = wg.GetRegion
        let computeTransportationCost coalition =
            let byRoad = computeRegionDistances (fun world -> world.Roads) getOwner (coalition, world)
            let byRail = computeRegionDistances (fun world -> world.Rails) getOwner (coalition, world)
            byRoad
            |> Map.map (fun region hops ->
                let roadCost = System.Math.Pow(2.0, float hops)
                match Map.tryFind region byRail with
                | Some hops ->
                    let railCost = System.Math.Pow(1.1, float hops)
                    min roadCost railCost
                | None ->
                    roadCost)
        let getTransportationCosts =
            let axisTransportationCosts = computeTransportationCost Axis
            let alliesTransportationCosts = computeTransportationCost Allies
            function
            | Axis -> axisTransportationCosts
            | Allies -> alliesTransportationCosts
        let regions =
            world.Regions
            |> List.map (fun region ->
                let owner = getOwner region.RegionId
                let shellCount, vehicles =
                    match owner with
                    | None -> 0.0f, Map.empty
                    | Some owner ->
                        let transportationCosts = getTransportationCosts owner
                        match Map.tryFind region.RegionId transportationCosts with
                        | None ->
                            0.0f, Map.empty
                        | Some costs ->
                            let costs = float32 costs
                            let ammo =
                                region.Storage
                                |> Seq.sumBy (fun storage -> getShellsPerBuilding storage.Model / costs)
                            let scale (n : int) =
                                int(ceil(float32 n / costs))
                            let vehicles =
                                [(HeavyTank, scale 2); (MediumTank, scale 5); (LightArmor, scale 10)]
                                |> Map.ofList
                            ammo, vehicles
                { RegionId = region.RegionId
                  Owner = owner
                  StorageHealth = region.Storage |> List.map (fun _ -> 1.0f)
                  ProductionHealth = region.Production |> List.map (fun _ -> 1.0f)
                  ShellCount = shellCount
                  NumVehicles = vehicles
                }
            )
        let frontLine =
            let stateOfRegion =
                let m =
                    regions
                    |> List.map(fun state -> state.RegionId, state)
                    |> dict
                fun x -> m.[x]
            seq {
                for region in regions do
                    match region.Owner with
                    | None -> ()
                    | Some owner ->
                        let neighbours = (getRegion region.RegionId).Neighbours
                        let enemies =
                            neighbours
                            |> List.map stateOfRegion
                            |> List.filter (fun region -> region.Owner.IsSome && region.Owner.Value <> owner)
                        for enemy in enemies do
                            yield (region.RegionId, enemy.RegionId)
            }
            |> Set.ofSeq
        let fromDefenseArea (baseNumUnits : DefenseArea -> int) (area : DefenseArea) =
            let owner = getOwner area.Home.Home
            let numUnits =
                match owner with
                | None -> 0
                | Some _ ->
                    match area.Home with
                    | Central _ -> baseNumUnits area
                    | FrontLine(home, other) ->
                        if frontLine.Contains((home, other)) then
                            baseNumUnits area
                        else
                            0
            { DefenseAreaId = area.DefenseAreaId
              NumUnits = numUnits
            }
        let antiAirDefenses =
            world.AntiAirDefenses
            |> List.map (fromDefenseArea getAntiAirCanonsForArea)
        let antiTankDefenses =
            world.AntiTankDefenses
            |> List.map (fromDefenseArea getAntiTankCanonsForArea)
        let getDefenseArea =
            let m =
                world.AntiAirDefenses @ world.AntiTankDefenses
                |> Seq.map (fun area -> area.DefenseAreaId, area)
                |> dict
            fun x -> m.[x]
        let unitsPerRegion =
            let m =
                antiAirDefenses @ antiTankDefenses
                |> Seq.groupBy (fun defense ->
                    getDefenseArea(defense.DefenseAreaId).Home.Home
                )
                |> Seq.map (fun (key, states) -> key, states |> Seq.sumBy (fun state -> state.NumUnits))
                |> dict
            fun x -> m.[x]
        let getRegionState =
            let m =
                regions
                |> List.map (fun state -> state.RegionId, state)
                |> dict
            fun x -> m.[x]
        let adjustNumUnits (state : DefenseAreaState) =
            let region = (getDefenseArea state.DefenseAreaId).Home.Home
            let totalUnits =
                unitsPerRegion(region)
            let shells =
                int(ceil(getRegionState(region).ShellCount))
            if totalUnits > shells then
                let factor = float32 shells / float32 totalUnits
                { state with
                    NumUnits = int(ceil(factor * float32 state.NumUnits))
                }
            else
                state
        let antiAirDefenses = antiAirDefenses |> List.map adjustNumUnits
        let antiTankDefenses = antiTankDefenses |> List.map adjustNumUnits
        let mkAirfield (airfield : Airfield) =
            let hasFactories =
                not <| List.isEmpty (getRegion airfield.Region).Production
            let owner =
                getOwner airfield.Region
            let numPlanes =
                let numFighters = List.length airfield.ParkedFighters
                let numF1, numF2 = 2 * numFighters / 5, numFighters / 5
                let numAttackers = List.length airfield.ParkedAttackers
                let numBombers = List.length airfield.ParkedBombers
                let numJu52 = if numBombers >= 5 then 2 else 0
                if hasFactories then
                    match owner with
                    | None -> Map.empty
                    | Some Allies -> [ (I16, numF1); (IL2M41, numAttackers); (Mig3, numF1); (P40, numF2); (Pe2s35, numBombers) ] |> Map.ofList
                    | Some Axis -> [ (Bf109e7, numF1); (Bf110e, numAttackers); (Bf109f2, numF1); (Mc202, numF2); (Ju88a4, numBombers - numJu52); (Ju52, numJu52) ] |> Map.ofList
                else
                    Map.empty
            let storage =
                if hasFactories then
                    match owner with
                    | None -> 0.0f
                    | Some _ -> airfield.Storage |> Seq.sumBy (fun gr -> getWeightCapacityPerBuilding gr.Model)
                else
                    0.0f
            let bombWeight, rockets =
                match owner with
                | Some Allies -> 0.8f * storage, 0.2f * storage
                | Some Axis -> storage, 0.0f
                | None -> 0.0f, 0.0f
            { AirfieldId = airfield.AirfieldId
              NumPlanes = numPlanes
              StorageHealth = airfield.Storage |> List.map (fun _ -> 1.0f)
              BombWeight = bombWeight
              NumRockets = int(ceil(rockets / AirfieldState.RocketWeight))
            }
        let airfields = world.Airfields |> List.map mkAirfield
        { Airfields = airfields
          Regions = regions
          DefenseAreas = antiAirDefenses @ antiTankDefenses
          Date = world.StartDate
        }


type WorldState with
    static member Create(world : World, strategyFile : string) = Functions.mkInitialState(world, strategyFile)