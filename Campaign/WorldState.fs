namespace Campaign.WorldState

open Campaign.WorldDescription
open SturmovikMission.Blocks.BlocksMissionData
open Campaign.Util

type RegionState = {
    RegionId : RegionId
    Owner : CoalitionId option
    StorageHealth : float32 list
    ProductionHealth : float32 list
    ShellCount : float32
    NumHeavyTanks : int
    NumMediumTanks : int
    NumLightArmor : int
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

    let getWeightCapacityPerBuilding (model : string) = 5000.0f
    let getShellsPerBuilding (model : string) = (getWeightCapacityPerBuilding model) / RegionState.ShellWeight
    let getDurabilityForBuilding (model : string) = 15000
    let antiAirCanonsByArea = 5
    let antiTankCanonsByArea = 5

    let mkInitialState(description : World, strategyFile : string) =
        let data = T.GroupData(Stream.FromFile strategyFile)
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
        let getRegion =
            let m =
                description.Regions
                |> List.map (fun region -> region.RegionId, region)
                |> dict
            fun x ->
                m.[x]
        let regions =
            description.Regions
            |> List.map (fun desc ->
                let owner = getOwner desc.RegionId
                let shellCount =
                    match desc.Production with
                    | [] -> 0.0f // Initially, regions without factories are unsupplied
                    | _ ->
                        match owner with
                        | None -> 0.0f
                        | Some _ -> desc.Storage |> Seq.sumBy (fun storage -> getShellsPerBuilding storage.Model)
                { RegionId = desc.RegionId
                  Owner = owner
                  StorageHealth = desc.Storage |> List.map (fun _ -> 1.0f)
                  ProductionHealth = desc.Production |> List.map (fun _ -> 1.0f)
                  ShellCount = shellCount
                  NumHeavyTanks = 2
                  NumMediumTanks = 5
                  NumLightArmor = 10
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
        let fromDefenseArea (baseNumUnits : int) (area : DefenseArea) =
            let owner = getOwner area.Home.Home
            let numUnits =
                match owner with
                | None -> 0
                | Some _ ->
                    match area.Home with
                    | Central _ -> baseNumUnits
                    | FrontLine(home, other) ->
                        if frontLine.Contains((home, other)) then
                            baseNumUnits
                        else
                            0
            { DefenseAreaId = area.DefenseAreaId
              NumUnits = numUnits
            }
        let antiAirDefenses =
            description.AntiAirDefenses
            |> List.map (fromDefenseArea antiAirCanonsByArea)
        let antiTankDefenses =
            description.AntiTankDefenses
            |> List.map (fromDefenseArea antiTankCanonsByArea)
        let getDefenseArea =
            let m =
                description.AntiAirDefenses @ description.AntiTankDefenses
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
                let numJu52 = if numBombers > 5 then 2 else 0
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
        let airfields = description.Airfields |> List.map mkAirfield
        { Airfields = airfields
          Regions = regions
          DefenseAreas = antiAirDefenses @ antiTankDefenses
          Date = description.StartDate
        }


type WorldState with
    static member Create(world : World, strategyFile : string) = Functions.mkInitialState(world, strategyFile)