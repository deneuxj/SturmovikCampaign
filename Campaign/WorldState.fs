namespace Campaign.WorldState

open Campaign.WorldDescription

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


module Functions =
    open SturmovikMission.DataProvider.Parsing
    open SturmovikMission.DataProvider.Mcu

    let mkInitialState(description : World, strategyFile : string) =
        let data = T.GroupData(Stream.FromFile strategyFile)
        let ownedByRussia =
            data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea
            |> Seq.filter (fun region -> region.Country.Value = int CountryValue.Russia)
            |> Seq.map (fun region -> RegionId(region.Name.Value))
            |> Set.ofSeq
        let ownedByGermany =
            data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea
            |> Seq.filter (fun region -> region.Country.Value = int CountryValue.Germany)
            |> Seq.map (fun region -> RegionId(region.Name.Value))
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
                    match owner with
                    | None -> 0.0f
                    | Some _ -> desc.Storage |> Seq.sumBy (fun _ -> 1.0f)
                { RegionId = desc.RegionId
                  Owner = owner
                  StorageHealth = desc.Storage |> List.map (fun _ -> 1.0f)
                  ProductionHealth = desc.Production |> List.map (fun _ -> 1.0f)
                  ShellCount = shellCount
                  NumHeavyTanks = 0
                  NumMediumTanks = 0
                  NumLightArmor = 0
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
            |> List.map (fromDefenseArea 5)
        let antiTankDefenses =
            description.AntiTankDefenses
            |> List.map (fromDefenseArea 5)
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
        failwith "TODO: AirfieldStates"