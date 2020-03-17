module SturmovikMission.Blocks.Battlefield

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

/// A respawning tank on a battlefield
type RespawningTank = {
    Start : Mcu.McuTrigger
    Tank : Mcu.HasEntity
    Destination : Mcu.McuWaypoint
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, startPos : Vector2, destinationPos : Vector2, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("RespawningTank").CreateMcuList()
        for mcu in group do
            subst mcu
        // Key nodes
        let start = McuUtil.getTriggerByName group "Start"
        let tank = McuUtil.getVehicleByName group "HeavyTank"
        let destination = McuUtil.getWaypointByName group "Destination"
        // Position all nodes
        let refPos = Vector2.FromMcu tank.Pos
        let dr = (destinationPos - startPos).YOri
        for mcu in group do
            let rel = Vector2.FromMcu(mcu.Pos) - refPos
            (rel.Rotate(dr) + startPos).AssignTo mcu.Pos
            mcu.Ori.Y <- mcu.Ori.Y + float dr
        destinationPos.AssignTo destination.Pos
        // Set the country of anything that can have a country
        for mcu in group do
            match mcu with
            | :? Mcu.HasEntity as citizen -> citizen.Country <- Some country
            | _ -> ()
        // Return
        { Start = start
          Tank = tank
          Destination = destination
          All = McuUtil.groupFromList group
        }

/// A respawning canon on a battlefield
type RespawningCanon = {
    Start : Mcu.McuTrigger
    Canon : Mcu.HasEntity
    Wall : Mcu.HasEntity
    Target : Mcu.McuAttackArea
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, startPos : Vector2, targetPos : Vector2, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("RespawningArtillery").CreateMcuList()
        for mcu in group do
            subst mcu
        // Key nodes
        let start = McuUtil.getTriggerByName group "Start"
        let canon = McuUtil.getVehicleByName group "Gun"
        let wall = McuUtil.getVehicleByName group "Wall"
        let target = McuUtil.getTriggerByName group "AttackArea" :?> Mcu.McuAttackArea
        // Position all nodes
        let refPos = Vector2.FromMcu canon.Pos
        let dr = (targetPos - startPos).YOri
        for mcu in group do
            let rel = Vector2.FromMcu(mcu.Pos) - refPos
            (rel.Rotate(dr) + startPos).AssignTo mcu.Pos
            mcu.Ori.Y <- mcu.Ori.Y + float dr
        targetPos.AssignTo target.Pos
        // Set the country of anything that can have a country
        for mcu in group do
            match mcu with
            | :? Mcu.HasEntity as citizen -> citizen.Country <- Some country
            | _ -> ()
        // Return
        { Start = start
          Canon = canon
          Wall = wall
          Target = target
          All = McuUtil.groupFromList group
        }

/// A tank base where players can spawn
type PlayerTankSpawn =
    { TankSpawn : Mcu.McuBase
      All : McuUtil.IMcuGroup
    }
    static member Ceate(store : NumericalIdentifiers.IdStore, position : Vector2, yori : float32, country : Mcu.CountryValue, numTanks : int) =
        let spawn =
            blocksData.ListOfAirfield |> Seq.find(fun af -> af.GetName().Value = "Tankfield")
        let entity = newEntity 2
        let spawn = spawn.SetLinkTrId(T.Integer.N 2).SetIndex(T.Integer.N 1).SetCountry(T.Integer.N (int country))
        entity.MisObjID <- 1
        // Tank selection
        let m =
            match country with
            | Mcu.CountryValue.Germany -> vehicles.GermanPlayerTank
            | Mcu.CountryValue.Russia -> vehicles.RussianPlayerTank
            | _ -> failwith "Unknown coalition"
        let tank = newAirfieldTank("Heavy tank", m.Model, m.Script, numTanks).SetRenewable(T.Boolean.N true).SetRenewTime(T.Integer.N 900)
        let spawn =
            spawn.SetPlanes(T.Airfield.Planes.Default.SetVehicle([tank]))
        let spawn = spawn.CreateMcu()
        position.AssignTo(spawn.Pos)
        position.AssignTo(entity.Pos)
        spawn.Ori.Y <- float yori
        entity.Ori.Y <- float yori
        let group = [ spawn; upcast entity ]
        let subst = Mcu.substId <| store.GetIdMapper()
        for mcu in group do
            subst mcu
        // Set the country of anything that can have a country
        for mcu in group do
            match mcu with
            | :? Mcu.HasEntity as citizen -> citizen.Country <- Some country
            | _ -> ()
        { TankSpawn = spawn
          All = McuUtil.groupFromList group
        }

type BattleSide =
    | Attackers of Mcu.CoalitionValue
    | Defenders of Mcu.CoalitionValue
with
    member this.Side =
        match this with
        | Attackers side | Defenders side -> side

type BattleIcons =
    { Position : Vector2
      Rotation : float32
      All : McuUtil.IMcuGroup
    }
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, position : Vector2, yori : float32, numAttackers : int, numDefenders : int, side : BattleSide) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let substLc = Mcu.substLCId <| lcStore.GetIdMapper()
        let group = blocksData.GetGroup("BattleIcons").CreateMcuList()
        for mcu in group do
            subst mcu
            substLc mcu
        // Key nodes
        let iconAttackers = McuUtil.getTriggerByName group "NumAttackers"
        let iconDefenders = McuUtil.getTriggerByName group "NumDefenders"
        // Position
        let xmin =
            group
            |> Seq.map (fun mcu -> mcu.Pos.X)
            |> Seq.min
            |> float32
        let ymin =
            group
            |> Seq.map (fun mcu -> mcu.Pos.Z)
            |> Seq.min
            |> float32
        let xmax =
            group
            |> Seq.map (fun mcu -> mcu.Pos.X)
            |> Seq.max
            |> float32
        let ymax =
            group
            |> Seq.map (fun mcu -> mcu.Pos.Z)
            |> Seq.max
            |> float32
        let center = 0.5f * Vector2(xmin + xmax, ymin + ymax)
        for mcu in group do
            let rel = Vector2.FromMcu mcu.Pos - center
            let newPos = rel.Rotate(yori) + position
            newPos.AssignTo mcu.Pos
        // Coalition visibility
        for mcu in group do
            match mcu with
            | :? Mcu.McuIcon as icon ->
                icon.Coalitions <- [ side.Side ]
            | _ -> ()
        // Colors
        match side with
        | Attackers _ ->
            for mcu in group do
                match mcu with
                | :? Mcu.McuIcon as icon ->
                    if icon.LineType = Mcu.LineTypeValue.Defence then
                        icon.Red <- 10
                        icon.Green <- 0
                        icon.Blue <- 0
                    elif icon.IconId = Mcu.IconIdValue.AttackTankPlatoon then
                        icon.IconId <- Mcu.IconIdValue.CoverTankPlatoon
                | _ -> ()
        | Defenders _ ->
            ()
        // Num participants
        let labels =
            [
                for mcu in group do
                    match mcu with
                    | :? Mcu.McuIcon as icon ->
                        if List.contains icon.Index iconAttackers.Targets then
                            match icon.IconLC with
                            | Some data ->
                                yield data.LCName, string numAttackers
                                yield data.LCDesc, "Number of attacking vehicles"
                            | None ->
                                ()
                        if List.contains icon.Index iconDefenders.Targets then
                            match icon.IconLC with
                            | Some data ->
                                yield data.LCName, string numDefenders
                                yield data.LCDesc, "Number of defending vehicles"
                            | None ->
                                ()
                        icon.Coalitions <- [ side.Side ]
                    | _ -> ()
            ]
        // Result
        { Position = position
          Rotation = yori
          All = { new McuUtil.IMcuGroup with
                    member x.Content = group
                    member x.LcStrings = labels
                    member x.SubGroups = []
                }
        }