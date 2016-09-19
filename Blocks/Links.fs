module SturmovikMission.Blocks.Links

open SturmovikMission.DataProvider

type Links =
    { Columns : (Mcu.McuEntity * Mcu.McuEntity * int) list
      Objects : (Mcu.McuTrigger * Mcu.McuBase) list
      Targets : (Mcu.McuTrigger * Mcu.McuBase) list
    }
with
    member this.Apply(mcus: Mcu.McuBase list) =
        let mcuById =
            mcus
            |> List.map (fun mcu -> mcu.Index, mcu)
            |> dict
        // Set columns/wings of vehicles/planes.
        for follower, leader, position in this.Columns do
            Mcu.addTargetLink follower leader.Index
            match mcuById.TryGetValue(follower.MisObjID) with
            | true, (:? Mcu.HasEntity as vehicle) ->
                match vehicle.NumberInFormation with
                | Some formation -> formation.Number <- position
                | None -> failwithf "Entity '%s' (%d)cannot be in a column" vehicle.Name vehicle.Index
            | true, _ ->
                failwithf "Node with id %d is not a vehicle or building with entity" follower.MisObjID
            | false, _ ->
                failwithf "Failed to find node with id %d" follower.MisObjID
        // Set object links
        for trigger, entity in this.Objects do
            Mcu.addObjectLink trigger entity.Index
        // Set target links
        for source, target in this.Targets do
            Mcu.addTargetLink source target.Index