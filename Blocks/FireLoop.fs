module SturmovikMission.Blocks.FireLoop

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.WhileEnemyClose

type FireType =
    | CityFire
    | CityFireSmall
    | VillageSmoke
with
    member this.Script =
        match this with
        | CityFire -> @"luascripts\worldobjects\mapemitters\city_fire.txt"
        | CityFireSmall -> @"luascripts\worldobjects\mapemitters\city_firesmall.txt"
        | VillageSmoke -> @"luascripts\worldobjects\mapemitters\villagesmoke.txt"

type FireLoop = {
    Proximity : WhileEnemyClose
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, alt : float32, ori : float32, fireType : FireType) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("CityFire").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let effect1 = getVehicleByName group T.Blocks.Effect1
        let effect2 = getVehicleByName group T.Blocks.Effect2
        let init = getTriggerByName group T.Blocks.INITIALLY
        let startLoop = getTriggerByName group T.Blocks.START_LOOP
        let stopLook = getTriggerByName group T.Blocks.STOP_LOOP
        // Position of all nodes
        let refPoint = Vector2.FromMcu(effect1.Pos)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
            mcu.Pos.Y <- float alt
        // Orientation of smoke
        effect1.Ori.Y <- float ori
        effect2.Ori.Y <- float ori
        // Fire type
        effect1.Script <- fireType.Script
        effect2.Script <- fireType.Script
        // Proximity logic
        let wec = WhileEnemyClose.Create(true, true, store, pos, Mcu.CoalitionValue.Neutral)
        // Set coalitions to both Axis and Allies
        match wec.Proximity with
        | :? Mcu.McuProximity as prox ->
            prox.PlaneCoalitions <- [Mcu.CoalitionValue.Axis; Mcu.CoalitionValue.Allies]
        | _ -> ()
        // Connection fire loop <-> proximity logic
        Mcu.addTargetLink init wec.StartMonitoring.Index
        Mcu.addTargetLink wec.WakeUp startLoop.Index
        Mcu.addTargetLink wec.Sleep stopLook.Index
        // result
        {
          Proximity = wec
          All =
            { new McuUtil.IMcuGroup with
                  member this.Content = group
                  member this.LcStrings = []
                  member this.SubGroups = [ wec.All ]
            }
        }
