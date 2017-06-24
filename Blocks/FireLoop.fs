module SturmovikMission.Blocks.FireLoop

open System.Numerics
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData

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
        // result
        {
          All = McuUtil.groupFromList group
        }
