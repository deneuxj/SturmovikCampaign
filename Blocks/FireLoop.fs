module SturmovikMission.Blocks.FireLoop

open System.Numerics
open VectorExtension
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
        | CityFire -> @"luascripts\worldobjects\mapemitters\city_fire_loop.txt"
        | CityFireSmall -> @"luascripts\worldobjects\mapemitters\city_firesmall_loop.txt"
        | VillageSmoke -> @"luascripts\worldobjects\mapemitters\villagesmoke_loop.txt"

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
        let effect = getVehicleByName group "Effect1"
        // Position of all nodes
        let refPoint = Vector2.FromMcu(effect.Pos)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
            mcu.Pos.Y <- float alt
        // Orientation of smoke
        effect.Ori.Y <- float ori
        // Fire type
        effect.Script <- fireType.Script
        // result
        {
          All =
            { new McuUtil.IMcuGroup with
                  member this.Content = group
                  member this.LcStrings = []
                  member this.SubGroups = []
            }
        }
