module SturmovikMission.Blocks.MissionEnd

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData

type MissionEnd = {
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("ServerInputMissionEnd").CreateMcuList()
        for mcu in group do
            subst mcu
        // result
        {
          All = McuUtil.groupFromList group
        }
