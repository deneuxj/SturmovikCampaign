module SturmovikMission.Blocks.Timer

open SturmovikMission.DataProvider
open System.Numerics
open BlocksMissionData
open SturmovikMission.DataProvider.McuUtil
open Vector

type Timer =
    { Start : Mcu.McuTrigger
      Stop : Mcu.McuTrigger
      Elapsed : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, time : float) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["Timer"] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let start = getByName T.Blocks.Start
        let stop = getByName T.Blocks.Stop
        let elapsed = getByName T.Blocks.Elapsed
        // Timer value
        let timer = getByName T.Blocks.Timer :?> Mcu.McuTimer
        timer.Time <- time
        // Position of all nodes
        let diff = pos - Vector2.FromMcu(elapsed.Pos)
        let diff = diff + Vector2(100.0f, -100.0f)
        for mcu in group do
            let pos2 = diff + Vector2.FromMcu(mcu.Pos)
            pos2.AssignTo(mcu.Pos)
        // Result
        { Start = start
          Stop = stop
          Elapsed = elapsed
          All = McuUtil.groupFromList group
        }
