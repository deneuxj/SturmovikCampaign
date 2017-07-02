module SturmovikMission.Blocks.WhileEnemyClose

open SturmovikMission.DataProvider
open System.Numerics
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider.McuUtil
open Vector

let private randomDelaySource = System.Random(0)

type WhileEnemyClose =
    { StartMonitoring : Mcu.McuTrigger
      StopMonitoring : Mcu.McuTrigger
      Deactivate : Mcu.McuTrigger
      Activate : Mcu.McuTrigger
      WakeUp : Mcu.McuTrigger
      Sleep : Mcu.McuTrigger
      Proximity : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(usePulses : bool, checkzoneOnly : bool, store : NumericalIdentifiers.IdStore, pos : Vector2, coalition : Mcu.CoalitionValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group =
            let groupName =
                match usePulses, checkzoneOnly with
                | false, false -> "WhileEnemyClose"
                | false, true -> "WhileEnemyCloseCZ"
                | true, true -> "WhileEnemyCloseCZAlt"
                | true, false -> "WhileEnemyCloseAlt"
            McuUtil.filterByPath [ groupName ] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let start = getByName T.Blocks.StartMonitoring
        let stop = getByName T.Blocks.StopMonitoring
        let deactivate = getByName T.Blocks.Deactivate
        let activate = getByName T.Blocks.Activate
        let wakeup = getByName T.Blocks.WakeUp
        let sleep = getByName T.Blocks.Sleep
        let proximity = getByName T.Blocks.EnemyClose :?> Mcu.McuProximity
        let enemyEnters = getByName T.Blocks.EnemyEnters :?> Mcu.McuProximity
        let randomDelay = getByName T.Blocks.RandomDelay :?> Mcu.McuTimer
        // Set random delay to some random value 0-60s
        randomDelay.Time <- randomDelaySource.NextDouble() * 60.0
        // Correct coalition fields
        proximity.SetRelativeCoalitions(coalition, Mcu.CoalitionValue.Allies)
        enemyEnters.SetRelativeCoalitions(coalition, Mcu.CoalitionValue.Allies)
        // Position of all nodes
        let diff = pos - Vector2.FromMcu(proximity.Pos)
        let diff = diff + Vector2(100.0f, 100.0f)
        for mcu in group do
            let pos2 = diff + Vector2.FromMcu(mcu.Pos)
            pos2.AssignTo(mcu.Pos)
        // Result
        { StartMonitoring = start
          StopMonitoring = stop
          Deactivate = deactivate
          Activate = activate
          WakeUp = wakeup
          Sleep = sleep
          Proximity = proximity
          All = McuUtil.groupFromList group
        }
