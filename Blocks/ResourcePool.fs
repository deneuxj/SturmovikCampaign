module SturmovikMission.Blocks.ResourcePool

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open PriorityList

type ResourceLock = {
    Grab : Mcu.McuTrigger
    Release : Mcu.McuTrigger
    GrabEnabled : Mcu.McuTrigger
    ReleaseEnabled : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, position : Vector2) : ResourceLock =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = SturmovikMission.Blocks.BlocksMissionData.blocksData.GetGroup("ResourceLock").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let grab = getTriggerByName group T.Blocks.GRAB
        let release = getTriggerByName group T.Blocks.RELEASE
        let grabEnabled = getTriggerByName group T.Blocks.ENABLE_GRAB
        let releaseEnabled = getTriggerByName group T.Blocks.ENABLE_RELEASE
        // Position of all nodes
        let refPoint = Vector2.FromMcu grab.Pos
        let dv = position - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // result
        { Grab = grab
          Release = release
          GrabEnabled = grabEnabled
          ReleaseEnabled = releaseEnabled
          All = groupFromList group
        }

type ResourcePool = {
    TryGrab : Mcu.McuTrigger
    SomeReleased : Mcu.McuTrigger
    AllGrabbed : Mcu.McuTrigger
    Release : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store, size : int, position : Vector2, step : Vector2, start : Mcu.McuTrigger) =
        if size < 1 then
            invalidArg "size" "Must be at least 1"
        let grabChain = NodeList.Create(store, List.init size (fun i -> position + step.Rotate(90.0f) + (float32 i) * step))
        let releaseChain = NodeList.Create(store, List.init size (fun i -> position + step.Rotate(270.0f) + (float32 i) * step))
        let locks = List.init size (fun i -> ResourceLock.Create(store, position + (float32 i) * step))
        let conds = List.init size (fun i -> Conjunction.Conjunction.Create(store, position + (float32 i) * step).ClearABOn(start))
        // Start-up: All released
        for lock in locks do
            Mcu.addTargetLink start lock.GrabEnabled.Index
        // Connections between locks and grab/release chains
        for lock, grab in Seq.zip locks grabChain.Nodes do
            Mcu.addTargetLink lock.GrabEnabled grab.Enable.Index
            Mcu.addTargetLink lock.ReleaseEnabled grab.Disable.Index
        for lock, release in Seq.zip locks releaseChain.Nodes do
            Mcu.addTargetLink lock.GrabEnabled release.Disable.Index
            Mcu.addTargetLink lock.ReleaseEnabled release.Enable.Index
        // Keeps track of whether all locks are grabbed
        for cond, grab in Seq.zip conds grabChain.Nodes do
            Mcu.addTargetLink grab.Do cond.SetB.Index
        for cond, release in Seq.zip conds releaseChain.Nodes do
            Mcu.addTargetLink release.Do cond.ClearB.Index
        for n, m in Seq.pairwise conds do
            Mcu.addTargetLink n.AllTrue m.SetA.Index
            Mcu.addTargetLink n.SomeFalse m.ClearA.Index
        // Overall status
        let someReleased, allGrabbed =
            match List.tryLast conds with
            | Some finalCond -> finalCond.SomeFalse, finalCond.AllTrue
            | None -> failwith "Empty condition list"
        // Result
        { TryGrab = grabChain.Try
          Release = releaseChain.Try
          SomeReleased = someReleased
          AllGrabbed = allGrabbed
          All = { new McuUtil.IMcuGroup with
                      member this.Content = []
                      member this.LcStrings = []
                      member this.SubGroups =
                        [
                            yield grabChain.All
                            yield releaseChain.All
                            for lock in locks do
                                yield lock.All
                            for cond in conds do
                                yield cond.All
                        ]
          }
        }