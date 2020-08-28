module SturmovikMission.Blocks.Train

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.IconDisplay
open SturmovikMission.Blocks.EventReporting
open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.Conjunction
open SturmovikMission.Blocks.Util.String

type Train = {
    Start : Mcu.McuTrigger
    StopTravel : Mcu.McuTrigger
    ResumeTravel : Mcu.McuTrigger
    Arrived : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Blocked : Mcu.McuTrigger
    Completed : Mcu.McuTrigger
    IconCover : IconDisplay
    IconAttack : IconDisplay
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, isHeavyAA : bool, path : PathVertex list, bridges : (PathVertex * Mcu.McuEntity) list, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
        if path.IsEmpty then
            failwith "Cannot create train with empty path"
        let startV, midV, destV =
            let x = Array.ofList path
            x.[0], x.[x.Length / 2], Array.last x
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Train").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group "START"
        let stopTravel = getTriggerByName group "STOP"
        let resumeTravel = getTriggerByName group "RESUME"
        let arrived = getTriggerByName group "ARRIVED"
        let killed = getTriggerByName group "KILLED"
        let blocked = getTriggerByName group "BLOCKED"
        let completed = getTriggerByName group "COMPLETED"
        let destWp = getWaypointByName group "Destination"
        let discard = getTriggerByName group "DELAYED_DISCARD"
        let train = getVehicleByName group "Train"
        let enemyPlaneClose = getTriggerByName group "EnemyPlaneNear" :?> Mcu.McuProximity
        // Logic to stop and block train when it reaches a destroyed bridge
        let conds =
            seq {
                for v, entity in bridges do
                    let conj = Conjunction.Create(store, v.Pos + Vector2(0.0f, 100.0f)).MakeInitiallyFalse(store)
                    // Link bridge destruction to conjA and to blocked
                    entity.OnEvents <-
                        { Mcu.Type = int Mcu.EventTypes.OnKilled
                          Mcu.TarId = conj.SetA.Index }
                        :: entity.OnEvents
                    // Non-passed destroyed bridge reports convoy as blocked
                    Mcu.addTargetLink conj.SetA blocked.Index
                    // Link conj result to stop train
                    Mcu.addTargetLink conj.AllTrue stopTravel.Index
                    Mcu.addTargetLink conj.AllTrue discard.Index
                    Mcu.addTargetLink conj.AllTrue completed.Index
                    // Result
                    yield v, conj
            }
            |> Seq.groupBy fst
            |> Seq.map (fun (k, vs) -> k, vs |> Seq.map snd |> List.ofSeq)
            |> dict
        let getConjOfVertex v =
            match conds.TryGetValue v with
            | true, x -> x
            | false, _ -> []
        // Intermediate waypoints to destWp
        let wps =
            let subst = Mcu.substId <| store.GetIdMapper()
            [
                for idx, (v, _) in Seq.indexed(Seq.pairwise path) do
                    let waypoint = newWaypoint (2 * idx + 1) v.Pos v.Ori v.Radius v.Speed v.Priority
                    let passedDisable = newDeactivate (2 * idx + 2)
                    subst passedDisable
                    subst waypoint
                    (Vector2.FromMcu waypoint.Pos + Vector2(100.0f, 0.0f)).AssignTo passedDisable.Pos
                    // Object-link to train
                    Mcu.addObjectLink waypoint train.LinkTrId
                    // PassedDisable
                    Mcu.addTargetLink waypoint passedDisable.Index
                    // Target-link to conjunctions, if any
                    match getConjOfVertex v with
                    | [] -> ()
                    | conds ->
                        waypoint.Radius <- max waypoint.Radius 1000 // Make sure the train has enough time to stop before getting to the destroyed bridge
                        for conj in conds do
                            Mcu.addTargetLink waypoint conj.SetB.Index
                            Mcu.addTargetLink passedDisable conj.SetA.Index
                        yield waypoint
            ]
        // start -> first intermediate waypoint
        match wps with
        | wp1 :: _ ->
            Mcu.addTargetLink start wp1.Index
            // remove any link to destWp
            start.Targets <-
                start.Targets
                |> List.filter ((<>) destWp.Index)
        | [] ->
            ()
        // intermediate wp -> next wp
        for v1, v2 in Seq.pairwise wps do
            Mcu.addTargetLink v1 v2.Index
        // Last intermediate wp -> destWp
        match List.tryLast wps with
        | Some last ->
            Mcu.addTargetLink last destWp.Index
        | None ->
            ()
        // Override train
        let train2 =
            match country with
            | Mcu.CountryValue.Russia ->
                if isHeavyAA then 
                    vehicles.MkRussianTrainMcu()
                else
                    vehicles.MkRussianTrainMgAAMcu()
            | Mcu.CountryValue.Germany ->
                if isHeavyAA then
                    vehicles.MkGermanTrainMcu()
                else
                    vehicles.MkGermanTrainMgAAMcu()
            | Mcu.CountryValue.UnitedStates | Mcu.CountryValue.GreatBritain ->
                if isHeavyAA then
                    vehicles.MkRussianTrainMgAAMcu()
                else
                    vehicles.MkRussianTrainMcu()
            | _ -> failwith "Unsupported country when making train"
        train2.Index <- train.Index
        train2.LinkTrId <- train.LinkTrId
        train2.Ori.Y <- float startV.Ori
        train2.AILevel <- Some 2
        // Watch for enemy planes
        enemyPlaneClose.PlaneCoalitions <- [ McuUtil.swapCoalition coalition ]
        // Position of all nodes
        let refPoint = Vector2(float32 train.Pos.X, float32 train.Pos.Z)
        let dv = startV.Pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        McuUtil.vecCopy train.Pos train2.Pos
        // Position waypoint
        destV.Pos.AssignTo destWp.Pos
        // Icons
        let iconPos = midV.Pos
        let iconCover, iconAttack = IconDisplay.CreatePair(store, lcStore, iconPos, "", coalition, Mcu.IconIdValue.CoverTrains)
        // Hide icon when train is blocked and stopped
        Mcu.addTargetLink stopTravel iconCover.Hide.Index
        Mcu.addTargetLink stopTravel iconAttack.Hide.Index
        // result
        let group : Mcu.McuBase list =
            group
            |> List.map (
                function
                | :? Mcu.HasEntity as vehicle when vehicle.Index = train.Index -> upcast train2
                | x -> x
            )
        { Start = start
          StopTravel = stopTravel
          ResumeTravel = resumeTravel
          Arrived = arrived
          Killed = killed
          Blocked = blocked
          Completed = completed
          IconCover = iconCover
          IconAttack = iconAttack
          All = { new McuUtil.IMcuGroup with
                      member x.Content =  (wps |> List.map(fun x -> upcast x)) @ group
                      member x.LcStrings = []
                      member x.SubGroups = (conds.Values |> List.concat |> List.map (fun x -> x.All)) @ [ iconCover.All; iconAttack.All ]
          }
        }


type TrainWithNotification = {
    TheTrain : Train
    Started : EventReporting
    Arrived : EventReporting
    Blocked : EventReporting
    Destroyed : EventReporting
}
with
    interface McuUtil.IMcuGroup with
        member this.Content = []
        member this.LcStrings = []
        member this.SubGroups =
            [
                yield this.TheTrain.All
                yield this.Started.All
                yield this.Arrived.All
                yield this.Destroyed.All
                yield this.Blocked.All
            ]

    static member Create(store, lcStore, withHeavyAA, path : PathVertex list, bridges, country, coalition, eventName) =
        if path.IsEmpty then
            failwith "Cannot create train with empty path"
        let pos = path.Head.Pos
        let destinationPos = (List.last path).Pos
        let train = Train.Create(store, lcStore, withHeavyAA, path, bridges, country, coalition)
        let startEventName = sprintf "%s-D-0" eventName
        let started = EventReporting.Create(store, country, pos + Vector2(0.0f, 100.0f), startEventName)
        let arrivedEventName = sprintf "%s-A-0" eventName
        let arrived = EventReporting.Create(store, country, destinationPos + Vector2(0.0f, 100.0f), arrivedEventName)
        let destroyedEventName = sprintf "%s-K-0" eventName
        let destroyed = EventReporting.Create(store, country, pos + Vector2(0.0f, 200.0f), destroyedEventName)
        let blockedEventName = sprintf "%s-B-0" eventName
        let blocked = EventReporting.Create(store, country, pos + Vector2(0.0f, 300.0f), blockedEventName)
        { TheTrain = train
          Started = started
          Arrived = arrived
          Blocked = blocked
          Destroyed = destroyed
        }

    member this.CreateLinks() =
        let targets =
            [
                yield this.TheTrain.Start, this.Started.Trigger :> Mcu.McuBase
                yield this.TheTrain.Arrived, upcast this.Arrived.Trigger
                yield this.TheTrain.Killed, upcast this.Destroyed.Trigger
                yield this.TheTrain.Start, upcast this.TheTrain.IconAttack.Show
                yield this.TheTrain.Start, upcast this.TheTrain.IconCover.Show
                yield this.TheTrain.Killed, upcast this.TheTrain.IconAttack.Hide
                yield this.TheTrain.Killed, upcast this.TheTrain.IconCover.Hide
                yield this.TheTrain.Arrived, upcast this.TheTrain.IconAttack.Hide
                yield this.TheTrain.Arrived, upcast this.TheTrain.IconCover.Hide
                yield this.TheTrain.Blocked, upcast this.Blocked.Trigger
            ]
        { Links.Columns = []
          Links.Objects = []
          Links.Targets = targets
          Links.Events = []
        }