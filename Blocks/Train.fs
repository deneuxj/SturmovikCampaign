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
    Arrived : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Blocked : Mcu.McuTrigger
    IconCover : IconDisplay
    IconAttack : IconDisplay
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, path : PathVertex list, bridges : (PathVertex * Mcu.McuEntity) list, country : Mcu.CountryValue) =
        if path.IsEmpty then
            failwith "Cannot create train with empty path"
        let startV = List.head path
        let destV = List.last path
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Train").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.START
        let arrived = getTriggerByName group T.Blocks.ARRIVED
        let killed = getTriggerByName group T.Blocks.KILLED
        let blocked = getTriggerByName group T.Blocks.BLOCKED
        let destWp = getWaypointByName group T.Blocks.Destination
        let train = getVehicleByName group T.Blocks.Train
        // Logic to stop and block train when it reaches a destroyed bridge
        let conds =
            seq {
                for v, entity in bridges do
                    let conj = Conjunction.Create(store, v.Pos + Vector2(0.0f, 100.0f))
                    // Link bridge destruction to conjA
                    entity.OnEvents <-
                        { Mcu.Type = int Mcu.EventTypes.OnKilled
                          Mcu.TarId = conj.SetA.Index }
                        :: entity.OnEvents
                    // Link conj result to block signal
                    Mcu.addTargetLink conj.AllTrue blocked.Index
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
                    let waypoint = newWaypoint (idx + 1) v.Pos v.Ori v.Radius v.Speed v.Priority
                    subst waypoint
                    // Object-link to train
                    Mcu.addObjectLink waypoint train.LinkTrId
                    // Target-link to conjunctions, if any
                    match getConjOfVertex v with
                    | [] -> ()
                    | conds ->
                        waypoint.Radius <- max waypoint.Radius 1000 // Make sure the train has enough time to stop before getting to the destroyed bridge
                        for conj in conds do
                            Mcu.addTargetLink waypoint conj.SetB.Index
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
            | Mcu.CountryValue.Russia -> vehicles.MkRussianTrainMcu()
            | Mcu.CountryValue.Germany 
            | _ -> vehicles.MkGermanTrainMcu()
        train2.Index <- train.Index
        train2.LinkTrId <- train.LinkTrId
        train2.Ori.Y <- float startV.Ori
        // Position of all nodes
        let refPoint = Vector2(float32 train.Pos.X, float32 train.Pos.Z)
        let dv = startV.Pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        McuUtil.vecCopy train.Pos train2.Pos
        // Position waypoint
        destV.Pos.AssignTo destWp.Pos
        // Icons
        let iconPos =
            0.5f * (startV.Pos + destV.Pos)
        let coalition =
            match country with
            | Mcu.CountryValue.Germany -> Mcu.CoalitionValue.Axis
            | Mcu.CountryValue.Russia -> Mcu.CoalitionValue.Allies
            | _ -> invalidArg "country" "Must be Germany or Russia"
        let iconCover, iconAttack = IconDisplay.CreatePair(store, lcStore, iconPos, "", coalition, Mcu.IconIdValue.CoverTrains)
        // result
        let group : Mcu.McuBase list =
            group
            |> List.map (
                function
                | :? Mcu.HasEntity as vehicle when vehicle.Index = train.Index -> upcast train2
                | x -> x
            )
        { Start = start
          Arrived = arrived
          Killed = killed
          Blocked = blocked
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
            ]

    static member Create(store, lcStore, path : PathVertex list, bridges, country, eventName) =
        if path.IsEmpty then
            failwith "Cannot create train with empty path"
        let pos = path.Head.Pos
        let destinationPos = (List.last path).Pos
        let train = Train.Create(store, lcStore, path, bridges, country)
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
        }