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

type Train = {
    Start : Mcu.McuTrigger
    Arrived : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    IconCover : IconDisplay
    IconAttack : IconDisplay
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, pos : Vector2, yori : float32, destinationPos : Vector2, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Train").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.START
        let arrived = getTriggerByName group T.Blocks.ARRIVED
        let killed = getTriggerByName group T.Blocks.KILLED
        let destWp = getWaypointByName group T.Blocks.Destination
        let train = getVehicleByName group T.Blocks.Train
        // Override train
        let train2 =
            match country with
            | Mcu.CountryValue.Russia -> vehicles.MkRussianTrainMcu()
            | Mcu.CountryValue.Germany 
            | _ -> vehicles.MkGermanTrainMcu()
        train2.Index <- train.Index
        train2.LinkTrId <- train.LinkTrId
        train2.Ori.Y <- float yori
        // Position of all nodes
        let refPoint = Vector2(float32 train.Pos.X, float32 train.Pos.Z)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        McuUtil.vecCopy train.Pos train2.Pos
        // Position waypoint
        destinationPos.AssignTo destWp.Pos
        // Icons
        let iconPos =
            0.5f * (pos + destinationPos)
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
          IconCover = iconCover
          IconAttack = iconAttack
          All = { new McuUtil.IMcuGroup with
                      member x.Content = group
                      member x.LcStrings = []
                      member x.SubGroups = [ iconCover.All; iconAttack.All ]
          }
        }


type TrainWithNotification = {
    TheTrain : Train
    Started : EventReporting
    Arrived : EventReporting
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

    static member Create(store, lcStore, pos, yori, destinationPos, country, eventName) =
        let train = Train.Create(store, lcStore, pos, yori, destinationPos, country)
        let startEventName = sprintf "%s-D-0" eventName
        let started = EventReporting.Create(store, country, pos + Vector2(0.0f, 100.0f), startEventName)
        let arrivedEventName = sprintf "%s-A-0" eventName
        let arrived = EventReporting.Create(store, country, destinationPos + Vector2(0.0f, 100.0f), arrivedEventName)
        let destroyedEventName = sprintf "%s-K-0" eventName
        let destroyed = EventReporting.Create(store, country, pos + Vector2(0.0f, 200.0f), destroyedEventName)
        { TheTrain = train
          Started = started
          Arrived = arrived
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
            ]
        { Links.Columns = []
          Links.Objects = []
          Links.Targets = targets
        }