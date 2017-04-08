/// Types of groups of which virtual convoys are composed.
module SturmovikMission.Blocks.VirtualConvoy.Types

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData
open System.Numerics
open Vector

// The types. See Proto-VirtualConvoy.txt.
type Convoy =
    { LeadCarEntity : Mcu.McuEntity
      LeadCarDamaged : Mcu.McuTrigger
      ActivateGroup : Mcu.McuTrigger
      DeactivateGroup : Mcu.McuTrigger
      DeleteLeadCar : Mcu.McuTrigger
      TriggerGates : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["Convoy"] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let leadCar = getVehicleByName group T.Blocks.LeadCar
        leadCar.Country <- country
        match country with
        | Mcu.CountryValue.Germany ->
            leadCar.Model <- germanCar.Model
            leadCar.Script <- germanCar.Script
        | Mcu.CountryValue.Russia ->
            leadCar.Model <- russianCar.Model
            leadCar.Script <- russianCar.Script
        | _ ->
            ()
        let center = Vector2.FromMcu(leadCar.Pos)
        // Rotate and translate
        let rot = ori - float32 leadCar.Ori.Y
        let diff = pos - center
        for mcu in group do
            let pos2 = (Vector2.FromMcu(mcu.Pos) - center).Rotate(rot) + center
            let pos2 = pos2 + diff
            pos2.AssignTo(mcu.Pos)
        leadCar.Ori.Y <- float ori 
        // Result
        { LeadCarEntity = Seq.head <| McuUtil.filterByName T.Blocks.``LeadCar entity`` group :?> Mcu.McuEntity
          LeadCarDamaged = getByName T.Blocks.LeadCarDamaged
          ActivateGroup = getByName T.Blocks.ActivateGroup
          DeactivateGroup = getByName T.Blocks.DeactivateGroup
          DeleteLeadCar = getByName T.Blocks.DeleteLeadCar
          TriggerGates = getByName T.Blocks.TriggerGates
          All = McuUtil.groupFromList group
        }

type TruckInConvoy =
    { Entity : Mcu.McuEntity
      Damaged : Mcu.McuTrigger
      Delete : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, inFormation : int, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["TruckInConvoy" ; "Convoy"] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let truck = getVehicleByName group T.Blocks.Truck
        truck.Country <- country
        match country with
        | Mcu.CountryValue.Germany ->
            truck.Model <- germanTruck.Model
            truck.Script <- germanTruck.Script
        | Mcu.CountryValue.Russia ->
            truck.Model <- russianTruck.Model
            truck.Script <- russianTruck.Script
        | _ ->
            ()
        let center = Vector2.FromMcu(truck.Pos)
        // Rotation
        let rot = ori - float32 truck.Ori.Y
        // Actual position is to the right of the leader
        let offset = Vector2.UnitY * (float32 inFormation) * 20.0f
        let offset = offset.Rotate(ori)
        let pos2 = pos + offset
        let diff = pos2 - Vector2.FromMcu(truck.Pos)
        for mcu in group do
            let pos2 = (Vector2.FromMcu(mcu.Pos) - center).Rotate(rot) + center
            let pos2 = pos2 + diff
            pos2.AssignTo(mcu.Pos)
        truck.Ori.Y <- float ori 
        // Result
        { Entity = Seq.head <| McuUtil.filterByName T.Blocks.``Truck entity`` group :?> Mcu.McuEntity
          Damaged = getByName T.Blocks.TruckDamaged
          Delete = getByName T.Blocks.DeleteTruck
          All = McuUtil.groupFromList group
        }

type ActiveWaypoint =
    { Waypoint : Mcu.McuWaypoint
      Activate : Mcu.McuTrigger
      Deactivate : Mcu.McuTrigger
      Gate : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, speed : int, priority : int) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["ActiveWaypoint" ; "Convoy"] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let waypoint = getWaypointByName group "Waypoint"
        let activate = getByName T.Blocks.ActivateGate
        let deactivate = getByName T.Blocks.DeactivateGate
        let gate = getByName T.Blocks.Gate
        // Orientation of waypoint
        waypoint.Ori.Y <- float ori 
        waypoint.Speed <- speed
        waypoint.Priority <- priority
        // Position of all nodes
        let diff = pos - Vector2.FromMcu(waypoint.Pos)
        for mcu in group do
            let pos2 = diff + Vector2.FromMcu(mcu.Pos)
            pos2.AssignTo(mcu.Pos)
        // Result
        { Waypoint = waypoint
          Activate = activate
          Deactivate = deactivate
          Gate = gate
          All = McuUtil.groupFromList group
        }

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

type ConvoyControl =
    { Start : Mcu.McuTrigger
      Destroyed : Mcu.McuTrigger
      Arrived : Mcu.McuTrigger
      Captured : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, numTrucks : int) =
        // Create all nodes
        let start = newCounter 1
        let destroyed = newCounter 2
        Vector2(0.0f, 50.0f).AssignTo(destroyed.Pos)
        destroyed.Count <- numTrucks + 1
        let arrived = newCounter 3
        Vector2(0.0f, 100.0f).AssignTo(arrived.Pos)
        let captured = newCounter 4
        Vector2(0.0f, 150.0f).AssignTo(captured.Pos)
        let group : Mcu.McuBase list = [ start; destroyed; arrived; captured ]
        // Position
        for mcu in group do
            let pos2 = Vector2.FromMcu(mcu.Pos) + pos
            pos2.AssignTo(mcu.Pos)
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        for mcu in group do
            subst mcu
        // Result
        { Start = start
          Destroyed = destroyed
          Arrived = arrived
          Captured = captured
          All = McuUtil.groupFromList group
        }

type EventReporting = {
    Trigger : Mcu.McuTrigger
    Disable : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    /// <summary>
    /// Create a group that reports a given event, e.g. arrival of a convoy at destination.
    /// This event can later be retrieved from the mission log. The mechanism that is used is destruction of a fake block, which can be identified by its name and/or position
    /// </summary>
    /// <param name="pos">Position of the node.</param>
    static member Create(store : NumericalIdentifiers.IdStore, country, pos : Vector2, eventName : string) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("EventLogging").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let notifier = getVehicleByName group T.Blocks.Notification
        let entity = getEntityByIndex notifier.LinkTrId group 
        let getByName = getTriggerByName group
        let leaderArrived = getByName T.Blocks.Trigger
        let destroyed = getByName T.Blocks.Disable
        let isAlive = getByName T.Blocks.IsEnabled
        // Position of all nodes
        let refPoint = Vector2(float32 notifier.Pos.X, float32 notifier.Pos.Z)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // Result
        entity.Name <- eventName
        notifier.Name <- eventName
        notifier.Country <- country
        { Trigger = leaderArrived
          Disable = destroyed
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = []
                  member x.SubGroups = []
            }
        }

type IconDisplay = {
    Show : Mcu.McuTrigger
    Hide : Mcu.McuTrigger
    Icon : Mcu.McuIcon
    All : McuUtil.IMcuGroup
}
with
    /// <summary>
    /// Create an icon that can be shown or hidden
    /// </summary>
    /// <param name="pos">Location of the icon</param>
    /// <param name="label">Label of the icon</param>
    /// <param name="coalition">Coalition which can see the icon</param>
    /// <param name="iconType">The type of the icon</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, label : string, coalition : Mcu.CoalitionValue, iconType : Mcu.IconIdValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let substLc = Mcu.substLCId <| lcStore.GetIdMapper()
        let group = blocksData.GetGroup("Icon").CreateMcuList()
        for mcu in group do
            subst mcu
            substLc mcu
        // Get key nodes
        let show = getTriggerByName group T.Blocks.Show
        let hide = getTriggerByName group T.Blocks.Hide
        let icon = getIconByIndex show.Targets.Head group
        // Position of all nodes
        let refPos = Vector2.FromMcu icon.Pos
        let dv = pos - refPos
        for mcu in group do
            (Vector2.FromMcu mcu.Pos + dv).AssignTo(mcu.Pos)
        // Icon type
        icon.Coalitions <- [ coalition ]
        icon.IconId <- iconType
        { Show = show
          Hide = hide
          Icon = icon
          All =
            { new McuUtil.IMcuGroup with
                member x.Content = group
                member x.LcStrings = [ icon.IconLC.Value.LCName, label ]
                member x.SubGroups = []
            }
        }

    /// <summary>
    /// Create a pair of colocated icons, visible to each coalition as attack/cover
    /// </summary>
    /// <param name="coalition">The coalition with the attacking role</param>
    /// <param name="iconType">The icon type, should be of one of the CoverXXX types</param>
    static member CreatePair(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, label : string, coalition : Mcu.CoalitionValue, iconType : Mcu.IconIdValue) =
        let one = IconDisplay.Create(store, lcStore, pos, label, coalition, iconType)
        let other =
            match coalition with
            | Mcu.CoalitionValue.Allies -> Mcu.CoalitionValue.Axis
            | Mcu.CoalitionValue.Axis -> Mcu.CoalitionValue.Allies
            | _ -> invalidArg "coalition" "Must be Axis or Allies"
        let two = IconDisplay.Create(store, lcStore, pos, label, other, enum((int iconType) - 50))
        one, two