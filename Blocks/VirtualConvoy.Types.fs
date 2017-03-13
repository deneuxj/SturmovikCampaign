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
        let db = T.GroupData(Parsing.Stream.FromFile "Blocks.Mission").CreateMcuList()
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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, coalition : Mcu.CoalitionValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["WhileEnemyCloseAlt"] db |> List.ofSeq
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

type AtDestination = {
    LeaderArrived : Mcu.McuTrigger
    Destroyed : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    /// <summary>
    /// Create a group that reports a given event, e.g. arrival of a convoy at destination.
    /// This event can later be retrieved from the mission log, which includes the position, coalition, success,
    /// objective id (primary, secondary n) and the icon type.
    /// </summary>
    /// <param name="pos">Position of the node.</param>
    /// <param name="objectiveTaskType">Objective task: 0 means primary, 1 to 15 are secondary.</param>
    /// <param name="iconType">The type of icon. Not actually used by the game, can be used for own purposes.</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, coalition : Mcu.CoalitionValue, objectiveTaskType : int, iconType : int) =
        if objectiveTaskType < 0 || objectiveTaskType > 15 then
            invalidArg "objectiveId" "Must be between 0 and 15 inclusive"
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let lcSubst = Mcu.substLCId <| lcStore.GetIdMapper()
        let group = blocksData.GetGroup("Vehicle arrived").CreateMcuList()
        for mcu in group do
            subst mcu
            lcSubst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let leaderArrived = getByName T.Blocks.LeaderArrived
        let destroyed = getByName T.Blocks.Destroyed
        let isAlive = getByName T.Blocks.IsAlive
        let objective = getByIndex isAlive.Targets.Head group
        let lcData = objective.IconLC
        let lcDesc, lcName =
            match lcData with
            | Some data -> data.LCDesc, data.LCName
            | None -> failwith "Mission LC data in objective node"
        // Position of all nodes
        let refPoint = Vector2(float32 leaderArrived.Pos.X, float32 leaderArrived.Pos.Z)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // Replace objective node by a new objective node with the fields set to the desired values
        let objective2 =
            let x =
                newObjective objective.Index lcDesc lcName
            x.SetCoalition(T.Integer(int(coalition)))
                .SetIconType(T.Integer iconType)
                .SetTaskType(T.Integer objectiveTaskType)
                .CreateMcu()
        // Result
        let group =
            group
            |> List.map (fun mcu ->
                if mcu.Index = objective2.Index then
                    objective2
                else
                    mcu)
        { LeaderArrived = leaderArrived
          Destroyed = destroyed
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = [ (lcDesc, ""); (lcName, "") ]
                  member x.SubGroups = []
            }
        }