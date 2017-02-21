/// Types of groups of which virtual convoys are composed.
module SturmovikMission.Blocks.VirtualConvoy.Types

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Mcu.Vec3, ori : Mcu.Vec3, country : Mcu.CountryValue) =
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
        let center = McuUtil.newVec3(leadCar.Pos.X, leadCar.Pos.Y, leadCar.Pos.Z)
        // Rotate and translate
        let rot = ori.Y - leadCar.Ori.Y
        let diff = McuUtil.vecMinus pos center
        for mcu in group do
            let pos2 = McuUtil.rotate center rot mcu.Pos
            let pos2 = McuUtil.translate pos2 diff
            McuUtil.vecCopy pos2 mcu.Pos
        McuUtil.vecCopy ori leadCar.Ori
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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Mcu.Vec3, ori : Mcu.Vec3, inFormation : int, country : Mcu.CountryValue) =
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
        let center = McuUtil.newVec3(0.0, 0.0, 0.0)
        McuUtil.vecCopy truck.Pos center
        // Rotation
        let rot = ori.Y - truck.Ori.Y
        // Actual position is to the right of the leader
        let offset = McuUtil.newVec3(0.0, 0.0, (float inFormation) * 20.0)
        let offset = McuUtil.rotate (McuUtil.newVec3(0.0, 0.0, 0.0)) ori.Y offset
        let pos2 = McuUtil.translate pos offset
        let diff = McuUtil.vecMinus pos2 truck.Pos
        for mcu in group do
            let pos2 = McuUtil.rotate center rot mcu.Pos
            let pos2 = McuUtil.translate pos2 diff
            McuUtil.vecCopy pos2 mcu.Pos
        McuUtil.vecCopy ori truck.Ori
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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Mcu.Vec3, ori : Mcu.Vec3, speed : int, priority : int) =
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
        McuUtil.vecCopy ori waypoint.Ori
        waypoint.Speed <- speed
        waypoint.Priority <- priority
        // Position of all nodes
        let diff = McuUtil.vecMinus pos waypoint.Pos
        for mcu in group do
            let pos2 = McuUtil.translate diff mcu.Pos
            McuUtil.vecCopy pos2 mcu.Pos
        // Result
        { Waypoint = waypoint
          Activate = activate
          Deactivate = deactivate
          Gate = gate
          All = McuUtil.groupFromList group
        }

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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Mcu.Vec3, coalition : Mcu.CoalitionValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["WhileEnemyClose"] db |> List.ofSeq
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
        proximity.SetRelativeCoalitions(coalition, Mcu.CoalitionValue.Allies)
        enemyEnters.SetRelativeCoalitions(coalition, Mcu.CoalitionValue.Allies)
        // Position of all nodes
        let diff = McuUtil.vecMinus pos proximity.Pos
        let diff = McuUtil.translate diff (McuUtil.newVec3(100.0, 0.0, 100.0))
        for mcu in group do
            let pos2 = McuUtil.translate diff mcu.Pos
            McuUtil.vecCopy pos2 mcu.Pos
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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Mcu.Vec3, time : float) =
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
        let diff = McuUtil.vecMinus pos elapsed.Pos
        let diff = McuUtil.translate diff (McuUtil.newVec3(100.0, 0.0, -100.0))
        for mcu in group do
            let pos2 = McuUtil.translate diff mcu.Pos
            McuUtil.vecCopy pos2 mcu.Pos
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
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Mcu.Vec3, numTrucks : int) =
        // Create all nodes
        let start = newCounter 1
        let destroyed = newCounter 2
        McuUtil.vecCopy (McuUtil.newVec3(0.0, 0.0, 50.0)) destroyed.Pos
        destroyed.Count <- numTrucks + 1
        let arrived = newCounter 3
        McuUtil.vecCopy (McuUtil.newVec3(0.0, 0.0, 100.0)) arrived.Pos
        let group : Mcu.McuBase list = [ start; destroyed; arrived ]
        // Position
        for mcu in group do
            let pos2 = McuUtil.translate mcu.Pos pos
            McuUtil.vecCopy pos2 mcu.Pos
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        for mcu in group do
            subst mcu
        // Result
        { Start = start
          Destroyed = destroyed
          Arrived = arrived
          All = McuUtil.groupFromList group
        }
