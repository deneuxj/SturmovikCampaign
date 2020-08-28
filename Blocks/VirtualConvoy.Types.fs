/// Types of groups of which virtual convoys are composed.
module SturmovikMission.Blocks.VirtualConvoy.Types

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData
open System.Numerics
open VectorExtension

/// Side of the first waypoint where vehicles of a convoy spawn.
type SpawnSide = Left | Right | Center
with
    member this.Mirrored =
        match this with
        | Left -> Right
        | Right -> Left
        | Center -> Center

// The types. See Proto-VirtualConvoy.txt.
type Convoy =
    { LeadCarEntity : Mcu.McuEntity
      LeadCarDamaged : Mcu.McuTrigger
      ActivateGroup : Mcu.McuTrigger
      DeactivateGroup : Mcu.McuTrigger
      DeleteLeadCar : Mcu.McuTrigger
      Discard : Mcu.McuTrigger
      StopTravel : Mcu.McuTrigger
      Resumetravel : Mcu.McuTrigger
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
        let leadCar = getVehicleByName group "LeadCar"
        leadCar.Country <- Some country
        match country with
        | Mcu.CountryValue.Germany ->
            vehicles.GermanCar.AssignTo(leadCar)
        | Mcu.CountryValue.Russia ->
            vehicles.RussianCar.AssignTo(leadCar)
        | _ ->
            ()
        let center = Vector2.FromMcu(leadCar.Pos)
        let stopTravel = getByName "StopTravel"
        let resumeTravel = getByName "ResumeTravel"
        let enemyPlaneNear = getByName "EnemyPlaneNear" :?> Mcu.McuProximity
        // Rotate and translate
        let rot = ori - float32 leadCar.Ori.Y
        let diff = pos - center
        for mcu in group do
            let pos2 = (Vector2.FromMcu(mcu.Pos) - center).Rotate(rot) + center
            let pos2 = pos2 + diff
            pos2.AssignTo(mcu.Pos)
        leadCar.Ori.Y <- float ori 
        // Set coalition to watch to the enemy's
        enemyPlaneNear.PlaneCoalitions <- [ McuUtil.coalitionOf country |> McuUtil.swapCoalition ]
        // Result
        { LeadCarEntity = Seq.head <| McuUtil.filterByName "LeadCar entity" group :?> Mcu.McuEntity
          LeadCarDamaged = getByName "LeadCarDamaged"
          ActivateGroup = getByName "ActivateGroup"
          DeactivateGroup = getByName "DeactivateGroup"
          DeleteLeadCar = getByName "DeleteLeadCar"
          Discard = getByName "Discard"
          StopTravel = stopTravel
          Resumetravel = resumeTravel
          All = McuUtil.groupFromList group
        }

type TruckInConvoy =
    { Entity : Mcu.McuEntity
      Damaged : Mcu.McuTrigger
      Delete : Mcu.McuTrigger
      Discard : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, inFormation : int, spawnSide : SpawnSide, withAA : bool, country : Mcu.CountryValue, formationName : string) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["TruckInConvoy" ; "Convoy"] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let truck = getVehicleByName group "Truck"
        truck.Name <- sprintf "%s-%d" formationName inFormation
        truck.Country <- Some country
        // Truck model. Every fifth truck is a mobile AA truck.
        let m =
            match country with
            | Mcu.CountryValue.Germany | Mcu.CountryValue.Italy ->
                if withAA && inFormation % 5 = 0 then
                    vehicles.GermanMobileAA
                else
                    vehicles.GermanTruck
            | Mcu.CountryValue.Russia ->
                if withAA && inFormation % 5 = 0 then
                    vehicles.RussianMobileAA
                else
                    vehicles.RussianTruck
            | Mcu.CountryValue.UnitedStates | Mcu.CountryValue.GreatBritain ->
                if withAA && inFormation % 5 = 0 then
                    vehicles.AmericanMobileAA
                else
                    vehicles.AmericanTruck
            | _ ->
                failwith "Unsupported country"
        m.AssignTo(truck)
        let center = Vector2.FromMcu(truck.Pos)
        // Rotation
        let rot = ori - float32 truck.Ori.Y
        // Spawn position relative to the leader
        let spawnRel =
            match spawnSide with
            | Right -> Vector2.UnitY
            | Left -> -Vector2.UnitY
            | Center -> -Vector2.UnitX
        let offset = spawnRel * (float32 inFormation) * 20.0f
        let offset = offset.Rotate(ori)
        let pos2 = pos + offset
        let diff = pos2 - Vector2.FromMcu(truck.Pos)
        for mcu in group do
            let pos2 = (Vector2.FromMcu(mcu.Pos) - center).Rotate(rot) + center
            let pos2 = pos2 + diff
            pos2.AssignTo(mcu.Pos)
        truck.Ori.Y <- float ori 
        // Result
        { Entity = Seq.head <| McuUtil.filterByName "Truck entity" group :?> Mcu.McuEntity
          Damaged = getByName "TruckDamaged"
          Delete = getByName "DeleteTruck"
          Discard = getByName "Discard"
          All = McuUtil.groupFromList group
        }


type Waypoint =
    { Waypoint : Mcu.McuWaypoint
      PassedDisable : Mcu.McuTrigger // A "deactivate" MCU triggered when the waypoint is reached
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, speed : int, priority : int) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["Waypoint" ; "Convoy"] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let waypoint = getWaypointByName group "Waypoint"
        let passed = getTriggerByName group "PastDisable"
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
          PassedDisable = passed
          All = McuUtil.groupFromList group
        }

type ConvoyApi =
    { Start : Mcu.McuTrigger
      Destroyed : Mcu.McuTrigger
      Completed : Mcu.McuTrigger // Destroyed, arrived, blocked at destroyed bridge
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, numTrucks : int) =
        // Create all nodes
        let start = newCounter 1
        let completed = newCounter 2
        let destroyed = newCounter 3
        Vector2(0.0f, 50.0f).AssignTo(completed.Pos)
        Vector2(0.0f, 100.0f).AssignTo(destroyed.Pos)
        destroyed.Count <- numTrucks
        let group : Mcu.McuBase list = [ start; destroyed; completed ]
        // Position
        for mcu in group do
            let pos2 = Vector2.FromMcu(mcu.Pos) + pos
            pos2.AssignTo(mcu.Pos)
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        for mcu in group do
            subst mcu
        // destroyed -> completed
        Mcu.addTargetLink destroyed completed.Index
        // Result
        { Start = start
          Destroyed = destroyed
          Completed = completed
          All = McuUtil.groupFromList group
        }
