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
        let leadCar = getVehicleByName group T.Blocks.LeadCar
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
          Discard = getByName T.Blocks.Discard
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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, inFormation : int, spawnSide : SpawnSide, country : Mcu.CountryValue, formationName : string) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.CreateMcuList()
        let group = McuUtil.filterByPath ["TruckInConvoy" ; "Convoy"] db |> List.ofSeq
        for mcu in group do
            subst mcu
        // Get key nodes
        let getByName = getTriggerByName group
        let truck = getVehicleByName group T.Blocks.Truck
        truck.Name <- sprintf "%s-%d" formationName inFormation
        truck.Country <- Some country
        // Truck model. Every fifth truck is a mobile AA truck.
        let m =
            match country with
            | Mcu.CountryValue.Germany ->
                if inFormation % 5 = 0 then
                    vehicles.GermanMobileAA
                else
                    vehicles.GermanTruck
            | Mcu.CountryValue.Russia ->
                if inFormation % 5 = 0 then
                    vehicles.RussianMobileAA
                else
                    vehicles.RussianTruck
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
        { Entity = Seq.head <| McuUtil.filterByName T.Blocks.``Truck entity`` group :?> Mcu.McuEntity
          Damaged = getByName T.Blocks.TruckDamaged
          Delete = getByName T.Blocks.DeleteTruck
          Discard = getByName T.Blocks.Discard
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
      Arrived : Mcu.McuTrigger
      Blocked : Mcu.McuTrigger // e.g. because of a destroyed bridge
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, numTrucks : int) =
        // Create all nodes
        let start = newCounter 1
        let destroyed = newCounter 2
        let arrived = newCounter 3
        let blocked = newCounter 4
        Vector2(0.0f, 50.0f).AssignTo(destroyed.Pos)
        Vector2(0.0f, 100.0f).AssignTo(arrived.Pos)
        Vector2(0.0f, 150.0f).AssignTo(blocked.Pos)
        destroyed.Count <- numTrucks
        let group : Mcu.McuBase list = [ start; destroyed; arrived; blocked ]
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
          Blocked = blocked
          All = McuUtil.groupFromList group
        }
