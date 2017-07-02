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
      Discard : Mcu.McuTrigger
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
            leadCar.Model <- vehicles.GermanCar.Model
            leadCar.Script <- vehicles.GermanCar.Script
        | Mcu.CountryValue.Russia ->
            leadCar.Model <- vehicles.RussianCar.Model
            leadCar.Script <- vehicles.RussianCar.Script
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
          Discard = getByName T.Blocks.Discard
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
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, inFormation : int, country : Mcu.CountryValue, formationName : string) =
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
        truck.Country <- country
        match country with
        | Mcu.CountryValue.Germany ->
            truck.Model <- vehicles.GermanTruck.Model
            truck.Script <- vehicles.GermanTruck.Script
        | Mcu.CountryValue.Russia ->
            truck.Model <- vehicles.RussianTruck.Model
            truck.Script <- vehicles.RussianTruck.Script
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
          Discard = getByName T.Blocks.Discard
          All = McuUtil.groupFromList group
        }

type Waypoint =
    { Waypoint : Mcu.McuWaypoint
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
        let getByName = getTriggerByName group
        let waypoint = getWaypointByName group "Waypoint"
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
          All = McuUtil.groupFromList group
        }

type ConvoyApi =
    { Start : Mcu.McuTrigger
      Destroyed : Mcu.McuTrigger
      Arrived : Mcu.McuTrigger
      All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, numTrucks : int) =
        // Create all nodes
        let start = newCounter 1
        let destroyed = newCounter 2
        let arrived = newCounter 3
        Vector2(0.0f, 50.0f).AssignTo(destroyed.Pos)
        Vector2(0.0f, 100.0f).AssignTo(arrived.Pos)
        destroyed.Count <- numTrucks
        let group : Mcu.McuBase list = [ start; destroyed; arrived ]
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
          All = McuUtil.groupFromList group
        }
