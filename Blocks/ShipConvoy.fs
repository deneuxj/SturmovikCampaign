module SturmovikMission.Blocks.ShipConvoy

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

type WaterType =
    | Sea
    | River

/// A ship convoy with escort
type ShipConvoy = {
    Start : Mcu.McuTrigger
    Arrived : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Completed : Mcu.McuTrigger
    IconCover : IconDisplay
    IconAttack : IconDisplay
    Ships : Mcu.HasEntity list
    Escort : Mcu.HasEntity list
    All : McuUtil.IMcuGroup
}
with

    /// <summary>
    /// Create a ship convoy with escort
    /// </summary>
    /// <param name="store">Provides unique ids for MCUs</param>
    /// <param name="lcStore">Provides unique ids for text</param>
    /// <param name="numShips">Number of cargo ships, including leader</param>
    /// <param name="escortModel">Model and script of escort ships</param>
    /// <param name="cargoModel">Model and script of cargo ships</param>
    /// <param name="waterType">Sea or river</param>
    /// <param name="path">Waypoints the convoy will sail along</param>
    /// <param name="country">Country owning the ships</param>
    /// <param name="eventName">Base event name for convoy start, arrival and destruction.</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, numShips : int, escortModel, cargoModel, waterType : WaterType, path : PathVertex list, country : Mcu.CountryValue, eventName) =
        if numShips < 1 then
            invalidArg "numShips" "Ship convoys must have at least one ship"
        // Instantiate leader, escort and group logic
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("ShipConvoy").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group "START"
        let arrived = getTriggerByName group "ARRIVED"
        let killed = getTriggerByName group "KILLED" :?> Mcu.McuCounter
        let wp1 = getWaypointByName group "WP1"
        let destWp = getWaypointByName group "Destination"
        let escort1 = getVehicleByName group "Escort1"
        let escort2 = getVehicleByName group "Escort2"
        let ship1 = getVehicleByName group "Cargo1"
        let completed = getTriggerByName group "COMPLETED"
        let ship1Entity = getEntityByIndex ship1.LinkTrId group
        // Adjust killed count
        killed.Count <- 2 + numShips
        // Instantiate convoy members
        let shipGroups =
            let random = System.Random()
            List.init (numShips - 1) (fun i ->
                // Instantiate
                let subst = Mcu.substId <| store.GetIdMapper()
                let group = blocksData.GetGroup("ShipConvoyMember").CreateMcuList()
                for mcu in group do
                    subst mcu
                // Get key nodes
                let ship = getVehicleByName group "Cargo2"
                let entity = getEntityByIndex ship.LinkTrId group
                let shipKilled = getTriggerByName group "Cargo2Killed"
                // Link ship killed to group killed counter
                Mcu.addTargetLink shipKilled killed.Index
                // Link ship to leader
                Mcu.addTargetLink entity ship1Entity.Index
                // Position
                let leadPos = Vector2.FromMcu(ship1.Pos)
                let dv = leadPos - Vector2.FromMcu(ship.Pos)
                let side = dv.Rotate(90.0f)
                let side = side / side.Length()
                // Move ships to a random amount to the side, to avoid making them easy targets
                let offSide =
                    let mag =
                        match waterType with
                        | Sea -> 500.0f
                        | River -> 50.0f
                    2.0f * mag * float32(random.NextDouble() - 0.5) * side
                let newPos = leadPos - (1.0f + float32 i) * dv + offSide
                newPos.AssignTo(ship.Pos)
                newPos.AssignTo(entity.Pos)
                let newPos = Vector2.FromMcu(shipKilled.Pos) - (float32 i) * dv
                newPos.AssignTo(shipKilled.Pos)
                // Result
                group)
        let ships =
            shipGroups
            |> List.map (fun group -> getVehicleByName group "Cargo2")
        let group = group @ List.concat shipGroups
        // Override model escort
        do
            let model = escortModel
            for escort in [ escort1; escort2 ] do
                escort.Script <- model.Script
                escort.Model <- model.Model
                escort.Country <- Some country
        // Override model of cargo ships
        for ship in ship1 :: ships do
            let model = cargoModel
            ship.Model <- model.Model
            ship.Script <- model.Script
            ship.Country <- Some country
        // Position of all nodes
        let v1 =
            match path with
            | v1 :: _ -> v1
            | [] -> invalidArg "path" "Must not be empty"
        let refPoint = Vector2(float32 wp1.Pos.X, float32 wp1.Pos.Z)
        let dv, rot = v1.Pos - refPoint, v1.Ori
        for mcu in group do
            ((Vector2.FromMcu(mcu.Pos) - refPoint).Rotate(rot) + dv + refPoint).AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float rot
        // waypoints
        let mkWp(v : PathVertex) =
            let subst = Mcu.substId <| store.GetIdMapper()
            let wp = newWaypoint 1 v.Pos v.Ori v.Radius v.Speed v.Priority
            subst wp
            Mcu.addObjectLink wp escort1.LinkTrId
            Mcu.addObjectLink wp ship1.LinkTrId
            wp
        let rec work xs =
            match xs with
            | [v : PathVertex] ->
                v.Pos.AssignTo destWp.Pos
                destWp.Ori.Y <- float v.Ori
                destWp.Speed <- v.Speed
                destWp.Priority <- v.Priority
                destWp.Radius <- v.Radius
                []
            | [] ->
                invalidArg "path" "Must have at least two items"
            | v :: rest ->
                let tail = work rest
                let wp = mkWp v
                match tail with
                | (x : Mcu.McuWaypoint) :: _ -> Mcu.addTargetLink wp x.Index
                | [] -> Mcu.addTargetLink wp destWp.Index
                wp :: tail
        let midWps =
            match path with
            | v :: rest ->
                v.Pos.AssignTo wp1.Pos
                wp1.Ori.Y <- float v.Ori
                wp1.Speed <- v.Speed
                wp1.Priority <- v.Priority
                wp1.Radius <- v.Radius
                work rest
            | [] ->
                invalidArg "path" "Must have at least two items"
        // Set target link from first waypoint
        wp1.Targets <- []
        match midWps with
        | x :: _ -> Mcu.addTargetLink wp1 x.Index
        | [] -> Mcu.addTargetLink wp1 destWp.Index
        // Icons
        let iconPos =
            0.5f * (Vector2.FromMcu(wp1.Pos) + Vector2.FromMcu(destWp.Pos))
        let coalition = McuUtil.coalitionOf country
        let iconCover, iconAttack = IconDisplay.CreatePair(store, lcStore, iconPos, "", coalition, Mcu.IconIdValue.CoverShips)
        // Events
        let startEventName = sprintf "%s-D-0" eventName
        let startEvent = EventReporting.Create(store, country, Vector2.FromMcu wp1.Pos + Vector2(0.0f, 100.0f), startEventName)
        let arrivedEventName = sprintf "%s-A-0" eventName
        let arrivedEvent = EventReporting.Create(store, country, Vector2.FromMcu destWp.Pos + Vector2(0.0f, 100.0f), arrivedEventName)
        let destroyedEvents =
            [
                for rank, ship in Seq.indexed (ship1 :: ships) do
                    let destroyedEventName = sprintf "%s-K-%d" eventName rank
                    let destroyedEvent = EventReporting.Create(store, country, Vector2.FromMcu ship.Pos + Vector2(0.0f, 100.0f), destroyedEventName)
                    let entity = getEntityByIndex ship.LinkTrId group
                    entity.OnEvents <-
                        { Mcu.Type = int Mcu.EventTypes.OnKilled
                          Mcu.TarId = destroyedEvent.Trigger.Index }
                        :: entity.OnEvents
                    yield destroyedEvent
            ]
        // Connections to icons
        for icon in [ iconAttack; iconCover] do
            Mcu.addTargetLink start icon.Show.Index
            Mcu.addTargetLink killed icon.Hide.Index
            Mcu.addTargetLink arrived icon.Hide.Index
        // Connections to events
        Mcu.addTargetLink start startEvent.Trigger.Index
        Mcu.addTargetLink arrived arrivedEvent.Trigger.Index
        // result
        let midWps = midWps |> List.map (fun x -> x :> Mcu.McuBase)
        { Start = start
          Arrived = arrived
          Killed = killed
          Completed = completed
          IconCover = iconCover
          IconAttack = iconAttack
          Ships = ship1 :: ships
          Escort = [ escort1; escort2 ]
          All = { new McuUtil.IMcuGroup with
                      member x.Content = group @ midWps
                      member x.LcStrings = []
                      member x.SubGroups = [ iconCover.All; iconAttack.All; startEvent.All; arrivedEvent.All ] @ (destroyedEvents |> List.map (fun ev -> ev.All))
          }
        }
