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
    /// <param name="escortModels">Models and scripts of escort ships</param>
    /// <param name="cargoModels">Models and scripts of cargo ships</param>
    /// <param name="waterType">Sea or river</param>
    /// <param name="path">Waypoints the convoy will sail along</param>
    /// <param name="country">Country owning the ships</param>
    /// <param name="eventName">Base event name for convoy start, arrival and destruction.</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, escortModels : VehicleTypeData list, cargoModels : VehicleTypeData list, waterType : WaterType, path : PathVertex list, country : Mcu.CountryValue, eventName) =
        let random = System.Random(hash path)
        let numCargoShips = cargoModels.Length
        let numEscortShips = escortModels.Length
        if numCargoShips < 1 then
            invalidArg "numShips" "Ship convoys must have at least one cargo ship"
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
        let destWp = getWaypointByName group "WPDest"
        let wp1e = getWaypointByName group "WP1e"
        let destWpE = getWaypointByName group "WPDestE"
        let escort1 = getVehicleByName group "Escort1"
        let ship1 = getVehicleByName group "Cargo1"
        let completed = getTriggerByName group "COMPLETED"
        let ship1Entity = getEntityByIndex ship1.LinkTrId group
        let escort1Entity = getEntityByIndex escort1.LinkTrId group
        let proximity = getTriggerByName group "Proximity" :?> Mcu.McuProximity
        let activate = getTriggerByName group "Activate"

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

        // Adjust killed count
        killed.Count <- numEscortShips + numCargoShips

        let cargoSeparation = match waterType with Sea -> 2500.0f | River -> 500.0f
        let escortSeparation = match waterType with Sea -> 1000.0f | River -> 300.0f

        // Positions of cargo and escort ships along path
        let foldPath =
            List.fold (fun (path, positions) offset ->
                let path, pos = PathVertex.tryPlaceOnPath offset path
                match pos with
                | Some pos -> (path, pos :: positions)
                | None -> (path, positions))
        let pathCargo, positionsCargo =
            let path0 = (path, 0.0f)
            ((path0, []), 0.0f :: List.init (numCargoShips - 1) (fun _ -> cargoSeparation))
            ||> foldPath
        let pathEscort, positionsEscort =
            if numEscortShips <= 0 then
                pathCargo, []
            else
                ((pathCargo, []), 300.0f :: List.init (numEscortShips - 1) (fun _ -> escortSeparation))
                ||> foldPath

        // Instantiate convoy members
        let processShipGroup(groupName, shipName, killedName, leaderIdx, i, pos, ori) =
            // Instantiate
            let subst = Mcu.substId <| store.GetIdMapper()
            let group = blocksData.GetGroup(groupName).CreateMcuList()
            for mcu in group do
                subst mcu
            // Get key nodes
            let ship = getVehicleByName group shipName
            let entity = getEntityByIndex ship.LinkTrId group
            let shipKilled = getTriggerByName group killedName
            // Link ship killed to group killed counter
            Mcu.addTargetLink shipKilled killed.Index
            // Link ship to leader
            ship.NumberInFormation |> Option.iter (fun data -> data.Number <- (i + 2))
            Mcu.addTargetLink entity leaderIdx
            // Position
            let dv = Vector2.UnitX.Rotate(ori)
            let side = dv.Rotate(90.0f)
            // Spacing between ships
            // Move ships to a random amount to the side, to avoid making them easy targets
            let offSide =
                let mag =
                    match waterType with
                    | Sea -> 500.0f
                    | River -> 20.0f
                2.0f * mag * float32(random.NextDouble() - 0.5) * side
            let newPos = pos + offSide
            newPos.AssignTo(ship.Pos)
            newPos.AssignTo(entity.Pos)
            ship.Ori.Y <- float ori
            entity.Ori.Y <- float ori
            let newPos = pos + 200.0f * side
            newPos.AssignTo(shipKilled.Pos)
            // Result
            group

        let shipGroups =
            positionsCargo
            |> List.skip 1
            |> List.mapi (fun i (pos, ori) -> processShipGroup("ShipConvoyMember", "Cargo2", "Cargo2Killed", ship1Entity.Index, i, pos, ori))
        let cargoShips =
            shipGroups
            |> List.map (fun group -> getVehicleByName group "Cargo2")

        // Instantiate escort ships
        let escortGroups =
            if numEscortShips = 0 then
                []
            else
            positionsEscort
            |> List.skip 1
            |> List.mapi (fun i (pos, ori) -> processShipGroup("ShipConvoyEscortMember", "Escort2", "EscortKilled", escort1Entity.Index, i, pos, ori))
        let escortShips =
            escortGroups
            |> List.map (fun group -> getVehicleByName group "Escort2")

        // Set position of cargo lead ship
        positionsCargo
        |> List.tryHead
        |> Option.iter (fun (pos, ori) ->
            pos.AssignTo(ship1.Pos)
            pos.AssignTo(ship1Entity.Pos)
            ship1.Ori.Y <- float ori
            ship1Entity.Ori.Y <- float ori)

        // Set position of escort lead ship
        positionsEscort
        |> List.tryHead
        |> Option.iter (fun (pos, ori) ->
            pos.AssignTo(escort1.Pos)
            pos.AssignTo(escort1Entity.Pos)
            escort1.Ori.Y <- float ori
            escort1Entity.Ori.Y <- float ori)

        // Adjust proximity of lead escort and lead cargo depending on number of escort ships
        if numEscortShips > 0 then
            proximity.Distance <- (numEscortShips + 1) * (int escortSeparation) + 500

        let group = List.concat (group :: shipGroups @ escortGroups)

        // Deactivate escort lead ship if there is no escort
        if numEscortShips <= 0 then
            start.Targets <- start.Targets |> List.filter (fun idx -> idx <> activate.Index && idx <> proximity.Index)

        // Override model escort
        if numEscortShips > 0 then
            for model, escort in List.zip escortModels (escort1 :: escortShips) do
                escort.Script <- model.Script
                escort.Model <- model.Model
                escort.Country <- Some country

        // Override model of cargo ships
        for model, ship in List.zip cargoModels (ship1 :: cargoShips) do
            ship.Model <- model.Model
            ship.Script <- model.Script
            ship.Country <- Some country

        // Disctinct waypoints for escort and cargo ships, first wp ahead of leader of each respective group
        let mkWp(v : PathVertex, idxObj) =
            let subst = Mcu.substId <| store.GetIdMapper()
            let wp = newWaypoint 1 v.Pos v.Ori v.Radius v.Speed v.Priority
            subst wp
            Mcu.addObjectLink wp idxObj
            wp
        let rec work idxObj (destWp : Mcu.McuWaypoint) xs =
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
                let tail = work idxObj destWp rest
                let wp = mkWp(v, idxObj)
                match tail with
                | (x : Mcu.McuWaypoint) :: _ -> Mcu.addTargetLink wp x.Index
                | [] -> Mcu.addTargetLink wp destWp.Index
                wp :: tail
        let midWps(path, idxObj, wp1 : Mcu.McuWaypoint, destWp) =
            match path with
            | v :: rest ->
                v.Pos.AssignTo wp1.Pos
                wp1.Ori.Y <- float v.Ori
                wp1.Speed <- v.Speed
                wp1.Priority <- v.Priority
                wp1.Radius <- v.Radius
                work idxObj destWp rest
            | [] ->
                invalidArg "path" "Must have at least two items"
        let setMidWps(wp1 : Mcu.McuWaypoint, destWp : Mcu.McuWaypoint, path, idxObj) =
            // Set target link from first waypoint
            wp1.Targets <- []
            let midWps = midWps(path, idxObj, wp1, destWp)
            match midWps with
            | x :: _ -> Mcu.addTargetLink wp1 x.Index
            | [] -> Mcu.addTargetLink wp1 destWp.Index
            midWps
        let midWpsEscort =
            if numEscortShips > 0 then
                let path =
                    try
                        pathEscort
                        |> fst
                        |> List.tail
                    with _ -> []
                setMidWps(wp1e, destWpE, path, escort1.LinkTrId)
            else
                []
        let midWpsCargo =
            let path =
                try
                    pathCargo
                    |> fst
                    |> List.tail
                with _ -> []
            setMidWps(wp1, destWp, path, ship1.LinkTrId)
        let midWps = midWpsEscort @ midWpsCargo

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
                for rank, ship in Seq.indexed (ship1 :: cargoShips) do
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
          Ships = ship1 :: cargoShips
          Escort = if numEscortShips > 0 then escort1 :: escortShips else []
          All = { new McuUtil.IMcuGroup with
                      member x.Content = group @ midWps
                      member x.LcStrings = []
                      member x.SubGroups = [ iconCover.All; iconAttack.All; startEvent.All; arrivedEvent.All ] @ (destroyedEvents |> List.map (fun ev -> ev.All))
          }
        }
