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

/// A ship convoy with escort
type ShipConvoy = {
    Start : Mcu.McuTrigger
    Arrived : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    IconCover : IconDisplay
    IconAttack : IconDisplay
    All : McuUtil.IMcuGroup
}
with
    /// <summary>
    /// Create a ship convoy with escort
    /// </summary>
    /// <param name="store">Provides unique ids for MCUs</param>
    /// <param name="lcStore">Provides unique ids for text</param>
    /// <param name="path">Waypoints the convoy will sail along</param>
    /// <param name="country">Country owning the ships</param>
    /// <param name="eventName">Base event name for convoy start, arrival and destruction.</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, path : (Vector2 * float32) list, country : Mcu.CountryValue, eventName) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("ShipConvoy").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.START
        let arrived = getTriggerByName group T.Blocks.ARRIVED
        let killed = getTriggerByName group T.Blocks.KILLED
        let wp1 = getWaypointByName group T.Blocks.WP1
        let destWp = getWaypointByName group T.Blocks.Destination
        let escort1 = getVehicleByName group T.Blocks.Escort1
        let escort2 = getVehicleByName group T.Blocks.Escort2
        let ship1 = getVehicleByName group T.Blocks.Cargo1
        let ship2 = getVehicleByName group T.Blocks.Cargo2
        // Override escort
        do
            let model =
                match country with
                | Mcu.CountryValue.Russia -> vehicles.RussianTorpedoBoat
                | Mcu.CountryValue.Germany -> vehicles.GermanTorpedoBoat
                | _ -> failwith "Unknown country value"
            for escort in [ escort1; escort2 ] do
                escort.Script <- model.Script
                escort.Model <- model.Model
                escort.Country <- country
        // Set country of cargo ships
        for ship in [ ship1; ship2 ] do
            ship.Country <- country
        // Position of all nodes
        let refPoint = Vector2(float32 wp1.Pos.X, float32 wp1.Pos.Z)
        let dv, rot =
            match path with
            | (pos, dir) :: _ ->
                pos - refPoint, dir
            | _ ->
                invalidArg "path" "Must have at least two items"
        for mcu in group do
            ((Vector2.FromMcu(mcu.Pos) - refPoint).Rotate(rot) + dv + refPoint).AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float rot
        // waypoints
        let mkWp(pos, dir) =
            let subst = Mcu.substId <| store.GetIdMapper()
            let wp = newWaypoint 1 pos dir wp1.Radius wp1.Speed wp1.Priority
            subst wp
            Mcu.addObjectLink wp escort1.LinkTrId
            Mcu.addObjectLink wp ship1.LinkTrId
            wp
        let rec work xs =
            match xs with
            | [pos : Vector2, dir : float32] ->
                pos.AssignTo destWp.Pos
                destWp.Ori.Y <- float dir
                []
            | [] ->
                invalidArg "path" "Must have at least two items"
            | (pos, dir) :: rest ->
                let tail = work rest
                let wp = mkWp(pos, dir)
                match tail with
                | (x : Mcu.McuWaypoint) :: _ -> Mcu.addTargetLink wp x.Index
                | [] -> Mcu.addTargetLink wp destWp.Index
                wp :: tail
        let midWps =
            match path with
            | (pos, dir) :: rest ->
                pos.AssignTo wp1.Pos
                wp1.Ori.Y <- float dir
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
        let destroyedEventName = sprintf "%s-K-0" eventName
        let destroyedEvent = EventReporting.Create(store, country, Vector2.FromMcu escort1.Pos + Vector2(0.0f, 100.0f), destroyedEventName)
        // Connections to icons
        for icon in [ iconAttack; iconCover] do
            Mcu.addTargetLink start icon.Show.Index
            Mcu.addTargetLink killed icon.Hide.Index
            Mcu.addTargetLink arrived icon.Hide.Index
        // Connections to events
        Mcu.addTargetLink start startEvent.Trigger.Index
        Mcu.addTargetLink arrived arrivedEvent.Trigger.Index
        Mcu.addTargetLink killed destroyedEvent.Trigger.Index
        // result
        let midWps = midWps |> List.map (fun x -> x :> Mcu.McuBase)
        { Start = start
          Arrived = arrived
          Killed = killed
          IconCover = iconCover
          IconAttack = iconAttack
          All = { new McuUtil.IMcuGroup with
                      member x.Content = group @ midWps
                      member x.LcStrings = []
                      member x.SubGroups = [ iconCover.All; iconAttack.All; startEvent.All; arrivedEvent.All ]
          }
        }
