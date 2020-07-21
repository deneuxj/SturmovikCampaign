module Campaign.MissionResults

open FSharp.Control
open System.Numerics

open Util
open VectorExtension

open Campaign.GameLogEvents
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.WorldDescription

type IWarStateQuery with
    /// Get the nearest airfield to a position
    member this.GetNearestAirfield((x, _, z) : Position) =
        let p = Vector2(x, z)
        this.World.Airfields.Values
        |> Seq.minBy (fun af -> (af.Position - p).Length())

    /// Try to get a plane by its name as it appears in the logs
    member this.TryGetPlane(logName : string) =
        this.World.PlaneSet.Values
        |> Seq.tryFind (fun plane -> logName.Contains(plane.LogName))

    /// Get all buildings that match a name, have the given subpart as reported as important, and cover the given position.
    member this.GetBuildingsAt(logName : string, part : int, (x, _, z) : Position) =
        let logName = logName.ToLowerInvariant()
        let pos = Vector2(x, z)
        Seq.append this.World.Buildings.Values this.World.Bridges.Values
        |> Seq.filter (fun building ->
            building.Properties.Model.ToLowerInvariant().Contains(logName) &&
            List.contains part building.Properties.SubParts &&
            pos.IsInConvexPolygon building.Boundary)

    /// Try to get the static plane at the given position, and the airfield it is assigned to.
    member this.TryGetStaticPlaneAt(logName : string, ((x, _, z) as pos)) =
        // We don't check that there actually is a plane at that position, only that's it's "close enough" to the nearest airfield.
        let af = this.GetNearestAirfield(pos)
        if (af.Position - Vector2(x, z)).Length() < 3000.0f then
            let logName = logName.ToLowerInvariant()
            this.World.PlaneSet.Values
            |> Seq.tryFind (fun plane -> logName.Contains(plane.StaticBasename.ToLowerInvariant()))
            |> Option.map (fun plane -> af.AirfieldId, plane)
        else
            None

/// The status of a plane piloted by a human pilot.
type FlightStatus =
    | Spawned of AirfieldId
    | InAir of TookOffFrom: AirfieldId
    | Landed of AirfieldId

/// Extract war state updade commands from the game logs.
let commandsFromLogs (state : IWarStateQuery) (logs : AsyncSeq<string>) =
    asyncSeq {
        let bindings = Seq.mutableDict []
        let pilotOf = Seq.mutableDict []
        let vehicleOf = Seq.mutableDict []
        let flights = Seq.mutableDict []
        let healthOf = Seq.mutableDict []

        for line in logs do
            match line with
            | ObjectEvent(_, ObjectBound binding) ->
                // Update mappings
                bindings.[binding.Id] <- binding
                healthOf.[binding.Id] <- 1.0f

            | PlayerEvent(_, PlayerTakesObject taken) ->
                // Update mappings
                pilotOf.[taken.VehicleId] <- taken
                vehicleOf.[taken.PilotId] <- taken
                flights.[taken.VehicleId] <- Spawned(state.GetNearestAirfield(taken.Position).AirfieldId)

                // Emit RemovePlane command
                match bindings.TryGetValue(taken.VehicleId) with
                | true, binding ->
                    match state.TryGetPlane(binding.Name) with
                    | Some plane ->
                        yield RemovePlane(state.GetNearestAirfield(taken.Position).AirfieldId, plane.Id, 1.0f)
                    | None ->
                        ()
                | false, _ ->
                    ()

            | ObjectEvent(_, ObjectTakesOff takeOff) ->
                // Update mappings
                match flights.TryGetValue(takeOff.Id) with
                | true, Spawned afId | true, Landed afId ->
                    flights.[takeOff.Id] <- InAir afId
                | _ ->
                    flights.[takeOff.Id] <- InAir (state.GetNearestAirfield(takeOff.Position).AirfieldId)

            | ObjectEvent(_, ObjectDamaged damaged) ->
                // Update mappings
                let health =
                    healthOf.TryGetValue(damaged.TargetId)
                    |> Option.ofPair
                    |> Option.defaultValue 1.0f
                let health = health - damaged.Damage
                healthOf.[damaged.TargetId] <- health
                // Emit DamageBuildingPart
                // Emit RemovePlane for damages to static planes
                match bindings.TryGetValue(damaged.TargetId) with
                | true, binding ->
                    for building in state.GetBuildingsAt(binding.Name, binding.Sub, damaged.Position) do
                        yield DamageBuildingPart(building.Id, binding.Sub, damaged.Damage)
                    match state.TryGetStaticPlaneAt(binding.Name, damaged.Position) with
                    | Some(afId, plane) ->
                        yield RemovePlane(afId, plane.Id, damaged.Damage)
                    | None ->
                        ()
                | _ ->
                    ()

            | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                // Emit AddPlane command
                match bindings.TryGetValue(missionEnded.VehicleId), flights.TryGetValue(missionEnded.VehicleId), healthOf.TryGetValue(missionEnded.VehicleId) with
                | (true, binding), (true, Landed afId), (true, health) when health > 0.0f ->
                    match state.TryGetPlane(binding.Name) with
                    | Some plane ->
                        yield AddPlane(afId, plane.Id, health)
                    | None ->
                        ()
                | _ ->
                    ()
                // Update mappings
                pilotOf.Remove(missionEnded.VehicleId) |> ignore
                vehicleOf.Remove(missionEnded.PilotId) |> ignore
                flights.Remove(missionEnded.VehicleId) |> ignore
                healthOf.Remove(missionEnded.VehicleId) |> ignore

            | _ ->
                ()
    }