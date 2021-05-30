// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2021 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

/// Extension methods to IWarStateQuery and other types that are used for extracting results from game logs.
module Campaign.MissionResultsExtensions

open FSharp.Control
open FSharp.Json
open System.Numerics

open Util
open Util.RegexActivePatterns
open VectorExtension

open Campaign.Common.BasicTypes
open Campaign.Common.Targets.ActivePatterns
open Campaign.Common.Targets

open Campaign.GameLogEvents
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.WarStateUpdate.CommandExecution
open Campaign.Pilots
open System.Collections.Generic
open Campaign.MissionGen.MissionFileGeneration

/// Transform a name from a log: lower case, delete all non-alphanum chars.
let normalizeLogName (name : string) =
    name.ToLowerInvariant()
    |> Array.ofSeq
    |> Array.filter System.Char.IsLetterOrDigit
    |> System.String

/// Transform a script or model name: retain filename only, lower case, delete all non-alphanum chars.
let normalizeScript (path : string) =
    System.IO.Path.GetFileNameWithoutExtension(path).ToLowerInvariant()
    |> Array.ofSeq
    |> Array.filter System.Char.IsLetterOrDigit
    |> System.String

type IWarStateQuery with
    /// Get the nearest airfield to a position
    member this.TryGetNearestAirfield((x, _, z) : Position, coalitionFilter : CoalitionId option) =
        let p = Vector2(x, z)
        try
            this.World.Airfields.Values
            |> Seq.filter (fun af ->
                match coalitionFilter, this.GetOwner(af.Region) with
                | None, _ -> true
                | Some c, Some owner -> c = owner
                | Some _, None -> false)
            |> Seq.map (fun af -> af, (af.Position - p).Length())
            |> Seq.minBy snd
            |> Some
        with _ -> None

    /// Try to get a plane by its name as it appears in the logs
    member this.TryGetPlane(logName : string) =
        let logName = normalizeLogName logName
        this.World.PlaneSet.Values
        |> Seq.tryFind (fun plane -> logName.Contains(normalizeLogName plane.LogName))
        |> Option.orElseWith (fun () ->
            // Look in plane alternatives, and if found return the plane it replaces.
            // There should be at most one such plane, otherwise the plane that's returned is whichever is matched first.
            this.World.PlaneAlts
            |> Seq.tryPick (fun kvp ->
                let plane, planeAlts = kvp.Key, kvp.Value
                let found =
                    planeAlts
                    |> List.exists (fun planeAlt -> logName.Contains(normalizeLogName planeAlt.LogName))
                if found then
                    this.World.PlaneSet.TryGetValue plane
                    |> Option.ofPair
                else
                    None
            )
        )

    /// Get all buildings and bridges that match a name, have the given subpart as reported as important, and cover the given position.
    member this.GetBuildingsAt(logName : string, part : int, (x, _, z) : Position) =
        let logName = normalizeLogName logName
        let pos = Vector2(x, z)
        Seq.append this.World.Buildings.Values this.World.Bridges.Values
        |> Seq.filter (fun building ->
            match this.World.BuildingProperties.TryGetValue(building.Script) with
            | true, properties ->
                normalizeScript(properties.Script) = logName &&
                List.contains part properties.SubParts &&
                pos.IsInConvexPolygon properties.Boundary
            | false, _ ->
                false)

    /// Try to get the static plane at the given position, and the airfield it is assigned to.
    member this.TryGetStaticPlaneAt(logName : string, ((x, _, z) as pos)) =
        // We don't check that there actually is a plane at that position, only that's it's "close enough" to the nearest airfield.
        match this.TryGetNearestAirfield(pos, None) with
        | Some (af, dist) when dist < 3000.0f ->
            let logName = normalizeLogName logName
            this.World.PlaneSet.Values
            |> Seq.tryFind (fun plane -> normalizeScript(plane.StaticScriptModel.Script) = logName)
            |> Option.map (fun plane -> af.AirfieldId, plane)
        | _ ->
            None

    /// Try to get the region at some position
    member this.TryGetRegionAt((x, _, z)) =
        let pos = Vector2(x, z)
        this.World.Regions.Values
        |> Seq.tryFind (fun region -> pos.IsInConvexPolygon region.Boundary)

    /// Get an existing pilot of a player given the starting airfield, or create a new pilot
    member this.GetPlayerPilotFrom(playerGuid : string, afId : AirfieldId, country, isFemale) =
        let logger = NLog.LogManager.GetCurrentClassLogger()

        let candidates =
            this.GetPlayerPilots(playerGuid)
            |> Seq.filter (fun pilot -> pilot.Country = country && pilot.IsFemale = isFemale)
            |> Seq.filter (fun pilot -> this.IsPilotHealthy(pilot.Id))
            // Filter out captured pilots
            |> Seq.filter (fun pilot ->
                this.GetPilot(pilot.Id).Flights
                |> List.tryLast
                |> Option.map (fun flight -> flight.Return <> CrashedInEnemyTerritory)
                |> Option.defaultValue true)
            |> Seq.filter (fun pilot -> match this.TryGetPilotHome(pilot.Id) with None -> true | Some home -> home = afId)
            |> Seq.sortByDescending (fun pilot -> pilot.LatestFlightStart, this.TryGetPilotHome(pilot.Id) = Some afId)
            |> Seq.cache

        logger.Debug(sprintf "Pilots of player %s: %s" playerGuid (candidates |> Seq.map (fun pilot -> pilot.FullName) |> String.concat ", "))

        Seq.tryHead candidates
        |> Option.defaultWith (fun () -> this.NewPilot(playerGuid, country, isFemale))

    /// Try to get the coalition of a country from its event log value
    member this.TryGetCoalitionOfCountry(country : int) =
        let country = CountryId.FromMcuValue(enum country)
        country
        |> Option.bind (fun country ->
            this.World.Countries.TryGetValue(country)
            |> Option.ofPair)

    member this.HealthStatusFromHealthLevel(timeStamp : System.TimeSpan, pilotHealth : float32) =
        let injury =
            1.0f - pilotHealth
        let healthStatus =
            if injury < 0.01f then
                Healthy
            else if injury >= 1.0f then
                Dead
            else
                this.Date + timeStamp + System.TimeSpan(int(ceil(injury * 30.0f)), 0, 0, 0)
                |> Injured
        healthStatus

type AmmoType with
    static member FromLogName(logName : string) : AmmoType = AmmoName logName

type Binding with
    /// Try to get the vehicle target type of the object in this binding
    member this.TryGetVehicleTargetType(war : IWarStateQuery) =
        match this.Name with
        | TargetTypeByName tt -> Some tt
        | _ ->
        let logName = normalizeLogName this.Typ
        let country = CountryId.FromMcuValue (enum this.Country)
        let countryGroundUnits =
            war.World.GroundUnitsList
            |> Seq.filter (fun vehicle ->
                match war.World.CountryOfGroundUnit.TryGetValue(vehicle.Id) with
                | false, _ -> false
                | true, c -> Some c = country)
        let countryShips =
            war.World.ShipsList
            |> List.collect (fun (country2, ships) -> if country = Some country2 then ships else [])

        let dynVehicle =
            lazy
                countryGroundUnits
                |> Seq.filter(fun vehicle -> Some logName = (vehicle.DynamicScriptModel |> Option.map(fun data -> normalizeScript data.Script)))
                |> Seq.tryPick(fun vehicle ->
                    [ TargetType.Tank; TargetType.ArmoredCar; TargetType.Artillery; TargetType.Truck ]
                    |> List.tryFind (fun tt -> tt.IsCompatibleWith(vehicle))
                )
        let staVehicle =
            lazy
                countryGroundUnits
                |> Seq.filter(fun vehicle -> Some logName = (vehicle.StaticScriptModel |> Option.map(fun data -> normalizeScript data.Script)))
                |> Seq.tryPick(fun vehicle ->
                    [ TargetType.Tank; TargetType.ArmoredCar; TargetType.Artillery; TargetType.Truck ]
                    |> List.tryFind (fun tt -> tt.IsCompatibleWith(vehicle))
                )

        let convoyVehicle =
            lazy
                seq {
                    for v in ConvoyMember.All do
                        for c in CountryId.All do
                            yield v, c
                }
                |> Seq.tryFind (fun (vehicle, country) -> this.Name = vehicle.Name || normalizeScript (vehicle.StaticVehicleData country).Script = logName)
                |> Option.map (
                    fst
                    >> function
                    | Train -> TargetType.Train
                    | Truck -> TargetType.Truck
                    | Tank -> TargetType.Tank
                    | ArmoredCar -> TargetType.ArmoredCar
                    | AntiAirTruck -> TargetType.Artillery
                    | StaffCar -> TargetType.ArmoredCar
                )

        let ship =
            lazy
                countryShips
                |> Seq.filter(fun vehicle -> logName = vehicle.ScriptModel.Script)
                |> Seq.tryPick(fun vehicle ->
                    [ TargetType.Battleship; TargetType.CargoShip; TargetType.TroopLandingShip ]
                    |> List.tryFind (fun tt -> tt.IsCompatibleWith(vehicle))
                )

        match convoyVehicle.Value with
        | Some x -> Some x
        | None ->
        match dynVehicle.Value with
        | Some x -> Some x
        | None ->
        match staVehicle.Value with
        | Some x -> Some x
        | None ->
        match ship.Value with
        | Some x -> Some x
        | None ->
            None


