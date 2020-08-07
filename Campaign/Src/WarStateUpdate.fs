// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2019 Johann Deneux <johann.deneux@gmail.com>
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

namespace Campaign.WarStateUpdate

open System.Numerics
open VectorExtension

open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.BasicTypes
open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.Buildings
open Campaign.Pilots
open Campaign.Targets

// We use explicit command and result types for all changes to the state of the war.
// This makes it easier to present relevant data to players and help them understand what effect their actions have.

/// Commands to change a WarState
type Commands =
    // Damage a part of a building or a bridge
    | DamageBuildingPart of Instance: BuildingInstanceId * Part: int * Damage: float32
    // Repair a part of a building or a bridge
    | RepairBuildingPart of Instance: BuildingInstanceId * Part: int * Healing: float32
    // Remove a plane from an airfield
    | RemovePlane of AirfieldId * PlaneModelId * Health: float32
    // Add a plane to an airfield
    | AddPlane of AirfieldId * PlaneModelId * Health: float32
    // Add ground forces to a region
    | AddGroundForces of RegionId * CoalitionId * Amount: float32<MGF>
    // Destroy ground forces in a region
    | DestroyGroundForces of RegionId * CoalitionId * Amount: float32<MGF>
    // Move ground forces from a region to another
    | MoveGroundForces of RegionId * RegionId * CoalitionId * Amount: float32<MGF>
    // Set the owner of a region, typically after a conquest
    | SetRegionOwner of RegionId * CoalitionId option
    // Advance time
    | AdvanceTime of System.TimeSpan
    // Update player registration
    | UpdatePlayer of Guid: string * NickName: string
    // Update player ban status
    | UpdatePlayerBan of Guid: string * Ban: BanStatus
    // Register flight and health status of pilot
    | RegisterPilotFlight of PilotId * FlightRecord * PilotHealth

/// Interesting data to report from execution of commands
type Results =
    | UpdatedStorageValue of Instance: BuildingInstanceId * Amount: float32<M^3>
    | UpdatedPlanesAtAirfield of AirfieldId * Map<PlaneModelId, float32>
    | UpdatedGroundForces of RegionId * CoalitionId * float32<MGF>
    | RegionOwnerSet of RegionId * CoalitionId option
    | TimeSet of System.DateTime
    | PlayerUpdated of NickName: string
    | PlayerBanUpdated of NickName: string * Ban: BanStatus
    | PilotUpdated of Pilot

module Results =
    let asString (war : IWarStateQuery) result =
        match result with
        | UpdatedStorageValue(BuildingInstanceId orPos as bid, amount) ->
            let buildingDescr =
                match war.World.Buildings.TryGetValue bid with
                | true, building ->
                    sprintf "Building %s" building.Properties.Model
                | false, _ ->
                    match war.World.Bridges.TryGetValue bid with
                    | true, bridge ->
                        sprintf "Bridge %s" bridge.Properties.Model
                    | false, _ ->
                        "Unknown bridge or building"
            sprintf "%s at %4.0f, %4.0f now has capacity %1.0f" buildingDescr orPos.Pos.X orPos.Pos.Y amount
        | UpdatedPlanesAtAirfield(AirfieldId name, planes) ->
            let planeStr =
                planes
                |> Map.toSeq
                |> Seq.filter (fun (_, qty) -> qty > 0.0f)
                |> Seq.map (fun (PlaneModelId plane, qty) -> sprintf "%s: %1.0f" plane qty)
                |> String.concat ", "
            sprintf "Airfield %s now has the following planes: %s" name planeStr
        | UpdatedGroundForces(region, coalition, forces) ->
            sprintf "Ground forces of %s in %s is now at %0.1f" (string region) (string coalition) forces
        | RegionOwnerSet(rid, None) ->
            sprintf "%s has become neutral" (string rid)
        | RegionOwnerSet(rid, Some coalition) ->
            sprintf "%s is now controlled by %s" (string rid) (string coalition)
        | TimeSet t ->
            sprintf "Time set to %s" (t.ToString("F"))
        | PlayerUpdated(nickName) ->
            sprintf "Registration of %s was updated" nickName
        | PlayerBanUpdated(nickName, ban) ->
            sprintf "Status of %s is %s" nickName (string ban)
        | PilotUpdated(pilot) ->
            sprintf "Pilot %s %s updated, who is now %s" pilot.PilotFistName pilot.PilotLastName (string pilot.Health)

module CommandExecution =
    type IWarState with
        /// Apply damage to or repair a part of a building. Return new storage volume of the whole building.
        member this.ChangeHealth(bid, part, delta) =
            let health = this.GetBuildingPartHealthLevel(bid, part)
            let health = health + delta |> max 0.0f |> min 1.0f
            let isBridge = this.World.Bridges.ContainsKey(bid)
            this.SetBuildingPartHealthLevel(bid, part, health)
            let store =
                if isBridge then
                    0.0f<M^3>
                else
                    this.GetBuildingCapacity(bid)
            store

        /// Add or remove planes from an airfield, return all the planes at that airfield after the change
        member this.ChangePlanes(afid, plane, delta) =
            let qty =
                this.GetNumPlanes(afid, plane) + delta
                |> max 0.0f
            this.SetNumPlanes(afid, plane, qty)
            this.GetNumPlanes(afid)

        /// Add or remove forces of a coalition in a region, return the new amount of forces
        member this.ChangeGroundForces(rid, coalition, delta) =
            let forces =
                this.GetGroundForces(coalition, rid) + delta
                |> max 0.0f<MGF>
            this.SetGroundForces(coalition, rid, forces)
            forces

    type Commands with
        /// Execute commands on a WarState. Return the result of the command.
        member this.Execute(state : IWarState) =
            match this with
            | DamageBuildingPart(bid, part, dmg) ->
                let storage = state.ChangeHealth(bid, part, -dmg)
                [ UpdatedStorageValue(bid, storage) ]
            | RepairBuildingPart(bid, part, heal) ->
                let storage = state.ChangeHealth(bid, part, heal)
                [ UpdatedStorageValue(bid, storage) ]
            | AddPlane(afid, plane, health) ->
                let newStatus = state.ChangePlanes(afid, plane, health)
                [ UpdatedPlanesAtAirfield(afid, newStatus) ]
            | RemovePlane(afid, plane, health) ->
                let newStatus = state.ChangePlanes(afid, plane, -health)
                [ UpdatedPlanesAtAirfield(afid, newStatus) ]
            | AddGroundForces(rid, coalition, amount) ->
                let newForces = state.ChangeGroundForces(rid, coalition, amount)
                [ UpdatedGroundForces(rid, coalition, newForces) ]
            | DestroyGroundForces(rid, coalition, destroyed) ->
                let newForces = state.ChangeGroundForces(rid, coalition, -destroyed)
                [ UpdatedGroundForces(rid, coalition, newForces) ]
            | MoveGroundForces(start, destination, coalition, forces) ->
                let beforeMove = state.GetGroundForces(coalition, start)
                let startForces = state.ChangeGroundForces(start, coalition, -forces)
                let actuallyMoved = beforeMove - startForces
                let destinationForces = state.ChangeGroundForces(destination, coalition, actuallyMoved)
                [ UpdatedGroundForces(start, coalition, startForces)
                  UpdatedGroundForces(destination, coalition, destinationForces) ]
            | SetRegionOwner(rid, owner) ->
                state.SetOwner(rid, owner)
                [ RegionOwnerSet(rid, owner) ]
            | AdvanceTime span ->
                let newTime = state.Date + span
                state.SetDate(newTime)
                [ TimeSet(newTime) ]
            | UpdatePlayer(guid, nickName) ->
                let player =
                    state.TryGetPlayer(guid)
                    |> Option.defaultValue 
                        {
                            Guid = guid
                            Name = nickName
                            OtherNames = Set.empty
                            BanStatus = BanStatus.Clear
                        }
                let player =
                    if player.Name = nickName then
                        player
                    else
                        { player with
                            Name = nickName
                            OtherNames = player.OtherNames.Add(player.Name)
                        }
                state.UpdatePlayer(player)
                [ PlayerUpdated nickName ]
            | UpdatePlayerBan(guid, ban) ->
                let player =
                    state.TryGetPlayer(guid)
                    |> Option.defaultValue 
                        {
                            Guid = guid
                            Name = ""
                            OtherNames = Set.empty
                            BanStatus = BanStatus.Clear
                        }
                let player = { player with BanStatus = ban }
                state.UpdatePlayer(player)
                [ PlayerBanUpdated(player.Name, ban) ]
            | RegisterPilotFlight(pid, flight, health) ->
                let pilot = state.GetPilot(pid)
                let pilot = { pilot with Flights = pilot.Flights @ [flight]; Health = health }
                state.UpdatePilot(pilot)
                [ PilotUpdated(pilot) ]
