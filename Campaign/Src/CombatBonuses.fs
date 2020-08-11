// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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

/// Missions are simulated events whose outcome changes the state of war.
/// Success rate is affected by player actions.
namespace Campaign.CombatBonuses

open System

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.NewWorldDescription
open Campaign.Targets
open Campaign.WarState
open Util
open Campaign.Pilots

/// Domains of combat affected by experience bonuses
type ExperienceDomain =
    | AirSupremacy of PlaneModelId // Attacks by a specific plane model on fighters
    | Interception of PlaneModelId // Attacks by a specific plane model on bombers and ground attackers
    | Bombing of PlaneModelId // Attacks by a specific plane model using bombs
    | Strafing of PlaneModelId // Attacks by a specific plane model using other means

/// Experience bonuses granted by successful flight records
type ExperienceBonus =
    {
        Start : RegionId
        Domain : ExperienceDomain
        Bonus : float32
    }
with
    member this.Key =
        this.Start, this.Domain

    /// Compute the contributions from a player
    static member ContributedByPilot(war : IWarStateQuery, pilot : PilotId) =
        let pilot = war.GetPilot(pilot)
        let bonuses =
            seq {
                for flight in pilot.Flights do
                    let plane = war.World.PlaneSet.[flight.Plane]
                    let airfield = war.World.Airfields.[flight.Start]
                    for target, ammo, amount in flight.TargetsDamaged do
                        match target, ammo with
                        | Air plane2, _ ->
                            let plane2 = war.World.PlaneSet.[plane2]
                            match plane2.Kind with
                            | PlaneType.Fighter ->
                                {
                                    Start = airfield.Region
                                    Bonus = 0.01f * amount
                                    Domain = AirSupremacy plane.Id
                                }
                            | PlaneType.Attacker | PlaneType.Bomber | PlaneType.Transport ->
                                {
                                    Start = airfield.Region
                                    Bonus = 0.01f * amount
                                    Domain = Interception plane.Id
                                }
                        | _, AmmoName "explosion" ->
                            {
                                Start = airfield.Region
                                Bonus = 0.01f * amount
                                Domain = Bombing plane.Id
                            }
                        | _, _ ->
                            {
                                Start = airfield.Region
                                Bonus = 0.01f * amount
                                Domain = Strafing plane.Id
                            }
            }
        bonuses

/// Mapping from start airfields, objective regions and experience domains to bonus values
type ExperienceBonuses =
    {
        Bonuses : Map<RegionId * ExperienceDomain, float32>
    }
with
    static member Create(war : IWarStateQuery) =
        let bonuses =
            {
                Bonuses = Map.empty
            }
        let pilots =
            war.Pilots
            |> List.groupBy (fun pilot -> war.TryGetPilotHome(pilot.Id))
            |> List.choose (function (Some af, pilots) -> Some(af, pilots) | _ -> None)
            |> List.map (fun (af, pilots) ->
                // Retain the most experienced healthy pilot, for each player
                let pilots =
                    pilots
                    |> List.filter (fun pilot ->
                        // Remove banned and injured/dead pilots
                        let isBanned =
                            lazy
                                war.TryGetPlayer(pilot.PlayerGuid)
                                |> Option.map (fun player -> match player.BanStatus with BanStatus.Banned -> true | _ -> false)
                                |> Option.defaultValue false
                        pilot.Health = Healthy && not isBanned.Value)
                    |> List.groupBy(fun pilot -> pilot.PlayerGuid) // At most one pilot per player per airfield
                    |> List.map (fun (_, pilots) -> pilots |> List.maxBy (fun pilot -> countCompletedFlights pilot.Flights)) // Retain the pilot with the most completed flights
                // Result
                af, pilots
            )
            |> dict
        (bonuses, war.World.Airfields.Values)
        ||> Seq.fold (fun bonuses airfield ->
            let contributions =
                pilots.TryGetValue(airfield.AirfieldId)
                |> Option.ofPair
                |> Option.defaultValue []
                |> Seq.collect (fun pilot -> if pilot.Health = Healthy then ExperienceBonus.ContributedByPilot(war, pilot.Id) else Seq.empty)
            contributions
            |> Seq.fold (fun bonuses contrib -> bonuses.Update contrib) bonuses
        )

    member this.GetBonus(key) =
        let x =
            this.Bonuses.TryFind(key)
            |> Option.defaultValue 0.0f
        1.0f - 1.0f / exp(x)

    member this.Update(bonus : ExperienceBonus) =
        let oldValue =
            this.GetBonus(bonus.Key)
        { this with
            Bonuses = this.Bonuses.Add(bonus.Key, oldValue + bonus.Bonus) }

