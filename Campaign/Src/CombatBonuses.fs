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

open Util

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel
open Campaign.Common.Targets

open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.Pilots

type TargetSize = SmallTarget | LargeTarget
with
    member this.Rank =
        match this with 
        | SmallTarget -> 1
        | LargeTarget -> 0

type TargetMobility = Nimble | Moving | Static
with
    member this.Rank =
        match this with
        | Nimble -> 2
        | Moving -> 1
        | Static -> 0

type TargetSpace = Ground | Water | Air
with
    member this.Rank =
        match this with
        | Air -> 0
        | Ground -> 1
        | Water -> 2

type TargetDifficulty =
    {
        Size : TargetSize
        Mobility : TargetMobility
        Space : TargetSpace
    }
with
    member this.Rank = (this.Space, this.Mobility, this.Size.Rank)

    static member OfTarget(target : TargetType) =
        match target with
        | Truck | ArmoredCar -> { Size = SmallTarget; Mobility = Nimble; Space = Ground }
        | Tank -> { Size = SmallTarget; Mobility = Moving; Space = Ground }
        | Train -> { Size = LargeTarget; Mobility = Moving; Space = Ground }
        | Artillery -> { Size = SmallTarget; Mobility = Static; Space = Ground }
        | TroopLandingShip -> { Size = SmallTarget; Mobility = Moving; Space = Water }
        | CargoShip | Battleship -> { Size = LargeTarget; Mobility = Moving; Space = Water }
        | Bridge _ -> { Size = LargeTarget; Mobility = Static; Space = Water }
        | Building _ -> { Size = LargeTarget; Mobility = Static; Space = Ground }
        | TargetType.Air(_, Fighter) -> { Size = SmallTarget; Mobility = Nimble; Space = Air }
        | TargetType.Air(_, Attacker) -> { Size = SmallTarget; Mobility = Moving; Space = Air }
        | TargetType.Air(_, (Bomber | Transport)) -> { Size = LargeTarget; Mobility = Moving; Space = Air }
        | ParkedPlane _ -> { Size = SmallTarget; Mobility = Static; Space = Ground }

// Note: current method of data extraction from logs doesn't distinguish very well between ammo type.
// Ideally we would want: small caliber guns, cannon rounds, rockets, bombs (large, medium, small, cluster)
type GroundAttackAmmo = Bombs | Guns

module AmmoPatterns =
    /// Identify "explosion" from the logs as damage by bombs, everything else as guns
    let (|AsGroundAttackAmmo|) =
        function
        | AmmoName "explosion" -> AsGroundAttackAmmo Bombs
        | _ -> AsGroundAttackAmmo Guns

/// Domains of combat affected by experience bonuses
type ExperienceDomain =
    /// Attacks by a specific plane model on other planes in the air
    | AirSupremacy of PlaneModelId * TargetDifficulty
    /// Attacks on ground targets by a specific plane model
    | GroundAttack of PlaneModelId * TargetDifficulty * GroundAttackAmmo
with
    /// Check if the bonus of this domain can be counted as bonus for another domain
    member this.Covers other =
        match this, other with
        | AirSupremacy(plane1, diff1), AirSupremacy(plane2, diff2) ->
            plane1 = plane2 && diff1.Rank >= diff2.Rank
        | GroundAttack(plane1, diff1, ammo1), GroundAttack(plane2, diff2, ammo2) ->
            plane1 = plane2 && ammo1 = ammo2 && diff1.Rank >= diff2.Rank
        | _ ->
            false

/// Experience bonuses granted by successful flight records
type ExperienceBonus =
    {
        Start : AirfieldId
        Domain : ExperienceDomain
        Bonus : float32
    }
with
    /// Compute the contributions from a player
    static member ContributedByPilot(war : IWarStateQuery, pilot : PilotId) =
        let pilot = war.GetPilot(pilot)
        let bonuses =
            seq {
                for flight in pilot.Flights do
                    let plane = war.World.PlaneSet.[flight.Plane]
                    let airfield = war.World.Airfields.[flight.Start]
                    for target, ammo, amount in flight.TargetsDamaged do
                        let domain =
                            match target, ammo with
                            | TargetType.Air _, _ ->
                                AirSupremacy(plane.Id, TargetDifficulty.OfTarget target)
                            | _, AmmoPatterns.AsGroundAttackAmmo ammo ->
                                GroundAttack(plane.Id, TargetDifficulty.OfTarget target, ammo)
                        yield {
                            Start = airfield.AirfieldId
                            Bonus = 0.01f * amount
                            Domain = domain
                        }
            }
        bonuses

/// Mapping from start airfields, objective regions and experience domains to bonus values
type ExperienceBonuses =
    {
        Bonuses : Map<AirfieldId, (ExperienceDomain * float32) list>
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
                        // Remove injured/dead pilots
                        pilot.Health = Healthy)
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

    member this.GetBonus(afId, domain) =
        let x =
            this.Bonuses.TryFind(afId)
            |> Option.defaultValue []
            |> List.filter (fun (bonus, _) -> bonus.Covers domain)
            |> List.sortByDescending snd
            |> List.tryHead
            |> Option.map snd
            |> Option.defaultValue 0.0f
        x

    member this.Update(bonus : ExperienceBonus) =
        let oldList =
            this.Bonuses.TryFind(bonus.Start)
            |> Option.defaultValue []
        let oldValue =
            oldList
            |> List.tryFind (fun (entry, _) -> entry = bonus.Domain)
            |> Option.map snd
            |> Option.defaultValue 0.0f
        let newList =
            (bonus.Domain, oldValue + bonus.Bonus) ::
            List.filter (fun (domain, _) -> domain <> bonus.Domain) oldList
        { this with
            Bonuses = this.Bonuses.Add(bonus.Start, newList) }

