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

namespace Campaign.WebController.Dto

type HealthStatus =
    | Healthy
    | Dead
    | Injured of {| Until : DateTime |}

type ReturnStatus =
    | LandedAtAirfield of string
    | CrashedInFriendlyTerritory
    | CrashedInEnemyTerritory

type TargetType =
    | Ship of string
    | Vehicle of string
    | Artillery of string
    | ParkedPlane of string
    | Plane of string
    | Building of string
    | Bridge of string

type DamagedTarget =
    {
        Amount : float
        Ammo : string
        Target : TargetType
    }

type MissionRecord =
    {
        StartAirfield : string
        StartDate : DateTime
        EndDate : DateTime
        DamagedTargets : DamagedTarget list
        AirKills : int
        ReturnStatus : ReturnStatus
        Plane : string
        PlaneHealth : float
    }

type Rank =
    {
        RankName : string
        RankAbbrev : string
    }

type Award =
    {
        AwardName : string
        Description : string
    }

type Pilot =
    {
        Id : string
        Rank : string
        RankAbbrev : string
        FirstName : string
        LastName : string
        Country : string
        PlayerName : string
        Health : HealthStatus
        Flights : int
        AirKills : int
    }

type BanStatus =
    | NotBanned
    | Banned of {| Until : DateTime |}

type HashedGuid =
    {
        Guid : string
    }
with
    static member Create(guid) = { Guid = guid }

type Player =
    {
        Name : string
        Guid : HashedGuid
        BanStatus : BanStatus
        Pilots : string list
    }
