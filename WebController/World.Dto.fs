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

/// Properties of a building or bridge
type BuildingProperties = 
    {
        /// Unique numerical identifier among BuildingProperties
        Id : int
        Model : string
        Script : string
        Durability : int
        NumParts : int
        /// Storage capacity of the entire building, or null for bridges
        Capacity : float32 option
    }

type BuildingInstance =
    {
        Position : OrientedPosition
        PropertiesId : int
    }

type Region =
    {
        Id : string
        Boundary : Vector2[]
        Position : Vector2
        Neighbours : string[]
        InitialOwner : string
        IsEntry : bool
        Buildings : BuildingInstance[]
    }

type Airfield =
    {
        Id : string
        Position : Vector2
        Region : string
        Buildings : BuildingInstance[]
    }

type PlaneModel =
    {
        Name : string
        Kind : string
        Roles : string[]
        Coalition : string
        Script : string
        Model : string
        BombCapacity : float32
        CargoCapacity : float32
    }

/// Description of the play area, in its initial pristine state.
/// In other words, damage and dynamic properties of the war are not stored here.
type World =
    {
        Scenario : string
        Map : string
        MapSouthWest : Vector2
        MapNorthEast : Vector2
        StartDate : DateTime
        Regions : Region[]
        Airfields : Airfield[]
        // Properties of buildings and bridges
        BuildingProperties : BuildingProperties[]
        Bridges : BuildingInstance[]
        PlaneSet : PlaneModel[]
    }