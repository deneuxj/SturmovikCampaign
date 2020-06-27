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

type Weather =
    {
        CloudDensity : float32
        CloudHeight : float32
        CloudThickness : float32
        Precipitation : float32
        WindSpeed : float32
        WindDirectionTo : float32
        Turbulence : float32
        Temperature : float32
        Pressure : float32
    }

type BuildingStatus =
    {
        Position : OrientedPosition
        HealthLevel : float32
        FunctionalityLevel : float32
    }

type GroundForces =
    {
        Region : string
        Forces : float32
        Coalition : string
    }

type TransportCapacity =
    {
        RegionA : string
        RegionB : string
        Capacity : float32
    }

type WarState =
    {
        Date : DateTime
        Weather : Weather
        BuildingHealth : BuildingStatus[]
        BridgeHealth : BuildingStatus[]
        GroundForces : GroundForces[]
        RoadTransport : TransportCapacity[]
        RailTransport : TransportCapacity[]
        SupplyStatus : Map<string, float32>
        Planes : Map<string, Map<string, float32>>
        RegionOwner : Map<string, string>
    }

type Command =
    {
        Verb : string
        Args : Map<string, obj>
    }

type Result =
    {
        ChangeDescription : string
        Values : Map<string, obj>
    }

type SimulationStep =
    {
        Description : string
        Command : Command[]
        Results : Result[]
    }