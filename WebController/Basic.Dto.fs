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

type Version =
    {
        Major : int
        Minor : int
    }

type Vector2 =
    { 
        X : float32
        Y : float32
    }

type OrientedPosition =
    {
        Position : Vector2
        Altitude : float32
        Rotation : float32
    }

type DateTime =
    {
        Year : int
        Month : int
        Day : int
        Hour : int
        Minute : int
    }