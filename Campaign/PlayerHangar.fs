// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.PlayerHangar

open WorldDescription
open PlaneModel
open System
open BasicTypes

let private logger = NLog.LogManager.GetCurrentClassLogger()

/// <summary>
/// The planes that a player took to an airfield. That player is allowed to take off in any of these planes, if the airfield has it available.
/// </summary>
type AirfieldHangar =
    { Airfield : AirfieldId
      Planes : Map<PlaneModel, float32> }
with
    /// <summary>
    /// Add a plane to the player's hangar at this airfield
    /// </summary>
    /// <param name="plane">Plane to add</param>
    /// <param name="qty">Quantity to add, can be less than 1.0 if the plane is damaged</param>
    member this.AddPlane(plane, qty) =
        let qty = max qty 0.0f
        let oldQty = this.Planes.TryFind(plane) |> Option.defaultValue 0.0f
        let newQty = oldQty + qty
        let planes = Map.add plane newQty this.Planes
        { this with Planes = planes }

    /// <summary>
    /// Remove a plane from the player's hangar at this airfield.
    /// If the player doesn't have "enough plane", i.e. no plane, or a damaged plane, the player has to pay for the plane using their reserve.
    /// </summary>
    /// <param name="qty">Health of the plane. Not necessarily 1.0f, as players can land damages planes and take off again in the same plane without respawning.
    member this.RemovePlane(plane, qty, costFactor) =
        let qty = max qty 0.0f
        let oldQty = this.Planes.TryFind(plane) |> Option.defaultValue 0.0f
        let newQty = oldQty - qty
        let credit, newQty =
            if newQty >= 0.0f then
                0.0f<E>, newQty
            else
                costFactor * newQty * plane.Cost, 0.0f
        let newQty = max newQty 0.0f
        let planes = Map.add plane newQty this.Planes
        { this with Planes = planes }, credit

/// <summary>
/// The planes that a player has at each airfield, and a reserve that can be used to spawn in planes that a player does not own.
/// </summary>
type PlayerHangar =
    { Player : Guid
      PlayerName : string
      Reserve : float32<E>
      Airfields : Map<AirfieldId, AirfieldHangar> }
with
    /// <summary>
    /// Add a plane to an airfield, normally after landing there.
    /// </summary>
    /// <param name="af">Airfield where the player landed.</param>
    /// <param name="plane">Plane model that landed.</param>
    /// <param name="qty">Health of the plane that landed.</param>
    member this.AddPlane(af, plane, qty) =
        let hangar = this.Airfields.TryFind(af) |> Option.defaultValue {Airfield = af; Planes = Map.empty}
        let hangar = hangar.AddPlane(plane, qty)
        { this with Airfields = Map.add af hangar this.Airfields }

    /// <summary>
    /// Try to remove a plane from an airfield, normally after taking off from there.
    /// If the player has insufficient reserve, return None. The campaign system will notify the player and cancel that flight.
    /// </summary>
    /// <param name="af">Airfield the player took off from</param>
    /// <param name="plane">Plane model that took off.</param>
    member this.TryRemovePlane(af, plane, qty, costFactor) =
        let hangar = this.Airfields.TryFind(af) |> Option.defaultValue {Airfield = af; Planes = Map.empty}
        let hangar, credit = hangar.RemovePlane(plane, qty, costFactor)
        let reserve = this.Reserve + credit
        if credit = 0.0f<E> || reserve >= 0.0f<E> then
            let afs = Map.add af hangar this.Airfields
            Some { this with Reserve = reserve; Airfields = afs }
        else
            None

    /// <summary>
    /// Remove planes from an airfield, and decrease cash reserve.
    /// </summary>
    member this.RemovePlane(af, plane, qty, cost) =
        let hangar = this.Airfields.TryFind(af) |> Option.defaultValue {Airfield = af; Planes = Map.empty}
        let hangar, _ = hangar.RemovePlane(plane, qty, 0.0f)
        let reserve = this.Reserve - cost
        let afs = Map.add af hangar this.Airfields
        { this with Reserve = reserve; Airfields = afs }

    /// Get the available planes at an airfield
    member this.ShowAvailablePlanes(af) =
        let hangar = this.Airfields.TryFind(af) |> Option.defaultValue {Airfield = af; Planes = Map.empty}
        [
            for kvp in hangar.Planes do
                if kvp.Value >= 1.0f then
                    yield kvp.Key.PlaneName
        ]

/// Get the total amount of planes of a given model at a given airfield
let getTotalPlanesReservedAtAirfield af plane hangars =
    hangars
    |> Map.toSeq
    |> Seq.choose (fun (_, hangar : PlayerHangar) -> hangar.Airfields.TryFind af)
    |> Seq.choose (fun afHangar -> afHangar.Planes.TryFind plane)
    |> Seq.sum

/// Get the price factor of a plane depending on its (over)booking level
let getPriceFactor af plane available hangars =
    let numReserved = getTotalPlanesReservedAtAirfield af plane hangars
    if available > numReserved then
        1.0f
    elif numReserved < 100.0f * Single.Epsilon then
        1.0f
    elif available < 100.0f * Single.Epsilon then
        Single.PositiveInfinity
    else
        2.0f * numReserved / available
    |> min 100.0f
    |> max 1.0f

open MBrace.FsPickler
open System.IO


let tryLoadHangars path =
    let serializer = FsPickler.CreateXmlSerializer()
    try
        use file = File.OpenText(path)
        serializer.Deserialize<Map<Guid, PlayerHangar>>(file) |> Some
    with
    | exc ->
        logger.Error(sprintf "Failed to parse state.xml: %s" exc.Message)
        None


let saveHangars path (hangars : Map<Guid, PlayerHangar>) =
    let serializer = FsPickler.CreateXmlSerializer(indent=true)
    use file = File.CreateText(path)
    serializer.Serialize(file, hangars)


let guidToStrings hangars =
    hangars
    |> Map.toSeq
    |> Seq.map (fun (k : Guid, v) -> string k, v)
    |> Map.ofSeq

let stringsToGuids hangars =
    hangars
    |> Map.toSeq
    |> Seq.map (fun (k : string, v) -> Guid(k), v)
    |> Map.ofSeq
