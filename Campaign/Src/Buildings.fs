﻿module Campaign.Buildings

open System.Numerics

open VectorExtension
open BasicTypes

type BuildingProperties = {
    Model : string
    Script : string
    Boundary : Vector2 list
    SubParts : int list
    Durability : int
}

module private BuildingProperties_ =
    let Areas = Util.cachedProperty (fun this -> 1.0f<M^2> * Vector2.ConvexPolygonArea this.Boundary)

type BuildingProperties with
    /// Volume of storage per surface unit
    static member CapacityDensity = 0.25f<M^3/M^2>

    member this.Area = BuildingProperties_.Areas this

    /// Total capacity of all parts in the building
    member this.Capacity =
        BuildingProperties.CapacityDensity * this.Area

    /// Volume of storage in a single part
    member this.PartCapacity =
        match this.SubParts.Length with
        | 0 -> 0.0f<M^3>
        | n -> this.Capacity / float32 n

/// Identify buildings by their position.
[<Struct>]
type BuildingInstanceId = BuildingInstanceId of OrientedPosition

type BuildingInstance = {
    Pos : OrientedPosition
    Properties : BuildingProperties
}
with
    member this.Id = BuildingInstanceId this.Pos

    member this.Boundary =
        this.Properties.Boundary
        |> List.map (fun v -> v.Rotate(this.Pos.Rotation) + this.Pos.Pos)

