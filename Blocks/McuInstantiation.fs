﻿module SturmovikMission.Blocks.McuInstantiation

open System.Numerics
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open VectorExtension
open SturmovikMission.Blocks.BlocksMissionData

type DirectedPoint =
    { Pos : Vector2
      Direction : float32
    }
with
    member this.AssignTo(mcu : Mcu.McuTrigger) =
        this.Pos.AssignTo mcu.Pos
        mcu.Ori.Y <- float this.Direction

    static member inline FromMCU(mcu) =
        let pos = Vector2.FromPos(mcu)
        let direction = mcu |> CommonMethods.getYOri |> CommonMethods.valueOf |> float32
        { Pos = pos; Direction = direction }

/// Extract a group of nodes and a node to use as reference when relocating.
let extractGroup group groupName refName =
    let selected =
        group |> filterByPath [groupName] |> List.ofSeq
    selected,
    selected |> filterByName refName |> Seq.exactlyOne

/// Move and rotate a group to a new location.
let relocateGroup (newCenter : DirectedPoint) (group : #Mcu.McuBase seq * Mcu.McuBase) =
    let mcus, refMcu = group
    let refPos = Vector2.FromMcu refMcu.Pos
    let rotation = newCenter.Direction - float32 refMcu.Ori.Y
    for mcu in mcus do
        let r = (Vector2.FromMcu mcu.Pos) - refPos
        let r = newCenter.Pos + r.Rotate(rotation)
        r.AssignTo(mcu.Pos)
        mcu.Ori.Y <- (mcu.Ori.Y + float rotation) % 360.0

/// Set the altitude of a group of nodes.
let setAltitude (altitude : int) (mcus : #Mcu.McuBase seq) =
    for mcu in mcus do
        mcu.Pos.Y <- float altitude

/// Get nodes from a group and give them new numerical IDs.
let getFreshGroup (source : T.GroupData) (store : NumericalIdentifiers.IdStore) (name : string) =
    let subst = Mcu.substId(store.GetIdMapper())
    let mcus = source.GetGroup(name).CreateMcuList()
    for mcu in mcus do
        subst mcu
    mcus, subst

/// Clone a group, giving its members new numerical IDs.
/// Note that the order of nodes in the group is not necessarilly retained.
let cloneFresh (store : NumericalIdentifiers.IdStore) (group : #Mcu.McuBase seq) =
    let repr =
        group
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
    let subst = Mcu.substId(store.GetIdMapper())
    let mcus = T.GroupData.Parse(Parsing.Stream.FromString repr).CreateMcuList()
    for mcu in mcus do
        subst mcu
    mcus

/// Put a nodes in a named group
let gatherInNamedGroup (store : NumericalIdentifiers.IdStore) name (mcus : #Mcu.McuBase seq) =
    let idx = store.GetIdMapper() 1
    for mcu in mcus do
        mcu.Path <- mcu.Path @ [name, idx]

type IMcuGroup with
    /// Put all nodes in this group under one named group
    member this.PushGroupName(store, name) =
        deepContentOf this
        |> gatherInNamedGroup store name

type IHasVehicles =
    abstract ReplaceVehicleWith : int * Mcu.HasEntity -> unit
    abstract Vehicles : Mcu.HasEntity seq

[<AbstractClass>]
type PlaneWingReplacement() =
    abstract Planes : Mcu.HasEntity[]

    interface IHasVehicles with
        member this.Vehicles =
            upcast this.Planes

        member this.ReplaceVehicleWith(oldVehicleIdx, newVehicle) =
            this.Planes
            |> Array.iteri(fun idx plane ->
                if plane.Index = oldVehicleIdx then
                    this.Planes.[idx] <- newVehicle)

let setVehiclesAfterPlane (proto : T.Plane) (hasVehicles : IHasVehicles) =
    for plane in hasVehicles.Vehicles do
        let newPlane =
            proto
            |> fun newPlane ->
                match plane.NumberInFormation with
                | Some x ->
                    newPlane.SetNumberInFormation(T.Integer.N x.Number)
                | None ->
                    newPlane
            |> fun newPlane ->
                newPlane
                    .SetZOri(T.Float.N plane.Ori.Z)
                    .SetYOri(T.Float.N plane.Ori.Y)
                    .SetXOri(T.Float.N plane.Ori.X)
                    .SetZPos(T.Float.N plane.Pos.Z)
                    .SetYPos(T.Float.N plane.Pos.Y)
                    .SetXPos(T.Float.N plane.Pos.X)
                    .SetLinkTrId(T.Integer.N plane.LinkTrId)
                    .SetIndex(T.Integer.N plane.Index)
                    .SetName(T.String.N plane.Name)
        let mcu2 = newPlane.CreateMcu() :?> Mcu.HasEntity
        mcu2.Path <- plane.Path
        hasVehicles.ReplaceVehicleWith(plane.Index, mcu2)

/// Make a group where nodes in the group can be replaced by other nodes.
/// Useful e.g. when replacing vehicles in a template.
let rec mcuGroupWithReplaceables getReplacement (grp : McuUtil.IMcuGroup) =
    { new McuUtil.IMcuGroup with
          member this.Content =
            grp.Content
            |> List.map (fun mcu ->
                let mcu2 : Mcu.McuBase = getReplacement mcu
                assert (mcu2.Index = mcu.Index)
                mcu2)

          member this.LcStrings =
            grp.LcStrings

          member this.SubGroups =
            grp.SubGroups
            |> List.map (mcuGroupWithReplaceables getReplacement)
    }