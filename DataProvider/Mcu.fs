//    Copyright 2015 Johann Deneux
//
//    This file is part of SturmovikMission.
//
//    SturmovikMission is free software: you can redistribute it and/or modify
//    it under the terms of the GNU Lesser General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    SturmovikMission is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public License
//    along with SturmovikMission.  If not, see <http://www.gnu.org/licenses/>.


module SturmovikMission.DataProvider.Mcu

open System.Collections.Generic
open SturmovikMission.DataProvider.Ast

/// <summary>
/// 3d vector type used for positions and orientations.
/// </summary>
type Vec3 =
    abstract X : float with get, set
    abstract Y : float with get, set
    abstract Z : float with get, set

/// <summary>
/// Localization data for icons.
/// </summary>
type IconLCData =
    abstract LCName : int with get, set
    abstract LCDesc : int with get, set

/// <summary>
/// Localization data for subtitles.
/// </summary>
type SubtitleLCData =
    abstract LCText : int with get, set

/// <summary>
/// Base interface for all MCUs.
/// A subset of the properties of objects is made accessible. These are the
/// properties that would typically need to be changed when instancing a
/// template.
/// Instances of this type are mutable.
/// </summary>
type McuBase =
    abstract Index : int with get, set
    abstract Name : string with get, set
    abstract Pos : Vec3
    abstract Ori : Vec3
    abstract IconLC : IconLCData option
    abstract SubtitleLC : SubtitleLCData option
    /// <summary>
    /// Build a string using the syntax of mission files that specifies all the
    /// fields of this instance.
    /// </summary>
    abstract AsString : unit -> string

/// <summary>
/// Interface of commands (timers, proximity triggers...)
/// </summary>
type McuCommand =
    inherit McuBase
    abstract Objects : int list with get, set
    abstract Targets : int list with get, set

/// <summary>
/// Event codes, to be used in EventConnection.Type
/// </summary>
type EventTypes =
    | OnPilotKilled = 0
    | OnPilotWounded = 1
    | OnPlaneCrashed = 2
    | OnPlaneCriticalDamage = 3
    | OnPlaneDestroyed = 4
    | OnPlaneLanded = 5
    | OnPlaneTookOff = 6
    | OnPlaneBingoFuel = 7
    | OnPlaneBingoMainMG = 8
    | OnPlaneBingoBombs = 9
    | OnPlaneBingoTurrets = 10
    | OnPlaneGunnersKilled = 11
    | OnDamaged = 12
    | OnKilled = 13
    | OnMovedTo = 15
    | OnPlaneSpawned = 20
    | OnOutOfPlanes = 21
    | OnPlaneAdded = 22
    | OnSpottingStarted = 74

/// <summary>
/// Connection of an event from an entity to a target command.
/// </summary>
type EventConnection =
    { Type : int
      TarId : int }

/// <summary>
/// Connection of a command completion report from an entity to a target command.
/// </summary>
type ReportConnection =
    { Type : int
      CmdId : int
      TarId : int }

/// <summary>
/// Interface of entities, i.e. active parts of vehicles and other 3d objects.
/// </summary>
type McuEntity =
    inherit McuCommand
    abstract MisObjID : int with get, set
    abstract OnEvents : EventConnection list with get, set
    abstract OnReports : ReportConnection list with get, set

/// <summary>
/// Interface of things that have entities: ground vehicles, planes, artillery, buildings, bridges...
/// </summary>
type HasEntity =
    inherit McuBase
    /// <summary>
    /// Get or set the link to the entity. Set to 0 it lacks an entity.
    /// </summary>
    abstract LinkTrId : int with get, set

/// <summary>
/// Substitute occurrences of numerical ids in an MCU.
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose ids are changed. Instance is mutated.</param>
let substId (getNewId : int -> int) (mcu : McuBase) =
    mcu.Index <- getNewId mcu.Index
    match mcu with
    | :? McuCommand as cmd ->
        cmd.Objects <- cmd.Objects |> List.map getNewId
        cmd.Targets <- cmd.Targets |> List.map getNewId
    | _ -> ()
    match mcu with
    | :? McuEntity as ent ->
        ent.MisObjID <- getNewId ent.MisObjID
        ent.OnEvents <- ent.OnEvents |> List.map (fun ev -> { ev with TarId = getNewId ev.TarId })
    | _ -> ()
    match mcu with
    | :? HasEntity as veh ->
        veh.LinkTrId <- getNewId veh.LinkTrId
    | _ -> ()

/// <summary>
/// Substitute occurrences of numerical ids of localization strings in an MCU
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose localization string ids are changed. Instance is mutated.</param>
let substLCId (getNewId : int -> int) (mcu : McuBase) =
    match mcu.IconLC with
    | Some op ->
        op.LCDesc <- getNewId op.LCDesc
        op.LCName <- getNewId op.LCName
    | None -> ()
    match mcu.SubtitleLC with
    | Some op ->
        op.LCText <- getNewId op.LCText
    | None -> ()

/// <summary>
/// Add an object link to a command.
/// </summary>
/// <param name="mcu">The command, which is mutated.</param>
/// <param name="objekt">The id of the object.</param>
let addObjectLink (mcu : McuCommand) (objekt : int) =
    mcu.Objects <- objekt :: mcu.Objects

/// <summary>
/// Add a target link to a command.
/// </summary>
/// <param name="mcu">The command, which is mutated.</param>
/// <param name="target">The id of the target.</param>
let addTargetLink (mcu : McuCommand) (target : int) =
    mcu.Targets <- target :: mcu.Targets

/// <summary>
/// Connect a entity-holding object and its entity.
/// </summary>
/// <param name="veh">The entity owner, which is mutated.</param>
/// <param name="ent">The entity, which is mutated.</param>
let connectEntity (veh : HasEntity) (ent : McuEntity) =
    veh.LinkTrId <- ent.Index
    ent.MisObjID <- veh.Index

/// <summary>
/// Given a member in a graph of connected entities, objects and commands, return the nodes in the graph.
/// </summary>
/// <param name="isExcluded">
/// Predicate that decides whether a node should be included in the result.
/// Nodes that depend solely on excluded nodes are not included in the result.
/// </param>
/// <param name="root">
/// A node which is part of the graph. All nodes that are linked to it and that are not excluded
/// will be included in the result.
/// </param>
/// <param name="all">
/// The list of nodes that are considered for inclusion. The implementation has quadratic
/// complexity in the size of this argument.
/// </param>
let getGroup (isExcluded : McuBase -> bool) (root : McuBase) (all : McuBase list) =
    let visited = HashSet<McuBase>()
    let rec work (working : McuBase list) =
        match working with
        | [] -> ()
        | item :: rest ->
            if not(isExcluded(item) || visited.Contains(item)) then
                visited.Add(item) |> ignore
                // In the comments below, "an" refers the thing in all, "the" refers to "item" from working.
                let dependents =
                    all
                    |> List.filter(function
                        | :? HasEntity as owner ->
                            // Retain an owner of the entity
                            owner.LinkTrId = item.Index
                        | :? McuEntity as entity ->
                            // Retain an entity of the object
                            entity.MisObjID = item.Index
                            // Retain a wing of the entity
                            || entity.Targets |> List.exists ((=) item.Index)
                            // Retain an entity that targets the command through an event
                            || entity.OnEvents |> List.exists (fun ev -> ev.TarId = item.Index)
                            // retain an entity that targets the command through a command report
                            || entity.OnReports |> List.exists (fun rep -> rep.TarId = item.Index)
                        | :? McuCommand as cmd ->
                            // retain a command that has the entity
                            cmd.Objects |> List.exists ((=) item.Index)
                            // retain a command that targets the entity or the command
                            || cmd.Targets |> List.exists ((=) item.Index)
                        | _ -> false
                    )
                work (dependents @ rest)
            else
                work rest
    work [root]
    visited
