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
/// Interface of icons.
/// </summary>
type McuIcon =
    inherit McuBase
    abstract Targets : int list with get, set

/// <summary>
/// Interface of commands (timers, proximity triggers...)
/// </summary>
type McuCommand =
    inherit McuBase
    abstract Name : string with get, set
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
    | OnObjectSpawned = 57
    | OnObjectEntered = 58
    | OnObjectEnteredAlive = 59 
    | OnObjectLeft = 60
    | OnObjectLeftAlive = 61 
    | OnObjectFinished = 62 
    | OnObjectFinishedAlive = 63
    | OnObjectStationaryAndAlive = 64 
    | OnObjectFinishedStationaryAndAlive = 65
    | OnObjectTookOff = 66
    | OnObjectDamaged = 67
    | OnObjectCriticallyDamaged = 68
    | OnObjectRepaired = 69
    | OnObjectKilled = 70
    | OnObjectDroppedBombs = 71
    | OnObjectFiredRockets = 72
    | OnObjectFiredFlare = 73

/// <summary>
/// Get the name of an event type as a string.
/// </summary>
let getEventTypeName =
    function
    | EventTypes.OnPilotKilled -> "OnPilotKilled"
    | EventTypes.OnPilotWounded -> "OnPilotWounded"
    | EventTypes.OnPlaneCrashed -> "OnPlaneCrashed"
    | EventTypes.OnPlaneCriticalDamage -> "OnPlaneCriticalDamage"
    | EventTypes.OnPlaneDestroyed -> "OnPlaneDestroyed"
    | EventTypes.OnPlaneLanded -> "OnPlaneLanded"
    | EventTypes.OnPlaneTookOff -> "OnPlaneTookOff"
    | EventTypes.OnPlaneBingoFuel -> "OnPlaneBingoFuel"
    | EventTypes.OnPlaneBingoMainMG -> "OnPlaneBingoMainMG"
    | EventTypes.OnPlaneBingoBombs -> "OnPlaneBingoBombs"
    | EventTypes.OnPlaneBingoTurrets -> "OnPlaneBingoTurrets"
    | EventTypes.OnPlaneGunnersKilled -> "OnPlaneGunnersKilled"
    | EventTypes.OnDamaged -> "OnDamaged"
    | EventTypes.OnKilled -> "OnKilled"
    | EventTypes.OnMovedTo -> "OnMovedTo"
    | EventTypes.OnPlaneSpawned -> "OnPlaneSpawned"
    | EventTypes.OnOutOfPlanes -> "OnOutOfPlanes"
    | EventTypes.OnPlaneAdded -> "OnPlaneAdded"
    | EventTypes.OnSpottingStarted -> "OnSpottingStarted"
    | EventTypes.OnObjectSpawned -> "OnObjectSpawned"
    | EventTypes.OnObjectEntered -> "OnObjectEntered"
    | EventTypes.OnObjectEnteredAlive -> "OnObjectEnteredAlive" 
    | EventTypes.OnObjectLeft -> "OnObjectLeft"
    | EventTypes.OnObjectLeftAlive -> "OnObjectLeftAlive" 
    | EventTypes.OnObjectFinished -> "OnObjectFinished" 
    | EventTypes.OnObjectFinishedAlive -> "OnObjectFinishedAlive"
    | EventTypes.OnObjectStationaryAndAlive -> "OnObjectStationaryAndAlive" 
    | EventTypes.OnObjectFinishedStationaryAndAlive -> "OnObjectFinishedStationaryAndAlive"
    | EventTypes.OnObjectTookOff -> "OnObjectTookOff"
    | EventTypes.OnObjectDamaged -> "OnObjectDamaged"
    | EventTypes.OnObjectCriticallyDamaged -> "OnObjectCriticallyDamaged"
    | EventTypes.OnObjectRepaired -> "OnObjectRepaired"
    | EventTypes.OnObjectKilled -> "OnObjectKilled"
    | EventTypes.OnObjectDroppedBombs -> "OnObjectDroppedBombs"
    | EventTypes.OnObjectFiredRockets -> "OnObjectFiredRockets"
    | EventTypes.OnObjectFiredFlare -> "OnObjectFiredFlare"
    | x -> sprintf "Event%d" (int x)

/// <summary>
/// Connection of an event from an entity to a target command.
/// </summary>
type EventConnection =
    { Type : int
      TarId : int }

/// <summary>
/// Interface of complex triggers.
/// </summary>
type McuComplex =
    inherit McuBase
    abstract Name : string with get, set
    abstract OnEvents : EventConnection list with get, set

/// <summary>
/// Report codes, to be used in ReportConnection.Type
/// </summary>
type ReportTypes =
    | OnSpawned = 0
    | OnTargetAttacked = 1
    | OnAreaAttacked = 2
    | OnTookOff = 3
    | OnLanded = 4

/// <summary>
/// Get the name of a report type as a string.
/// </summary>
let getReportTypeName =
    function
    | ReportTypes.OnSpawned -> "OnSpawned"
    | ReportTypes.OnTargetAttacked -> "OnTargetAttacked"
    | ReportTypes.OnAreaAttacked -> "OnAreaAttacked"
    | ReportTypes.OnTookOff -> "OnTookOff"
    | ReportTypes.OnLanded -> "OnLanded"
    | x -> sprintf "Report%d" (int x)

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
    abstract Name : string with get, set
    abstract MisObjID : int with get, set
    abstract OnEvents : EventConnection list with get, set
    abstract OnReports : ReportConnection list with get, set

/// <summary>
/// Interface of things that can have entities: ground vehicles, planes, artillery, buildings, bridges...
/// </summary>
type HasEntity =
    inherit McuBase
    /// <summary>
    /// Get or set the link to the entity. Set to 0 it lacks an entity.
    /// </summary>
    abstract LinkTrId : int with get, set
    abstract Name : string with get, set
    abstract Model : string with get, set
    abstract Script : string with get, set
    abstract Country : int with get, set

/// <summary>
/// Substitute occurrences of numerical ids in an MCU.
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose ids are changed. Instance is mutated.</param>
let substId (getNewId : int -> int) (mcu : McuBase) =
    mcu.Index <- getNewId mcu.Index
    match mcu with
    | :? McuIcon as icon ->
        icon.Targets <- icon.Targets |> List.map getNewId
    | _ -> ()
    match mcu with
    | :? McuCommand as cmd ->
        cmd.Objects <- cmd.Objects |> List.map getNewId
        cmd.Targets <- cmd.Targets |> List.map getNewId
    | _ -> ()
    match mcu with
    | :? McuEntity as ent ->
        ent.MisObjID <- getNewId ent.MisObjID
        ent.OnEvents <- ent.OnEvents |> List.map (fun ev -> { ev with TarId = getNewId ev.TarId })
        ent.OnReports <- ent.OnReports |> List.map (fun rep -> { rep with TarId = getNewId rep.TarId; CmdId = getNewId rep.CmdId })
    | _ -> ()
    match mcu with
    | :? HasEntity as veh ->
        if veh.LinkTrId <> 0 then
            veh.LinkTrId <- getNewId veh.LinkTrId
    | _ -> ()
    match mcu with
    | :? McuComplex as complex ->
        complex.OnEvents <- complex.OnEvents |> List.map (fun ev -> { ev with TarId = getNewId ev.TarId })
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
/// Given members in a graph of connected entities, objects and commands, return the nodes in the graph.
/// </summary>
/// <param name="isExcluded">
/// Predicate that decides whether a node should be included in the result.
/// Nodes that depend solely on excluded nodes are not included in the result.
/// </param>
/// <param name="roots">
/// Nodes which are part of the graph. All nodes that point to them through links and that are not excluded
/// will be included in the result.
/// </param>
/// <param name="all">
/// The list of nodes that are considered for inclusion. The implementation has quadratic
/// complexity in the size of this argument.
/// </param>
/// <param name="visited">
/// Incoming sequence of nodes that have already been visited. Useful when chaining calls.
/// </param>
let getGroupMulti (isExcluded : McuBase -> bool) (roots : McuBase list) (all : McuBase list) (visited : McuBase seq) =
    let visited = new HashSet<McuBase>(visited)
    let rec work (working : McuBase list) =
        match working with
        | [] -> ()
        | item :: rest ->
            if not(isExcluded(item) || visited.Contains(item)) then
                visited.Add(item) |> ignore
                // In the comments below, "an" refers to the thing in all, "the" refers to "item" from working.
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
                        | :? McuComplex as complex ->
                            // Retain a complex trigger that targets the command through an event
                            complex.OnEvents |> List.exists (fun ev -> ev.TarId = item.Index)
                        | _ -> false
                    )
                work (dependents @ rest)
            else
                work rest
    work roots
    visited

/// Deprecated. Use getGroupMulti instead.
[<System.Obsolete("Use getGroupMulti instead")>]
let getGroup (isExcluded : McuBase -> bool) (root : McuBase) (all : McuBase list) =
    getGroupMulti isExcluded [root] all (HashSet<McuBase>())