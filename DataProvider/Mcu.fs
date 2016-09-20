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
    abstract Path : (string * int) list with get, set

/// <summary>
/// Interface of icons.
/// </summary>
type McuIcon =
    inherit McuBase
    abstract Targets : int list with get, set

/// <summary>
/// Interface of triggers (timers, counters...).
/// </summary>
type McuTrigger =
    inherit McuBase
    abstract Name : string with get, set
    abstract Objects : int list with get, set
    abstract Targets : int list with get, set

/// <summary>
/// Interface of proximity and check zone triggers.
/// </summary>
type McuProximity =
    inherit McuTrigger
    abstract PlaneCoalitions : int list with get, set
    abstract VehicleCoalitions : int list with get, set

/// <summary>
/// Interface of waypoint triggers.
/// </summary>
type McuWaypoint =
    inherit McuTrigger
    /// <summary>
    /// Radius in m.
    /// </summary>
    abstract Radius : int with get, set
    /// <summary>
    /// 0 -> Low, 1 -> Medium, 2 -> High
    /// </summary>
    abstract Priority : int with get, set
    /// <summary>
    /// Speed in km/h.
    /// </summary>
    abstract Speed : int with get, set

/// <summary>
/// Interface of timer triggers.
/// </summary>
type McuTimer =
    inherit McuTrigger
    abstract Time : int with get, set

/// <summary>
/// Interface of counters.
/// </summary>
type McuCounter =
    inherit McuTrigger
    abstract Count : int with get, set
    abstract WrapAround : bool with get, set

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
    inherit McuTrigger
    abstract Name : string with get, set
    abstract MisObjID : int with get, set
    abstract OnEvents : EventConnection list with get, set
    abstract OnReports : ReportConnection list with get, set

/// <summary>
/// Provide access to the position in formation for instances of HasEntity that can be in formations:
/// ground vehicles, planes... but not buildings, bridges.
/// </summary>
type NumberInFormationData =
    abstract Number : int with get, set

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
    abstract NumberInFormation : NumberInFormationData option

/// <summary>
/// Substitute occurrences of numerical ids in an MCU.
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose ids are changed. Instance is mutated.</param>
let substId (getNewId : int -> int) (mcu : McuBase) =
    mcu.Index <- getNewId mcu.Index
    mcu.Path <-
        mcu.Path
        |> List.map (fun (name, idx) -> (name, getNewId idx))
    match mcu with
    | :? McuIcon as icon ->
        icon.Targets <- icon.Targets |> List.map getNewId
    | _ -> ()
    match mcu with
    | :? McuTrigger as cmd ->
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
let addObjectLink (mcu : McuTrigger) (objekt : int) =
    mcu.Objects <- objekt :: mcu.Objects

/// <summary>
/// Add a target link to a command.
/// </summary>
/// <param name="mcu">The command, which is mutated.</param>
/// <param name="target">The id of the target.</param>
let addTargetLink (mcu : McuTrigger) (target : int) =
    mcu.Targets <- target :: mcu.Targets

/// <summary>
/// Connect a entity-holding object and its entity.
/// </summary>
/// <param name="veh">The entity owner, which is mutated.</param>
/// <param name="ent">The entity, which is mutated.</param>
let connectEntity (veh : HasEntity) (ent : McuEntity) =
    veh.LinkTrId <- ent.Index
    ent.MisObjID <- veh.Index
