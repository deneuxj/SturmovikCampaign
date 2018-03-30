﻿//    Copyright 2015 Johann Deneux
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
    /// Path through groups, from leaf to top. Each level is identified by the name and id of the group.
    abstract Path : (string * int) list with get, set

/// <summary>
/// Numerical values of coalitions.
/// </summary>
type CoalitionValue =
    | Neutral = 0
    | Allies = 1
    | Axis = 2

/// <summary>
/// Numerical values of line types between connected icons.
/// </summary>
type LineTypeValue =
    | Normal = 0
    | Bold = 1
    | Border = 2
    | ZoneType1 = 3
    | ZoneType2 = 4
    | ZoneType3 = 5
    | ZoneType4 = 6
    | SectorType1 = 7
    | SectorType2 = 8
    | SectorType3 = 9
    | SectorType4 = 10
    | Attack = 11
    | Defence = 12
    | PositionType0 = 13
    | PositionType1 = 14
    | PositionType2 = 15
    | PositionType3 = 16
    | PositionType4 = 17
    | PositionType5 = 18
    | PositionType6 = 19
    | PositionType7 = 20
    | PositionType8 = 21
    | PositionType9 = 22

/// <summary>
/// Numerical values of icon types.
/// </summary>
type IconIdValue =
    | None = 0
    | AttackBombersFlight = 102
    | CoverBombersFlight = 152
    | OffensivePatrol = 201
    | AttackTransportColumn = 501
    | AttackArmorColumn = 502
    | AttackAntiAirPosition = 504
    | AttackBuildings = 507
    | AttackTrains = 508
    | AttackShips = 509
    | CoverTransportColumn = 551
    | CoverArmorColumn = 552
    | CoverAntiAirPosition = 554
    | CoverBuildings = 557
    | CoverTrains = 558
    | CoverShips = 559
    | Waypoint = 901
    | ActionPoint = 902

/// <summary>
/// Interface of icons.
/// </summary>
type McuIcon =
    inherit McuBase
    abstract Targets : int list with get, set
    abstract IconId : IconIdValue with get, set
    abstract Red : int with get, set
    abstract Green : int with get, set
    abstract Blue : int with get, set
    abstract LineType : LineTypeValue with get, set
    abstract Coalitions : CoalitionValue list with get, set
    abstract Enabled : bool with get, set

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
    abstract PlaneCoalitions : CoalitionValue list with get, set
    abstract VehicleCoalitions : CoalitionValue list with get, set
    abstract Distance : int with get, set

/// <summary>
/// Interface of attack area MCUs.
/// </summary>
type McuAttackArea =
    inherit McuTrigger
    abstract AttackArea : int with get, set

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
    /// Time in seconds
    abstract Time : float with get, set

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
/// Known country values.
/// </summary>
type CountryValue =
    | Russia = 101
    | Germany = 201

/// <summary>
/// Interface of complex triggers.
/// </summary>
type McuComplex =
    inherit McuBase
    abstract Name : string with get, set
    abstract OnEvents : EventConnection list with get, set
    abstract Countries : CountryValue list with get, set

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
    abstract Enabled : bool with get, set

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
    abstract Country : CountryValue option with get, set
    abstract NumberInFormation : NumberInFormationData option
    abstract PayloadId : int option with get, set
    abstract WMMask : int option with get, set
    abstract AILevel : int option with get, set

/// <summary>
/// Substitute occurrences of numerical ids in an MCU.
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose ids are changed. Instance is mutated.</param>
let substId (getNewId : int -> int) (mcu : McuBase) =
    let newIndex = getNewId mcu.Index
    mcu.Index <- newIndex
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
