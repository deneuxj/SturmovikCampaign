/// Functions to reduce size of generated mission files
module Campaign.MissionGen.McuTrim

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Mcu
open SturmovikMission.DataProvider.McuUtil

open System.Numerics
open VectorExtension

let private logger = NLog.LogManager.GetCurrentClassLogger()

/// Get the ID of an MCU
let idOf (mcu : McuBase) = mcu.Index

/// Filter McuBases from a group, producing a new group
let rec trimGroup filter (group : IMcuGroup) =
    { new IMcuGroup with
        member this.Content: Mcu.McuBase list = 
            group.Content |> List.filter filter
        member this.LcStrings: (int * string) list = 
            group.LcStrings
        member this.SubGroups: IMcuGroup list = 
            group.SubGroups |> List.map (trimGroup filter)
    }

/// Get components of a script or model path 
let getFilepath (s : string) =
    s.Split('/', '\\')
    |> Array.map (fun s -> s.ToLowerInvariant())
    |> List.ofArray

/// Check if an MCU is a block, and if so get its script
let tryGetBlockScript (mcu : McuBase) =
    match mcu with
    | :? HasEntity as entityHolder ->
        let path = getFilepath entityHolder.Script
        match List.rev path with
        | _ :: "blocks" :: _
        | _ :: "blocksdetail" :: _ -> Some path
        | _ -> None
    | _ ->
        None

/// Check if an MCU is a multiplayer spawn, and if so get its position
let tryGetSpawnPos (mcu : McuBase) =
    match mcu with
    | :? HasEntity as entityHolder ->
        let path = getFilepath entityHolder.Script
        match List.rev path with
        | _ :: "airfields" :: _ ->
            Some mcu.Pos
        | _ -> None
    | _ ->
        None

/// Get IDs of MCUs that are referenced by links, or that must be retained (e.g. entities of anything but blocks)
let getReferredIds (mcus : McuBase seq) =
    let blocks =
        mcus
        |> Seq.filter (tryGetBlockScript >> Option.isSome)
        |> Seq.map idOf
        |> Set
    mcus
    |> Seq.collect (
        function
        | :? McuEntity as entity ->
            let self =
                if blocks.Contains entity.MisObjID then
                    []
                else
                    [entity.Index]
            [
                self
                entity.Objects
                entity.Targets
                entity.OnEvents |> List.map (fun ev -> ev.TarId)
                entity.OnReports |> List.map (fun ev -> ev.TarId)
            ]
            |> Seq.concat 
        | :? McuTrigger as trigger ->
            Seq.append trigger.Objects trigger.Targets
        | _ ->
            Seq.empty
    )

/// Get the entities of static blocks that aren't referenced by anyone other than their holder, and that don't have
/// references to other MCUs
let getLooseEntities (mcus : McuBase seq) =
    let entities =
        mcus
        |> Seq.choose (function :? McuEntity as entity -> Some entity | _ -> None)
        |> Seq.cache
    let referred =
        Set(getReferredIds mcus)
    entities
    |> Seq.filter (fun entity ->
        entity.Targets.IsEmpty &&
        entity.OnEvents.IsEmpty &&
        entity.OnReports.IsEmpty &&
        not(referred.Contains entity.Index))

/// Sort blocks by min distance to a set of positions called "anchors"
let prioritizeBlocks (anchors : Vector2 seq) (mcus : McuBase seq) =
    let anchors = Seq.cache anchors
    mcus
    |> Seq.filter (tryGetBlockScript >> Option.isSome)
    |> Seq.sortBy(fun mcu ->
        anchors
        |> Seq.map (fun anchor ->
            let v = Vector2.FromMcu mcu.Pos
            let dist = (anchor - v).LengthSquared()
            dist
        )
        |> Seq.min
    )

/// Try to get the position, radius and connected objects of an area where entities should be kept, e.g. in attack
/// areas targetted by AIs.
let tryGetZoneOfInterest (mcu : McuBase) =
    match mcu with
    | :? McuAttackArea as area -> Some(area.Pos, area.AttackArea, area.Objects)
    | _ -> None

/// Try to get the country of an MCU.
let tryGetCountry (mcu : McuBase) =
    match mcu with
    | :? HasEntity as owner -> owner.Country
    | _ -> None

/// Remove entities that are loose and aren't within a zone of interest
let cullEntities coalitionOf (groups : IMcuGroup seq) =
    let mcus =
        groups
        |> Seq.collect deepContentOf
        |> Seq.cache
    let loose =
        mcus
        |> getLooseEntities
        |> Seq.map idOf
        |> Set
    logger.Info(sprintf "Found %d loose entities" loose.Count)
    let countryOf =
        mcus
        |> Seq.choose (fun mcu -> mcu |> tryGetCountry |> Option.map (fun country -> idOf mcu, country))
        |> Map.ofSeq
    let zonesOfInterest =
        mcus
        |> Seq.choose tryGetZoneOfInterest
        |> Seq.map (fun (pos, radius, objs) -> Vector2.FromMcu pos, float32 (radius * radius), objs |> List.choose countryOf.TryFind)
        |> Seq.sortByDescending (fun (_, rad, _) -> rad)
        |> Seq.cache
    let filter (mcu : McuBase) =
        match mcu with
        | :? McuEntity as entity ->
            let coalition =
                entity.MisObjID
                |> countryOf.TryFind
                |> Option.bind coalitionOf
            (
                not(loose.Contains entity.Index) ||
                zonesOfInterest
                |> Seq.exists (fun (pos, radius2, countries) ->
                    (
                        countries
                        |> List.exists (fun country -> coalitionOf country <> coalition) &&
                        let mpos = Vector2.FromMcu entity.Pos
                        (pos - mpos).LengthSquared() <= radius2
                    )
                )
            )
        | _ -> true
    groups
    |> Seq.map (trimGroup filter)

/// Try to retain at most limit blocks in groups, by removing blocks without entities that are fare
/// from spawn positions.
let limitBlocks limit (groups : IMcuGroup seq) =
    let spawns =
        groups
        |> Seq.collect deepContentOf
        |> Seq.choose tryGetSpawnPos
        |> Seq.map Vector2.FromMcu
    let blocks =
        groups
        |> Seq.collect deepContentOf
        |> Seq.filter (tryGetBlockScript >> Option.isSome)
        |> prioritizeBlocks spawns
    let erasedBlocks =
        try
            blocks
            |> Seq.skip limit
            |> Seq.map idOf
            |> Set
        with
        :? System.InvalidOperationException -> Set.empty
    let filter (mcu : McuBase) =
        match mcu with
        | :? HasEntity as owner ->
            owner.LinkTrId > 0 || not (erasedBlocks.Contains owner.Index)
        | _ -> true
    groups
    |> Seq.map (trimGroup filter)

/// Try to reduce the number of MCUs in groups by removing loose entities that aren't inside zones
/// of interest, and trying to remove blocks that are far from player spawns.
let trimMcus(coalitionOf, maxBlocks, groups) =
    groups
    |> cullEntities coalitionOf
    |> limitBlocks maxBlocks

/// Clear entity links to non-existing entities
let clearBrokenEntityRefs (mcus : McuBase seq) =
    let entities =
        mcus
        |> Seq.choose (
            function
            | :? McuEntity as entity -> idOf entity |> Some
            | _ -> None)
        |> Set
    for mcu in mcus do
        match mcu with
        | :? HasEntity as owner ->
            if not(entities.Contains owner.LinkTrId) then
                owner.LinkTrId <- 0
        | _ ->
            ()