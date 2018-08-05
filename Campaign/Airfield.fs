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

module Campaign.Airfield

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.Blocks
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.LandingTee
open SturmovikMission.Blocks.WhileEnemyClose
open SturmovikMission.Blocks.Vehicles

open Util

open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.BasicTypes
open Campaign.WorldState
open Campaign.NewWorldState


let maxPlaneSpawns = 8

/// Select up to a maximum number of planes that will be available for spawn at an airfield
/// Returns an array where the rank can be used to identify planes
let selectPlaneSpawns (maxSpawns : int) (coalition : CoalitionId) (numPlanes : Map<PlaneModel, int>) =
    numPlanes
    |> Map.toSeq
    |> Seq.sortByDescending(fun (plane, qty) ->
        plane.Roles.Contains(CargoTransporter),
        plane.Coalition = coalition,
        qty)
    |> Seq.map fst
    |> Seq.truncate maxSpawns
    |> Array.ofSeq

let mkPlaneSpecs totalBombs (planes : PlaneModel[]) =
    let maxIndex = 1 <<< (planes.Length)
    [
        for i, plane in Seq.indexed planes do
            let mask = 1 <<< i
            let model = plane.ScriptModel
            let defaultPayload =
                if plane.PlaneType = PlaneType.Fighter then
                    0
                else
                    plane.BombLoads
                    |> List.tryFind (fun (idx, w) -> w <= totalBombs)
                    |> Option.map fst
                    |> Option.defaultValue plane.EmptyPayload
            let planeSpec = newAirfieldPlane("0..99", plane.LoadOuts(totalBombs), 0, defaultPayload, "", plane.PlaneName, -1)
                                .SetScript(T.String model.Script)
                                .SetModel(T.String model.Model)
                                .SetStartInAir(T.Integer 2)
            for j in 0 .. maxIndex do
                if (j &&& mask) <> 0 then
                    yield planeSpec.SetSetIndex(T.Integer j)
                else
                    yield planeSpec.SetNumber(T.Integer 0).SetSetIndex(T.Integer j)
    ]

let createAirfieldSpawns (maxCapturedPlanes : int) (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (missionStarted : Mcu.McuTrigger) =
    let sg = state.FastAccess
    let rearAirfields =
        [Axis; Allies]
        |> Seq.choose (fun coalition -> tryFindRearAirfield world coalition state)
        |> Set.ofSeq
    [
        for airfield, state in Seq.zip world.Airfields state.Airfields do
            match sg.GetRegion(airfield.Region).Owner with
            | None -> ()
            | Some coalition ->
                let af =
                    let spawn =
                        airfield.Spawn
                        |> List.minBy(fun spawn ->
                            let chart = spawn.TryGetChart()
                            match chart with
                            | None -> System.Single.MaxValue
                            | Some chart ->
                                let points = chart.GetPoints()
                                let distance =
                                    points
                                    |> List.pick(fun p1 ->
                                        if p1.GetType().Value = 2 then
                                            let p =
                                                Vector2(float32 <| p1.GetX().Value, float32 <| p1.GetY().Value).Rotate(float32 (spawn.GetYOri().Value)) + Vector2.FromPos(spawn)
                                            Some ((p - fst state.Runway).Length())
                                        else
                                            None)
                                distance)
                        |> fun spawn ->
                            spawn
                                .SetReturnPlanes(T.Boolean true)
                                .SetRefuelFriendlies(T.Boolean true)
                                .SetRearmFriendlies(T.Boolean true)
                                .SetMaintenanceRadius(T.Integer 3000)
                                .SetRefuelTime(T.Integer 0)
                                .SetRearmTime(T.Integer 0)

                    match coalition with
                    | Axis ->
                        spawn.SetCountry(T.Integer(int(Mcu.CountryValue.Germany)))
                    | Allies ->
                        spawn.SetCountry(T.Integer(int(Mcu.CountryValue.Russia)))
                let totalBombs = state.Supplies / bombCost
                let spawnPlanes =
                    state.NumPlanes
                    |> Map.map (fun _ number -> number |> floor |> int)
                    // Limit number of captured planes available for spawning
                    |> Util.expandMap
                    |> Array.shuffle (new System.Random())
                    |> Array.fold (fun (filtered, capturedLeft) plane ->
                        if plane.Coalition <> coalition then
                            if capturedLeft > 0 then
                                plane :: filtered, capturedLeft - 1
                            else
                                filtered, 0
                        else
                            plane :: filtered, capturedLeft
                    ) ([], maxCapturedPlanes)
                    |> fst
                    |> Util.compactSeq
                    |> Map.filter (fun _ qty -> qty > 0)
                    |> selectPlaneSpawns maxPlaneSpawns coalition
                let planeSpecs : T.Airfield.Planes.Plane list =
                    spawnPlanes
                    |> mkPlaneSpecs totalBombs
                let planes =
                    T.Airfield.Planes()
                        .SetPlane(planeSpecs)
                let afName =
                    if rearAirfields.Contains(airfield.AirfieldId) || sg.GetRegion(airfield.Region).HasInvaders then
                        airfield.AirfieldId.AirfieldName.ToUpper()
                    else
                        airfield.AirfieldId.AirfieldName
                let afName = sprintf "%s (%d Kg)" afName (state.Supplies / bombCost |> float32 |> ceil |> int)
                let af = af.SetPlanes(planes).SetIndex(T.Integer 1).SetLinkTrId(T.Integer 2).SetName(T.String afName)
                let entity = newEntity 2
                entity.MisObjID <- 1
                let mcu = af.CreateMcu()
                let subst = Mcu.substId <| store.GetIdMapper()
                subst mcu
                subst entity
                let runwayStartPos =
                    af.TryGetChart()
                    |> Option.map (fun chart -> chart.GetPoints() |> Seq.find (fun point -> point.GetType().Value = 2))
                    |> Option.map (fun point -> Vector2(point.GetX().Value |> float32, point.GetY().Value |> float32).Rotate(af.GetYOri().Value |> float32) + Vector2.FromPos(af))
                    |> Option.map (fun pos -> pos, coalition)
                let serverInputs =
                    let maxIndex = 1 <<< spawnPlanes.Length
                    [
                        for i in 0..(maxIndex - 1) do
                            let subst = Mcu.substId <| store.GetIdMapper()
                            let input = newServerInput 0 (sprintf "%s-%d" airfield.AirfieldId.AirfieldName i)
                            let alpha = 2.0f * float32(System.Math.PI * (if maxIndex = 0 then 0.0 else (float i) / (float maxIndex)))
                            let pos = Vector2.FromPos(af) + 200.0f * Vector2(cos alpha, sin alpha)
                            pos.AssignTo(input.Pos)
                            let behave = newBehaviour 1 32 0 0 false (float i)  false false false false false
                            let pos = Vector2.FromPos(af) + 100.0f * Vector2(cos alpha, sin alpha)
                            pos.AssignTo(behave.Pos)
                            subst input
                            subst behave
                            Mcu.addObjectLink behave entity.Index
                            Mcu.addTargetLink input behave.Index
                            if i = maxIndex - 1 then
                                Mcu.addTargetLink missionStarted behave.Index
                            yield input :> Mcu.McuBase
                            yield behave :> Mcu.McuBase
                    ]
                match runwayStartPos with
                | Some x -> yield (x, [ mcu; upcast entity ] @ serverInputs)
                | None -> ()
    ]

let createLandingDirections store (world : World) (state : WorldState) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    [
        for af in state.Airfields do
            let pos, ori = af.Runway
            let back = Vector2.UnitX.Rotate(ori)
            match sg.GetRegion(wg.GetAirfield(af.AirfieldId).Region).Owner with
            | Some owner ->
                let tee = LandingTee.Create(store, pos - 150.0f * back, ori, owner.ToCountry)
                yield tee.All
            | None ->
                ()
    ]

let createParkedPlanes store (world : World) (state : WorldState) (maxStaticPlanes : int) inAttackArea =
    let mkParkedPlane(model : PlaneModel, pos : OrientedPosition, country) =
        let modelScript = world.PlaneSet.StaticPlaneModel model
        let mcus =
            let durability =
                match model.PlaneType with
                | PlaneType.Fighter -> 6000
                | PlaneType.Attacker -> 8000
                | PlaneType.Bomber | PlaneType.Transport -> 8000
            if inAttackArea pos.Pos then
                let block, entity = newBlockWithEntityMcu store country modelScript.Model modelScript.Script durability
                [ block; upcast entity ]
            else
                [ newBlockMcu store country modelScript.Model modelScript.Script durability ]
        let p = McuUtil.newVec3(float pos.Pos.X, 0.0, float pos.Pos.Y)
        let ori = McuUtil.newVec3(0.0, float pos.Rotation, 0.0)
        for mcu in mcus do
            McuUtil.vecCopy p mcu.Pos
            McuUtil.vecCopy ori mcu.Ori
        mcus

    let wg = world.FastAccess
    let sg = state.FastAccess

    // Random distribution of planes among fitting parking slots
    let rnd = System.Random()

    let mkParkedPlanesAtAirfield(afs : AirfieldState) =
        [|
            let af = wg.GetAirfield afs.AirfieldId
            let reg = sg.GetRegion af.Region
            match reg.Owner with
            | Some coalition ->
                let country = coalition.ToCountry
                let fighterPlaces = ref(af.ParkedFighters |> Array.ofList |> Array.shuffle rnd |> List.ofArray)
                let attackerPlaces = ref(af.ParkedAttackers |> Array.ofList |> Array.shuffle rnd |> List.ofArray)
                let bomberPlaces = ref(af.ParkedBombers |> Array.ofList |> Array.shuffle rnd |> List.ofArray)
                // Assign a plane to a spot where it fits. Prioritize the smallest spots.
                let assign (plane : PlaneModel) =
                    match plane.PlaneType with
                    | PlaneType.Transport
                    | PlaneType.Bomber ->
                        match bomberPlaces.Value with
                        | [] -> None
                        | spot :: rest -> bomberPlaces := rest; Some spot
                    | PlaneType.Attacker ->
                        match bomberPlaces.Value, attackerPlaces.Value with
                        | _, spot :: rest -> attackerPlaces := rest; Some spot
                        | spot :: rest, [] -> bomberPlaces := rest; Some spot
                        | [], [] -> None
                    | PlaneType.Fighter ->
                        match bomberPlaces.Value, attackerPlaces.Value, fighterPlaces.Value with
                        | _, _, spot :: rest -> fighterPlaces := rest; Some spot
                        | _, spot :: rest, _ -> attackerPlaces := rest; Some spot
                        | spot :: rest, _, _ -> bomberPlaces := rest; Some spot
                        | [], [], [] -> None
                // Sort planes by decreasing size
                let planes =
                    afs.NumPlanes
                    |> Map.toSeq
                    |> Seq.sortBy(fun (plane, _) ->
                        match plane.PlaneType with
                        | PlaneType.Bomber
                        | PlaneType.Transport -> 0
                        | PlaneType.Attacker -> 1
                        | PlaneType.Fighter ->2)
                for plane, qty in planes do
                    for i in 0..(int qty - 1) do
                        match assign plane with
                        | Some spot ->
                            yield fun() -> mkParkedPlane(plane, spot, int country)
                        | None ->
                            ()
            | None ->
                ()
        |]

    [
        for afs in state.Airfields do
            let planes =
                mkParkedPlanesAtAirfield afs
                |> Array.shuffle rnd
                |> Seq.truncate maxStaticPlanes
                |> Seq.collect (fun f -> f())
                |> List.ofSeq
            yield! planes
    ]

let createLandFires (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (missionBegin : Mcu.McuTrigger) (group : Mcu.McuBase list) =
    let subst = Mcu.substId <| store.GetIdMapper()
    for mcu in group do
        subst mcu
    let areClose (mcu1 : Mcu.McuBase) (mcu2 : Mcu.McuBase) =
        let v1 = Vector2.FromMcu(mcu1.Pos)
        let v2 = Vector2.FromMcu(mcu2.Pos)
        (v1 - v2).Length() < 750.0f
    let parted =
        Algo.computePartition areClose group
    [
        for grp in parted do
            match grp with
            | [] -> ()
            | hd :: _ ->
                let pos = Vector2.FromMcu(hd.Pos)
                let owner =
                    List.zip world.Regions state.Regions
                    |> List.tryPick(fun (region, regState) ->
                        if pos.IsInConvexPolygon region.Boundary then
                            regState.Owner
                        else
                            None
                    )
                match owner with
                | None -> ()
                | Some owner ->
                    yield McuUtil.groupFromList grp
                    let coalition = owner.Other.ToCoalition
                    // Actually looking for friendlies, not enemies. It works the same, even though the name is misleading.
                    let wec = WhileEnemyClose.Create(true, true, store, pos, coalition)
                    yield wec.All
                    Mcu.addTargetLink missionBegin wec.StartMonitoring.Index
                    for mcu in grp do
                        match mcu with
                        | :? Mcu.McuEntity as entity ->
                            let startStop = Effect.EffectControl.Create(store, pos)
                            Mcu.addObjectLink startStop.Start entity.Index
                            Mcu.addObjectLink startStop.Stop entity.Index
                            Mcu.addTargetLink wec.WakeUp startStop.Start.Index
                            Mcu.addTargetLink wec.Sleep startStop.Stop.Index
                            yield startStop.All
                        | _ -> ()
    ]

let createLandLights(store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (missionBegin : Mcu.McuTrigger) (runwayStarts : (Vector2 * CoalitionId) list) (landLights : Mcu.McuBase list) =
    let lightsOn(lights : Mcu.McuEntity list) =
        let subst = Mcu.substId <| store.GetIdMapper()
        let prio = T.Integer 0
        let lowPrio = T.MCU_CMD_ForceComplete(T.String "Switch lights on", T.Boolean false, T.Integer 1, T.String "LightsOn", T.VectorOfIntegers[], prio, T.VectorOfIntegers[], T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0).CreateMcu() :?> Mcu.McuTrigger
        subst lowPrio
        for light in lights do
            Mcu.addObjectLink lowPrio light.Index
        let pos =
            let sum =
                lights
                |> List.sumBy (fun light -> Vector2.FromMcu light.Pos)
            sum / (float32 lights.Length)
        pos.AssignTo(lowPrio.Pos)
        Mcu.addTargetLink missionBegin lowPrio.Index
        lowPrio

    let subst = Mcu.substId <| store.GetIdMapper()
    for mcu in landLights do
        subst mcu

    [
        for mcu in landLights do
            match mcu with
            | :? Mcu.HasEntity as light when light.Name = "LandLight" ->
                let entity =
                    try
                        McuUtil.getEntityByIndex light.LinkTrId landLights
                    with _ ->
                        failwith "land lights must all have entities"
                let lightPos = Vector2.FromMcu light.Pos
                let runwayStart =
                    runwayStarts
                    |> List.tryFind (fun (pos, owner) -> (pos - lightPos).Length() < 200.0f)
                match runwayStart with
                | Some(_, owner) ->
                    light.Country <- Some owner.ToCountry
                    match owner with
                    | Allies ->
                        vehicles.RussianLandLight.AssignTo(light)
                    | Axis ->
                        vehicles.GermanLandLight.AssignTo(light)
                    let prioNode = lightsOn([entity])
                    yield light :> Mcu.McuBase
                    yield upcast entity
                    yield upcast prioNode
                | None ->
                    ()
            | _ ->
                ()
    ]
