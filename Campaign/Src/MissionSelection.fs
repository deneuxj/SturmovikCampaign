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

/// Selection of missions from the campaign to create a small subset to be included in a game mission file.
module Campaign.MissionSelection

open Campaign.WarState
open BasicTypes
open Campaign.Missions

type MissionSelection =
    {
        MainMission : AirMission
        HomeCover : AirMission option
        TargetCover : AirMission option
        Interception : AirMission option
        HomeAttack : AirMission option
        GroundBattleAtTarget : GroundMission option
        OtherMissions : Mission list
        RiskLevel : float32
    }
with
    member this.AreMissionsDistinct =
        let optDiff opt1 opt2 =
            match opt1, opt2 with
            | Some m1, Some m2 -> m1 <> m2
            | _ -> true
        optDiff (Some this.MainMission) this.HomeCover &&
        optDiff (Some this.MainMission) this.TargetCover &&
        optDiff this.HomeCover this.TargetCover &&
        optDiff this.Interception this.HomeAttack

    static member Create(mainMission) =
        {
            MainMission = mainMission
            HomeCover = None
            TargetCover = None
            Interception = None
            HomeAttack = None
            GroundBattleAtTarget = None
            OtherMissions = []
            RiskLevel = 0.0f
        }

    /// All the regions involved in the selected missions
    member this.Regions(world : NewWorldDescription.World) =
        let optAir (m : AirMission option) =
            seq {
                match m with
                | Some m ->
                    yield m.Objective
                    yield world.Airfields.[m.StartAirfield].Region
                    match m.MissionType with
                    | PlaneTransfer afId -> yield world.Airfields.[afId].Region
                    | _ -> ()
                | None -> ()
            }
        let optGround (m : GroundMission option) =
            seq {
                match m with
                | Some m ->
                    yield m.Objective
                    match m.MissionType with
                    | GroundForcesTransfer(_, startRegion, _) -> yield startRegion
                    | _ -> ()
                | None -> ()
            }
        seq {
            yield this.MainMission.Objective
            yield world.Airfields.[this.MainMission.StartAirfield].Region
            yield! optAir this.HomeCover
            yield! optAir this.TargetCover
            yield! optAir this.Interception
            yield! optAir this.HomeAttack
            yield! optGround this.GroundBattleAtTarget
            for m in this.OtherMissions do
                match m.Kind with
                | AirMission m -> yield m.Objective
                | GroundMission m -> yield m.Objective
        }
        |> Seq.distinct

    member this.GroundMissions =
        seq {
            match this.GroundBattleAtTarget with
            | Some m -> yield m
            | None -> ()
            for m in this.OtherMissions do
                match m.Kind with
                | GroundMission m -> yield m
                | _ -> ()
        }

let enumerateGroundAttackMissions (state : IWarStateQuery) (coalition : CoalitionId) (missions : Mission list) =
    let airMissionCoalition (mission : AirMission) =
        state.World.Airfields.[mission.StartAirfield].Region |> state.GetOwner

    let airMissions =
        missions
        |> Seq.choose (
            function
                | { Kind = AirMission airMission } -> Some airMission
                | _ -> None)

    let groundMissions =
        missions
        |> Seq.choose (
            function
                | { Kind = GroundMission groundMission } -> Some groundMission
                | _ -> None)

    let groundAttackMissions =
        seq {
            for mission in missions do
                match mission.Kind with
                | AirMission ({ MissionType = GroundTargetAttack _ } as airMission) ->
                    if airMissionCoalition airMission = Some coalition then
                        let interceptions =
                            airMissions
                            |> Seq.filter (
                                function
                                    | { MissionType = AreaProtection; Objective = objective } as mission ->
                                        airMission.Objective = objective && airMissionCoalition mission <> Some coalition
                                    | _ -> false)
                        let homeAttacks =
                            airMissions
                            |> Seq.filter (
                                function
                                    | { MissionType = AreaProtection; Objective = objective } as mission ->
                                        state.World.Airfields.[airMission.StartAirfield].Region = objective && airMissionCoalition mission <> Some coalition
                                    | _ -> false)
                        let targetCovers =
                            airMissions
                            |> Seq.filter (
                                function
                                    | { MissionType = AreaProtection; Objective = objective } as mission ->
                                        airMission.Objective = objective && airMissionCoalition mission = Some coalition
                                    | _ -> false)
                        let homeCovers =
                            airMissions
                            |> Seq.filter (
                                function
                                    | { MissionType = AreaProtection; Objective = objective } as mission ->
                                        state.World.Airfields.[airMission.StartAirfield].Region = objective && airMissionCoalition mission = Some coalition
                                    | _ -> false)
                        // Quiet, unopposed
                        let basic = MissionSelection.Create(airMission)
                        yield basic
                        for targetCover in targetCovers do
                            // Unopposed, with escort. So that players don't always expect to be intercepted whenever they have an escort
                            yield
                                { basic with
                                    TargetCover = Some targetCover
                                }
                        for interception in interceptions do
                            // Without escort, intercepted on way to target
                            let intercepted =
                                { basic with
                                    Interception = Some interception
                                    RiskLevel = 10.0f
                                }
                            yield intercepted
                            for targetCover in targetCovers do
                                // Intercepted on way to target, with escort
                                yield
                                    { intercepted with
                                        TargetCover = Some targetCover
                                        RiskLevel = 5.0f
                                    }
                            for homeAttack in homeAttacks |> Seq.filter ((<>) interception) do
                                // Intercepted on way to target, and on way back home. No escort
                                let egressAmbush =
                                    { intercepted with
                                        HomeAttack = Some homeAttack
                                        RiskLevel = 20.0f
                                    }
                                yield egressAmbush
                                for targetCover in targetCovers do
                                    // Intercepted on way to target, and on way back home. With escort to target
                                    yield
                                        { egressAmbush with
                                            TargetCover = Some targetCover
                                            RiskLevel = 15.0f
                                        }
                                    // Intercepted on way to target, and on way back home. With escort to target, and cover at home airfield
                                    for homeCover in homeCovers |> Seq.filter ((<>) targetCover) do
                                        yield
                                            { egressAmbush with
                                                TargetCover = Some targetCover
                                                HomeCover = Some homeCover
                                                RiskLevel = 12.5f
                                            }
                                // Intercepted on way to target, and on way back home. Only help from cover at home airfield
                                for homeCover in homeCovers do
                                    yield
                                        { egressAmbush with
                                            HomeCover = Some homeCover
                                            RiskLevel = 15.f
                                        }
                        for homeAttack in homeAttacks do
                            // Uneventful to target, but caught on way back without help
                            let egressAmbush =
                                { basic with
                                    HomeAttack = Some homeAttack
                                    RiskLevel = 10.0f
                                }
                            yield egressAmbush
                            for homeCover in homeCovers do
                                // Uneventful to target, caught on way back, with help on the way
                                yield
                                    { egressAmbush with
                                        HomeCover = Some homeCover
                                        RiskLevel = 5.f
                                    }
                        ()
                | _ ->
                    ()
        }
    // If the main mission is targetting a ground battle, include the ground battle mission in the selection
    groundAttackMissions
    |> Seq.collect (fun selection ->
        match selection.MainMission.MissionType with
        | GroundTargetAttack(GroundForces coalition, _) ->
            let groundBattles =
                groundMissions
                |> Seq.filter (fun gm ->
                    gm.Objective = selection.MainMission.Objective &&
                    Some coalition <> airMissionCoalition selection.MainMission)
                |> List.ofSeq
            seq {
                if groundBattles.IsEmpty then
                    yield selection
                else
                    for gm in groundBattles do
                        yield
                            { selection with
                                GroundBattleAtTarget = Some gm
                            }
            }
        | _ ->
            Seq.singleton selection
    )