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

/// Types to maintain the state of a campaign and plan missions
namespace Campaign.CampaignScenario

open System.Numerics
open VectorExtension
open Util

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel

open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.Missions
open Campaign.Common.GroundUnit


module WorldWar2Internal =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    type ImplData =
        {
            // The side that is launching attacks against ground assets (airfields, troops...)
            OffensiveCoalition : CoalitionId
        }

    type Constants =
        {
            /// For mission planning: typical distance from airbase to target for a fighter-based strafing mission
            TypicalRange : float32<M>
            /// Volume of consumables and maintenance per flown meter
            PlaneRunCost : float32<M^3/M>
            /// Volume of consumables and maintenance per kg of bomb
            BombCost : float32<M^3/K>
            /// Max number of planes at each airfield during initial plane distribution
            MaxPlanesAtAirfield : float32
            /// Minimum amount of resources at an airfield to be considered as a target during mission planning
            MinActiveAirfieldResources : float32<M^3>
            /// Minimum amount of storage capacity in a region to be considered as a target during mission planning
            MinRegionBuildingCapacity : float32<M^3>
            /// New plane delivery period
            NewPlanesPeriod : float32<H>
            /// Numbers of planes delivered
            NumNewPlanes : float32
            /// New troops delivery period
            NewTroopsPeriod : float32<H>
            /// Amount of ground forces delivered
            NumNewTroops : float32<MGF>
            /// Earliest mission time, in time from midnight
            EarliestStart : float32<H>
            /// Latest missiont time
            LatestStart : float32<H>
            /// Minimum time between mission starts
            MinStartDiff : float32<H>
            /// Maximum time between mission starts
            MaxStartDiff : float32<H>
        }
    with
        static member Default(startDate : System.DateTime) =
            let typicalRange = 400.0e3f<M>
            let planeRunCost = 5.0f<M^3> / typicalRange
            let bombCost = 0.01f<M^3/K>
            let maxPlanesAtAirfield = 200.0f
            let minActiveAirfieldResources = planeRunCost * 10.0f * typicalRange
            let minRegionBuildingCapacity = 1000.0f<M^3>
            let day = 24.0f<H>
            let seasonShorten =
                if startDate.Month <= 3 || startDate.Month >= 11 then
                    2.0f<H>
                elif startDate.Month <= 5 || startDate.Month >= 9 then
                    1.0f<H>
                else
                    0.0f<H>
            {
                TypicalRange = typicalRange
                PlaneRunCost = planeRunCost
                BombCost = bombCost
                MaxPlanesAtAirfield = maxPlanesAtAirfield
                MinActiveAirfieldResources = minActiveAirfieldResources
                MinRegionBuildingCapacity = minRegionBuildingCapacity
                NewPlanesPeriod = 3.0f * day
                NumNewPlanes = 300.0f
                NewTroopsPeriod = 3.0f * day
                NumNewTroops = 1000.0f<MGF>
                EarliestStart = 7.0f<H> + seasonShorten
                LatestStart = 17.0f<H> - seasonShorten
                MinStartDiff = 5.0f<H>
                MaxStartDiff = 12.0f<H>
            }

    let groundUnitsSet(date : System.DateTime) =
        [
            Russia, [
                "gaz-m"
                "gaz-aa"
                "zis5-72k"
                "t34-76stz-41"
                "t70"
                "bm13"
                "zis3gun"
                "52k"
                "61k"
                "ba10m"
                "searchlightsu"
            ]
            Germany, [
                "opel-blitz"
                "horch830"
                "mg34-aa"
                "sdkfz10-flak38"
                "pziv-f1"
                "pziii-h"
                "sdkfz251-szf"
                "pak40"
                "flak37"
                "flak36"
                "sdkfz251-1c"
                "searchlightger"
            ]
            UnitedStates, [
                "m1gun"
                "m1a1gun-aa"
                "m2mg-aa"
                "m2mg"
                "m5gun"
                "gmc-cckw"
                "m16"
                "m4a3"
                "m4a2"
                "willys"
                "searchlightus"
            ]
            GreatBritain, []
            Italy, []
        ]

    type GroundUnitSet =
        {
            GroundUnits : (CountryId * string list) list
        }
    with

        static member Default =
            {
                GroundUnits = groundUnitsSet(System.DateTime(1945, 1, 1))
            }

        member this.Setup(world: World): World =
            let planeDb =
                Campaign.PlaneModelDb.planeDb
                |> List.map (fun plane -> plane.Id, plane)
                |> Map.ofList

            // Alternatives for AI-only planes
            let planeAlts =
                match planeDb.TryFind (PlaneModelId "b25"), planeDb.TryFind (PlaneModelId "a20") with
                | Some b25, Some a20 ->
                    [ b25.Id, [a20] ]
                | Some b25, None ->
                    [ b25.Id, [] ]
                | None, _ ->
                    []

            let groundUnits =
                this.GroundUnits
                |> List.map (fun (country, units) -> country, units |> List.map GroundUnitId)

            for _, groundUnits in groundUnits do
                for gu in groundUnits do
                    match world.GroundUnits.TryGetValue(gu) with
                    | false, _ ->
                        logger.Error(sprintf "No unit '%s'" (string gu))
                    | true, _ ->
                        ()

            { world with PlaneAltsList = planeAlts; GroundUnitsOfCountryList = groundUnits }

        interface IScenarioWorldSetup with
            member this.Setup(world) = this.Setup(world)

open WorldWar2Internal
open FSharp.Json
open Util.Json

/// A campaign scenario implementation for WWII in the European and East front theaters: Bomb industry, airfields, harass and protect ground troops
type WorldWar2(world : World, C : Constants) =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    let totalPlanes : Map<_, float32> -> float32 =
        Map.toSeq >> Seq.sumBy (snd >> floor)

    let typicalRange = C.TypicalRange
    let planeRunCost = C.PlaneRunCost
    let bombCost = C.BombCost
    let maxPlanesAtAirfield = C.MaxPlanesAtAirfield
    let minActiveAirfieldResources = C.MinActiveAirfieldResources
    let minRegionBuildingCapacity = C.MinRegionBuildingCapacity

    let checkoutDataAir (mission : AirMission) =
        let distance =
            1.0f<M> * (world.Regions.[mission.Objective].Position - world.Airfields.[mission.StartAirfield].Position).Length()
        let bombs =
            match mission.MissionType with
            | Strafing _ | Bombing _ -> world.PlaneSet.[mission.Plane].BombCapacity * bombCost
            | AreaProtection -> 0.0f<M^3>
            | PlaneTransfer _ -> 0.0f<M^3>
        (mission.Plane, distance * planeRunCost + bombs, float32 mission.NumPlanes)

    let checkoutDataGround (mission : GroundMission) =
            match mission with
            | { MissionType = GroundBattle _ } -> None
            | { MissionType = GroundForcesTransfer(coalition, start, forces) } -> Some(coalition, start, forces)

    let withRole role coalition =
        world.PlaneModelsList
        |> List.filter (fun plane -> plane.Coalition = coalition && plane.Roles |> List.exists ((=) role))
        |> List.sortByDescending (fun plane -> plane.Cost)
    let attackersOf = withRole PlaneRole.GroundAttacker
    let interceptorsOf = withRole PlaneRole.Interceptor
    let fightersOf coalition =
        (withRole PlaneRole.Patroller coalition) @ (withRole PlaneRole.Interceptor coalition)
        |> List.distinctBy (fun plane -> plane.Id)
    let bombersOf = withRole PlaneRole.LevelBomber

    let allPlanesOf coalition =
        world.PlaneModelsList
        |> List.filter (fun plane -> plane.Coalition = coalition)
        |> List.distinctBy (fun plane -> plane.Id)

    /// Get the total number of planes of give types at an airfield
    let sumPlanes (atAirfield : Map<PlaneModelId, float32>) (planes : PlaneModelId seq) =
        planes
        |> Seq.fold (fun s plane ->
            atAirfield.TryFind(plane)
            |> Option.defaultValue 0.0f
            |> (+) s) 0.0f

    /// Set the number of planes at each airfield controlled by a coalition.
    let initAirfields (factor : float32) (friendly : CoalitionId) (war : IWarState) =
        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy

        let planes =
            allPlanesOf friendly
            |> Seq.filter (fun plane -> plane.Kind <> PlaneType.Transport)
            |> Seq.sortByDescending (fun plane -> plane.Cost)
            |> List.ofSeq

        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> af.IsActive && war.GetOwner(af.Region) = Some friendly)
            |> Seq.sortByDescending (fun af -> distanceToEnemy.[af.Region])
            |> List.ofSeq

        let avgCost =
            let allPlanes = allPlanesOf friendly
            allPlanes
            |> Seq.map (fun plane -> plane.Cost)
            |> Seq.sum
            |> fun total -> total / (float32 (List.length allPlanes))
        
        let mutable planesCostLeft = 3.0f * C.NumNewPlanes * avgCost

        // Non-transport planes: Set according to airfield resources and respective cost
        let planeRunCost = typicalRange * planeRunCost
        match planes with
        | [] -> ()
        | [plane] ->
            for af in airfields do
                if planesCostLeft > 0.0f<E> then
                    let numPlanes =
                        war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                    war.SetNumPlanes(af.AirfieldId, plane.Id, factor * numPlanes)
                    planesCostLeft <- planesCostLeft - numPlanes * plane.Cost
        | expansive :: regular ->
            // Rear airfields get regular planes and the expensive plane
            // Other airfields get only regular planes
            let regular = List.rev regular
            airfields
            |> List.iteri (fun i af ->
                let planes =
                    regular
                let planes =
                    if war.World.Regions.[af.Region].IsEntry then
                        planes @ [expansive]
                    else
                        planes
                // Remove planes that require longer runways than available
                let planes =
                    planes
                    |> List.filter (fun plane ->
                        af.Runways
                        |> List.exists (fun rw -> (rw.End - rw.Start).Length() * 1.0f<M> >= plane.MinRunwayLength))
                let totalInvCost =
                    planes
                    |> List.sumBy (fun plane -> 1.0f / plane.Cost)
                let numPlanes =
                    war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                    |> min maxPlanesAtAirfield
                for plane in planes do
                    if planesCostLeft > 0.0f<E> then
                        let qty = numPlanes * (1.0f / plane.Cost) / totalInvCost |> max 1.0f
                        planesCostLeft <- planesCostLeft - qty * plane.Cost
                        war.SetNumPlanes(af.AirfieldId, plane.Id, factor * qty)
                )

        logger.Info(sprintf "Planes cost left for %s: %3.0f" (string friendly) planesCostLeft)

        // Transport planes at the rearmost airfield
        match List.tryLast airfields with
        | Some rear ->
            let transport =
                allPlanesOf friendly
                |> List.filter (fun plane -> plane.Kind = PlaneType.Transport)
            for plane in transport do
                war.SetNumPlanes(rear.AirfieldId, plane.Id, 20.0f)
        | None ->
            ()


    /// Compute forces needed to defend a region under one's control
    let neededForDefenses (war : IWarStateQuery, friendly : CoalitionId, rId : RegionId) =
        if war.GetOwner(rId) = Some friendly then
            let numAirfields =
                war.World.Airfields.Values
                |> Seq.filter (fun af -> af.Region = rId)
                |> Seq.length
            let threatsFactor =
                if numAirfields > 0 then
                    1.0f
                else
                    0.25f
            let neededForAA =
                let numNests = 3.0f
                let numGuns = 5.0f
                (float32 numAirfields) * numNests * numGuns * Campaign.Common.Targets.TargetType.Artillery.GroundForceValue / war.World.AntiAirGroundForcesRatio
            let needed = max neededForAA (war.GetGroundForces(friendly.Other, rId) + threatsFactor * war.GroundThreatsToRegion(rId, friendly))
            needed
        else
            0.0f<MGF>

    /// Set ground forces of a coalition in each region
    let initGroundForces (factor : float32, coalition : CoalitionId, war : IWarState) =
        for region in war.World.Regions.Values do
            let rId = region.RegionId
            let owner = war.GetOwner(region.RegionId)
            if owner = Some coalition then
                let numAirfields =
                    war.World.Airfields.Values
                    |> Seq.filter (fun af -> af.Region = rId)
                    |> Seq.length
                let neededForAA =
                    let numNests = 3.0f
                    let numGuns = 5.0f
                    (1.0f + float32 numAirfields) * numNests * numGuns * Campaign.Common.Targets.TargetType.Artillery.GroundForceValue / war.World.AntiAirGroundForcesRatio
                let needed = neededForAA
                war.SetGroundForces(coalition, region.RegionId, needed * factor)
            else
                war.SetGroundForces(coalition, region.RegionId, 0.0f<MGF>)

    // Balance ground forces by giving the coalition with fewest troops the difference at the first rear region
    let balanceGroundForces (axisFactor : float32, alliesFactor : float32, war : IWarState) =
        let axisForces =
            war.World.Regions.Values
            |> Seq.sumBy (fun region -> war.GetGroundForces(Axis, region.RegionId))
        let alliesForces =
            war.World.Regions.Values
            |> Seq.sumBy (fun region -> war.GetGroundForces(Allies, region.RegionId))
        let axisUnder = alliesForces / alliesFactor - axisForces / axisFactor
        let alliesUnder = -axisUnder
        let balance(coalition, amount) =
            war.World.Regions.Values
            |> Seq.tryFind (fun region -> region.InitialOwner = Some coalition && region.IsEntry)
            |> Option.iter (fun region -> war.SetGroundForces(coalition, region.RegionId, war.GetGroundForces(coalition, region.RegionId) + amount))
        if axisUnder > 0.0f<MGF> then
            balance(Axis, axisUnder * axisFactor)
        if alliesUnder > 0.0f<MGF> then
            balance(Allies, alliesUnder * alliesFactor)

    /// Try to make missions to attack generic enemy ground targets, with sufficient cover over target, and over home airfield
    let tryMakeAirRaids (war : IWarStateQuery) (adapter : TargetAdapter<'Target>) (description : string) (raidTargets : 'Target list) (friendly : CoalitionId) (budget : ForcesAvailability)  =
        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy

        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> af.IsActive && war.GetOwner(af.Region) = Some friendly)
            |> Seq.sortBy (fun af -> distanceToEnemy.[af.Region])
            |> List.ofSeq

        let readyAirfields =
            airfields
            |> List.filter (fun af ->
                war.GetGroundForces(enemy, af.Region) = 0.0f<MGF> &&
                war.GetAirfieldCapacity(af.AirfieldId) >= minActiveAirfieldResources &&
                totalPlanes(war.GetNumPlanes(af.AirfieldId)) >= 5.0f)

        match readyAirfields, raidTargets with
        | [], _ -> Plan("No planes for " + description, [], budget)
        | _, [] ->
            TooFewTargets description
        | _ :: _, _ :: _ ->
            let raids =
                let isInPlaneRange (plane : PlaneModelId) (start : Airfield) (target : Vector2) =
                    let maxRange = war.World.PlaneSet.[plane].MaxRange
                    (start.Position - target).Length() * 1.0f<M> < maxRange
                // For each attack plane type, and for each potential target airfield,
                // Try to plan an attack flight, a target cover flight with fighters,
                // and a home cover flight with intercepters.
                // Prioritized solutions to maximize number of attack planes, then number of fighters/interceptors,
                // then fighter plane model, and finally interceptor plane model.
                [
                    let mutable budget = budget
                    // At most one attack mission and pair of CAP missions from each airfield
                    let readyAttackAirfields = ResizeArray(readyAirfields)
                    let readyCAPAirfields = ResizeArray(readyAirfields)

                    for target in raidTargets do
                        let targetRegion = adapter.GetRegion target
                        let targetPos = adapter.GetPos target
                        let missionAlternatives readyAirfields (budget : ForcesAvailability) mkMissionFrom destination =
                            readyAirfields 
                            |> Seq.choose (fun af ->
                                let mission : AirMission option = mkMissionFrom af
                                match mission with
                                | None ->
                                    None
                                | Some mission ->
                                match budget.TryCheckoutPlane(af.AirfieldId, checkoutDataAir mission) with
                                | None -> None
                                | Some budget ->
                                    if isInPlaneRange mission.Plane af destination then
                                        Some(mission, budget)
                                    else
                                        None)
                        let groupAndBudget =
                            seq {
                                for (mType, minDist), attacker in (List.allPairs [Bombing, 70000.0f] (bombersOf friendly)) @ (List.allPairs [Strafing, 15000.0f] (attackersOf friendly)) do
                                for numAttackers in [15; 10; 5] do
                                let mkAttackerMission af =
                                    if (af.Position - targetPos).Length() >= minDist then
                                        { StartAirfield = af.AirfieldId
                                          Objective = targetRegion
                                          MissionType = mType(adapter.MkGroundTarget target)
                                          Plane = attacker.Id
                                          NumPlanes = numAttackers
                                        } |> Some
                                    else
                                        None
                                for attackerMission, budget in missionAlternatives readyAttackAirfields budget mkAttackerMission targetPos do
                                let attackerStart = war.World.Airfields.[attackerMission.StartAirfield]
                                for numFighters in [8; 4; 2] do
                                for fighter in fightersOf friendly do
                                let mkFighterMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = targetRegion
                                      MissionType = AreaProtection
                                      Plane = fighter.Id
                                      NumPlanes = numFighters }
                                    |> Some
                                for fighterMission, budget in missionAlternatives readyCAPAirfields budget mkFighterMission targetPos do
                                for numInterceptors in [4; 3; 2] do
                                for interceptor in interceptorsOf friendly do
                                let mkInterceptorMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = attackerStart.Region
                                      MissionType = AreaProtection
                                      Plane = interceptor.Id
                                      NumPlanes = numInterceptors }
                                    |> Some
                                // Do not use the same airfield as the CAP mission for the interception
                                let readyCAPAirfields2 =
                                    readyCAPAirfields
                                    |> Seq.filter (fun af -> af.AirfieldId <> fighterMission.StartAirfield)
                                for interceptorMission, budget in missionAlternatives readyCAPAirfields2 budget mkInterceptorMission attackerStart.Position do
                                    yield [
                                        attackerMission, description
                                        fighterMission, "Target cover"
                                        interceptorMission, "Home base cover"
                                    ], budget
                            }
                            |> Seq.tryHead
                        match groupAndBudget with
                        | Some(group, x) ->
                            budget <- x
                            readyAttackAirfields.RemoveAll(fun af -> af.AirfieldId = fst(group.[0]).StartAirfield) |> ignore
                            for i in 1..2 do
                                readyCAPAirfields.RemoveAll(fun af -> af.AirfieldId = fst(group.[i]).StartAirfield) |> ignore
                            yield! group
                        | None ->
                            ()
                ]
                |> List.map (fun (m, description) ->
                    { 
                        Kind = AirMission m
                        Description =
                            sprintf "%s: %d %s from %s to %s"
                                description
                                m.NumPlanes
                                (string m.Plane)
                                m.StartAirfield.AirfieldName
                                (string m.Objective)
                    })
            match raids with
            | [] ->
                Plan ("No planes in range for " + description, [], budget)
            | _::_ ->
                // Compute again resource consumption by missions on each start airfield
                let budget =
                    raids
                    |> Seq.fold (fun budget mission ->
                        match mission with
                        | { Kind = AirMission mission } ->
                            budget
                            |> Option.bind (fun (budget : ForcesAvailability) ->
                                budget.TryCheckoutPlane(mission.StartAirfield, checkoutDataAir mission))
                        | _ -> budget
                    ) (Some budget)
                Plan (description, raids, Option.get budget)

    /// Try to make missions to attack enemy airfields, with sufficient cover over target, and over home airfield
    let tryMakeAirfieldRaids (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability)  =
        let distanceToFriendly = war.ComputeDistancesToCoalition friendly
        let enemy = friendly.Other

        let enemyAirfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> af.IsActive && war.GetOwner(af.Region) = Some enemy)
            |> Seq.sortBy (fun af -> distanceToFriendly.[af.Region])
            |> List.ofSeq

        let raidTargets =
            enemyAirfields
            |> List.filter (fun af ->
                let numPlanes = totalPlanes(war.GetNumPlanes(af.AirfieldId))
                let resources = war.GetAirfieldCapacity(af.AirfieldId)
                numPlanes >= 1.0f && resources >= minActiveAirfieldResources)

        tryMakeAirRaids war (AirfieldTargetAdapter()) "Airfield strike" raidTargets friendly budget

    /// Try to make missions to attack enemy industry, with cover over target, and over home airfield
    let tryMakeIndustryRaids (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let distanceToFriendly = war.ComputeDistancesToCoalition friendly
        let enemy = friendly.Other

        let enemyRegions =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                war.GetOwner(region.RegionId) = Some enemy &&
                distanceToFriendly.[region.RegionId] > 1 &&
                war.GetRegionBuildingCapacity(region.RegionId) > minRegionBuildingCapacity)
            |> Seq.sortByDescending (fun region -> distanceToFriendly.[region.RegionId])
            |> List.ofSeq

        tryMakeAirRaids war (RegionTargetAdapter()) "Industry raid" enemyRegions friendly budget

    /// Try to plan missions to defend ground forces in friendly territory.
    let tryMakeDefensiveGroundForcesRaids (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let threatenedRegions =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                war.GetOwner(region.RegionId) = Some friendly &&
                war.GetGroundForces(friendly, region.RegionId) < 5.0f * war.GetGroundForces(friendly.Other, region.RegionId))
            |> Seq.sortByDescending (fun region ->
                war.GetGroundForces(friendly.Other, region.RegionId) / war.GetGroundForces(friendly, region.RegionId),
                war.GetGroundForces(friendly.Other, region.RegionId))
            |> List.ofSeq

        match threatenedRegions with
        | [] ->
            Plan("No regions need defensive air support", [], budget)
        | _ :: _ ->
            tryMakeAirRaids war (GroundForcesTargetAdapter(friendly.Other)) "Close air support" threatenedRegions friendly budget

    /// Try to plan missions to attack ground forces in enemy territory
    let tryMakeOffensiveGroundForcesRaids (war : IWarStateQuery) (onlySupport : bool) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let weakRegions =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                war.GetOwner(region.RegionId) = Some friendly.Other &&
                war.GetGroundForces(friendly.Other, region.RegionId) > 0.0f<MGF> &&
                (not onlySupport || war.GetGroundForces(friendly, region.RegionId) > 0.0f<MGF>))
            |> Seq.sortByDescending (fun region ->
                war.GetGroundForces(friendly.Other, region.RegionId) / if onlySupport then war.GetGroundForces(friendly, region.RegionId) else 1.0f<MGF>)
            |> List.ofSeq

        match onlySupport, weakRegions with
        | true, [] ->
            Plan("No invasion to cover", [], budget)
        | false, [] ->
            TooFewTargets "Enemy ground troops destroyed"
        | _, _::_ ->
            tryMakeAirRaids war (GroundForcesTargetAdapter(friendly.Other)) "Close air support" weakRegions friendly budget

    /// Try to make CAP missions over regions targetted by enemy air missions.
    let tryMakeCovers (war : IWarStateQuery) (friendly : CoalitionId) (enemyMissions : AirMission list) (budget : ForcesAvailability) =
        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy

        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some friendly)
            |> Seq.sortBy (fun af -> distanceToEnemy.[af.Region])
            |> List.ofSeq

        let readyAirfields =
            airfields
            |> List.filter (fun af ->
                war.GetGroundForces(enemy, af.Region) = 0.0f<MGF> &&
                war.GetAirfieldCapacity(af.AirfieldId) >= minActiveAirfieldResources &&
                totalPlanes(war.GetNumPlanes(af.AirfieldId)) >= 2.0f)

        let targettedRegions =
            enemyMissions
            |> List.map (fun mission -> mission.Objective)
            |> Set.ofList
            |> List.ofSeq

        match readyAirfields, targettedRegions with
        | [], _ -> Plan("No planes for air defense", [], budget)
        | _, [] -> Plan("No enemy plane incursions", [], budget)
        | _ :: _, _ :: _ ->
            let mutable budget = budget
            let missions =
                [
                    for target in targettedRegions do
                        let mission =
                            let planes = interceptorsOf friendly @ fightersOf friendly
                            let numbers = [8; 4; 2]
                            readyAirfields
                            |> Seq.allPairs planes
                            |> Seq.allPairs numbers
                            |> Seq.tryPick (fun (planes, (plane, start)) ->
                                let distance =
                                    (war.World.Regions.[target].Position - start.Boundary.Head).Length() * 1.0f<M>
                                if distance <= plane.MaxRange then
                                    let mission =
                                        {
                                            StartAirfield = start.AirfieldId
                                            Objective = target
                                            MissionType = AreaProtection
                                            NumPlanes = planes
                                            Plane = plane.Id
                                        }
                                    let budget2 =
                                        budget.TryCheckoutPlane(start.AirfieldId, checkoutDataAir mission)
                                    match budget2 with
                                    | Some budget2 ->
                                        budget <- budget2
                                        Some mission
                                    | None ->
                                        None
                                else
                                    None
                            )
                        yield! Option.toList mission
                ]
                |> List.map (fun m ->
                    { Kind = AirMission m
                      Description = "Combat air patrol" })
            Plan("Air defense", missions, budget)

    /// Try to send planes closer to the frontline.
    let tryTransferPlanesForward (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> af.IsActive && war.GetOwner(af.Region) = Some friendly)
            |> List.ofSeq

        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy
        let planeRunCost = typicalRange * planeRunCost + 500.0f<K> * bombCost
        let mutable budget = budget
        let missions =
            [
                let forward =
                    Seq.allPairs airfields airfields
                    |> Seq.where (fun (af1, af2) -> distanceToEnemy.[af1.Region] > distanceToEnemy.[af2.Region])
                for af1, af2 in forward do
                    let mutable excessPlanes =
                        let minAmount =
                            if distanceToEnemy.[af1.Region] > 4 then
                                5.0f
                            else
                                15.0f
                        let afid = af1.AirfieldId
                        let af = budget.Airfields.[afid]
                        totalPlanes af.Planes - minAmount
                    let mutable sustainable =
                        let afid = af2.AirfieldId
                        let af = budget.Airfields.[afid]
                        (af.Resources - totalPlanes af.Planes * planeRunCost) / planeRunCost
                    let allButTransport =
                        allPlanesOf friendly
                        |> List.filter (fun plane -> plane.Kind <> PlaneType.Transport)
                    for plane in allButTransport do
                        if excessPlanes > 0.0f && sustainable > 0.0f then
                            let numPlanes =
                                sumPlanes budget.Airfields.[af1.AirfieldId].Planes [plane.Id]
                                |> min excessPlanes
                                |> int
                                |> min 25
                            let planeModel = war.World.PlaneSet.[plane.Id]
                            let runwayLength =
                                try
                                    af2.Runways
                                    |> List.map (fun rw -> 1.0f<M> * (rw.End - rw.Start).Length())
                                    |> List.max
                                with _ -> 0.0f<M>
                            if numPlanes > 0 && planeModel.MinRunwayLength <= runwayLength then
                                let mission =
                                    {
                                        StartAirfield = af1.AirfieldId
                                        Objective = af2.Region
                                        MissionType = PlaneTransfer af2.AirfieldId
                                        NumPlanes = numPlanes
                                        Plane = plane.Id
                                    }
                                let budget2 = budget.TryCheckoutPlane(af1.AirfieldId, checkoutDataAir mission)
                                match budget2 with
                                | Some b ->
                                    budget <- b
                                    let numPlanesF = (float32 numPlanes)
                                    excessPlanes <- excessPlanes - numPlanesF
                                    sustainable <- sustainable - numPlanesF
                                    yield mission, sprintf "Transfer %d %s from %s to %s" numPlanes plane.Name af1.AirfieldId.AirfieldName af2.AirfieldId.AirfieldName
                                | None ->
                                    ()
            ]
            |> List.map (fun (m, description) ->
                { Kind = AirMission m
                  Description = description })
        Plan ("Transfers", missions, budget)

    /// Evacuate airfields in regions under threat
    let tryEvacuatePlanes (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> af.IsActive && war.GetOwner(af.Region) = Some friendly)
            |> List.ofSeq

        let mutable budget = budget
        let missions =
            [
                for af in airfields do
                    let enemyForces = war.GetGroundForces(friendly.Other, af.Region)
                    let friendlyForces = war.GetGroundForces(friendly, af.Region)
                    if enemyForces >= 0.75f * friendlyForces then
                        let destinations =
                            airfields
                            |> List.sortBy (fun af2 -> (af.Position - af2.Position).LengthSquared())
                            |> List.filter (fun af2 ->
                                let enemyForces = war.GetGroundForces(friendly.Other, af2.Region)
                                let friendlyForces = war.GetGroundForces(friendly, af2.Region)
                                enemyForces <= 0.75f * friendlyForces || enemyForces = 0.0f<MGF>
                            )
                        for af2 in destinations do
                            for (plane, qty) in war.GetNumPlanes(af.AirfieldId) |> Map.toSeq do
                                let airMission =
                                    {
                                        StartAirfield = af.AirfieldId
                                        Objective = af2.Region
                                        MissionType = PlaneTransfer af2.AirfieldId
                                        NumPlanes = int qty
                                        Plane = plane
                                    }
                                let mission =
                                    {
                                        Kind = AirMission airMission
                                        Description = sprintf "Evacuation of %s to from %s to %s" (string plane) af.AirfieldId.AirfieldName af2.AirfieldId.AirfieldName
                                    }
                                let budget2 = budget.TryCheckoutPlane(af.AirfieldId, checkoutDataAir airMission)
                                match budget2 with
                                | Some b ->
                                    budget <- b
                                    yield mission
                                | None ->
                                    ()
            ]
        Plan ("Airfield evacuation", missions, budget)

    /// Try to plan troop movements in enemy territory
    let tryPlanInvasions (timeSpan : float32<H>) (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let distanceToAirfields =
            war.ComputeDistancesToAirfields()
        let roads = war.ComputeRoadCapacity()
        let targets =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                match war.GetOwner(region.RegionId) with
                | Some owner when owner <> friendly ->
                    region.Neighbours
                    |> Seq.exists (fun ngh -> war.GetGroundForces(friendly, ngh) > 0.0f<MGF>)
                | None ->
                    true
                | Some _ ->
                    false)
            |> Seq.sortBy (fun region -> distanceToAirfields.[region.RegionId])
            |> List.ofSeq
        seq {
            let mutable budget = budget
            for target in targets do
                logger.Debug(sprintf "Considering invasion of %s" (string target.RegionId))
                let friendlyForces = war.GetGroundForces(friendly, target.RegionId)
                let ennemyForces = war.GetGroundForces(friendly.Other, target.RegionId)
                let transportableFrom = Seq.mutableDict []
                let targetOwner = war.GetOwner(target.RegionId)
                for ngh in target.Neighbours do
                    let capacity = roads(ngh, target.RegionId)
                    let available = budget.Regions.TryFind(ngh, friendly) |> Option.defaultValue 0.0f<MGF>
                    let transportable =
                        // Prevent moves within enemy territory
                        if targetOwner <> Some friendly && war.GetOwner(ngh) <> Some friendly then
                            0.0f<MGF>
                        else
                            war.World.GroundForcesTransport(capacity, available, timeSpan)
                    if transportable > 0.0f<MGF> then
                        logger.Debug(sprintf "Can transport %0.0f from %s" transportable (string ngh))
                        transportableFrom.[ngh] <- transportable
                let neighbours =
                    transportableFrom.Keys
                    |> Seq.sortByDescending (fun ngh -> transportableFrom.[ngh])
                let totalTransportable = transportableFrom.Values |> Seq.sum
                if friendlyForces + totalTransportable >= 0.5f * ennemyForces then
                    let mutable desiredForceAddition = 2.0f * ennemyForces - friendlyForces
                    let missions =
                        [
                            for ngh in neighbours do
                                let force = transportableFrom.[ngh]
                                match budget.TryCheckoutGroundForce(friendly, ngh, force) with
                                | None -> ()
                                | Some budget2 ->
                                    budget <- budget2
                                    if force > 0.0f<MGF> && desiredForceAddition >= 0.0f<MGF> then
                                        desiredForceAddition <- desiredForceAddition - force
                                        logger.Debug(sprintf "Will move %0.0f from %s" force (string ngh))
                                        yield {
                                            Objective = target.RegionId
                                            MissionType = GroundForcesTransfer(friendly, ngh, force)
                                        }
                        ]
                    if not missions.IsEmpty then
                        yield missions
                else
                    logger.Debug("Insufficient forces to send for invasion compared to defenses")
        }
        |> Seq.choose (fun missions ->
            missions
            |> List.fold (fun (budget : ForcesAvailability option) mission ->
                budget
                |> Option.bind (fun budget ->
                    match checkoutDataGround mission with
                    | Some(coalition, start, force) ->
                        budget.TryCheckoutGroundForce(coalition, start, force)
                    | None ->
                        Some budget)
            ) (Some budget)
            |> Option.map (fun budget ->
                missions
                |> List.map (fun m ->
                    let startRegion =
                        match m.MissionType with
                        | GroundForcesTransfer(_, startRegion, _) -> string startRegion
                        | _ -> ""
                    { Kind = GroundMission m
                      Description = sprintf "Invasion %s -> %s" startRegion (string m.Objective) } ),
                budget))
        |> Seq.tryHead
        |> Option.map (fun (missions, budget) ->
            Plan("Invasion", missions, budget))
        |> Option.defaultValue (
            match targets with
            | [] -> Plan("No region suitable for invasion", [], budget)
            | _ :: _ -> Plan("Insufficient forces to invade", [], budget))

    /// Remove ground forces required for defense purposes from the available budget
    let lockDefenses (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let budget =
            (budget, war.World.Regions.Values)
            ||> Seq.fold (fun budget region ->
                if war.GetOwner(region.RegionId) = Some friendly then
                    let needed = neededForDefenses(war, friendly, region.RegionId)
                    let available =
                        budget.Regions.TryFind(region.RegionId, friendly)
                        |> Option.defaultValue 0.0f<MGF>
                    let locked = min needed available
                    if locked > 0.0f<MGF> then
                        logger.Debug(sprintf "Lock %0.1f%% of forces in %s" (100.0f * locked / available) (string region.RegionId))
                    budget.TryCheckoutGroundForce(friendly, region.RegionId, locked)
                    |> Option.defaultValue budget
                else
                    budget
            )
        Plan("Assignment of defense forces", [], budget)

    /// Try to plan troop movements in friendly territory
    let tryPlanReinforcements (timeSpan : float32<H>) (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let enemy = friendly.Other
        let suitableSources =
            war.World.Regions.Values
            |> Seq.filter (fun source ->
                war.GetOwner(source.RegionId) = Some friendly &&
                let available =
                    budget.Regions.TryFind(source.RegionId, friendly)
                    |> Option.defaultValue 0.0f<MGF>
                available > 0.0f<MGF>)
            |> List.ofSeq
        match suitableSources with
        | [] ->
            logger.Debug("No region with excess forces available to send reinforcements")
            Plan("No region with excess forces to move to the front", [], budget)
        | _ ->
        let missions =
            [
                let railCapacity = war.ComputeRailCapacity()
                let roadCapacity = war.ComputeRoadCapacity()
                let distanceToEnemy = war.ComputeDistancesToCoalition enemy
                let mutable budget = budget
                for source in suitableSources do
                    logger.Debug(sprintf "Considering source region %s for %s" (string source.RegionId) (string friendly))
                    let availableToMove = budget.Regions.TryFind(source.RegionId, friendly) |> Option.defaultValue 0.0f<MGF>
                    let sourceHasAirfield = war.World.RegionHasAirfield(source.RegionId)
                    let destinations =
                        source.Neighbours
                        |> List.filter (fun ngh -> war.GetOwner(ngh) = Some friendly)
                        |> List.map (fun ngh ->
                            let friendlyForces2 = budget.Regions.TryFind(ngh, friendly) |> Option.defaultValue 0.0f<MGF>
                            let needed = neededForDefenses(war, friendly, ngh)
                            let excess = friendlyForces2 - needed
                            let dist = distanceToEnemy.[ngh]
                            logger.Debug(sprintf "Potential destination: %s with excess friendly forces %0.0f and hops %d" (string ngh) excess dist)
                            excess, dist, ngh)
                        |> List.filter (fun (excess, dist, ngh) ->
                            // Destination has a defense deficit, or it's closer to the front, or it has an airfield and the source hasn't
                            excess < 0.0f<MGF> || dist < distanceToEnemy.[source.RegionId] || war.World.RegionHasAirfield(ngh) && not sourceHasAirfield)
                        |> List.sortBy (fun (excess, dist, ngh) -> excess, dist)
                        |> List.map (fun (_, _, ngh) -> ngh)

                    // Shuffle the 3 destinations with highest priority
                    // to break pattern where troops tend to always follow
                    // the same path from rear regions to the front
                    let destinations =
                        let random = System.Random(war.Seed)
                        let shuffled =
                            destinations
                            |> List.truncate 3
                            |> Array.ofList
                            |> Array.shuffle random
                            |> List.ofArray
                        let rest =
                            destinations
                            |> List.skip shuffled.Length
                        shuffled @ rest

                    let missions, budget2, _ =
                        (([], budget, availableToMove), destinations)
                        ||> List.fold (fun (missions, budget, availableToMove) ngh ->
                            logger.Debug(sprintf "Picked %s" (string ngh))
                            let maxTransport =
                                let capacity = railCapacity(source.RegionId, ngh) + roadCapacity(source.RegionId, ngh)
                                war.World.GroundForcesTransport(capacity, availableToMove, timeSpan)
                            let actualToMove = min availableToMove maxTransport
                            if actualToMove > 0.0f<MGF> then
                                match budget.TryCheckoutGroundForce(friendly, source.RegionId, actualToMove) with
                                | Some budget2 ->
                                    logger.Debug("Confirmed, allowed by budget")
                                    let mission =
                                        {
                                            Kind = GroundMission
                                                    {
                                                        Objective = ngh
                                                        MissionType = GroundForcesTransfer(friendly, source.RegionId, actualToMove)
                                                    }
                                            Description = sprintf "Reinforcement %s -> %s" (string source.RegionId) (string ngh)
                                        }
                                    mission :: missions, budget2, availableToMove - actualToMove
                                | None ->
                                    logger.Debug("Cancelled, insufficient budget ?!")
                                    missions, budget, availableToMove
                            else
                                logger.Debug("No transport ways available")
                                missions, budget, availableToMove)
                    yield! List.rev missions
                    budget <- budget2
            ]
        match missions with
        | [] ->
            Plan("No region needs or can receive reinforcements", [], budget)
        | _ ->
            let budget =
                (budget, missions)
                ||> List.fold (fun budget mission ->
                    match mission with
                    | { Kind = GroundMission { MissionType = GroundForcesTransfer(coalition, region, forces) } } ->
                        budget.TryCheckoutGroundForce(coalition, region, forces)
                        |> Option.defaultValue budget
                    | _ ->
                        budget)
            Plan("Reinforcement of regions close to the front", missions, budget)

    let tryPlanBattles (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        [
            for region in war.World.Regions.Values do
                let friendlyForce =
                    budget.Regions.TryFind(region.RegionId, friendly)
                    |> Option.defaultValue 0.0f<MGF>
                let enemyForce =
                    war.GetGroundForces(friendly.Other, region.RegionId)
                if enemyForce > 0.0f<MGF> && friendlyForce >= 1.5f * enemyForce then
                    yield {
                        Objective = region.RegionId
                        MissionType = GroundBattle friendly
                    }
        ]
        |> List.map (fun m -> { Kind = GroundMission m; Description = sprintf "Battle in %s" (string m.Objective) })
        |> function
            | [] -> Plan("No battle to start", [], budget)
            | ms -> Plan(sprintf "Battles started by %s" (string friendly), ms, budget)

    let rec oneSideStrikes (side : CoalitionId) comment depth (war : IWarStateQuery, timeSpan : float32<H>) =
        let tryMakeAirfieldRaids = tryMakeAirfieldRaids war side
        let tryMakeIndustryRaids = tryMakeIndustryRaids war side
        let tryMakeGroundForcesHarassment = tryMakeOffensiveGroundForcesRaids war false side
        let tryMakeGroundForcesSupport = tryMakeOffensiveGroundForcesRaids war true side
        let tryMakeGroundForcesDefense = tryMakeDefensiveGroundForcesRaids war side
        let tryTransferPlanesForward side = tryTransferPlanesForward war side
        let tryPlanTroops = Planning.chain [ tryPlanInvasions timeSpan war side; tryPlanBattles war side ]

        let sideAttacks =
            Planning.chain [
                lockDefenses war side
                Planning.pickFirst [
                    tryMakeAirfieldRaids |> Planning.andThen (Planning.chain [ tryMakeGroundForcesDefense; tryMakeGroundForcesSupport; tryMakeIndustryRaids; tryMakeGroundForcesHarassment; tryPlanTroops ])
                    tryMakeIndustryRaids |> Planning.andThen (Planning.chain [ tryMakeGroundForcesDefense; tryMakeGroundForcesSupport; tryMakeGroundForcesHarassment; tryPlanTroops ])
                    tryMakeGroundForcesHarassment |> Planning.andThen (Planning.chain [ tryMakeGroundForcesSupport; tryPlanTroops ])
                    tryPlanTroops
                ]
                Planning.chain [ tryTransferPlanesForward side; tryPlanReinforcements timeSpan war side; tryEvacuatePlanes war side ]
            ]

        let budget = ForcesAvailability.Create war

        let g1 = side.Grammar
        let g2 = side.Other.Grammar

        match sideAttacks budget with
        | TooFewTargets comment ->
            Victory(side, comment)
        | Plan(comment, [], _) ->
            let comment2 = sprintf "%s lose%s initiative because of %s" (string side) g1.Suffix3 comment
            if depth > 0 then
                oneSideStrikes side.Other comment2 (depth - 1) (war, timeSpan)
            else
                Stalemate (sprintf "%s and %s" comment comment2)
        | Plan(description, missions, budget) ->
            let raids =
                missions
                |> List.choose (function { Kind = AirMission x } -> Some x | _ -> None)
            let otherSideDefends =
                Planning.chain [
                    tryMakeCovers war side.Other raids
                    tryMakeDefensiveGroundForcesRaids war side.Other
                    tryTransferPlanesForward side.Other
                    lockDefenses war side.Other
                    tryPlanReinforcements timeSpan war side.Other
                    tryEvacuatePlanes war side.Other
                ] budget
            let defenseMissions, _ = MissionPlanningResult.getMissions budget otherSideDefends
            let momentum =
                if depth = 0 then
                    "keeping the pressure on"
                else
                    "striking against"
            let briefing = sprintf "%s. %s %s %s %s assets with %s. %s wheather%s the storm with %s" comment (string side) g1.VerbIs momentum (string side.Other) description (string side.Other) g2.Suffix3 otherSideDefends.Description
            logger.Info(briefing)
            Ongoing {
                Briefing = briefing
                Missions = missions @ defenseMissions
                Data = {
                    OffensiveCoalition = side
                }
            }

    interface IScenarioController with
        member this.InitGroundForces(axisForcesNumberCoefficient, alliesForcesNumberCoefficient, war) =
            initGroundForces(axisForcesNumberCoefficient, Axis, war)
            initGroundForces(alliesForcesNumberCoefficient, Allies, war)
            balanceGroundForces(axisForcesNumberCoefficient, alliesForcesNumberCoefficient, war)

        member this.InitAirfields(planeNumberCoefficient, coalition, war) =
            initAirfields planeNumberCoefficient coalition war

        member this.NextStep(stepData) =
            let data = stepData.Data :?> ImplData
            // Switch the initiative to the other side.
            let side = data.OffensiveCoalition.Other
            let comment = sprintf "%s has the initiative" (string side)
            oneSideStrikes side comment 1

        member this.NewDay(war) =
            seq {
                let random = System.Random(int32(war.Date.Ticks &&& 0x7FFFFFFFL))
                let timeDiff = C.MinStartDiff + float32(random.NextDouble() * double(C.MaxStartDiff - C.MinStartDiff)) * 1.0f<H>
                let span(h : float32<H>) =
                    System.TimeSpan(int h, int(60.0f * (float32 h - floor(float32 h))), 0)
                let newTime = war.Date + span timeDiff
                let hour = 1.0f<H> * float32 newTime.Hour
                let newTime =
                    if hour > C.LatestStart then
                        newTime.Date + span(24.0f<H>) + span(C.EarliestStart)
                    elif hour < C.EarliestStart then
                        newTime.Date + span(C.EarliestStart)
                    else
                        newTime
                let period (t : System.DateTime) =
                    int(24.0f<H> * float32 (t - war.World.StartDate).Days / C.NewPlanesPeriod)
                // Add new planes
                if period war.Date < period newTime then
                    let newPlanesDelivery(numPlanes : float32) =
                        let forCoalition (coalition : CoalitionId) =
                            let planes = allPlanesOf coalition
                            let totalCost =
                                planes
                                |> List.sumBy (fun plane -> plane.Cost)
                            planes
                            |> List.map (fun plane -> plane, numPlanes * plane.Cost / totalCost)

                        Map.ofList [
                            Axis, forCoalition Axis
                            Allies, forCoalition Allies
                        ]

                    let newPlanes = newPlanesDelivery(C.NumNewPlanes)
                    for coalition in [Axis; Allies] do
                        let airfields =
                            war.World.Airfields.Values
                            |> Seq.filter (fun af -> war.World.Regions.[af.Region].IsEntry && war.GetOwner(af.Region) = Some coalition)
                            |> Array.ofSeq
                        let numAirfields = float32 airfields.Length
                        for af in airfields do
                            for plane, qty in newPlanes.[coalition] do
                                yield Some(AddPlane(af.AirfieldId, plane.Id, qty / numAirfields)), "New plane delivery"
                // Add new troops
                let period (t : System.DateTime) =
                    int(24.0f<H> * float32 (t - war.World.StartDate).Days / C.NewTroopsPeriod)
                if period war.Date < period newTime then
                    for coalition in [Axis; Allies] do
                        let region =
                            war.World.Regions.Values
                            |> Seq.filter (fun region -> war.GetOwner(region.RegionId) = Some coalition)
                            |> Seq.tryFind (fun region -> region.IsEntry)
                        match region with
                        | Some region ->
                            yield Some(AddGroundForces(region.RegionId, coalition, C.NumNewTroops)), sprintf "Reinforcements for %s" (string coalition)
                        | None ->
                            yield None, sprintf "Reinforcements for %s cancelled" (string coalition)
                // Repairs
                let supplies = war.ComputeSupplyAvailability()
                // Repairs at airfields require supplies from the rear regions, and limited by the max repair speed
                let availableForBuildings = Seq.mutableDict (war.World.Regions.Keys |> Seq.map (fun region -> region, timeDiff * (min war.World.RepairSpeed (supplies region))))
                // Repairs of bridges are only limited by the max repair speed
                let availableForBridges = Seq.mutableDict (war.World.Regions.Keys |> Seq.map (fun region -> region, timeDiff * war.World.RepairSpeed))
                let repairObjects =
                    [|
                        for af in war.World.Airfields.Values do
                            for bid in af.Facilities do
                                match world.TryGetBuildingInstance(bid) with
                                | Some building ->
                                    for part in building.Properties.SubParts do
                                        let health = war.GetBuildingPartHealthLevel(bid, part)
                                        if health < 1.0f then
                                            yield Choice1Of2(building, bid, part, health, af)
                                | None ->
                                    logger.Warn(sprintf "Building or bridge %s not found" (string bid))
                        for bridge in world.Bridges.Values do
                            let bid = bridge.Id
                            let region = war.World.FindRegionAt(bridge.Pos.Pos)
                            for part in bridge.Properties.SubParts do
                                let health = war.GetBuildingPartHealthLevel(bid, part)
                                if health < 1.0f then
                                    yield Choice2Of2(bridge, bid, part, health, region)
                    |]
                let repairObjects = Array.shuffle random repairObjects
                for repairObj in repairObjects do
                    match repairObj with
                    | Choice1Of2(building, bid, part, health, af) ->
                        let healing = 1.0f - health
                        let cost = healing * war.World.RepairCostRatio * building.Properties.PartCapacity * (float32 building.Properties.Durability / 50000.0f)
                        let avail = availableForBuildings.[af.Region]
                        let spent = min cost avail |> max 0.0f<E>
                        availableForBuildings.[af.Region] <- avail - cost
                        let healing = spent / cost
                        if healing > 0.0f then
                            yield Some(RepairBuildingPart(bid, part, healing)), sprintf "Repairs to building part to %0.0f%% at %s airfield" (100.0f * (health + healing)) af.AirfieldId.AirfieldName
                    | Choice2Of2(bridge, bid, part, health, region) ->
                        let healing = 1.0f - health
                        let cost = healing * war.World.TransportRepairCostRatio * war.World.BridgeCapacity * (float32 bridge.Properties.Durability / 50000.0f)
                        let avail = availableForBridges.[region.RegionId]
                        let spent = min cost avail |> max 0.0f<E>
                        availableForBridges.[region.RegionId] <- avail - cost
                        let healing = spent / cost
                        if healing > 0.0f then
                            yield Some(RepairBuildingPart(bid, part, healing)), sprintf "Repairs of bridge section to %0.0f%% in %s" (100.0f * (health + healing)) (string region.RegionId)

                // Update time
                yield Some(AdvanceTime { Span = newTime - war.Date }), "Advance time"
            }

        member this.Start(war, timeSpan) =
            oneSideStrikes Axis "Axis opens the hostilities" 1 (war, timeSpan)

        member this.TrySelectMissions(stepData, war, seed, numSelected) =
            let data = stepData.Data :?> ImplData
            let candidates =
                Campaign.MissionSelection.enumerateGroundAttackMissions war data.OffensiveCoalition stepData.Missions
                |> Seq.sortByDescending (fun selection -> selection.NumMissions)
                |> fun missions ->
                    Seq.concat [
                        missions
                        Campaign.MissionSelection.enumerateOffensivePatrols war data.OffensiveCoalition stepData.Missions
                        Campaign.MissionSelection.enumerateTransfers war data.OffensiveCoalition stepData.Missions
                    ]
                // Add ground forces transfers
                |> Seq.map (fun selection ->
                    let groundTraffic =
                        stepData.Missions
                        |> List.filter (
                            function
                            | { Kind = GroundMission { MissionType = GroundForcesTransfer _ } } -> true
                            | _ -> false
                        )
                    { selection with
                        OtherMissions = groundTraffic @ selection.OtherMissions
                    }
                )
                |> Seq.truncate numSelected
                |> Array.ofSeq
            if candidates.Length = 0 then
                None
            else
                let random = System.Random(seed)
                Some(candidates.[random.Next(candidates.Length)])

        member this.DeserializeStepData(json : string) =
            let implData : ImplData = Json.deserializeEx JsonConfig.IL2Default json
            implData :> obj

    static member LoadFromFile(world : World, path : string) =
        let json = System.IO.File.ReadAllText(path)
        let constructorData : {| Constants : Constants |} =
            Json.deserializeEx JsonConfig.IL2Default json
        WorldWar2(world, constructorData.Constants)

    member this.SaveToFile(path : string) =
        let json = Json.serializeEx JsonConfig.IL2Default {| Constants = C |}
        System.IO.File.WriteAllText(path, json)
