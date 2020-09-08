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


module BodenplatteInternal =
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
            /// MinimumTime between mission starts
            MinStartDiff : float32<H>
        }
    with
        static member Default =
            let typicalRange = 400.0e3f<M>
            let planeRunCost = 5.0f<M^3> / typicalRange
            let bombCost = 0.01f<M^3/K>
            let maxPlanesAtAirfield = 100.0f
            let minActiveAirfieldResources = planeRunCost * 10.0f * typicalRange
            let minRegionBuildingCapacity = 1000.0f<M^3>
            let day = 24.0f<H>
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
                NumNewTroops = 2000.0f<MGF>
                EarliestStart = 5.0f<H>
                LatestStart = 19.0f<H>
                MinStartDiff = 5.0f<H>
            }

    type PlaneSet =
        {
            IdMe262 : string
            IdBf109g14 : string
            IdFw190a8 : string
            IdBf109k4 : string
            IdFw190d9 : string
            IdP51 : string
            IdP38 : string
            IdP47 : string
            IdSpitfire : string
            IdTempest : string
        }
    with
        static member Default =
            {
                IdMe262 = "me262"
                IdBf109g14 = "bf109g14"
                IdFw190a8 = "fw190a8"
                IdBf109k4 = "bf109k4"
                IdFw190d9 = "fw190d9"
                IdP51 = "p51"
                IdP38 = "p38"
                IdP47 = "p47"
                IdSpitfire = "spitfireMkIXe"
                IdTempest = "Tempest MkV s2"
            }

        member this.NewPlanesDelivery(numPlanes : float32) =
            Map.ofList [
                Axis, [
                    this.IdMe262, 0.1f * numPlanes
                    this.IdBf109k4, 0.3f * numPlanes
                    this.IdFw190d9, 0.2f * numPlanes
                ]
                Allies, [
                    this.IdP51, 0.4f * numPlanes
                    this.IdP38, 0.2f * numPlanes
                    this.IdP47, 0.1f * numPlanes
                    this.IdSpitfire, 0.2f * numPlanes
                    this.IdTempest, 0.1f * numPlanes
                ]
            ]

        member this.AllPlanesOf coalition =
            match coalition with
            | Axis ->
                [
                    this.IdMe262
                    this.IdBf109g14
                    this.IdFw190a8
                    this.IdBf109k4
                    this.IdFw190d9
                ]
            | Allies ->
                [
                    this.IdP51
                    this.IdP38
                    this.IdP47
                    this.IdSpitfire
                    this.IdTempest
                ]

        /// Get the list of attackers of a coalition, preferred ones first
        member this.Attackers =
            function
            | Axis -> [ this.IdMe262; this.IdFw190d9; this.IdFw190a8; this.IdBf109k4; this.IdBf109g14]
            | Allies -> [ this.IdP38; this.IdP47; this.IdP51 ]

        /// Get the list of interceptors of a coalition, preferred ones first
        member this.InterceptorsOf =
            function
            | Axis -> [ this.IdFw190d9; this.IdBf109k4; this.IdFw190a8; this.IdBf109g14 ]
            | Allies -> [ this.IdP38; this.IdP51; this.IdTempest; this.IdP47; this.IdSpitfire ]

        /// Get the list of fighters of a coalition, preferred ones first
        member this.FightersOf =
            function
            | Axis -> [ this.IdBf109g14; this.IdFw190a8; this.IdBf109k4; this.IdFw190d9 ]
            | Allies -> [ this.IdP51; this.IdSpitfire; this.IdTempest; this.IdP47; this.IdP38 ]

        member this.Setup(world: World): World = 
            let planes =
                List.concat [ this.AllPlanesOf Axis; this.AllPlanesOf Allies ]
                |> List.map PlaneModelId

            let planeDb =
                Campaign.PlaneModelDb.planeDb
                |> List.map (fun plane -> plane.Id, plane)
                |> Map.ofList
                    
            let planeSet =
                planes
                |> List.choose (fun plane -> planeDb.TryFind plane |> Option.orElseWith (fun () -> eprintfn "%s missing" (string plane); None))
                |> Seq.map (fun plane -> plane.Id, plane)
                |> dict

            { world with PlaneSet = planeSet }

        interface IScenarioWorldSetup with
            member this.Setup(world) = this.Setup(world)

open BodenplatteInternal

type Bodenplatte(world : World, C : Constants, PS : PlaneSet) =
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

    let attackersOf = PS.Attackers >> List.map PlaneModelId
    let interceptorsOf = PS.InterceptorsOf >> List.map PlaneModelId
    let fightersOf = PS.FightersOf >> List.map PlaneModelId

    let allPlanesOf coalition =
        [attackersOf; interceptorsOf; fightersOf]
        |> List.collect (fun f -> f coalition)
        |> List.distinct

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
            |> Seq.map (fun planeId -> war.World.PlaneSet.[planeId])
            |> Seq.filter (fun plane -> plane.Kind <> PlaneType.Transport)
            |> Seq.sortByDescending (fun plane -> plane.Cost)
            |> List.ofSeq

        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> af.IsActive && war.GetOwner(af.Region) = Some friendly)
            |> Seq.sortBy (fun af -> distanceToEnemy.[af.Region])
            |> List.ofSeq

        // Non-transport planes: Set according to airfield resources and respective cost
        let planeRunCost = typicalRange * planeRunCost
        match planes with
        | [] -> ()
        | [plane] ->
            for af in airfields do
                let numPlanes =
                    war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                war.SetNumPlanes(af.AirfieldId, plane.Id, factor * numPlanes)
        | expansive :: regular ->
            // Rear airfields get regular planes and the expansive plane
            // Front airfields get regular planes and the cheap plane
            let regular = List.rev regular
            let cheap, regular = List.head regular, List.tail regular
            let numAirfields = List.length airfields
            let numFrontAirfields = numAirfields / 2 |> max 1
            let numRearAirfields = numAirfields - numFrontAirfields |> max 1
            airfields
            |> List.iteri (fun i af ->
                let planes =
                    regular
                let planes =
                    if i < numFrontAirfields then
                        cheap :: planes
                    else
                        planes
                let planes =
                    if i >= numAirfields - numRearAirfields then
                        expansive :: planes
                    else
                        planes
                let totalInvCost =
                    planes
                    |> List.sumBy (fun plane -> 1.0f / plane.Cost)
                let numPlanes =
                    war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                    |> min maxPlanesAtAirfield
                for plane in planes do
                    let qty = numPlanes * (1.0f / plane.Cost) / totalInvCost |> max 1.0f
                    war.SetNumPlanes(af.AirfieldId, plane.Id, factor * qty)
                )

        // Transport planes at the rearmost airfield
        match List.tryLast airfields with
        | Some rear ->
            let transport =
                allPlanesOf friendly
                |> List.filter (fun planeId -> war.World.PlaneSet.[planeId].Kind = PlaneType.Transport)
            for plane in transport do
                war.SetNumPlanes(rear.AirfieldId, plane, 20.0f)
        | None ->
            ()

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
                                let mission : AirMission = mkMissionFrom af
                                match budget.TryCheckoutPlane(af.AirfieldId, checkoutDataAir mission) with
                                | None -> None
                                | Some budget ->
                                    if isInPlaneRange mission.Plane af destination then
                                        Some(mission, budget)
                                    else
                                        None)
                        let groupAndBudget =
                            seq {
                                for attacker in attackersOf friendly do
                                for numAttackers in [15; 10; 5] do
                                let mkAttackerMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = targetRegion
                                      MissionType = Strafing(adapter.MkGroundTarget target)
                                      Plane = attacker
                                      NumPlanes = numAttackers
                                    }
                                for attackerMission, budget in missionAlternatives readyAttackAirfields budget mkAttackerMission targetPos do
                                let attackerStart = war.World.Airfields.[attackerMission.StartAirfield]
                                for numFighters in [8; 4; 2] do
                                for fighter in fightersOf friendly do
                                let mkFighterMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = targetRegion
                                      MissionType = AreaProtection
                                      Plane = fighter
                                      NumPlanes = numFighters }
                                for fighterMission, budget in missionAlternatives readyCAPAirfields budget mkFighterMission targetPos do
                                for numInterceptors in [4; 3; 2] do
                                for interceptor in interceptorsOf friendly do
                                let mkInterceptorMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = attackerStart.Region
                                      MissionType = AreaProtection
                                      Plane = interceptor
                                      NumPlanes = numInterceptors }
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
                                let plane = war.World.PlaneSet.[plane]
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
                        |> List.filter (fun planeId -> war.World.PlaneSet.[planeId].Kind <> PlaneType.Transport)
                    for plane in allButTransport do
                        if excessPlanes > 0.0f && sustainable > 0.0f then
                            let numPlanes =
                                sumPlanes budget.Airfields.[af1.AirfieldId].Planes [plane]
                                |> min excessPlanes
                                |> int
                                |> min 25
                            if numPlanes > 0 then
                                let mission =
                                    {
                                        StartAirfield = af1.AirfieldId
                                        Objective = af2.Region
                                        MissionType = PlaneTransfer af2.AirfieldId
                                        NumPlanes = numPlanes
                                        Plane = plane
                                    }
                                let budget2 = budget.TryCheckoutPlane(af1.AirfieldId, checkoutDataAir mission)
                                match budget2 with
                                | Some b ->
                                    budget <- b
                                    let numPlanesF = (float32 numPlanes)
                                    excessPlanes <- excessPlanes - numPlanesF
                                    sustainable <- sustainable - numPlanesF
                                    yield mission, sprintf "Transfer %d %s from %s to %s" numPlanes (string plane) af1.AirfieldId.AirfieldName af2.AirfieldId.AirfieldName
                                | None ->
                                    ()
            ]
            |> List.map (fun (m, description) ->
                { Kind = AirMission m
                  Description = description })
        Plan ("Transfers", missions, budget)

    let tryPlanInvasions (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let distanceToAirfields =
            war.ComputeDistancesToAirfields()
        let targets =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                match war.GetOwner(region.RegionId) with
                | Some owner when owner <> friendly ->
                    region.Neighbours
                    |> Seq.exists (fun ngh -> war.GetGroundForces(friendly, ngh) > 0.0f<MGF>)
                | Some _ | None ->
                    false)
            |> Seq.sortBy (fun region -> distanceToAirfields.[region.RegionId])
            |> List.ofSeq
        seq {
            for target in targets do
                let friendlyForces = war.GetGroundForces(friendly, target.RegionId)
                let ennemyForces = war.GetGroundForces(friendly.Other, target.RegionId)
                let targetForceDifference = friendlyForces - ennemyForces
                let mutable forceDifference = targetForceDifference
                let localDifferences = Seq.mutableDict []
                for ngh in target.Neighbours do
                    let diff = war.GetGroundForces(friendly, ngh) - war.GetGroundForces(friendly.Other, ngh)
                    localDifferences.[ngh] <- diff
                    forceDifference <- forceDifference + diff
                if forceDifference > 0.0f<MGF> then
                    let neighbours =
                        target.Neighbours
                        |> Seq.sortByDescending (fun ngh -> localDifferences.[ngh])
                    let mutable desiredForceAddition = 2.0f * ennemyForces - friendlyForces
                    let mutable minimumForceAddition = 1.5f * ennemyForces - friendlyForces
                    let missions =
                        [
                            for ngh in neighbours do
                                let force = min localDifferences.[ngh] desiredForceAddition
                                if force > 0.0f<MGF> then
                                    desiredForceAddition <- desiredForceAddition - force
                                    minimumForceAddition <- minimumForceAddition - force
                                    yield {
                                        Objective = target.RegionId
                                        MissionType = GroundForcesTransfer(friendly, ngh, force)
                                    }
                        ]
                    if minimumForceAddition <= 0.0f<MGF> && not missions.IsEmpty then
                        yield missions
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

    let tryPlanReinforcements (war : IWarStateQuery) (friendly : CoalitionId) (budget : ForcesAvailability) =
        let enemy = friendly.Other
        let suitableSources =
            war.World.Regions.Values
            |> Seq.filter (fun source ->
                war.GetOwner(source.RegionId) = Some friendly &&
                let available =
                    budget.Regions.TryFind(source.RegionId, friendly)
                    |> Option.defaultValue 0.0f<MGF>
                let needed = war.GetGroundForces(enemy, source.RegionId)
                available > needed)
            |> List.ofSeq
        match suitableSources with
        | [] ->
            logger.Debug("No region with excess forces available to send reinforcements")
            Plan("No region with excess forces to move to the front", [], budget)
        | _ ->
        let missions =
            [
                let distanceToEnemy = war.ComputeDistancesToCoalition enemy
                let mutable budget = budget
                for source in suitableSources do
                    logger.Debug(sprintf "Considering source region %s for %s" (string source.RegionId) (string friendly))
                    let friendlyForces = budget.Regions.TryFind(source.RegionId, friendly) |> Option.defaultValue 0.0f<MGF>
                    let availableToMove = friendlyForces - war.GetGroundForces(enemy, source.RegionId)
                    let destination =
                        source.Neighbours
                        |> List.filter (fun ngh -> war.GetOwner(ngh) = Some friendly)
                        |> List.map (fun ngh ->
                            let friendlyForces2 = budget.Regions.TryFind(ngh, friendly) |> Option.defaultValue 0.0f<MGF>
                            let excess = friendlyForces2 - war.GetGroundForces(enemy, ngh) |> min 0.0f<MGF>
                            let dist = distanceToEnemy.[ngh]
                            logger.Debug(sprintf "Potential destination: %s with excess %f and hops %d" (string ngh) excess dist)
                            excess, dist, ngh)
                        |> List.filter (fun (excess, dist, _) -> excess < 0.0f<MGF> || dist < distanceToEnemy.[source.RegionId])
                        |> function
                            | [] -> None
                            | nghs -> List.min nghs |> Some
                    match destination with
                    | Some (excess, dist, ngh) ->
                        logger.Debug(sprintf "Picked %s" (string ngh))
                        match budget.TryCheckoutGroundForce(friendly, source.RegionId, availableToMove) with
                        | Some budget2 ->
                            logger.Debug("Confirmed, allowed by budget")
                            budget <- budget2
                            yield {
                                Kind = GroundMission
                                        {
                                            Objective = ngh
                                            MissionType = GroundForcesTransfer(friendly, source.RegionId, availableToMove)
                                        }
                                Description = sprintf "Reinforcement %s -> %s" (string source.RegionId) (string ngh)
                            }, budget2
                        | None ->
                            logger.Debug("Cancelled, insufficient budget ?!")
                    | None ->
                        logger.Debug("No suitable destination")
            ]
        match missions with
        | [] ->
            Plan("No region needs or can receive reinforcements", [], budget)
        | _ ->
            let budget = List.last missions |> snd
            let missions = missions |> List.map fst
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

    let rec oneSideStrikes (side : CoalitionId) comment depth (war : IWarStateQuery) =
        let tryMakeAirfieldRaids = tryMakeAirfieldRaids war side
        let tryMakeIndustryRaids = tryMakeIndustryRaids war side
        let tryMakeOtherDefensiveGroundForcesRaids = tryMakeDefensiveGroundForcesRaids war side.Other
        let tryMakeGroundForcesHarassment = tryMakeOffensiveGroundForcesRaids war false side
        let tryMakeGroundForcesSupport = tryMakeOffensiveGroundForcesRaids war true side
        let tryMakeGroundForcesDefense = tryMakeDefensiveGroundForcesRaids war side
        let tryTransferPlanesForward = tryTransferPlanesForward war side
        let tryPlanTroops = Planning.chain [ tryPlanInvasions war side; tryPlanBattles war side; tryPlanReinforcements war side ]

        let sideAttacks =
            Planning.orElse [
                tryMakeAirfieldRaids |> Planning.andThen [ tryMakeGroundForcesDefense; tryMakeGroundForcesSupport; tryMakeIndustryRaids; tryMakeGroundForcesHarassment; tryPlanTroops ]
                tryMakeIndustryRaids |> Planning.andThen [ tryMakeGroundForcesDefense; tryMakeGroundForcesSupport; tryMakeGroundForcesHarassment; tryPlanTroops ]
                tryMakeGroundForcesHarassment |> Planning.andThen [ tryMakeGroundForcesSupport; tryPlanTroops ]
                tryPlanTroops
            ] |> Planning.andThen [ tryTransferPlanesForward ]

        let budget = ForcesAvailability.Create war
        match sideAttacks budget with
        | TooFewTargets comment ->
            Victory(side, comment)
        | Plan(comment, [], _) ->
            let comment2 = sprintf "%s loses initiative because of %s" (string side) comment
            if depth > 0 then
                oneSideStrikes side.Other comment2 (depth - 1) war
            else
                Stalemate (sprintf "%s and %s" comment comment2)
        | Plan(description, missions, budget) ->
            let raids =
                missions
                |> List.choose (function { Kind = AirMission x } -> Some x | _ -> None)
            let covers, budget =
                Planning.chain [ tryMakeCovers war side.Other raids; tryMakeOtherDefensiveGroundForcesRaids; tryTransferPlanesForward ] budget
                |> MissionPlanningResult.getMissions budget
            let momentum =
                if depth = 0 then
                    "keeping the pressure on"
                else
                    "striking against"
            Ongoing {
                Briefing = sprintf "%s. %s is %s %s assets with %s" comment (string side) momentum (string side.Other) description
                Missions = missions @ covers
                Data = {
                    OffensiveCoalition = side
                }
            }

    interface IScenarioController with
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
                let timeDiff = C.MinStartDiff + float32(random.NextDouble() * (24.0 - double C.MinStartDiff)) * 1.0f<H>
                let span(h : float32<H>) =
                    System.TimeSpan(int h, int(60.0f * h % 1.0f<H>), 0)
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
                    let newPlanes = PS.NewPlanesDelivery(C.NumNewPlanes)
                    for coalition in [Axis; Allies] do
                        let airfields =
                            war.World.Airfields.Values
                            |> Seq.filter (fun af -> war.World.Regions.[af.Region].IsEntry && war.GetOwner(af.Region) = Some coalition)
                            |> Array.ofSeq
                        let numAirfields = float32 airfields.Length
                        for af in airfields do
                            for plane, qty in newPlanes.[coalition] do
                                let plane = PlaneModelId plane
                                yield Some(AddPlane(af.AirfieldId, plane, qty / numAirfields)), "New plane delivery"
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
                // Update time
                yield Some(AdvanceTime(newTime - war.Date)), "Advance time"
            }

        member this.Start(war) =
            oneSideStrikes Axis "Axis opens the hostilities" 1 war

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
                |> Seq.truncate numSelected
                |> Array.ofSeq
            if candidates.Length = 0 then
                None
            else
                let random = System.Random(seed)
                Some(candidates.[random.Next(candidates.Length)])
