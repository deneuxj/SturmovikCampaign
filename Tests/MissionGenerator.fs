module Campaign.Tests.MissionGenerator

open NUnit.Framework
open Campaign.MissionGenerator
open Campaign.Missions
open Campaign.WorldDescription
open Campaign.PlaneModel

let airMission = {
    StartAirfield = AirfieldId "AF"
    Objective = RegionId "RE"
    MissionType = AreaProtection
    NumPlanes = 4
    Plane = PlaneModelId "bf109"
}

let mission : Mission =
    AirMission airMission

let missionB = AirMission { airMission with Objective = RegionId "AC" }

let budget0 =
    { Airfields = Map.empty }

[<Test>]
let ``chain properly executes plans in a sequence``() =
    let planA _ =
        MissionPlanningResult.TooFewTargets
    let planB budget =
        MissionPlanningResult.Plan([], budget)
    let planC budget =
        MissionPlanningResult.Plan([mission], budget)

    let plan = Planning.chain [planA; planB]
    let res = plan budget0
    let expected =
        MissionPlanningResult.Plan([], budget0)
    Assert.AreEqual(expected, res)

    let plan = Planning.chain [planA; planB; planC]
    let res = plan budget0
    let expected =
        MissionPlanningResult.Plan([mission], budget0)
    Assert.AreEqual(expected, res)

    let plan = Planning.chain [planC; planB; planA]
    let res = plan budget0
    let expected =
        MissionPlanningResult.Plan([mission], budget0)
    Assert.AreEqual(expected, res)

[<Test>]
let ``chain properly accumulates missions``() =
    let planA _ =
        MissionPlanningResult.TooFewTargets
    let planB budget =
        MissionPlanningResult.Plan([ missionB ], budget)
    let planC budget =
        MissionPlanningResult.Plan([mission], budget)

    let plan = Planning.chain [planA; planB; planC; planC]
    let res = plan budget0
    let expected =
        MissionPlanningResult.Plan([missionB; mission; mission], budget0)
    Assert.AreEqual(expected, res)

[<Test>]
let ``andThen executes rest of sequence when first plan has work to do``() =
    let planA budget =
        MissionPlanningResult.Plan([mission], budget)
    let planB budget =
        MissionPlanningResult.Plan([missionB], budget)
    let plan = Planning.andThen [planB] planA
    let res = plan budget0
    let expected =
        MissionPlanningResult.Plan([mission; missionB], budget0)
    Assert.AreEqual(expected, res)

[<Test>]
let ``andThen does not let TooFewTargets in also arguments erase the first plan``() =
    let planA budget =
        MissionPlanningResult.Plan([mission], budget)
    let planB budget =
        MissionPlanningResult.TooFewTargets
    let plan = Planning.andThen [planB] planA
    let res = plan budget0
    let expected =
        MissionPlanningResult.Plan([mission], budget0)
    Assert.AreEqual(expected, res)

[<Test>]
let ``orElse picks the first plan with work to do``() =
    let planA _ =
        MissionPlanningResult.TooFewTargets
    let planB budget =
        MissionPlanningResult.Plan([], budget)
    let planC budget =
        MissionPlanningResult.Plan([mission], budget)
    let planD budget =
        MissionPlanningResult.Plan([missionB], budget)
    let plan = Planning.orElse [planA; planB; planC; planD; planA]
    let res = plan budget0
    let expected =
        MissionPlanningResult.Plan([mission], budget0)
    Assert.AreEqual(expected, res)
