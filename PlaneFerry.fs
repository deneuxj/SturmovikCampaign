module Campaign.PlaneFerry

open SturmovikMission.Blocks.FerryFlight
open SturmovikMission.DataProvider
open BasicTypes
open Orders
open WorldDescription
open WorldState

/// Generate mission logic of AI flights that realize plane transfer orders.
let generatePlaneTransfer store (world : World) (state : WorldState) (order : PlaneFerryOrder) (missionStart : Mcu.McuTrigger) =
    let sg = state.FastAccess
    // Create mission logic
    let spawnPos, spawnOri = sg.GetAirfield(order.Start).AiSpawnPos
    let landPos, landOri = sg.GetAirfield(order.Destination).Runway
    let flight = FerryFlight.Create(store, spawnPos, landPos, spawnOri, landOri, order.Qty, order.OrderId.Coalition.ToCountry)
    // Plane type and country
    flight.Plane.Script <- order.Plane.ScriptModel.Script
    flight.Plane.Model <- order.Plane.ScriptModel.Model
    // Connect to mission start
    Mcu.addTargetLink missionStart flight.Start.Index
    // Result
    flight.All