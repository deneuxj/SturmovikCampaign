module Campaign.PlaneFerry

open SturmovikMission.Blocks.IconDisplay
open SturmovikMission.Blocks.FerryFlight
open SturmovikMission.DataProvider
open BasicTypes
open Orders
open WorldDescription
open WorldState

/// Generate mission logic of AI flights that realize plane transfer orders.
let generatePlaneTransfer store lcStore (world : World) (state : WorldState) (order : PlaneFerryOrder) (missionStart : Mcu.McuTrigger) =
    let sg = state.FastAccess
    // Create mission logic
    let spawnPos, spawnOri = sg.GetAirfield(order.Start).AiSpawnPos
    let landPos, landOri = sg.GetAirfield(order.Destination).Runway
    let flight = FerryFlight.Create(store, spawnPos, landPos, spawnOri, landOri, order.Qty, order.OrderId.Coalition.ToCountry)
    let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, (5.0f * landPos + spawnPos) / 6.0f, "Transfer", order.OrderId.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
    // Plane type
    flight.Plane.Script <- order.Plane.ScriptModel.Script
    flight.Plane.Model <- order.Plane.ScriptModel.Model
    // Links
    Mcu.addTargetLink missionStart flight.Start.Index
    for icon in [icon1; icon2] do
        Mcu.addTargetLink flight.Start icon.Show.Index
        Mcu.addTargetLink flight.Killed icon.Hide.Index
        Mcu.addTargetLink flight.Landed icon.Hide.Index
    // Result
    { new McuUtil.IMcuGroup with
          member x.Content = []
          member x.LcStrings = []
          member x.SubGroups = [flight.All; icon1.All; icon2.All]
    }