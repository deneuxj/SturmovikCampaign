module Campaign.PlaneFerry

open SturmovikMission.Blocks.IconDisplay
open SturmovikMission.Blocks.EventReporting
open SturmovikMission.Blocks.FerryFlight
open SturmovikMission.DataProvider
open BasicTypes
open Orders
open WorldDescription
open WorldState
open SturmovikMission.Blocks.BlocksMissionData
open System.Numerics

/// Generate mission logic of AI flights that realize plane transfer orders.
let generatePlaneTransfer store lcStore (world : World) (state : WorldState) (missionStart : Mcu.McuTrigger) (numSimultaneous : int) (orders : PlaneFerryOrder list) =
    let sg = state.FastAccess
    let blocksAndGroups =
        [
            for order in orders do
                // Create mission logic
                let spawnPos, spawnOri = sg.GetAirfield(order.Start).AiSpawnPos
                let landPos, landOri = sg.GetAirfield(order.Destination).Runway
                let flight = FerryFlight.Create(store, spawnPos, landPos, spawnOri, landOri, order.Qty, order.OrderId.Coalition.ToCountry)
                let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, (5.0f * landPos + spawnPos) / 6.0f, "Transfer", order.OrderId.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
                let reportSpawned = EventReporting.Create(store, order.OrderId.Coalition.ToCountry, spawnPos + Vector2(0.0f, 100.0f), order.SpawnedEventName)
                let reportLanded = EventReporting.Create(store, order.OrderId.Coalition.ToCountry, landPos + Vector2(0.0f, 100.0f), order.LandedEventName)
                let reportKilled = EventReporting.Create(store, order.OrderId.Coalition.ToCountry, spawnPos + Vector2(0.0f, 200.0f), order.KilledEventName)
                // Plane type
                order.Plane.ScriptModel.AssignTo(flight.Plane)
                // Links
                for icon in [icon1; icon2] do
                    Mcu.addTargetLink flight.Spawned icon.Show.Index
                    Mcu.addTargetLink flight.Killed icon.Hide.Index
                    Mcu.addTargetLink flight.Landed icon.Hide.Index
                Mcu.addTargetLink flight.Spawned reportSpawned.Trigger.Index
                Mcu.addTargetLink flight.Landed reportLanded.Trigger.Index
                Mcu.addTargetLink flight.Killed reportKilled.Trigger.Index
                // Result
                yield
                    flight,
                    { new McuUtil.IMcuGroup with
                          member x.Content = []
                          member x.LcStrings = []
                          member x.SubGroups = [flight.All; icon1.All; icon2.All; reportSpawned.All; reportLanded.All; reportKilled.All]
                    }
        ]
    let additionalLogic =
        [
            // Enable all flights at mission start
            for block, _ in blocksAndGroups do
                Mcu.addTargetLink missionStart block.Enable.Index
            // The first numSimultaneous flights that spawn start the next flight
            for (curr, _), (next, _) in Seq.pairwise blocksAndGroups |> Seq.truncate (max 0 (numSimultaneous - 1)) do
                let once = newCounter 1
                once.Count <- 1
                once.WrapAround <- false
                once.Pos.X <- curr.Spawned.Pos.X + 50.0
                once.Pos.Z <- curr.Spawned.Pos.Z
                let subst = Mcu.substId <| store.GetIdMapper()
                subst once
                Mcu.addTargetLink curr.Spawned once.Index
                Mcu.addTargetLink once next.TryStart.Index
                yield once :> Mcu.McuBase
            // Chain the blocks one after another
            for (block1, _), (block2, _) in Seq.pairwise blocksAndGroups do
                Mcu.addTargetLink block1.Pass block2.TryStart.Index
            match blocksAndGroups with
            | (first, _) :: _ ->
                // A flight being finished tries to start another one
                for b, _ in blocksAndGroups do
                    Mcu.addTargetLink b.Next first.TryStart.Index
                // Start flight shortly after mission start - we need a small delay to give time to each prio node to enable itself
                let delay = newTimer 1
                delay.Time <- 5.0
                delay.Pos.X <- first.TryStart.Pos.X + 100.0
                delay.Pos.Z <- first.TryStart.Pos.Z - 100.0
                let subst = Mcu.substId <| store.GetIdMapper()
                subst delay
                Mcu.addTargetLink missionStart delay.Index
                Mcu.addTargetLink delay first.TryStart.Index
                yield delay :> Mcu.McuBase
            | [] ->
                ()
        ]
    // Result
    { new McuUtil.IMcuGroup with
        member x.Content = additionalLogic
        member x.LcStrings = []
        member x.SubGroups =
                    blocksAndGroups
                    |> List.map snd
    }
