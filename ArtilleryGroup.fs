module Campaign.ArtilleryGroup

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.StaticDefenses.Factory
open Util
open Campaign.WorldDescription
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.WorldState

type ArtilleryGroup = {
    All : Mcu.McuBase list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = []
        member x.SubGroups = []

    static member Create(random : System.Random, store, lcStore, includeSearchLights, missionBegin : Mcu.McuTrigger, world : World, state : WorldState, attackingColumns : ColumnMovement list) =
        let wg = world.FastAccess
        let sg = state.FastAccess
        let regionFillLevel =
            state.GetAmmoCostPerRegion(world)
            |> Map.map (fun region cost ->
                if cost > 0.0f<E> then
                    sg.GetRegion(region).Supplies / cost
                else
                    1.0f
                |> max 0.0f
                |> min 1.0f)
        let all =
            [
                for area in world.AntiAirDefenses do
                    let numUnits =
                        regionFillLevel
                        |> Map.tryFind area.Home
                        |> Option.defaultVal 0.0f
                        |> ((*) (float32 area.MaxNumGuns))
                        |> ceil
                        |> int
                    if numUnits > 0 then
                        let owner = sg.GetRegion(area.Home).Owner
                        let country, coalition =
                            match owner with
                            | None -> failwithf "No owner found for group of anti-air defenses '%A'" area.DefenseAreaId
                            | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                            | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                        let group = StaticDefenseGroup.Create(area.Role, includeSearchLights, random, store, lcStore, area.Boundary, area.Position.Rotation, numUnits, country, coalition)
                        let links = group.CreateLinks()
                        let mcus = McuUtil.deepContentOf group
                        links.Apply(mcus)
                        Mcu.addTargetLink missionBegin group.Api.Start.Index
                        yield! mcus
            ]
        { All = all
        }
