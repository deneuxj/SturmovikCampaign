#I @"C:\Users\johann\.nuget\packages\FSharp.Data\3.3.3\lib\netstandard2.0"
#I @"C:\Users\johann\.nuget\packages\sturmovikmission.dataprovider\7.0.1\lib\netstandard2.0"
#I @"C:\Users\johann\.nuget\packages\FsPickler\5.3.2\lib\netstandard2.0"
#I @"..\bin\Debug\netstandard2.0"
#r "Blocks.dll"
#r "Campaign.dll"
#r "System.Numerics.Vectors.dll"
#r "FSharp.Data"
#r "ploggy"
#r "FsPickler"

open System
open System.IO
open System.Numerics
open Campaign.BasicTypes
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.Missions
open Campaign.CampaignScenario
open Campaign.PlaneModel
open DamageExtension

let missionDir = @"C:\Users\johann\Documents\SturmovikCampaign\Campaign\data"

let planeSet = BodenplatteInternal.PlaneSet.Default

let world =
    let truck = 5.0f<M^3>
    let separation = 10.0f<M>
    let speed = 50000.0f<M/H>
    let numTrucks = speed / separation
    let roadCapacity = numTrucks * truck
    let x = Init.mkWorld(Path.Combine(missionDir, "RheinlandSummer.Mission"), roadCapacity, roadCapacity * 3.0f)
    (planeSet :> IScenarioWorldSetup).Setup x

let war = Init.mkWar world
let mgen : IScenarioController<_> = upcast(Bodenplatte(world, BodenplatteInternal.Constants.Default, planeSet))
mgen.InitAirfields(1.5f, Axis, war)
mgen.InitAirfields(1.0f, Allies, war)

let mutable step = mgen.Start(war)
let random = System.Random(0)

type WarState with
    member this.AllIndustry (coalition : CoalitionId) =
        this.World.Regions.Values
        |> Seq.filter (fun region -> this.GetOwner region.RegionId = Some coalition)
        |> Seq.sumBy (fun region -> this.GetRegionBuildingCapacity region.RegionId)

    member this.AllAirfieldsOperationality (coalition : CoalitionId) =
        this.World.Airfields.Values
        |> Seq.filter (fun af -> this.GetOwner af.Region = Some coalition)
        |> Seq.sumBy (fun af ->
            af.Facilities
            |> Seq.sumBy this.GetBuildingCapacity)

    member this.AllGroundForces (coalition : CoalitionId) =
        this.World.Regions.Values
        |> Seq.sumBy (fun region -> this.GetGroundForces(coalition, region.RegionId))

    member this.AllPlanes (coalition : CoalitionId, planeType : PlaneType) =
        this.World.Airfields.Values
        |> Seq.filter (fun af -> this.GetOwner af.Region = Some coalition)
        |> Seq.sumBy (fun af ->
            this.GetNumPlanes(af.AirfieldId)
            |> Map.toSeq
            |> Seq.sumBy (fun (plane, qty) -> if this.World.PlaneSet.[plane].Kind = planeType then floor qty else 0.0f)
        )

    member this.GetSummaryColums() =
        sprintf "%6s|%10s|%10s|%10s|%10s|%11s|%10s|%11s"
            "Side" "Industry" "Airfields" "Ground" "Bombers" "Attackers" "Fighters" "Transport"

    member this.GetSummary (coalition : CoalitionId) =
        sprintf "%6s|%10.0f|%10.0f|%10.0f|%10.0f|%11.0f|%10.0f|%11.0f"
            (string coalition)
            (this.AllIndustry coalition)
            (this.AllAirfieldsOperationality coalition)
            (this.AllGroundForces coalition)
            (this.AllPlanes(coalition, PlaneType.Bomber))
            (this.AllPlanes(coalition, PlaneType.Attacker))
            (this.AllPlanes(coalition, PlaneType.Fighter))
            (this.AllPlanes(coalition, PlaneType.Transport))

let mutable numSteps = 0

let advance verbose =
    printfn "%s" (war.GetSummaryColums())
    printfn "%s" (war.GetSummary Axis)
    printfn "%s" (war.GetSummary Allies)
    match step with
    | Stalemate comment -> printfn "Stalemate after %d steps %s" numSteps comment
    | Victory(side, comment) -> printfn "%s is victorious after %d steps thanks to %s" (string side) numSteps comment
    | Ongoing data ->
        numSteps <- numSteps + 1
        printfn "New mission %02d: %s" numSteps data.Briefing
        for mission in data.Missions do
            printfn "%s" mission.Description
        let sim = MissionSimulator(random, war, data.Missions, 10.0f<H>)
        let events = sim.DoAll()
        for cmd, descr in events do
            if verbose then
                printfn "Action: %s" descr
            cmd
            |> Option.iter (fun cmd ->
                let showStatus(prefix) =
                    match cmd with
                    | DamageBuildingPart(bid, part, damage) ->
                        let health = war.GetBuildingPartHealthLevel(bid, part)
                        printfn "%s Current health of part: %1.2f" prefix health
                    | _ -> ()
                //showStatus "BEFORE"
                for result in cmd.Execute(war) do
                    if verbose then
                        printfn "Result: %s" (Results.asString war result)
                //showStatus "AFTER"
                )
        //for region in war.World.Regions.Values do
        //    for coalition in [Axis; Allies] do
        //        let owner = war.GetOwner(region.RegionId)
        //        if war.GetGroundForces(coalition, region.RegionId) > 0.0f<MGF> && owner <> Some coalition then
        //            eprintfn "Forces from %s wandered into %s controlled by %s" (string coalition) (string region.RegionId) (string owner)
        step <- mgen.NextStep data (upcast war)

let advanceStar verbose =
    let rec work() =
        advance verbose
        match step with
        | Ongoing _ ->
            work()
        | _ ->
            // Can't advance, but try anyway to get the final status message
            advance verbose
    work()