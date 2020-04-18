namespace Campaign.WebController

open System.IO
open Campaign.WebController.Routes
open Util
open Campaign

[<AutoOpen>]
module internal Extensions =
    open Campaign.WebController.Dto
    open Campaign.WarState.IWarStateExtensions

    type System.Numerics.Vector2 with
        member this.ToDto() =
            { X = this.X
              Y = this.Y
            }

    type BasicTypes.OrientedPosition with
        member this.ToDto() =
            {
                Position = this.Pos.ToDto()
                Altitude = this.Altitude
                Rotation = this.Rotation
            }

    type System.DateTime with
        member this.ToDto() =
            {
                Year = this.Year
                Month = this.Month
                Day = this.Day
                Hour = this.Hour
                Minute = this.Minute
            }

    type NewWorldDescription.BuildingProperties with
        member this.ToDto(nid, useCapacity) =
            {
                Id = nid
                Model = this.Model
                Script = this.Script
                Durability = this.Durability
                NumParts = this.SubParts.Length
                Capacity = if useCapacity then System.Nullable(float32 this.Capacity) else System.Nullable()
            }

    let mkIdMaps(buildings : NewWorldDescription.BuildingInstance seq, bridges : _ seq) =
        let allBuildingProperties =
            System.Collections.Generic.Dictionary<NewWorldDescription.BuildingProperties, int * bool>()

        let assignId isBridge (instance : NewWorldDescription.BuildingInstance) =
            if not(allBuildingProperties.ContainsKey(instance.Properties)) then
                let nextId = allBuildingProperties.Count
                allBuildingProperties.[instance.Properties] <- (nextId, not isBridge)

        buildings
        |> Seq.iter (assignId false)

        bridges
        |> Seq.iter (assignId true)

        let propIdOfInstance =
            seq {
                for building in Seq.append buildings bridges do
                    let nid, _ = allBuildingProperties.[building.Properties]
                    yield building.Pos, nid
            }
            |> dict

        let tryGetDtoParams props =
            allBuildingProperties.TryGetValue(props)
            |> Option.ofPair

        let getPropertiesId pos =
            propIdOfInstance.[pos]
            
        tryGetDtoParams, getPropertiesId

    let mkBuildingPropertiesDtos(properties : NewWorldDescription.BuildingProperties seq, tryGetDtoParams) =
        let dtos =
            properties
            |> Seq.mapi (fun i props ->
                match tryGetDtoParams props with
                | Some(idx, useCapacity) ->
                    props.ToDto(idx, useCapacity)
                | None ->
                    // Use negative ids for the building properties that do not appear in any instance
                    props.ToDto(-i, true)
            )
        dtos

    type NewWorldDescription.Region with
        member this.ToDto(getPropertiesId) =
            let bidToDto (NewWorldDescription.BuildingInstanceId pos) =
                {
                    Position = pos.ToDto()
                    PropertiesId = getPropertiesId pos
                }
            {
                Id = string this.RegionId
                Boundary = this.Boundary |> Seq.map (fun x -> x.ToDto()) |> Array.ofSeq
                Position = this.Position.ToDto()
                Neighbours = this.Neighbours |> Seq.map string |> Array.ofSeq
                InitialOwner = this.InitialOwner |> Option.map (fun owner -> string owner) |> Option.defaultValue null
                IsEntry = this.IsEntry
                Buildings = this.IndustryBuildings |> Seq.map bidToDto |> Array.ofSeq
            }

    type NewWorldDescription.Airfield with
        member this.ToDto(getPropertiesId) =
            let bidToDto (NewWorldDescription.BuildingInstanceId pos) =
                {
                    Position = pos.ToDto()
                    PropertiesId = getPropertiesId pos
                }
            {
                Id = this.AirfieldId.AirfieldName
                Position = this.Position.ToDto()
                Region = string this.Region
                Buildings = this.Facilities |> Seq.map bidToDto |> Array.ofSeq
            }

    type PlaneModel.PlaneModel with
        member this.ToDto() =
            {
                Name = this.Name
                Kind = string this.Kind
                Roles = this.Roles |> Seq.map string |> Array.ofSeq
                Coalition = string this.Coalition
                Script = this.ScriptModel.Script
                Model = this.ScriptModel.Model
                BombCapacity = float32 this.BombCapacity
                CargoCapacity = float32 this.CargoCapacity
            }

    type NewWorldDescription.World with
        member this.ToDto() =
            let fn, getPropertiesId = mkIdMaps(this.Buildings.Values, this.Bridges.Values)
            let buildingProps = 
                let props =
                    Seq.append this.Buildings.Values this.Bridges.Values
                    |> Seq.map (fun building -> building.Properties)
                    |> Seq.distinct
                mkBuildingPropertiesDtos(props, fn)
            let bridges =
                this.Bridges.Values
                |> Seq.map (fun bridge ->
                    {
                        Position = bridge.Pos.ToDto()
                        PropertiesId = getPropertiesId bridge.Pos
                    }
                )
            let mapSE, mapNE =
                match this.Map.ToLowerInvariant() with
                | _
                | "rheinland" -> { X = 30.0e3f; Y = 30.0e3f }, { X = 354.0e3f; Y = 431.0e3f }
            {
                Scenario = this.Scenario
                Map = this.Map
                MapSouthWest = mapSE
                MapNorthEast = mapNE
                StartDate = this.StartDate.ToDto()
                Regions = this.Regions.Values |> Seq.map (fun r -> r.ToDto(getPropertiesId)) |> Array.ofSeq
                Airfields = this.Airfields.Values |> Seq.map (fun af -> af.ToDto(getPropertiesId)) |> Array.ofSeq
                BuildingProperties = buildingProps |> Array.ofSeq
                Bridges = bridges |> Array.ofSeq
                PlaneSet = this.PlaneSet.Values |> Seq.map (fun plane -> plane.ToDto()) |> Array.ofSeq
            }

    type Weather.WeatherState with
        member this.ToDto() =
            {
                CloudDensity = float32 this.CloudDensity
                CloudHeight = float32 this.CloudHeight
                CloudThickness = float32 this.CloudThickness
                Precipitation = float32 this.Precipitation
                WindSpeed = float32 this.Wind.Speed
                WindDirectionTo = float32 this.Wind.Direction
                Turbulence = float32 this.Turbulence
                Temperature = float32 this.Temperature
                Pressure = float32 this.Pressure
            }

    type WarState.IWarStateQuery with
        member this.ToDto() =
            let mkBuildingHealth bids =
                [|
                    for bid in bids do
                        let health = this.GetBuildingHealth(bid)
                        if health < 1.0f then
                            let (NewWorldDescription.BuildingInstanceId pos) = bid
                            yield
                                {
                                    Position = pos.ToDto()
                                    HealthLevel = health
                                    FunctionalityLevel = this.GetBuildingFunctionalityLevel(bid)
                                }
                |]

            let groundForces =
                [|
                    for rid in this.World.Regions.Keys do
                        for coalition in [BasicTypes.Axis; BasicTypes.Allies] do
                            let forces = float32 <| this.GetGroundForces(coalition, rid)
                            if forces > 0.0f then
                                yield {
                                    Region = string rid
                                    Forces = forces
                                    Coalition = string coalition
                                }
                |]

            let mkTransport graph =
                [|
                    for kvp in this.World.Regions do
                        let rid, neighbours = kvp.Key, kvp.Value.Neighbours
                        for ngh in neighbours do
                            if string ngh > string rid then
                                yield {
                                    RegionA = string rid
                                    RegionB = string ngh
                                    Capacity = float32 <| graph(rid, ngh)
                                }
                |]

            let supplies =
                let x = this.ComputeSupplyAvailability()
                seq {
                    for rid in this.World.Regions.Keys do
                        yield string rid, float32(x rid)
                }
                |> dict

            let planes =
                seq {
                    for afid in this.World.Airfields.Keys do
                        let nofPlanes =
                            this.GetNumPlanes(afid)
                            |> Map.toSeq
                            |> Seq.map (fun (planeId, qty) -> string planeId, qty)
                            |> dict
                        yield afid.AirfieldName, nofPlanes
                }
                |> dict

            let owners =
                seq {
                    for rid in this.World.Regions.Keys do
                        match this.GetOwner(rid) with
                        | None -> ()
                        | Some coalition -> yield (string rid, string coalition)
                }
                |> dict
            {
                Date = this.Date.ToDto()
                Weather = this.Weather.ToDto()
                BuildingHealth = mkBuildingHealth this.World.Buildings.Keys
                BridgeHealth = mkBuildingHealth this.World.Bridges.Keys
                GroundForces = groundForces
                RoadTransport = this.ComputeRoadCapacity() |> mkTransport
                RailTransport = this.ComputeRailCapacity() |> mkTransport
                SupplyStatus = supplies
                Planes = planes
                RegionOwner = owners
            }

    type WarStateUpdate.Commands with
        member this.ToDto(state : WarState.IWarStateQuery) =
            let verb, args =
                match this with
                | WarStateUpdate.DamageBuildingPart(bid, part, damage) ->
                    let building = state.World.GetBuildingInstance(bid)
                    "DamageBuildingPart",
                    [ "BuildingAt", building.Pos.ToDto() :> obj
                      "Part", box part
                      "Damage", box damage
                    ] |> dict
                | WarStateUpdate.RepairBuildingPart(bid, part, repair) ->
                    let building = state.World.GetBuildingInstance(bid)
                    "RepairBuildingPart",
                    [ "BuildingAt", building.Pos.ToDto() :> obj
                      "Part", box part
                      "Repair", box repair
                    ] |> dict
                | WarStateUpdate.RemovePlane(afId, plane, health) ->
                    "RemovePlane",
                    [ "Airfield", afId.AirfieldName :> obj
                      "Plane", string plane :> obj
                      "Amount", box health
                    ] |> dict
                | WarStateUpdate.AddPlane(afId, plane, health) ->
                    "AddPlane",
                    [ "Airfield", afId.AirfieldName :> obj
                      "Plane", string plane :> obj
                      "Amount", box health
                    ] |> dict
                | WarStateUpdate.AddGroundForces(region, coalition, amount) ->
                    "AddGroundForces",
                    [ "Region", string region :> obj
                      "Coalition", string coalition :> obj
                      "Amount", box amount
                    ] |> dict
                | WarStateUpdate.DestroyGroundForces(region, coalition, amount) ->
                    "DestroyGroundForces",
                    [ "Region", string region :> obj
                      "Coalition", string coalition :> obj
                      "Amount", box amount
                    ] |> dict
                | WarStateUpdate.MoveGroundForces(regionA, regionB, coalition, amount) ->
                    "MoveGroundForces",
                    [ "Start", string regionA :> obj
                      "Destination", string regionB :> obj
                      "Coalition", string coalition :> obj
                      "Amount", box amount
                    ] |> dict
                | WarStateUpdate.SetRegionOwner(region, coalition) ->
                    "SetRegionOwner",
                    [ "Region", string region :> obj
                      "Coalition", coalition |> Option.map string |> Option.defaultValue "Neutral" :> obj
                    ] |> dict
                | WarStateUpdate.AdvanceTime(span) ->
                    "AdvanceTime",
                    [ "Hours", span.TotalHours |> box
                    ] |> dict
            { Verb = verb
              Args = args
            }

    type WarStateUpdate.Results with
        member this.ToDto(state : WarState.IWarStateQuery) =
            let desc, values =
                match this with
                | WarStateUpdate.UpdatedStorageValue(bid, amount) ->
                    "UpdatedStorageValue",
                    [ "BuildingAt", state.World.GetBuildingInstance(bid).Pos.ToDto() :> obj
                      "Amount", box amount
                    ] |> dict
                | WarStateUpdate.UpdatedPlanesAtAirfield(afId, content) ->
                    "UpdatedPlanesAtAirfield",
                    [ "Airfield", afId.AirfieldName :> obj
                      "Planes",
                        content
                        |> Map.toSeq
                        |> Seq.map (fun (k, v) -> string k, box v)
                        |> dict
                        :> obj
                    ] |> dict
                | WarStateUpdate.UpdatedGroundForces(region, coalition, forces) ->
                    "UpdatedGroundForces",
                    [ "Region", string region :> obj
                      "Coalition", string coalition :> obj
                      "Forces", box forces
                    ] |> dict
                | WarStateUpdate.RegionOwnerSet(region, coalition) ->
                    "RegionOwnerSet",
                    [ "Region", string region :> obj
                      "Coalition", coalition |> Option.map string |> Option.defaultValue "Neutral" :> obj
                    ] |> dict
                | WarStateUpdate.TimeSet(time) ->
                    "TimeSet",
                    [ "DateTime", time.ToDto() :> obj
                    ] |> dict
            { ChangeDescription = desc
              Values = values
            }

open Campaign.NewWorldDescription
open Campaign.NewWorldDescription.IO
open Campaign.WarState
open Campaign.WarState.IO
open Campaign.BasicTypes
open System.Collections.Generic
open Util
open Campaign.CampaignScenario
open Campaign.CampaignScenario.IO
open Campaign.WarStateUpdate.CommandExecution

type Settings =
    {
        WorkDir : string
        RoadsCapacity : float32
        RailsCapacity : float32
        SimulatedDuration : float32
    }
with
    static member Default =
        let truck = 5.0f<M^3>
        let separation = 10.0f<M>
        let speed = 50000.0f<M/H>
        let numTrucks = speed / separation
        let roadCapacity = float32(numTrucks * truck)

        {
            WorkDir = "CampaignData"
            RoadsCapacity = roadCapacity
            RailsCapacity = 3.0f * roadCapacity
            SimulatedDuration = 10.0f
        }

/// Internal state of a controller
type private ControllerState =
    {
        World : World option
        State : WarState option
        DtoStateCache : Map<int, Dto.WarState>
        DtoSimulationCache : Map<int, Dto.SimulationStep[]>
        ScenarioController : IScenarioController option
        Step : ScenarioStep option
    }

/// Interface between the web service and the campaign
type Controller(settings : Settings) =
    let worldFilename = "world.xml"
    let stateBaseFilename = "-state.xml"
    let stepBaseFilename = "-step.xml"
    let simulationBaseFilename = "-simulation.xml"

    let getStateFilename idx = sprintf "%03d%s" idx stateBaseFilename
    let getStepFilename idx = sprintf "%03d%s" idx stepBaseFilename
    let getSimulationFilename idx = sprintf "%03d%s" idx simulationBaseFilename

    let wkPath f = Path.Combine(settings.WorkDir, f)

    // A mailbox processor to serialize access to the ControllerState
    let mb = new MailboxProcessor<_>(fun mb ->
        let rec work (state : ControllerState) =
            async {
                let! req = mb.Receive()
                let! state  = req state
                return! work state
            }
        work {
            World = None
            State = None
            DtoStateCache = Map.empty
            DtoSimulationCache = Map.empty
            ScenarioController = None
            Step = None
        }
    )
    do mb.Start()

    let doOnState fn = mb.Post fn

    do
        // Create work area if it doesn't exist
        if not(Directory.Exists settings.WorkDir) then
            try
                Directory.CreateDirectory settings.WorkDir
                |> ignore
            with
            | e -> failwithf "Directory '%s' not found, and could not be created because: %s" settings.WorkDir e.Message

        // Load world
        let path = wkPath worldFilename
        if File.Exists path then
            fun s -> async {
                return
                    try
                        let w = World.LoadFromFile path
                        { s with World = Some w}
                    with e ->
                        eprintfn "Failed to load '%s': %s" path e.Message
                        s
            } |> doOnState

        // Load latest state
        let latestState =
            Directory.EnumerateFiles(settings.WorkDir, "*.xml")
            |> Seq.filter (fun s -> s.EndsWith(stateBaseFilename))
            |> Seq.sortDescending
            |> Seq.tryHead

        match latestState with
        | Some latestState ->
            fun s -> async {
                match s.World with
                | Some world ->
                    return
                        try
                            let war = WarState.LoadFromFile(latestState, world)
                            { s with State = Some war }
                        with e ->
                            eprintfn "Failed to load '%s': %s" latestState e.Message
                            s
                | None ->
                    eprintfn "Cannot load '%s' because world data could not be loaded." path
                    return s
            } |> doOnState
        | None ->
            ()

        // Restore scenario controller
        // TODO. For now we just create a controller for Bodenplatte
        fun s -> async {
            return
                match s.World with
                | Some world ->
                    let sctrl : IScenarioController =
                        let planeSet = BodenplatteInternal.PlaneSet.Default
                        upcast(Bodenplatte(world, BodenplatteInternal.Constants.Default, planeSet))
                    { s with ScenarioController = Some sctrl}
                | None ->
                    s
        } |> doOnState

        // Load scenario state
        let latestStep =
            Directory.EnumerateFiles(settings.WorkDir, "*.xml")
            |> Seq.filter (fun s -> s.EndsWith(stepBaseFilename))
            |> Seq.sortDescending
            |> Seq.tryHead
        fun s -> async {
            return
                match s.ScenarioController, s.State with
                | Some sctrl, Some state ->
                    match latestStep with
                    | Some path ->
                        use reader = new StreamReader(path)
                        let step = ScenarioStep.Deserialize(reader)
                        { s with Step = Some step }
                    | None ->
                        { s with Step = Some(sctrl.Start state) }
                | _ ->
                    s
        } |> doOnState

    member this.GetWorldDto() =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.World with
                | None ->
                    channel.Reply(Error "Campaign not initialized")
                    s
                | Some world ->
                    channel.Reply(Ok(world.ToDto()))
                    s
        }

    member this.GetStateDto() =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.State with
                | None ->
                    channel.Reply(Error "Campaign not started")
                    s
                | Some war ->
                    channel.Reply(Ok(war.ToDto()))
                    s
        }

    /// Try to get a recorded state by its number
    member this.GetStateDto(idx : int) =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.DtoStateCache.TryGetValue idx with
                | true, x ->
                    channel.Reply(Ok x)
                    s
                | false, _ ->
                    match s.World with
                    | None ->
                        channel.Reply(Error "Campaign not initialized")
                        s
                    | Some world ->
                        let path = wkPath(getStateFilename idx)
                        try
                            let state = WarState.LoadFromFile(path, world).ToDto()
                            channel.Reply(Ok state)
                            { s with DtoStateCache = s.DtoStateCache.Add(idx, state) }
                        with
                        | _ ->
                            channel.Reply(Error <| sprintf "Failed to load state n.%d" idx)
                            s
        }

    /// Try to get the simulation steps leading to the state identified by the given index
    member this.GetSimulationDto(idx : int) =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.DtoSimulationCache.TryGetValue idx with
                | true, x ->
                    channel.Reply(Ok x)
                    s
                | false, _ ->
                    let path = wkPath(getSimulationFilename idx)
                    try
                        let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
                        use reader = new StreamReader(path)
                        let steps =
                            serializer.DeserializeSequence(reader)
                            |> Array.ofSeq
                        channel.Reply(Ok steps)
                        { s with DtoSimulationCache = s.DtoSimulationCache.Add(idx, steps) }
                    with
                    | _ ->
                        channel.Reply(Error <| sprintf "Failed to load simulation n.%d" idx)
                        s
        }

    member this.GetDates() =
        mb.PostAndAsyncReply <| fun channel s -> async {
            match s.World with
            | Some world ->
                let dates =
                    Directory.EnumerateFiles(settings.WorkDir, "*.xml")
                    |> Seq.filter (fun filename -> filename.EndsWith(stateBaseFilename))
                    |> Seq.sort
                    |> Seq.map (fun path ->
                        try
                            WarState.LoadFromFile(path, world).Date.ToDto()
                        with _ ->
                            { Year = -1
                              Month = -1
                              Day = -1
                              Hour = -1
                              Minute = -1
                            }
                        )
                    |> Array.ofSeq
                channel.Reply(Ok dates)
                return s
            | None ->
                channel.Reply(Error "Campaign not initialized")
                return s
        }

    member this.ResetCampaign() =
        async {
            let bakDir =
                let up = Path.GetDirectoryName(Path.GetFullPath(settings.WorkDir))
                Seq.initInfinite (fun i -> sprintf "%s.bak%03d" settings.WorkDir i)
                |> Seq.find (fun dirname -> not(Directory.Exists(Path.Combine(up, dirname))))

            let moveDir _ =
                try
                    Directory.Move(settings.WorkDir, bakDir)
                    Ok "Old working dir backed up"
                with
                | _ -> Error <| sprintf "Failed to back up working dir '%s'" settings.WorkDir

            let recreateDir _ =
                try
                    Directory.CreateDirectory(settings.WorkDir) |> ignore
                    Ok "Fresh working dir created"
                with
                | _ -> Error <| sprintf "Failed to create fresh working dir '%s'" settings.WorkDir

            let prepareDir _ =
                if not(Directory.Exists(settings.WorkDir)) || not (Seq.isEmpty(Directory.EnumerateFileSystemEntries(settings.WorkDir))) then
                    moveDir()
                    |> Result.bind recreateDir
                else
                    Ok "No old campaign data to backup before reset"

            let initData _ =
                let scenario = "RheinlandSummer.Mission"
                let world = Init.mkWorld(scenario, settings.RoadsCapacity * 1.0f<M^3/H>, settings.RailsCapacity * 1.0f<M^3/H>)
                let (world, sctrl : IScenarioController, axisPlanesFactor, alliesPlanesFactor) =
                    let planeSet = BodenplatteInternal.PlaneSet.Default
                    let world = planeSet.Setup world
                    world, upcast(Bodenplatte(world, BodenplatteInternal.Constants.Default, planeSet)), 1.5f, 1.0f
                let state0 = Init.mkWar world
                sctrl.InitAirfields(axisPlanesFactor, Axis, state0)
                sctrl.InitAirfields(alliesPlanesFactor, Allies, state0)
                let step = sctrl.Start state0
                Ok
                    {
                        DtoStateCache = Map.empty
                        DtoSimulationCache = Map.empty
                        World = Some world
                        State = Some state0
                        ScenarioController = Some sctrl
                        Step = Some step
                    }

            let writeData (s : ControllerState) =
                match s.World, s.State, s.ScenarioController, s.Step with
                | Some world, Some state, _, Some step ->
                    world.SaveToFile(wkPath worldFilename)
                    state.SaveToFile(wkPath(getStateFilename 0))
                    step.SaveToFile(wkPath(getStepFilename 0))
                    Ok s
                | _ ->
                    Error "Internal error: world, state or controller data not set"

            let res =
                prepareDir()
                |> Result.bind initData
                |> Result.bind writeData

            let userRes =
                res
                |> Result.bind (fun _ -> Ok "Success")

            return! mb.PostAndAsyncReply <| fun channel s -> async {
                channel.Reply(userRes)

                return
                    match res with
                    | Ok s2 -> s2
                    | Error _ -> s
            }
        }

        member this.Advance(seed) =
            mb.PostAndAsyncReply <| fun channel s -> async {
                match s.State, s.ScenarioController, s.Step with
                | Some state, Some sctrl, Some(Ongoing stepData) ->
                    let random = System.Random(seed)
                    // Simulate missions
                    let sim = Campaign.Missions.MissionSimulator(random, state, stepData.Missions, settings.SimulatedDuration * 1.0f<H>)
                    let events = 
                        seq {
                            yield! sim.DoAll()
                            for cmd in sctrl.NewDay(state) do
                                yield cmd
                        }
                    let results =
                        events
                        |> Seq.map(fun (cmd, description) ->
                            let results =
                                cmd
                                |> Option.map (fun cmd -> cmd.Execute(state))
                                |> Option.defaultValue []
                            description, cmd, results)
                        |> List.ofSeq
                    // Plan next round
                    let advance = sctrl.NextStep(stepData)
                    let nextStep = advance state
                    // Write war state and campaign step files
                    let stateFile, stepFile, simFile =
                        Seq.initInfinite (fun i -> (wkPath(getStateFilename i), wkPath(getStepFilename i), wkPath(getSimulationFilename i)))
                        |> Seq.find (fun (stateFile, stepFile, simFile) ->
                            [stateFile; stepFile; simFile]
                            |> Seq.forall (File.Exists >> not))
                    state.SaveToFile(stateFile)
                    nextStep.SaveToFile(stepFile)
                    // Results from the commands generated by the simulation
                    let results =
                        results
                        |> Seq.map (fun (description, cmd, results) ->
                            { Dto.SimulationStep.Description = description
                              Dto.Command = cmd |> Option.map(fun cmd -> [| cmd.ToDto(state) |]) |> Option.defaultValue [||]
                              Dto.Results = results |> Seq.map(fun res -> res.ToDto(state)) |> Array.ofSeq
                            })
                        |> Array.ofSeq
                    use writer = new StreamWriter(simFile)
                    let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
                    serializer.SerializeSequence(writer, results) |> ignore
                    channel.Reply(Ok results)
                    // state changed was done by mutating s.State
                    return { s with Step = Some nextStep }
                | _, _, Some _ ->
                    channel.Reply(Error "Cannot advance campaign, it has reached its final state")
                    return s
                | _ ->
                    channel.Reply(Error "Campaign data missing")
                    return s
            }

        member this.Run(seed, maxSteps) =
            let rec work stepsLeft =
                async {
                    if stepsLeft <= 0 then
                        return Error ""
                    else
                        let! r = this.Advance(seed)
                        match r with
                        | Error s -> return Ok s
                        | Ok _ -> return! work (stepsLeft - 1)
                }
            work maxSteps

        interface IRoutingResponse with
            member this.GetWarState(idx) =
                match idx with
                | None -> this.GetStateDto()
                | Some idx -> this.GetStateDto(idx)

            member this.GetWorld() = this.GetWorldDto()
            member this.GetSimulation(idx) = this.GetSimulationDto(idx)
            member this.GetDates() = this.GetDates()

        interface IControllerInteraction with
            member this.Advance() = this.Advance(0)
            member this.Run() = this.Run(0, 15)
            member this.ResetCampaign(scenario) = this.ResetCampaign()