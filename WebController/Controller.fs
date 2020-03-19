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
            {
                Scenario = this.Scenario
                Map = this.Map
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
                seq {
                    for bid in bids do
                        let health = this.GetBuildingHealth(bid)
                        if health < 1.0f then
                            let (NewWorldDescription.BuildingInstanceId pos) = bid
                            yield
                                pos.ToDto(),
                                {
                                    HealthLevel = health
                                    FunctionalityLevel = this.GetBuildingFunctionalityLevel(bid)
                                }
                }
                |> dict

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

type Controller(settings : Settings) =
    let theLock = obj()
    let worldFilename = "world.json"
    let stateBaseFilename = "-state.json"
    let stepBaseFilename = "-step.json"

    let stateFilename idx = sprintf "%03d%s" idx stateBaseFilename
    let stepFilename idx = sprintf "%03d%s" idx stepBaseFilename

    let getWorld, setWorld, getState, setState =
        let mutable world : World option = None
        let mutable state : WarState option = None

        let getWorld() =
            lock theLock (fun () -> world)

        let setWorld w =
            lock theLock (fun () -> world <- w)

        let getState() =
            lock theLock (fun () -> state)

        let setState s =
            lock theLock (fun () -> state <- s)

        getWorld, setWorld, getState, setState

    let tryGetCachedState, addToCachedState, resetCachedState =
        let stateCache = Dictionary<int, Dto.WarState>()

        let tryGetCachedState idx =
            lock theLock (fun () ->
                stateCache.TryGetValue(idx)
                |> Option.ofPair
            )

        let addToCachedState(idx, state) =
            lock theLock (fun () ->
                stateCache.Add(idx, state)
            )

        let resetCachedState() =
            lock theLock (fun () ->
                stateCache.Clear()
            )

        tryGetCachedState, addToCachedState, resetCachedState

    let getScenarioController, setScenarioController =
        let mutable sctrl : IScenarioController option = None

        let getScenarioController() =
            lock theLock (fun () -> sctrl)

        let setScenarioController(x) =
            lock theLock (fun () -> sctrl <- x)

        getScenarioController, setScenarioController

    let getScenarioState, setScenarioState =
        let mutable step : ScenarioStep option = None

        let getScenarioState() =
            lock theLock (fun () -> step)

        let setScenarioState(x) =
            lock theLock (fun () -> step <- x)

        getScenarioState, setScenarioState

    let wkPath f = Path.Combine(settings.WorkDir, f)

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
            try
                setWorld(Some(World.LoadFromFile path))
            with
            | e -> eprintfn "Failed to load '%s': %s" path e.Message

        // Load latest state
        let latestState =
            try
                Directory.EnumerateFiles(settings.WorkDir, "*" + stateBaseFilename)
                |> Seq.max
                |> Some
            with _ -> None
        match getWorld(), latestState with
        | Some world, Some latestState ->
            let path = wkPath latestState
            try
                setState(Some(WarState.LoadFromFile(path, world)))
            with
            | e -> eprintfn "Failed to load '%s': %s" path e.Message
        | None, Some path ->
            eprintfn "Cannot load '%s' because world data could not be loaded." path
        | Some _, None ->
            ()
        | None, None ->
            // Do nothing, user must initialize campaign manually
            ()

        // Load scenario controller: TODO
        match getWorld() with
        | Some world ->
            let sctrl : IScenarioController =
                let planeSet = BodenplatteInternal.PlaneSet.Default
                upcast(Bodenplatte(world, BodenplatteInternal.Constants.Default, planeSet))
            setScenarioController(Some sctrl)
        | None ->
            ()

        // Load scenario state
        let latestStep =
            try
                Directory.EnumerateFiles(settings.WorkDir, "*" + stepBaseFilename)
                |> Seq.max
                |> Some
            with _ -> None
        match latestStep, getScenarioController(), getState() with
        | Some filename, _, _ ->
            let path = wkPath filename
            let json = File.ReadAllText(path)
            let step = ScenarioStep.Deserialize(json)
            setScenarioState(Some step)
        | None, Some sctrl, Some state ->
            setScenarioState(Some(sctrl.Start state))
        | _ ->
            ()

    member this.GetWorldDto() =
        match getWorld() with
        | None ->
            Error "Campaign not initialized"
        | Some world ->
            let dto = world.ToDto()
            Ok dto

    member this.GetStateDto() =
        match getState() with
        | None ->
            Error "Campaign not started"
        | Some war ->
            let dto = war.ToDto()
            Ok dto

    /// Try to get a recorded state by its number
    member this.GetStateDto(idx : int) =
        match tryGetCachedState idx with
        | Some cached -> Ok cached
        | None ->
            match getWorld() with
            | None ->
                Error "Campaign not initialized"
            | Some world ->
                let path = wkPath(stateFilename idx)
                try
                    let state = WarState.LoadFromFile(path, world).ToDto()
                    addToCachedState(idx, state)
                    Ok state
                with
                | _ -> Error <| sprintf "Failed to load state n.%d" idx

    member this.ResetCampaign() =
        lock theLock (fun () ->
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
                let (planeSet : IScenarioWorldSetup, sctrl : IScenarioController, axisPlanesFactor, alliesPlanesFactor) =
                    let planeSet = BodenplatteInternal.PlaneSet.Default
                    upcast planeSet, upcast(Bodenplatte(world, BodenplatteInternal.Constants.Default, planeSet)), 1.5f, 1.0f
                let world = planeSet.Setup world
                let state0 = Init.mkWar world
                sctrl.InitAirfields(axisPlanesFactor, Axis, state0)
                sctrl.InitAirfields(alliesPlanesFactor, Allies, state0)
                let step = sctrl.Start state0
                resetCachedState()
                setWorld(Some world)
                setState(Some state0)
                setScenarioController(Some sctrl)
                setScenarioState(Some step)
                Ok "Campaign data initialized"

            let writeData _ =
                match getWorld(), getState(), getScenarioController(), getScenarioState() with
                | Some world, Some state, _, Some step ->
                    world.SaveToFile(wkPath worldFilename)
                    state.SaveToFile(wkPath(stateFilename 0))
                    File.WriteAllText(wkPath(stepFilename 0), step.Serialize())
                    Ok "Initial campaign data written"
                | _ ->
                    Error "Internal error: world, state or controller data not set"

            prepareDir()
            |> Result.bind initData
            |> Result.bind writeData
        )

        member this.Advance(seed) =
            match getState(), getScenarioController(), getScenarioState() with
            | Some state, Some sctrl, Some(Ongoing stepData) ->
                let random = System.Random(seed)
                // Simulate missions
                let sim = Campaign.Missions.MissionSimulator(random, state, stepData.Missions, settings.SimulatedDuration * 1.0f<H>)
                let events = sim.DoAll()
                let results =
                    events
                    |> Seq.choose(fun (cmd, description) ->
                        cmd
                        |> Option.map (fun cmd -> description, cmd.Execute(state)))
                    |> List.ofSeq
                // Move to next day
                state.SetDate(state.Date + System.TimeSpan(24, 0, 0))
                // Plan next round
                let advance = sctrl.NextStep(stepData)
                let nextStep = advance state
                // Write war state and campaign step files
                let stateFile, stepFile =
                    Seq.initInfinite (fun i -> (stateFilename i, stepFilename i))
                    |> Seq.find (fun (stateFile, stepFile) ->
                        [stateFile; stepFile]
                        |> Seq.map (fun filename -> Path.Combine(settings.WorkDir, filename))
                        |> Seq.forall (File.Exists >> not))
                state.SaveToFile(stateFile)
                File.WriteAllText(stepFile, nextStep.Serialize())
                // Results from the commands generated by the simulation
                Ok results

            | _, _, Some _ ->
                Error "Cannot advance campaign, it has reached its final state"

            | _ ->
                Error "Campaign data missing"
