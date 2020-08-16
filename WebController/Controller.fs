namespace Campaign.WebController

open System.IO
open System.Security.Cryptography
open System.Text

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

    type Buildings.BuildingProperties with
        member this.ToDto(nid, useCapacity) =
            {
                Id = nid
                Model = this.Model
                Script = this.Script
                Durability = this.Durability
                NumParts = this.SubParts.Length
                Capacity = if useCapacity then Some(float32 this.Capacity) else None
            }

    let mkIdMaps(buildings : Buildings.BuildingInstance seq, bridges : _ seq) =
        let allBuildingProperties =
            System.Collections.Generic.Dictionary<Buildings.BuildingProperties, int * bool>()

        let assignId isBridge (instance : Buildings.BuildingInstance) =
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

    let mkBuildingPropertiesDtos(properties : Buildings.BuildingProperties seq, tryGetDtoParams) =
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
            let bidToDto (Buildings.BuildingInstanceId pos) =
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
            let bidToDto (Buildings.BuildingInstanceId pos) =
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
                            let (Buildings.BuildingInstanceId pos) = bid
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
                |> Map.ofSeq

            let planes =
                seq {
                    for afid in this.World.Airfields.Keys do
                        let nofPlanes =
                            this.GetNumPlanes(afid)
                            |> Map.toSeq
                            |> Seq.map (fun (planeId, qty) -> string planeId, qty)
                            |> Map.ofSeq
                        yield afid.AirfieldName, nofPlanes
                }
                |> Map.ofSeq

            let owners =
                seq {
                    for rid in this.World.Regions.Keys do
                        match this.GetOwner(rid) with
                        | None -> ()
                        | Some coalition -> yield (string rid, string coalition)
                }
                |> Map.ofSeq
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
                    ] |> Map.ofSeq
                | WarStateUpdate.RepairBuildingPart(bid, part, repair) ->
                    let building = state.World.GetBuildingInstance(bid)
                    "RepairBuildingPart",
                    [ "BuildingAt", building.Pos.ToDto() :> obj
                      "Part", box part
                      "Repair", box repair
                    ] |> Map.ofSeq
                | WarStateUpdate.RemovePlane(afId, plane, health) ->
                    "RemovePlane",
                    [ "Airfield", afId.AirfieldName :> obj
                      "Plane", string plane :> obj
                      "Amount", box health
                    ] |> Map.ofSeq
                | WarStateUpdate.AddPlane(afId, plane, health) ->
                    "AddPlane",
                    [ "Airfield", afId.AirfieldName :> obj
                      "Plane", string plane :> obj
                      "Amount", box health
                    ] |> Map.ofSeq
                | WarStateUpdate.AddGroundForces(region, coalition, amount) ->
                    "AddGroundForces",
                    [ "Region", string region :> obj
                      "Coalition", string coalition :> obj
                      "Amount", box amount
                    ] |> Map.ofSeq
                | WarStateUpdate.DestroyGroundForces(region, coalition, amount) ->
                    "DestroyGroundForces",
                    [ "Region", string region :> obj
                      "Coalition", string coalition :> obj
                      "Amount", box amount
                    ] |> Map.ofSeq
                | WarStateUpdate.MoveGroundForces(regionA, regionB, coalition, amount) ->
                    "MoveGroundForces",
                    [ "Start", string regionA :> obj
                      "Destination", string regionB :> obj
                      "Coalition", string coalition :> obj
                      "Amount", box amount
                    ] |> Map.ofSeq
                | WarStateUpdate.SetRegionOwner(region, coalition) ->
                    "SetRegionOwner",
                    [ "Region", string region :> obj
                      "Coalition", coalition |> Option.map string |> Option.defaultValue "Neutral" :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.AdvanceTime(span) ->
                    "AdvanceTime",
                    [ "Hours", span.TotalHours |> box
                    ] |> Map.ofSeq
                | WarStateUpdate.UpdatePlayer(_, nickName) ->
                    "UpdatePlayer",
                    [ "Name", nickName :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.UpdatePlayerBan(_, ban) ->
                    "UpdatePlayerBan",
                    [ "Ban", (string ban) :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.UpdatePilot(pilot) ->
                    "RegisterPilot",
                    [ "PilotId", box pilot.Id.AsInt
                      "Health", string pilot.Health :> obj
                      "FirstName", pilot.PilotFirstName :> obj
                      "LastName", pilot.PilotLastName :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.RegisterPilotFlight(pid, flight, health) ->
                    "RegisterPilotFlight",
                    [ "PilotId", box pid.AsInt
                      "Health", string health :> obj
                      "FlightStart", flight.Date.ToDto() :> obj
                      "Duration", box flight.Length.TotalMinutes
                      "Return", string flight.Return :> obj
                    ] |> Map.ofSeq

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
                    ] |> Map.ofSeq
                | WarStateUpdate.UpdatedPlanesAtAirfield(afId, content) ->
                    "UpdatedPlanesAtAirfield",
                    [ "Airfield", afId.AirfieldName :> obj
                      "Planes",
                        content
                        |> Map.toSeq
                        |> Seq.map (fun (k, v) -> string k, box v)
                        |> Map.ofSeq
                        :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.UpdatedGroundForces(region, coalition, forces) ->
                    "UpdatedGroundForces",
                    [ "Region", string region :> obj
                      "Coalition", string coalition :> obj
                      "Forces", box forces
                    ] |> Map.ofSeq
                | WarStateUpdate.RegionOwnerSet(region, coalition) ->
                    "RegionOwnerSet",
                    [ "Region", string region :> obj
                      "Coalition", coalition |> Option.map string |> Option.defaultValue "Neutral" :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.TimeSet(time) ->
                    "TimeSet",
                    [ "DateTime", time.ToDto() :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.PlayerUpdated(nickName) ->
                    "PlayerUpdated",
                    [ "Name", nickName :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.PlayerBanUpdated(nickName, ban) ->
                    "PlayerBanUpdated",
                    [ "Name", nickName :> obj
                      "Ban", string ban :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.PilotUpdated(pilot) ->
                    "PilotUpdated",
                    [ "FirstName", pilot.PilotFirstName :> obj
                      "LastName", pilot.PilotLastName :> obj
                      "Health", string pilot.Health :> obj
                      "Country", string pilot.Country :> obj
                      "Flights", box pilot.Flights.Length
                      "Airfield", state.TryGetPilotHome(pilot.Id) |> Option.map (fun afId -> string afId :> obj) |> Option.defaultValue null
                    ] |> Map.ofSeq

            { ChangeDescription = desc
              Values = values
            }

    type Pilots.PilotHealth with
        member this.ToDto() : Dto.HealthStatus =
            match this with
            | Pilots.Healthy -> Dto.Healthy
            | Pilots.Dead -> Dto.Dead
            | Pilots.Injured until -> Dto.Injured {| Until = until.ToDto() |}

    type Targets.ReturnType with
        member this.ToDto() : Dto.ReturnStatus =
            match this with
            | Targets.AtAirfield afId -> Dto.LandedAtAirfield afId.AirfieldName
            | Targets.CrashedInEnemyTerritory -> Dto.CrashedInEnemyTerritory
            | Targets.CrashedInFriendlyTerritory _ -> Dto.CrashedInFriendlyTerritory

    type Targets.TargetType with
        member this.ToDto(world : NewWorldDescription.World) : Dto.TargetType =
            match this with
            | Targets.Truck -> Dto.Vehicle "truck"
            | Targets.Train -> Dto.Vehicle "train" 
            | Targets.Ship -> Dto.Ship "cargo"
            | Targets.Battleship -> Dto.Ship "battleship"
            | Targets.GunBoat -> Dto.Ship "gunboat"
            | Targets.Artillery -> Dto.Artillery "artillery"
            | Targets.Tank -> Dto.Artillery "tank"
            | Targets.ArmoredCar -> Dto.Artillery "armored car"
            | Targets.Bridge(bid, _) ->
                world.Bridges.TryGetValue(bid)
                |> Option.ofPair
                |> Option.map (fun building -> building.Properties.Script)
                |> Option.defaultValue ""
                |> Dto.Bridge
            | Targets.Building(bid, _) ->
                world.Buildings.TryGetValue(bid)
                |> Option.ofPair
                |> Option.map (fun building -> building.Properties.Script)
                |> Option.defaultValue ""
                |> Dto.Building
            | Targets.ParkedPlane(_, plane) ->
                world.PlaneSet.TryGetValue(plane)
                |> Option.ofPair
                |> Option.map (fun plane -> plane.ScriptModel.Script)
                |> Option.defaultValue (string plane)
                |> Dto.ParkedPlane
            | Targets.Air(plane) ->
                world.PlaneSet.TryGetValue(plane)
                |> Option.ofPair
                |> Option.map (fun plane -> plane.ScriptModel.Script)
                |> Option.defaultValue (string plane)
                |> Dto.Plane

    type Dto.DamagedTarget with
        static member OfTuple (world : NewWorldDescription.World) (target : Targets.TargetType, Targets.AmmoName ammo, amount : float32) =
            {
                Dto.Amount = float amount
                Dto.Ammo = ammo
                Dto.Target = target.ToDto(world)
            }

    type Targets.FlightRecord with
        member this.ToDto(world) : Dto.MissionRecord =
            let startAirfield = this.Start.AirfieldName
            let startDate = this.Date.ToDto()
            let endDate = (this.Date + this.Length).ToDto()
            let damages =
                this.TargetsDamaged
                |> List.map (Dto.DamagedTarget.OfTuple world)
            let returnStatus = this.Return.ToDto()
            let plane =
                world.PlaneSet.TryGetValue(this.Plane)
                |> Option.ofPair
                |> Option.map (fun plane -> plane.ScriptModel.Script)
                |> Option.defaultValue (string this.Plane)
            {
                Dto.StartAirfield = startAirfield
                Dto.StartDate = startDate
                Dto.EndDate = endDate
                Dto.DamagedTargets = damages
                Dto.AirKills = this.AirKills
                Dto.ReturnStatus = returnStatus
                Dto.Plane = plane
                Dto.PlaneHealth = float this.PlaneHealth
            }

    type Dto.Rank with
        static member ofName (name : string, abbrev : string) =
            {
                Dto.RankName = name
                Dto.RankAbbrev = abbrev
            }

    type Dto.Award with
        static member ofName (world : NewWorldDescription.World) (country : BasicTypes.CountryId, name : string) : Dto.Award =
            world.Awards.Awards.TryGetValue(country)
            |> Option.ofPair
            |> Option.bind (fun awards ->
                awards
                |> List.tryPick (fun award ->
                    if award.AwardName = name then
                        {
                            Dto.AwardName = award.AwardName
                            Dto.Description = award.Description
                        } |> Some
                    else
                        None
                )
            )
            |> Option.defaultValue (
                {
                    Dto.AwardName = name
                    Dto.Description = ""
                }
            )

    type Pilots.Pilot with
        member this.ToDto(state : WarState.IWarStateQuery) : Dto.Pilot =
            let playerName =
                state.TryGetPlayer(this.PlayerGuid)
                |> Option.map (fun player -> player.Name)
                |> Option.defaultValue ""
            let flights = this.InitialNumFlights + Pilots.countCompletedFlights(this.Flights)
            let airKills = this.InitialAirKills + (this.Flights |> List.sumBy(fun flight -> flight.AirKills))
            let rank = Pilots.tryComputeRank state.World.Ranks this
            {
                Id = this.Id.AsInt
                Rank = rank |> Option.map (fun rank -> rank.RankName) |> Option.defaultValue ""
                RankAbbrev = rank |> Option.map (fun rank -> rank.RankAbbrev) |> Option.defaultValue ""
                FirstName = this.PilotFirstName
                LastName = this.PilotLastName
                Country = string this.Country
                PlayerName = playerName
                Health = this.Health.ToDto()
                Flights = flights
                AirKills = airKills
            }

    type Pilots.BanStatus with
        member this.ToDto() =
            match this with
            | Pilots.BanStatus.Clear | Pilots.BanStatus.Probation _ -> NotBanned
            | Pilots.BanStatus.Banned(since, duration) ->
                (since + duration).ToDto()
                |> Banned

    /// Hash a player's unique GUID, and encode it to base64
    // The userIDs from the logs should probably not be exposed to the public
    let hashGuid (guid : string) =
        use hasher = HashAlgorithm.Create("SHA256")
        guid
        |> Encoding.ASCII.GetBytes
        |> hasher.ComputeHash
        |> System.Convert.ToBase64String

    type Pilots.Player with
        member this.GuidHash =
            hashGuid this.Guid

        member this.ToDto(state : WarState.IWarStateQuery) : Dto.Player =
            let pilots =
                state.GetPlayerPilots(this.Guid)
                |> Seq.map (fun pilot -> pilot.Id.AsInt)
                |> List.ofSeq
            {
                Name = this.Name
                BanStatus = this.BanStatus.ToDto()
                Pilots = pilots
            }

open Campaign.NewWorldDescription
open Campaign.NewWorldDescription.IO
open Campaign.WarState
open Campaign.WarState.IO
open Util
open FSharp.Control
open Campaign.GameServerSync.BaseFileNames

/// Internal state of a controller
type private ControllerState =
    {
        War : IWarStateQuery option
        DtoWorld : Dto.World option
        DtoStateCache : Map<int, Dto.WarState>
        DtoSimulationCache : Map<int, Dto.SimulationStep[]>
        Sync : GameServerSync.Sync option
    }

/// Interface between the web service and the campaign
type Controller(settings : GameServerControl.Settings) =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    // Cache Player GUID hashes
    let cache = Seq.mutableDict []
    let hashGuid = SturmovikMission.Cached.cached cache hashGuid

    // A mailbox processor to serialize access to the ControllerState
    let mb = new MailboxProcessor<_>(fun mb ->
        let rec work (state : ControllerState) =
            async {
                let! req = mb.Receive()
                let! state  = req state
                return! work state
            }
        work {
            War = None
            DtoWorld = None
            DtoStateCache = Map.empty
            DtoSimulationCache = Map.empty
            Sync = None
        }
    )
    do mb.Start()

    let doOnState fn = mb.Post fn
    
    let wkPath f = Path.Combine(settings.WorkDir, f)

    do
        // Create work area if it doesn't exist
        if not(Directory.Exists settings.WorkDir) then
            try
                Directory.CreateDirectory settings.WorkDir
                |> ignore
            with
            | e -> failwithf "Directory '%s' not found, and could not be created because: %s" settings.WorkDir e.Message


    member this.GetWorldDto() =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.DtoWorld with
                | Some world ->
                    channel.Reply(Ok(world))
                    s
                | None ->
                    let path = wkPath worldFilename
                    if not(File.Exists(path)) then
                        channel.Reply(Error "Campaign not initialized")
                        s
                    else
                        try
                            let world = World.LoadFromFile(path)
                            let dto = world.ToDto()
                            channel.Reply(Ok(dto))
                            { s with DtoWorld = Some dto }
                        with e ->
                            logger.Warn(sprintf "Failed to load world: %s" e.Message)
                            logger.Warn(e)
                            channel.Reply(Error "Failed to load world")
                            s
        }

    member this.GetStateDto() =
        async {
            let idx = getCurrentIndex settings.WorkDir
            return! this.GetStateDto(idx)
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
                    let path = wkPath(getStateFilename idx)
                    try
                        let world = World.LoadFromFile(wkPath worldFilename)
                        let state = WarState.LoadFromFile(path, world).ToDto()
                        channel.Reply(Ok state)
                        { s with DtoStateCache = s.DtoStateCache.Add(idx, state) }
                    with
                    | e ->
                        channel.Reply(Error <| sprintf "Failed to load state n.%d" idx)
                        logger.Warn e
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
                        let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer()
                        let world = World.LoadFromFile(wkPath worldFilename)
                        let state = WarState.LoadFromFile(wkPath (getStateFilename idx), world)
                        use reader = new StreamReader(path)
                        let steps =
                            serializer.DeserializeSequence(reader)
                            |> Seq.map (fun (description : string, command : WarStateUpdate.Commands option, results : WarStateUpdate.Results list) ->
                                { Dto.SimulationStep.Description = description
                                  Dto.Command = command |> Option.map Array.singleton |> Option.defaultValue [||] |> Array.map (fun x -> x.ToDto(state))
                                  Dto.Results = results |> List.map (fun r -> r.ToDto(state)) |> Array.ofList
                                }
                            )
                            |> Array.ofSeq
                        channel.Reply(Ok steps)
                        { s with DtoSimulationCache = s.DtoSimulationCache.Add(idx, steps) }
                    with
                    | e ->
                        channel.Reply(Error <| sprintf "Failed to load simulation n.%d" idx)
                        logger.Warn e
                        s
        }

    member this.GetDates() =
        mb.PostAndAsyncReply <| fun channel s -> async {
            try
                let world =
                    lazy
                        World.LoadFromFile(wkPath worldFilename)
                let dates =
                    Directory.EnumerateFiles(settings.WorkDir, "*.xml")
                    |> Seq.filter (fun filename -> filename.EndsWith(stateBaseFilename))
                    |> Seq.sort
                    |> Seq.map (fun path ->
                        try
                            WarState.LoadFromFile(path, world.Value).Date.ToDto()
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
            with e ->
                channel.Reply(Error "Failed to retrieve dates")
                logger.Warn e
            return s
        }

        member this.Run(maxSteps) =
            mb.PostAndAsyncReply <| fun channel s -> async {
                let! sync =
                    match s.Sync with
                    | Some sync -> async.Return (Ok sync)
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            let! status = sync.Init()
                            match status with
                            | Ok _ -> return Ok sync
                            | Error e -> return Error e
                        }
                match sync with
                | Ok sync ->
                    let rec work stepsLeft =
                        async {
                            if stepsLeft <= 0 then
                                channel.Reply(Ok "Scenario advanced")
                            else
                                let! r = sync.Advance()
                                match r with
                                | Error s -> channel.Reply(Error s)
                                | Ok _ -> return! work (stepsLeft - 1)
                        }
                    do! work maxSteps
                    sync.Die("Scenario was forcibly advanced")
                    sync.Dispose()
                    return { s with Sync = None }
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }
            }

        member this.GetSyncState() =
            mb.PostAndAsyncReply <| fun channel s -> async {
                channel.Reply(
                    s.Sync
                    |> Option.map (fun sync ->
                        match sync.SyncState with
                        | Some(GameServerSync.PreparingMission _) -> "Preparing mission"
                        | Some GameServerSync.ResavingMission -> "Resaving missions"
                        | Some(GameServerSync.ExtractingResults _) -> "Extracting results"
                        | Some GameServerSync.AdvancingScenario -> "Advancing scenario"
                        | Some(GameServerSync.RunningMission(_, endTime)) -> sprintf "Running mission (%03d minutes left)" (int (endTime - System.DateTime.UtcNow).TotalMinutes)
                        | None -> "Unknown")
                    |> Option.defaultValue "Not running"
                    |> Ok
                    )
                return s
            }

        member this.WarState =
            mb.PostAndAsyncReply <| fun channel s -> async {
                match s.War with
                | Some war ->
                    channel.Reply(Ok war)
                    return s
                | None ->
                    let idx = getCurrentIndex settings.WorkDir
                    let path = wkPath(getStateFilename idx)
                    try
                        let world = World.LoadFromFile(wkPath worldFilename)
                        let war = WarState.LoadFromFile(path, world)
                        channel.Reply(Ok (upcast war))
                        return { s with War = Some(upcast war) }
                    with exc ->
                        channel.Reply(Error "Failed to load current war state")
                        return s
            }

        member this.GetPilots(filter : PilotSearchFilter) =
            async {
                let! war = this.WarState
                match war with
                | Ok war ->
                    let filterFuns =
                        [
                            match filter.Health with
                            | Some OnlyHealthy ->
                                yield fun (pilot : Pilots.Pilot) -> pilot.Health = Pilots.PilotHealth.Healthy
                            | Some NoDead ->
                                yield fun pilot -> not(pilot.Health = Pilots.PilotHealth.Dead)
                            | None ->
                                ()

                            match filter.Coalition with
                            | Some value ->
                                match
                                    (try
                                        BasicTypes.CoalitionId.FromString value
                                        |> Some
                                     with _ -> None)
                                    with
                                | None ->
                                    yield fun _ -> false
                                | Some coalition ->
                                    yield
                                        fun pilot ->
                                            war.World.Countries.TryGetValue(pilot.Country) = (true, coalition)
                            | None ->
                                ()

                            match filter.Country with
                            | Some value ->
                                match BasicTypes.CountryId.FromString value with
                                | None ->
                                    yield fun _ -> false
                                | Some country ->
                                    yield
                                        fun pilot -> pilot.Country = country
                            | None ->
                                ()

                            match filter.NamePattern with
                            | Some pattern ->
                                let pattern = pattern.Trim().ToLowerInvariant()
                                yield
                                    fun pilot ->
                                        (pilot.PilotFirstName + " " + pilot.PilotLastName).ToLowerInvariant().Contains(pattern)
                            | None ->
                                ()
                        ]
                    return
                        war.Pilots
                        |> List.choose (fun pilot ->
                            if filterFuns |> List.forall (fun f -> f pilot) then
                                pilot.ToDto(war) |> Some
                            else
                                None)
                        |> Ok
                | Error e ->
                    return Error e
            }

        member this.GetPilot(id : int) =
            async {
                let! war = this.WarState
                return
                    match war with
                    | Ok war ->
                        try
                            let pilot = war.GetPilot(Pilots.PilotId id)
                            let flights =
                                pilot.Flights
                                |> List.map (fun flight -> flight.ToDto(war.World))
                            Ok (pilot.ToDto(war), flights)
                        with _ -> Error "Pilot not found"
                    | Error e ->
                        Error e
            }

        member this.GetPlayerPilots(hashedGuid : string) =
            async {
                match! this.WarState with
                | Ok war ->
                    return
                        war.Pilots
                        |> Seq.choose (fun pilot ->
                            if hashGuid pilot.PlayerGuid = hashedGuid then
                                Some(pilot.ToDto(war))
                            else
                                None)
                        |> List.ofSeq
                        |> Ok
                | Error e ->
                    return Error e
            }

        member this.StartSync(doLoop : bool) =
            mb.PostAndAsyncReply <| fun channel s -> async {
                let! sync =
                    match s.Sync with
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            sync.StopAfterMission <- not doLoop
                            let! status = sync.Init()
                            match status with
                            | Ok _ ->
                                return Ok sync
                            | Error e ->
                                return Error e
                        }
                    | Some sync ->
                        async.Return (Ok sync)
                match sync with
                | Ok sync ->
                    sync.StateChanged.Add(fun war -> doOnState (fun s -> async.Return { s with War = Some war }))
                    sync.Resume()
                    channel.Reply(Ok "Synchronization started")
                    return { s with Sync = Some sync }
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }
            }

        member private this.RegisterSyncCleanUp(sync : GameServerSync.Sync) =
            // Run clean-up after sync is terminated
            async {
                let! condemned = Async.AwaitEvent(sync.Terminated)
                doOnState <| fun s -> async {
                    try
                        condemned.Dispose()
                    with _ -> ()
                    return { s with Sync = None }
                }
            }
            |> Async.Start

        member this.StopSyncAfterMission() =
            mb.PostAndAsyncReply <| fun channel s -> async {
                match s.Sync with
                | Some sync ->
                    this.RegisterSyncCleanUp(sync)
                    sync.StopAfterMission <- true
                    channel.Reply(Ok "Synchronization will stop after mission ends")
                | None ->
                    channel.Reply(Error "Synchronization is not active")
                return s
            }

        member this.InterruptSync() =
            mb.PostAndAsyncReply <| fun channel s -> async {
                match s.Sync with
                | Some sync ->
                    this.RegisterSyncCleanUp(sync)
                    sync.Die("Synchronization interrupted")
                    channel.Reply(Ok "Synchronization interruputed")
                | None ->
                    channel.Reply(Error "Synchronization is not active")
                return s
            }

        member this.ResetCampaign(scenario) =
            mb.PostAndAsyncReply <| fun channel s -> async {
                let sync =
                    match s.Sync with
                    | Some sync -> sync
                    | None ->
                        logger.Debug("Creating temporary sync")
                        // Create a temporary sync
                        GameServerSync.Sync.Create(settings)
                let! status = sync.ResetCampaign(scenario)
                logger.Debug("Campaign reset")
                // If we had to create a temporary sync, dispose it
                match s.Sync with
                | None ->
                    logger.Debug("Dispose temporary sync")
                    sync.Dispose()
                | Some _ -> ()
                // Reply and retain old sync, if any.
                match status with
                | Ok _ ->
                    channel.Reply(Ok "New campaign ready")
                | Error msg ->
                    channel.Reply(Error msg)
                return s
            }

        interface IRoutingResponse with
            member this.GetWarState(idx) =
                match idx with
                | None -> this.GetStateDto()
                | Some idx -> this.GetStateDto(idx)

            member this.GetWorld() = this.GetWorldDto()
            member this.GetSimulation(idx) = this.GetSimulationDto(idx)
            member this.GetDates() = this.GetDates()
            member this.GetSyncState() = this.GetSyncState()
            member this.GetPilots(filter) = this.GetPilots(filter)
            member this.GetPilot(id) = this.GetPilot(id)
            member this.GetPlayerPilots(hashedGuid) = this.GetPlayerPilots(hashedGuid)

        interface IControllerInteraction with
            member this.Advance() = this.Run(1)
            member this.Run() = this.Run(15)
            member this.ResetCampaign(scenario) = this.ResetCampaign(scenario)
            member this.StartSyncLoop() = this.StartSync(true)
            member this.StartSyncOnce() = this.StartSync(false)
            member this.StopSyncAfterMission() = this.StopSyncAfterMission()
            member this.InterruptSync() = this.InterruptSync()
