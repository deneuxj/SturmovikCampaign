namespace Campaign.WebController

open System.Security.Cryptography
open System.Text

open Util

open Campaign
open Campaign.Common
open Campaign.WebController.Dto
open Campaign.WarState.IWarStateExtensions

module DtoCreation =

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
                    [ "PilotId", box pilot.Id.Guid
                      "Health", string pilot.Health :> obj
                      "FirstName", pilot.PilotFirstName :> obj
                      "LastName", pilot.PilotLastName :> obj
                    ] |> Map.ofSeq
                | WarStateUpdate.RegisterPilotFlight(pid, flight, health) ->
                    "RegisterPilotFlight",
                    [ "PilotId", box pid.Guid
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
                      "Airfield", state.TryGetPilotHome(pilot.Id) |> Option.map (fun afId -> string afId) |> Option.defaultValue "" :> obj
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
                Id = string this.Id.Guid
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
                Banned {| Until = (since + duration).ToDto() |}

    /// Hash a player's unique GUID, and encode it to base64
    // The userIDs from the logs should probably not be exposed to the public
    let hashGuid (guid : string) =
        use hasher = HashAlgorithm.Create("SHA256")
        guid
        |> Encoding.ASCII.GetBytes
        |> hasher.ComputeHash
        |> System.Convert.ToBase64String
        |> HashedGuid.Create

    type Pilots.Player with
        member this.GuidHash =
            hashGuid this.Guid

        member this.ToDto(state : WarState.IWarStateQuery) : Dto.Player =
            let pilots =
                state.GetPlayerPilots(this.Guid)
                |> Seq.map (fun pilot -> string pilot.Id.Guid)
                |> List.ofSeq
            {
                Name = this.Name
                Guid = hashGuid(this.Guid)
                BanStatus = this.BanStatus.ToDto()
                Pilots = pilots
            }
