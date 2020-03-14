namespace Campaign.WebController

open System.IO
open Campaign.WebController.Routes
open Campaign.WebController
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


type Controller(workDir : string) =
    let theLock = obj()

    let getWorld, setWorld, getState, setState =
        let mutable world : NewWorldDescription.World option = None
        let mutable state : WarState.WarState option = None

        let getWorld() =
            lock theLock (fun () -> world)

        let setWorld w =
            lock theLock (fun () -> world <- w)

        let getState() =
            lock theLock (fun () -> state)

        let setState s =
            lock theLock (fun () -> state <- s)

        getWorld, setWorld, getState, setState

    do
        if not(Directory.Exists workDir) then
            try
                Directory.CreateDirectory workDir
                |> ignore
            with
            | e -> failwithf "Directory '%s' not found, and could not be created because: %s" workDir e.Message

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
