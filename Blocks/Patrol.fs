module SturmovikMission.Blocks.Patrol

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.WhileEnemyClose
open SturmovikMission.Blocks.McuInstantiation
open SturmovikMission.Blocks.GroundAttack

/// A pair of fighters that fly a circular pattern for one hour, then respawns.
type Patrol = {
    Start : Mcu.McuTrigger
    Plane : Mcu.HasEntity
    Spawned : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Completed : Mcu.McuTrigger
    WhileEnemyClose : WhileEnemyClose
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, pos : Vector2, planeAlt : float32, coalition : Mcu.CoalitionValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Patrol").CreateMcuList()
        for mcu in group do
            subst mcu
        let wec = WhileEnemyClose.Create(true, false, store, pos, coalition)
        // Get key nodes
        let start = getTriggerByName group "DelayedStart"
        let plane = getVehicleByName group "Plane"
        let killed = getTriggerByName group "Killed"
        let spawned = getTriggerByName group "Spawned"
        let setGerman = getTriggerByName group "SetGerman"
        let setRussian = getTriggerByName group "SetRussian"
        let completed = getTriggerByName group "Completed"
        // Connection with wec
        let wakeup = getTriggerByName group "WakeUp"
        let sleep = getTriggerByName group "Sleep"
        let startMonitoring = getTriggerByName group "StartMonitoring"
        Mcu.addTargetLink wec.WakeUp wakeup.Index
        Mcu.addTargetLink wec.Sleep sleep.Index
        Mcu.addTargetLink startMonitoring wec.StartMonitoring.Index
        Mcu.addObjectLink wec.Proximity plane.LinkTrId
        // Set country upon spawning
        match coalition with
        | Mcu.CoalitionValue.Axis -> Mcu.addTargetLink spawned setGerman.Index
        | Mcu.CoalitionValue.Allies -> Mcu.addTargetLink spawned setRussian.Index
        | _ -> failwith "Unsupported coalition value"
        // Position of all nodes
        let refPoint = Vector2.FromMcu(plane.Pos)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
            mcu.Pos.Y <- float planeAlt
        // result
        { Start = start
          Plane = plane
          Killed = killed
          Spawned = spawned
          WhileEnemyClose = wec
          Completed = completed
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = []
                  member x.SubGroups = [ wec.All ]
            }
        }


type PatrolGroupConfig = {
    StartType : StartType
    StartPos : DirectedPoint
    CruiseAltitude : int
    CruiseSpeed : int
    MidPoint : DirectedPoint
    PatrolCenter : DirectedPoint
    PatrolDuration : float32
    PatrolAltitude : int
    PatrolSpeed : int
    MaxRange : int
    Return : DirectedPoint
    Final : DirectedPoint
    LandAt : DirectedPoint
    NumPlanes : int
}

type PatrolGroup = {
    /// IN
    Start : Mcu.McuTrigger
    /// OUT
    FlightCannotPatrol : Mcu.McuTrigger
    /// OUT
    AllKilled : Mcu.McuTrigger
    LeadPlane : Mcu.HasEntity ref
    WingPlanes : Mcu.HasEntity[]
    All : McuUtil.IMcuGroup
}
with
    static member Create(store, config : PatrolGroupConfig) =
        // Instantiate
        let group, subst = getFreshGroup blocks2Data store "Patrol"

        // Nodes of interest
        let start = getTriggerByName group "START"
        let areaMaxRange = getTriggerByName group "MaxPursuit" :?> Mcu.McuProximity
        let flightCannotPatrol = getTriggerByName group "FLIGHT_CANNOT_PATROL" :?> Mcu.McuCounter
        let allKilled = getTriggerByName group "ALL_KILLED" :?> Mcu.McuCounter
        let wp1 = getWaypointByName group "Waypoint1"
        let wp2 = getWaypointByName group "Waypoint2"
        let startPatrol = getWaypointByName group "PatrolStart"
        let returnWp = getWaypointByName group "WaypointRTB"
        let finalWp = getWaypointByName group "Final"
        let patrolDuration = getTriggerByName group "PatrolDuration" :?> Mcu.McuTimer
        let planeVehicle = getVehicleByName group "PATROL"
        let plane = getEntityByIndex planeVehicle.LinkTrId group

        let minMaxRange =
            let v = (Vector2.FromMcu areaMaxRange.Pos) - (Vector2.FromMcu startPatrol.Pos)
            v.Length() + 1500.0f

        // groups of related nodes
        let extractGroup = extractGroup group
        let areaGroup = extractGroup "PatrolArea" "MaxPursuit"
        let returnGroup = extractGroup "Return" "Land"
        let startGroup = extractGroup "FlightStart" "TakeOff"
        let planeGroup = extractGroup "Plane" "PATROL"

        // General relocation
        relocateGroup config.PatrolCenter (group, areaMaxRange)
        match config.StartType with
        | AirStart x ->
            relocateGroup config.StartPos planeGroup
            relocateGroup config.StartPos startGroup
            setAltitude x.Altitude (fst planeGroup)
        | GroundStart x ->
            relocateGroup config.StartPos planeGroup
            relocateGroup x.TakeOffPoint startGroup
        relocateGroup config.PatrolCenter areaGroup
        relocateGroup config.LandAt returnGroup

        // Altitude
        for (group, _) in [startGroup; returnGroup] do
            setAltitude config.CruiseAltitude group
        setAltitude config.CruiseAltitude [wp2]
        setAltitude config.PatrolAltitude (fst areaGroup)

        // Detailed relocation
        config.Return.AssignTo returnWp
        config.Final.AssignTo finalWp
        config.MidPoint.AssignTo wp2
        setAltitude 1000 [finalWp]
        setAltitude 1000 [wp1]

        // Set speeds
        for mcu in group do
            match mcu with
            | :? Mcu.McuWaypoint as wp -> wp.Speed <- config.CruiseSpeed
            | _ -> ()
        for mcu in fst areaGroup do
            match mcu with
            | :? Mcu.McuWaypoint as wp -> wp.Speed <- config.PatrolSpeed
            | _ -> ()

        patrolDuration.Time <- max 10.0 (float config.PatrolDuration)
        areaMaxRange.Distance <- max (int minMaxRange) config.MaxRange

        // Set plane wing
        allKilled.Count <- config.NumPlanes
        flightCannotPatrol.Count <- config.NumPlanes

        let cx = Mcu.addTargetLink
        let dead = getTriggerByName group "Dead"
        let unable = getTriggerByName group "Unable"
        cx dead allKilled.Index
        cx unable flightCannotPatrol.Index

        let wing =
            let newGroup() = cloneFresh store (fst planeGroup)
            [|
                let offset =
                    match config.StartType with
                    | AirStart _ -> Vector2(-50.0f, 50.0f).Rotate(config.StartPos.Direction)
                    | GroundStart _ -> Vector2(0.0f, 35.0f).Rotate(config.StartPos.Direction)
                for i in 2..config.NumPlanes do
                    let group = newGroup()
                    let vehicle = getVehicleByName group "PATROL"
                    let dead = getTriggerByName group "Dead"
                    let unable = getTriggerByName group "Unable"
                    let entity = getEntityByIndex vehicle.LinkTrId group
                    for mcu in group do
                        let pos = Vector2.FromMcu(mcu.Pos)
                        let newPos = pos + (float32 (i - 1)) * offset
                        newPos.AssignTo mcu.Pos
                    vehicle.Name <- sprintf "PATROL %d" i
                    vehicle.NumberInFormation.Value.Number <- i - 1
                    cx entity plane.Index
                    cx dead allKilled.Index
                    cx unable flightCannotPatrol.Index
                    gatherInNamedGroup store (sprintf "Wingman %d" (i - 1)) group
                    yield vehicle, group
            |]

        // Result
        {
            Start = start
            FlightCannotPatrol = flightCannotPatrol
            LeadPlane = ref planeVehicle
            WingPlanes = wing |> Array.map fst
            AllKilled = allKilled
            All =
                { new McuUtil.IMcuGroup with
                      member this.Content = group
                      member this.LcStrings = []
                      member this.SubGroups = [
                        for _, group in wing do
                            yield McuUtil.groupFromList group
                      ]
                }
        }

    interface IHasVehicles with
        member this.Vehicles =
            Seq.append [this.LeadPlane.Value] this.WingPlanes

        member this.ReplaceVehicleWith(oldVehicleIdx, newVehicle) =
            if this.LeadPlane.Value.Index = oldVehicleIdx then
                this.LeadPlane := newVehicle
            this.WingPlanes
            |> Array.iteri(fun idx plane ->
                if plane.Index = oldVehicleIdx then
                    this.WingPlanes.[idx] <- newVehicle)
