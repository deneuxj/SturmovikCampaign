// CreateTankColumn, Copyright (C) 2015 Johann Deneux
// Licensed under the GNU Public License, see COPYING.txt

open System.IO

open SturmovikMission
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Mcu
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.DataProvider.NumericalIdentifiers

type T = SturmovikMissionTypes.Provider< @"..\data\Sample.Mission", @"..\data\TankPlatoon.Group" >


/// Create a vector from any value that has fields XPos, YPos and ZPos.
let inline getPos< ^T when
                    ^T : (member XPos : T.Float)
                    and ^T : (member YPos : T.Float)
                    and ^T : (member ZPos : T.Float) > (v : ^T) =
    let x = (^T : (member XPos : T.Float) v)
    let y = (^T : (member YPos : T.Float) v)
    let z = (^T : (member ZPos : T.Float) v)
    newVec3(x.Value, y.Value, z.Value)

/// Interface of objects that allow following a graph at run-time.
type IAccessors =
    abstract GetEntity : HasEntity -> McuEntity
    abstract GetOwner : McuEntity -> HasEntity
    abstract GetObjects : McuCommand -> McuEntity list
    abstract GetTargets : McuCommand -> McuCommand list

/// Create an IAccessors over a given list of MUCs.
/// The provided list must be self-contained. If it contains an MCU that points
/// forward to another MCU not in the list, the relevant Get method will raise
/// an exception (KeyNotFound).
let mkAccessors (mcus : McuBase list) =
    let getMcuById =
        mcus
        |> List.map (fun mcu -> mcu.Index, mcu)
        |> dict
    
    let getEntity (v : HasEntity) =
        let entityId = v.LinkTrId
        getMcuById.[entityId] :?> McuEntity

    let getOwner (v : McuEntity) =
        getMcuById.[v.MisObjID] :?> HasEntity

    let getObjects (v : McuCommand) =
        v.Objects
        |> List.map (fun o -> getMcuById.[o] :?> McuEntity)

    let getTargets (v : McuCommand) =
        v.Targets
        |> List.map (fun t -> getMcuById.[t] :?> McuCommand)

    { new IAccessors with
        member __.GetEntity(v) = getEntity v
        member __.GetOwner(v) = getOwner v
        member __.GetObjects(v) = getObjects v
        member __.GetTargets(v) = getTargets v
    }


let conv (xs : #McuBase seq) : McuBase seq =
    xs
    |> Seq.map (fun x -> upcast x)


let random = new System.Random(1)

/// A group of tanks advancing towards an objective.
/// A group is called a platoon, a linearly ordered groups of platoons is a column.
type Platoon =
    {
      // Input to the group: Create and start this platoon.
      Init : McuCommand
      // Input to the group: Deactivate this platoon.
      Shutdown : McuCommand
      /// Output from the group: Create and start the next platoon in the column.
      InitNext : McuCommand
      /// Input to the group: Disable the logic that creates the next platoon.
      DisableInitNext : McuCommand
      /// Output from the group: Platoon reached the objective waypoint.
      Success : McuCommand
      /// Output from the group: Platoon was destroyed.
      Failure : McuCommand
      /// Input : Stop because of traffic.
      TrafficStop : McuCommand
      /// Input : Resume movement after traffic ahead moves up.
      TrafficResume : McuCommand
      /// The entity of the leader.
      Leader : McuEntity
      /// Output : The waypoints. Used to add logic to stop units when they come to a damaged bridge.
      Waypoints : McuCommand list
      /// All objects and logic in the platoon.
      All : McuBase list
      /// Localized strings
      LcStrings : (int * string) list
    }
with
    interface IMcuGroup with
        member this.Content = this.All
        member this.SubGroups = []
        member this.LcStrings = this.LcStrings

    /// <summary>
    /// Create a group of vehicles (typically tanks) that travels to a combat zone and then advances towards an objective.
    /// </summary>
    /// <param name="handle">Entity of the vehicle that specifies the location where the platoon is created.</param>
    /// <param name="context">
    /// List of nodes that contains waypoints specifying the travel of the platoon.
    /// The waypoints must be linearly linked, and have handle as their object. The first waypoint is the waypoint that is not the target of any command.
    /// The waypoints must contain a waypoint whose name starts with "CombatZone". This is where the platoon leaves the road, and attacks enemy units.
    /// Following "CombatZone" should be a waypoint named "Spread". This waypoint is moved laterally by a random amount up to 500m. This is to have the column in a moving front formation, as opposed to single-filed.
    /// </param>
    /// <param name="rank">The rank of this platoon in the unit: 0 for the first one, 1 for the next...
    /// This is used to set the delay at the spread point before continuing, so that the unit advances simultaneously.
    /// </param>
    static member Create(subst : McuBase seq -> int -> int, unitSize : int, handle : HasEntity, context : McuBase list, rank : int) =
        // Util
        let mcus = T.TankPlatoon.CreateMcuList()
        let acc = mkAccessors mcus
        // Points of reference: leader in the template, corresponding point of instantiation (handle)
        let leader =
            let model = getHasEntityByIndex T.TankPlatoon.``Pzr leader``.Index.Value mcus
            acc.GetEntity model
        let center = newVec3(leader.Pos.X, leader.Pos.Y, leader.Pos.Z)
        let angle = handle.Ori.Y - (acc.GetOwner leader).Ori.Y
        // Content of the group
        let initNext = getCommandByIndex T.TankPlatoon.DelayNext.Index.Value mcus
        let allKilled = getCommandByIndex T.TankPlatoon.AllKilled.Index.Value mcus
        let debugAllKilled = getCommandByIndex T.TankPlatoon.DebugAllKilled.Index.Value mcus
        let content = getGroupMulti (fun _ -> false) [ leader; initNext; allKilled; debugAllKilled ] mcus Seq.empty |> List.ofSeq
        let init = getCommandByIndex T.TankPlatoon.Init.Index.Value content
        let disableInitNext = getCommandByIndex T.TankPlatoon.DeactivateNext.Index.Value content
        let combatZoneReached = getCommandByIndex T.TankPlatoon.CombatZoneReached.Index.Value content
        let startMoving = getCommandByIndex T.TankPlatoon.StartMoving.Index.Value content
        let shutdown = getCommandByIndex T.TankPlatoon.Shutdown.Index.Value content
        let flareVictory = getCommandByIndex T.TankPlatoon.FlareVictory.Index.Value content
        let startDefense = getCommandByIndex T.TankPlatoon.StartDefense.Index.Value content
        let trafficResume = getCommandByIndex T.TankPlatoon.TrafficResume.Index.Value content
        let trafficStop = getCommandByIndex T.TankPlatoon.TrafficStop.Index.Value content
        let formOnRoad = getCommandByIndex T.TankPlatoon.FormOnRoad.Index.Value content
        // Change vehicle types and country if the handle is a Russian vehicle
        let russia = 101
        if handle.Country = russia then
            let tanks =
                [ T.TankPlatoon.``Pzr leader``; T.TankPlatoon.``Pzr Right`` ]
                |> List.map (fun veh -> veh.Index.Value)
                |> List.map (fun idx -> getHasEntityByIndex idx content)
            for tank in tanks do
                tank.Model <- @"graphics\vehicles\t34-76stz\t34-76stz.mgm"
                tank.Script <- @"LuaScripts\WorldObjects\vehicles\t34-76stz.txt"
                tank.Country <- russia
            let aa = getHasEntityByIndex T.TankPlatoon.``AA Escort``.Index.Value content
            aa.Model <- @"graphics\vehicles\gaz\gaz-aa-m4-aa.mgm"
            aa.Script <- @"LuaScripts\WorldObjects\vehicles\gaz-aa-m4-aa.txt"
            aa.Country <- russia
            match getCommandByIndex T.TankPlatoon.EnemyProximity.Index.Value content with
            | :? McuProximity as proxi ->
                let axis = 2
                proxi.PlaneCoalitions <- [axis]
            | _ ->
                failwith "Command EnemyProximity is not a proximity trigger"

        // Fresh new ids.
        let getLcId = subst content
        // Translate and rotate content of platoon
        let transform (m : McuBase) =
            let diff = vecMinus handle.Pos center
            let pos2 =
                m.Pos
                |> rotate center angle
                |> translate diff
            vecCopy pos2 m.Pos
            m.Ori.Y <- angle
        for m in content do
            transform m
        // Extract the waypoints from the instantiation context
        let waypoints =
            let group = getGroupMulti (fun _ -> false) [handle] context Seq.empty
            let wps =
                group
                |> Seq.choose (function :? McuEntity -> None | :? McuCommand as wp -> Some wp | _ -> None) // Only retain the commands. We just assume they are all waypoints.
                |> List.ofSeq
            // Find the last waypoint
            let last =
                wps
                |> List.find (fun mcu -> List.isEmpty mcu.Targets)
            // From there order the waypoints linearly
            let rec sort (target : McuCommand) (wps : McuCommand list) =
                match wps |> List.tryFind (fun mcu -> mcu.Targets = [target.Index]) with
                | Some next ->
                    let rest =
                        wps
                        |> List.filter (fun mcu -> mcu.Index <> next.Index)
                    target :: sort next rest
                | None ->
                    [target]
            sort last wps
            |> List.rev
        // Clear object links of waypoints, as they refer to the entity from the context, not from the library.
        for wp in waypoints do
            wp.Objects <- []
        // Substitute ids before setting any links to MCUs from the library.
        conv waypoints |> subst |> ignore
        let travel = List.head waypoints
        let spread =
            waypoints
            |> List.filter (fun wp -> wp.Name.ToLowerInvariant().StartsWith "spread")
        let combatZone =
            waypoints
            |> List.filter (fun wp -> wp.Name.ToLowerInvariant().StartsWith "combatzone")
        let backOnRoad =
            waypoints
            |> List.filter (fun wp -> wp.Name.ToLowerInvariant().StartsWith "onroad")
        let arrived = Seq.last waypoints
        // Set object links of waypoints
        for wp in waypoints do
            wp.Objects <- [leader.Index]
        // Connect waypoints to group logic
        addTargetLink startMoving travel.Index
        for cz in combatZone do
            addTargetLink cz combatZoneReached.Index
        for bor in backOnRoad do
            addTargetLink bor formOnRoad.Index
        addTargetLink arrived flareVictory.Index
        let delayCmds : McuBase list =
            [
                for spread in spread do
                    // Move spread laterally by a random amount.
                    let right =
                        newVec3(0.0, 0.0, 1.0)
                        |> rotate (newVec3(0.0, 0.0, 0.0)) spread.Ori.Y
                    let shift = (random.NextDouble() - 0.5) * 800.0
                    right.X <- shift * right.X
                    right.Z <- shift * right.Z
                    let newPos = translate spread.Pos right
                    vecCopy newPos spread.Pos
                    // Insert delay at spread point
                    let delay = T.TankPlatoon.DelayNext.Time.Value * (float (unitSize - 1 - rank))
                    let delayCmd = T.TankPlatoon.ATimer.SetTime(T.Float delay).CreateMcuCommand()
                    delayCmd.Name <- "Sync delay"
                    delayCmd.Pos.X <- spread.Pos.X - 10.0
                    delayCmd.Pos.Y <- spread.Pos.Y
                    delayCmd.Pos.Z <- spread.Pos.Z
                    subst [delayCmd] |> ignore
                    // "spread -> next wp" becomes: "spread -> delay -> next wp"
                    let nexts = spread.Targets
                    spread.Targets <- [delayCmd.Index]
                    for next in nexts do
                        addTargetLink delayCmd next
                    yield upcast delayCmd
                    // Spread all subsequent waypoints until the next "OnRoad" waypoint
                    let rec findSpread waypoints =
                        match waypoints with
                        | wp :: rest when wp = spread -> work rest
                        | _ :: rest -> findSpread rest
                        | [] -> ()
                    and work waypoints =
                        match waypoints with
                        | onroad :: rest when onroad.Name.ToLowerInvariant().StartsWith("onroad") ->
                            ()
                        | wp :: rest ->
                            let factor =
                                let name = wp.Name.ToLowerInvariant()
                                if name.StartsWith("destination") || name.StartsWith("tighten") then
                                    // Tighter spread when going for the destination.
                                    0.2
                                else
                                    1.0
                            let right =
                                newVec3(0.0, 0.0, 1.0)
                                |> rotate (newVec3(0.0, 0.0, 0.0)) wp.Ori.Y
                            right.X <- factor * shift * right.X
                            right.Z <- factor * shift * right.Z
                            let newPos = translate wp.Pos right
                            vecCopy newPos wp.Pos
                            work rest
                        | [] ->
                            ()
                    findSpread waypoints
            ]
        let onRoadDelays : McuBase list =
            [
                for onRoad in backOnRoad do
                    // Insert delay (so that the next spread delay doesn't move later ranks ahead of leaders)
                    let delay = T.TankPlatoon.DelayNext.Time.Value * (float rank)
                    let delayCmd = T.TankPlatoon.ATimer.SetTime(T.Float delay).CreateMcuCommand()
                    delayCmd.Name <- "Unsync delay"
                    delayCmd.Pos.X <- onRoad.Pos.X - 10.0
                    delayCmd.Pos.Y <- onRoad.Pos.Y
                    delayCmd.Pos.Z <- onRoad.Pos.Z
                    subst [delayCmd] |> ignore
                    // "onRoad -> next wp" becomes: "onRoad -> delay -> next wp"
                    let nexts = onRoad.Targets
                    onRoad.Targets <- [delayCmd.Index]
                    addTargetLink delayCmd formOnRoad.Index
                    for next in nexts do
                        addTargetLink delayCmd next
                    yield upcast delayCmd
            ]
        // Result
        let all : McuBase list =
            let content =
                content
                |> Seq.sortBy (fun mcu -> mcu.Index)
                |> List.ofSeq
            let waypoints =
                waypoints
                |> Seq.cast<McuBase>
                |> List.ofSeq
            delayCmds @ onRoadDelays @ content @ waypoints
        { All = all
          Leader = leader
          Init = init
          InitNext = initNext
          DisableInitNext = disableInitNext
          Shutdown = shutdown
          Success = arrived
          Failure = allKilled
          TrafficStop = trafficStop
          TrafficResume = trafficResume
          Waypoints = waypoints
          LcStrings = Localization.transfer false getLcId (Path.Combine(T.ResolutionFolder, @"..\data\TankPlatoon.eng"))
        }


type Column =
    { Platoons : Platoon list
      /// Input: Init the first platoon.
      Init : McuCommand
      /// Input: Shut down all platoons.
      Shutdown : McuCommand
      /// Input: Enable reinforcements platoons.
      Reinforce : McuCommand
      /// Output: Success of some platoon.
      Success : McuCommand
      /// Output: Failure of all platoons.
      Failure : McuCommand
      /// Internal: Logic connecting platoons and column API.
      AllLogic : McuBase list
    }
with
    interface IMcuGroup with
        member this.Content = this.AllLogic
        member this.LcStrings = []
        member this.SubGroups =
            this.Platoons
            |> List.map (fun x -> upcast x)

    // reinforcements is a list of number of platoons whose spawning is conditioned by the arrival of reinforcements.
    // Those platoons are included in numCopies, and they are spawned last.
    // The first position in the list is the number of platoons after one reinforcement, the second position is for the second reinforcement, and so on.
    static member Create(subst, unitSize: int, numCopies : int, reinforcements : int list, handle : T.Vehicle, getContext : unit -> McuBase list) =
        let platoons =
            Array.init numCopies (fun n ->
                let context = getContext()
                let handle = getHasEntityByIndex handle.Index.Value context
                Platoon.Create(subst, unitSize, handle, context, n % unitSize)
            )
        // Chain the platoons into a column.
        let pairs =
            platoons
            |> Seq.pairwise
            |> List.ofSeq
            |> List.rev
        let reinforceOrder =
            T.TankPlatoon.ATimer
                .SetName(T.String "Reinforce")
                .CreateMcuCommand()
        subst [reinforceOrder] |> ignore
        if numCopies > 0 then
            platoons.[0].InitNext.Pos
            |> translate (newVec3(3.0, 0.0, 60.0))
            |> fun src -> vecCopy src reinforceOrder.Pos
        let rec work (i : int) (numReinforcements : int) (reinforcements : int list) (pairs : (Platoon * Platoon) list) : (McuCommand * McuBase list) list =
            match reinforcements, pairs with
            | _, [] ->
                // No platoons left to handle, return the empty list.
                List.empty
            | reinforcementSize :: reinforcements, _ when i >= reinforcementSize ->
                // Previous level of reinforcements (we work through the list backwards)
                work 0 (numReinforcements - 1) reinforcements pairs
            | reinforcementSize :: _, (leader, follower) :: pairs ->
                // The static node detecting the arrival of supplies/reinforcements goes into that node.
                let reinforcementCounter =
                    T.TankPlatoon.ACounter
                        .SetCounter(T.Integer numReinforcements)
                        .SetName(T.String "Reinforcements arrived")
                        .CreateMcuCommand()
                // Reinforcements have arrived in the required quantity and it's time to spawn.
                let condition =
                    T.TankPlatoon.ACounter
                        .SetCounter(T.Integer 2)
                        .SetName(T.String "AND")
                        .CreateMcuCommand()
                // It's time to spawn but reinforcements have not arrived: Do not spawn, and report self as killed .
                let suicide = 
                    T.TankPlatoon.ATimer
                        .SetTime(T.Float 1.0)
                        .SetName(T.String "Kill self unless reinforced")
                        .CreateMcuCommand()
                // Reinforcements have arrived in sufficient quantity, do not report self as killed when time to spawn comes.
                let disableSuicide =
                    T.TankPlatoon.ADeactivate
                        .SetName(T.String "No suicide reinforced")
                        .CreateMcuCommand()
                [reinforcementCounter; condition; suicide; disableSuicide]
                |> List.iter (fun x -> subst [x] |> ignore)
                // Logic links
                addTargetLink reinforceOrder reinforcementCounter.Index
                addTargetLink reinforcementCounter condition.Index
                addTargetLink leader.InitNext condition.Index
                addTargetLink leader.InitNext suicide.Index
                addTargetLink condition follower.Init.Index
                addTargetLink suicide follower.Failure.Index
                addTargetLink disableSuicide suicide.Index
                addTargetLink reinforcementCounter disableSuicide.Index
                // Move nodes appart
                let moveTo (cmd : McuCommand) (offX, offZ) =
                    leader.Init.Pos
                    |> translate (newVec3(offX, 0.0, offZ))
                    |> fun src -> vecCopy src cmd.Pos
                moveTo reinforcementCounter (0.0, 40.0)
                moveTo condition (20.0, 40.0)
                moveTo suicide (40.0, 40.0)
                moveTo disableSuicide (60.0, 40.0)
                (suicide, [ reinforcementCounter; condition; disableSuicide ]) :: (work (i + 1) numReinforcements reinforcements pairs)
            | [], (leader, follower) :: pairs ->
                // Not conditioned by reinforcements: simply link "spawn next" from leader to "spawn" of follower.
                addTargetLink leader.InitNext follower.Init.Index
                work i numReinforcements [] pairs
        let reinforcementLogic = work 0 (List.length reinforcements) (List.rev reinforcements) pairs
        // Chain suicide nodes, so that the first non-disabled suicide causes the rest to suicide.
        for (suicide1, suicide2) in reinforcementLogic |> List.map fst |> List.rev |> Seq.pairwise do
            addTargetLink suicide1 suicide2.Index
        // Make the list of pairs into a flat list of nodes.
        let reinforcementLogic =
            reinforcementLogic
            |> List.map (fun (cmd, rest) -> (cmd :> McuBase) :: rest)
            |> List.concat
        // Start the column by starting the first platoon.
        let init = platoons.[0].Init
        // Shut down the column by shutting down all platoons and deactivating init of next platoons.
        let shutdown =
            T.TankPlatoon.ATimer.SetName(T.String "ShutdownAll").CreateMcuCommand()
        subst [shutdown] |> ignore
        for p in platoons do
            addTargetLink shutdown p.Shutdown.Index
            addTargetLink shutdown p.DisableInitNext.Index
        // Success of the column: at least one platoon succeeded.
        let someSuccess =
            T.TankPlatoon.ACounter.SetName(T.String "SomeSuccess").SetCounter(T.Integer 1).CreateMcuCommand()
        subst [someSuccess] |> ignore
        for p in platoons do
            addTargetLink p.Success someSuccess.Index
        // Failure of the column: all platoons failed.
        let allFailure =
            T.TankPlatoon.ACounter.SetName(T.String "AllFailure").SetCounter(T.Integer numCopies).CreateMcuCommand()
        subst [allFailure] |> ignore
        for p in platoons do
            addTargetLink p.Failure allFailure.Index
        // Traffic management: Stop platoon n if a platoon m with m < n is too close.
        let traffic =
            [
                for m in 0..numCopies - 1 do
                    for n in m + 1 .. numCopies - 1 do
                    let firstPlatoon = platoons.[m]
                    let secondPlatoon = platoons.[n]
                    let traffic, vehicle1, vehicle2, diff =
                        let context = T.TankPlatoon.CreateMcuList()
                        let veh1 = getEntityByIndex T.TankPlatoon.Vehicle1.LinkTrId.Value context
                        let veh2 = getEntityByIndex T.TankPlatoon.Vehicle2.LinkTrId.Value context
                        let traffic = getGroupMulti (fun _ -> false) [ veh1; veh2 ] context Seq.empty
                        // Remove place-holder vehicles, they will be replaced by the platoon leaders.
                        traffic.Remove(veh1) |> ignore
                        traffic.Remove(veh2) |> ignore
                        traffic.RemoveWhere(fun mcu -> mcu.Index = veh1.MisObjID || mcu.Index = veh2.MisObjID) |> ignore
                        List.ofSeq traffic, veh1, veh2, vecMinus firstPlatoon.Leader.Pos veh1.Pos
                    // Move the logic close to the thing it's attached to
                    for mcu in traffic do
                        let pos2 = translate mcu.Pos diff
                        pos2.Z <- pos2.Z + 100.0
                        vecCopy pos2 mcu.Pos
                    // Grab interesting MCUs
                    let tooClose = getCommandByIndex T.TankPlatoon.TooClose.Index.Value traffic
                    let farEnough = getCommandByIndex T.TankPlatoon.FarEnough.Index.Value traffic
                    let turnOff = getCommandByIndex T.TankPlatoon.ProximityShutdown.Index.Value traffic
                    // Fresh ids
                    subst traffic |> ignore
                    // Replace place-holder vehicles by platoon leaders.
                    tooClose.Objects <- [ firstPlatoon.Leader.Index ; secondPlatoon.Leader.Index ]
                    farEnough.Objects <- [ firstPlatoon.Leader.Index ; secondPlatoon.Leader.Index ]
                    // Start the thing when the second platoon is created.
                    addTargetLink secondPlatoon.Init tooClose.Index
                    // Connect traffic logic to movement orders of the second platoon.
                    addTargetLink tooClose secondPlatoon.TrafficStop.Index
                    addTargetLink farEnough secondPlatoon.TrafficResume.Index
                    // Make sure the second platoon does not get stuck if the first platoon dies or is shutdown.
                    addTargetLink firstPlatoon.Failure secondPlatoon.TrafficResume.Index
                    addTargetLink firstPlatoon.Shutdown secondPlatoon.TrafficResume.Index
                    // Shutdown proximity logic when phase ends, or when one of the platoon dies
                    addTargetLink firstPlatoon.Failure turnOff.Index
                    addTargetLink secondPlatoon.Failure turnOff.Index
                    addTargetLink shutdown turnOff.Index
                    // Result
                    yield! traffic
            ]
        // Move the API nodes somewhere near the handle, but not all on top of eachother.
        let logicCenter = newVec3(handle.XPos.Value + 250.0, handle.YPos.Value, handle.ZPos.Value + 200.0)
        for idx, v in Seq.zip (Seq.initInfinite id) [ init.Pos; shutdown.Pos; someSuccess.Pos; allFailure.Pos ] do
            vecCopy logicCenter v
            v.Z <- v.Z + (float idx) * 50.0
        // Result
        { Platoons = List.ofArray platoons
          Init = init
          Shutdown = shutdown
          Success = someSuccess
          Failure = allFailure
          Reinforce = reinforceOrder
          AllLogic =            
            traffic @ reinforcementLogic @
            [ shutdown
              someSuccess
              allFailure
              reinforceOrder
            ] // Note: init is not included because it is already part of the first platoon.
        }


type CliOptions =
    { unitSize : int
      numPlatoons : int
      specGroup : string option
      outputFilename : string
    }
with
    static member Default =
        { unitSize = 2
          numPlatoons = 6
          specGroup = None
          outputFilename = "GeneratedTankColumn"
        }
    
    static member Parse(argv) =
        let usage = """
CreateTankColumn [-u <unit size>] [-n <number of platoons>]
                 -f <path to specification group> -o <output file name>
    <unit size> must be larger than 0 (default : 2)
    <number of platoons> must be larger than 0 (default : 6)
    <path to specification group> is the path to a .Group file that specifies
                                  the country of the platoons and their
                                  waypoints
    <output file name> is the name of the .Group file to be generated.
                """

        let rec work args res =
            match args with
            | "-u" :: n :: rest ->
                match System.Int32.TryParse(n) with
                | true, n ->
                    if n > 0 then
                        work rest { res with unitSize = n }
                    else
                        eprintfn "Unit size must be larger than 0 (%d)" n
                        None
                | false, _ ->
                    eprintfn "Unit size must be a number (%s)" n
                    None
            | "-n" :: n :: rest ->
                match System.Int32.TryParse(n) with
                | true, n ->
                    if n > 0 then
                        work rest { res with numPlatoons = n }
                    else
                        eprintfn "Number of platoons must be larger than 0 (%d)" n
                        None
                | false, _ ->
                    eprintfn "Number of platoons must be a number (%s)" n
                    None
            | "-f" :: path :: rest ->
                if System.IO.File.Exists(path) then
                    work rest { res with specGroup = Some path }
                else
                    eprintfn "Cannot read file '%s'" path
                    None
            | "-o" :: path :: rest ->
                let path =
                    if path.ToLowerInvariant().EndsWith(".group") then
                        path.Substring(0, path.Length - ".group".Length)
                    else
                        path
                work rest { res with outputFilename = path }
            | "-v" :: rest
            | "--version" :: rest ->
                printfn "CreateTankColumn %s" AssemblyInfo.version
                work rest res
            | [] ->
                Some res
            | "-h" :: _
            | "--help" :: _
            | "-help" :: _
            | "/?" :: _
            | _ ->
                eprintfn "Usage:%s" usage
                None
        work (List.ofArray argv) CliOptions.Default


[<EntryPoint>]
let main argv = 
    let opts = CliOptions.Parse(argv)

    // The stores that are responsible for providing collision-free new identifiers.
    /// MCU Index store.
    let idStore = new IdStore()
    /// Localized string index store.
    let lcIdStore = new IdStore()
    lcIdStore.SetNextId(3) // 0, 1, 2 reserved for mission title, briefing and author
    /// Convenience function that creates id allocators and assigns fresh ids to a given sequence of MCUs.
    let subst (mcus : #McuBase seq) =
        let getNewId = idStore.GetIdMapper()
        let getNewLcId = lcIdStore.GetIdMapper()
        for mcu in mcus do
            substId getNewId mcu
            substLCId getNewLcId mcu
        getNewLcId

    match opts with
    | None ->
        1
    | Some { specGroup = None } ->
        eprintfn "No specification group file specified"
        1
    | Some ({ specGroup = Some inPath } as opts) ->
        try
            let s = Parsing.Stream.FromFile(inPath)
            let data = T.GroupData(s)
            let columns = 
                [
                    for vehicle in data.ListOfVehicle do
                        yield Column.Create(subst, opts.unitSize, opts.numPlatoons, [], vehicle, data.CreateMcuList)
                ]
            try
                use file = System.IO.File.CreateText(opts.outputFilename + ".Group")
                file.Write(
                    columns
                    |> List.map McuUtil.asString
                    |> String.concat "\n"
                )
                0
            with
            | e ->
                eprintfn "Failed to write result to file '%s' because '%s'" opts.outputFilename e.Message
                1
        with
        | e ->
            eprintfn "Failed to create columns because '%s'" e.Message
            1
