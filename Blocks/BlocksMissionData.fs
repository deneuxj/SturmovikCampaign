module SturmovikMission.Blocks.BlocksMissionData

open SturmovikMission.DataProvider
open System.Numerics
open System.IO
open VectorExtension

[<Literal>]
let private dataDir = __SOURCE_DIRECTORY__ + "/../data"
[<Literal>]
let private sampleFile = __SOURCE_DIRECTORY__ + "/Sample.Mission"
[<Literal>]
let private blocksFile = dataDir + "/Blocks/Blocks.Mission"
[<Literal>]
let private vehiclesFile = dataDir + "/Blocks/Vehicles.mission"

type T = SturmovikMissionTypes.Provider<sampleFile>

let blocksData =
    let blockMission =
        let path = Path.GetDirectoryName(System.Reflection.Assembly.GetCallingAssembly().Location)
        //let path = System.Environment.CurrentDirectory
        Path.Combine(path, "Blocks.Mission")
    T.GroupData.Parse(Parsing.Stream.FromFile blockMission)

// Utility functions to create MCU programmatically.
let newTimer idx =
    T.MCU_Timer.Default
        .SetIndex(T.Integer.N idx)
        .SetRandom(T.Integer.N 100)
        .CreateMcu() :?> Mcu.McuTimer

let newCounter idx =
    T.MCU_Counter.Default
        .SetCounter(T.Integer.N 1)
        .SetIndex(T.Integer.N idx)
        .CreateMcu() :?> Mcu.McuCounter

let newCheckZone idx distance =
    let mcu =
        T.MCU_CheckZone.Default
            .SetCloser(T.Boolean.N true)
            .SetCylinder(T.Boolean.N true)
            .SetIndex(T.Integer.N idx)
            .SetZone(T.Integer.N distance)
            .CreateMcu() :?> Mcu.McuProximity
    // McuProximity setters remove empty plane and vehicle coalitions fields erroneously created by T.MCU_CheckZone
    mcu.PlaneCoalitions <- []
    mcu.VehicleCoalitions <- []
    mcu

let newActivate idx =
    T.MCU_Activate.Default
        .SetIndex(T.Integer.N idx)
        .CreateMcu() :?> Mcu.McuTrigger

let newDeactivate idx =
    T.MCU_Deactivate.Default
        .SetIndex(T.Integer.N idx)
        .CreateMcu() :?> Mcu.McuTrigger

let newWaypoint idx (pos : Vector2) (yori : float32) (radius : int) (speed : int) (priority : int) =
    T.MCU_Waypoint.Default
        .SetArea(T.Integer.N radius)
        .SetIndex(T.Integer.N idx)
        .SetPriority(T.Integer.N priority)
        .SetSpeed(T.Integer.N speed)
        .SetXPos(T.Float.N (float pos.X))
        .SetYOri(T.Float.N (float yori))
        .SetYPos(T.Float.N(float pos.Y))
        .CreateMcu() :?> Mcu.McuWaypoint

let newEntity idx =
    T.MCU_TR_Entity.Default
        .SetIndex(T.Integer.N idx)
        .CreateMcu() :?> Mcu.McuEntity

let newMissionBegin idx =
    T.MCU_TR_MissionBegin.Default
        .SetIndex(T.Integer.N idx)
        .SetEnabled(T.Boolean.N true)
        .CreateMcu() :?> Mcu.McuTrigger

let newServerInput idx name =
    T.MCU_TR_ServerInput.Default
        .SetEnabled(T.Boolean.N true)
        .SetIndex(T.Integer.N idx)
        .SetName(T.String.N name)
        .CreateMcu() :?> Mcu.McuTrigger

let newBehaviour idx filter aiLevel country engageable param limitAmmo rearmFriendlies refuelFriendlies rehealFriendlies repairFriendlies vulnerable =
    T.MCU_CMD_Behaviour.Default
        .SetAILevel(T.Integer.N aiLevel)
        .SetCountry(T.Integer.N country)
        .SetEngageable(T.Boolean.N engageable)
        .SetFilter(T.Integer.N filter)
        .SetFloatParam(T.Float.N param)
        .SetIndex(T.Integer.N idx)
        .SetLimitAmmo(T.Boolean.N limitAmmo)
        .SetRearmFriendlies(T.Boolean.N rearmFriendlies)
        .SetRefuelFriendlies(T.Boolean.N refuelFriendlies)
        .SetRehealFriendlies(T.Boolean.N rehealFriendlies)
        .SetRepairFriendlies(T.Boolean.N repairFriendlies)
        .SetVulnerable(T.Boolean.N vulnerable)
        .CreateMcu() :?> Mcu.McuTrigger

let newAirfieldPlane(modFilter, payloadFilter, mods, payload, skinFilter, name, number) =
    T.Airfield.Planes.Plane.Default
        .SetAILevel(T.Integer.N 2)
        .SetAvMods(T.String.N modFilter)
        .SetAvPayloads(T.String.N payloadFilter)
        .SetAvSkins(T.String.N skinFilter)
        .SetEngageable(T.Boolean.N true)
        .SetFuel(T.Float.N 1.0)
        .SetLimitAmmo(T.Boolean.N true)
        .SetName(T.String.N name)
        .SetPayloadId(T.Integer.N payload)
        .SetRouteTime(T.Integer.N 60)
        .SetNumber(T.Integer.N number)
        .SetSpotter(T.Integer.N -1)
        .SetVulnerable(T.Boolean.N true)
        .SetWMMask(T.Integer.N mods)
        .SetRenewable(T.Boolean.N false)

let newAirfieldTank(name, model, script, number) =
    T.Airfield.Planes.Vehicle.Default
        .SetAILevel(T.Integer.N 2)
        .SetEngageable(T.Boolean.N true)
        .SetLimitAmmo(T.Boolean.N true)
        .SetModel(T.String.N model)
        .SetName(T.String.N name)
        .SetNumber(T.Integer.N number)
        .SetScript(T.String.N script)
        .SetSpotter(T.Integer.N -1)
        .SetVulnerable(T.Boolean.N true)

let newBlock idx country model script =
    T.Block.Default
        .SetCountry(T.Integer.N country)
        .SetDamageReport(T.Integer.N 50)
        .SetDamageThreshold(T.Boolean.N true)
        .SetDurability(T.Integer.N 7000)
        .SetIndex(T.Integer.N idx)
        .SetModel(T.String.N model)
        .SetScript(T.String.N script)

let newBlockMcu (store : NumericalIdentifiers.IdStore) country model script durability =
    let block = (newBlock 1 country model script).SetDurability(T.Integer.N durability)
    let subst = Mcu.substId <| store.GetIdMapper()
    let block = block.CreateMcu()
    subst block
    block

let newBlockWithEntityMcu (store : NumericalIdentifiers.IdStore) country model script durability =
    let block = (newBlock 1 country model script).SetDurability(T.Integer.N durability).SetLinkTrId(T.Integer.N 2)
    let entity = newEntity 2
    entity.MisObjID <- 1
    let subst = Mcu.substId <| store.GetIdMapper()
    let block = block.CreateMcu()
    subst block
    subst entity
    block, entity

let newObjective idx lcDesc lcName =
    T.MCU_TR_MissionObjective.Default
        .SetEnabled(T.Boolean.N true)
        .SetIndex(T.Integer.N idx)
        .SetLCDesc(T.Integer.N lcDesc)
        .SetLCName(T.Integer.N lcName)
        .SetSuccess(T.Boolean.N true)

let runwayOfAirfieldSpawn (airfield : T.Airfield) =
    let chart = airfield.TryGetChart()
    match chart with
    | None ->
        None
    | Some chart ->
        let yori = airfield.GetYOri().Value |> float32
        let points = chart.GetPoints()
        let pos, direction =
            points
            |> Seq.pairwise
            |> Seq.pick(fun (p1, p2) ->
                if p1.GetType().Value = 2 && p2.GetType().Value = 2 then
                    let mkVec(p : T.Airfield.Chart.Point) =
                        Vector2(float32 <| p.GetX().Value, float32 <| p.GetY().Value)
                    Some(mkVec(p1).Rotate(yori) + Vector2.FromPos(airfield), (mkVec(p2) - mkVec(p1)).Rotate(yori))
                else
                    None)
        Some(pos, direction.YOri)

let parkingOfAirfieldSpawn (airfield : T.Airfield) =
    let chart = airfield.TryGetChart()
    match chart with
    | None ->
        None
    | Some chart ->
        let yori = airfield.GetYOri().Value |> float32
        let points = chart.GetPoints()
        let pos, direction =
            points
            |> Seq.pairwise
            |> Seq.pick(fun (p1, p2) ->
                if p1.GetType().Value = 0 && p2.GetType().Value = 1 then
                    let mkVec(p : T.Airfield.Chart.Point) =
                        Vector2(float32 <| p.GetX().Value, float32 <| p.GetY().Value)
                    Some(mkVec(p1).Rotate(yori) + Vector2.FromPos(airfield), (mkVec(p2) - mkVec(p1)).Rotate(yori))
                else
                    None)
        Some(pos, direction.YOri)

module CommonMethods =
    let inline createMcu(x : ^T) =
        (^T : (member CreateMcu : unit -> Mcu.McuBase) x)

    let inline getDamaged(x : ^T) : ^R =
        (^T : (member GetDamaged : unit -> ^R) x)

    let inline setDamaged (damaged : ^D) (x : ^T) =
        (^T : (member SetDamaged : ^D -> ^T) x, damaged)

    let inline getDurability (x : ^T) : ^R =
        (^T : (member GetDurability : unit -> ^R) x)

    let inline setDurability (durability : ^D) (x : ^T) =
        (^T : (member SetDurability : ^D -> ^T) x, durability)

    let inline setIndex (index : ^D) (x : ^T) =
        (^T : (member SetIndex : ^D -> ^T) x, index)

    let inline getModel(x : ^T) : ^R =
        (^T : (member GetModel : unit -> ^R) x)

    let inline getScript(x : ^T) : ^R =
        (^T : (member GetScript : unit -> ^R) x)

    let inline getYOri(x : ^T) : ^R =
        (^T : (member GetYOri : unit -> ^R) x)

    let inline getAlt(x : ^T) : ^R =
        (^T : (member GetYPos : unit -> ^R) x)

    let inline valueOf(v : ^V) : ^R =
        (^V : (member Value : ^R) v)

    let inline setLinkTrId (index : ^D) (x : ^T) =
        (^T : (member SetLinkTrId : ^D -> ^T) x, index)
