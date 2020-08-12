/// Parsing of events from the game logs
module Campaign.GameLogEvents

open Util.RegexActivePatterns
open System.Text.RegularExpressions

type Position = (float32 * float32 * float32)

type MissionEvent =
    | MissionStarts of {| GameDate : System.DateTime; MissionFile : string |}
    | MissionEnds

type Binding = {
    Id : int
    Typ : string
    Sub : int option
    Country : int
    Name : string
    Parent : int
}

type ObjectEvent =
    | ObjectBound of Binding
    | ObjectHit of {| AttackerId : int; TargetId : int; Ammo : string |}
    | ObjectDamaged of {| AttackerId : int; TargetId : int; Position : Position; Damage : float32 |}
    | ObjectKilled of {| AttackerId : int; TargetId : int; Position : Position |}
    | ObjectTakesOff of {| Id : int; Position : Position |}
    | ObjectLands of {| Id : int; Position : Position |}

type ObjectTaken = {
    VehicleId : int
    PilotId : int
    Bullets : int
    Shells : int
    Bombs : int
    Rockets : int
    UserNickId : string
    UserId : string
    Name : string
    Typ : string
    Country : int
    Position : Position
    Payload : int
    Fuel : float32
    Skin : string
    WeaponMods : uint64
}

type UserIds = {
    UserId : string
    UserNickId : string
}

type PlayerEvent =
    | PlayerJoins of UserIds
    | PlayerEndsMission of {| VehicleId : int; PilotId : int; Bullets : int; Shells : int; Bombs : int; Rockets : int; Position : Position |}
    | PlayerTakesObject of ObjectTaken
    | PlayerLeaves of UserIds

let (|AsDate|_|) (s : string) =
    match s.Split '.' with
    | [| AsInt year; AsInt month; AsInt day |] ->
        Some (System.DateTime(year, month, day))
    | _ ->
        None

let (|AsTime|_|) (s : string) =
    match s.Split ':' with
    | [| AsInt hour; AsInt minute; AsInt second |] ->
        Some (hour, minute, second)
    | _ ->
        None

let (|AsMask|_|) (s : string) =
    let rec work res s =
        match s with
        | [] -> res
        | '0' :: s-> work (res <<< 1) s
        | '1' :: s -> work ((res <<< 1) ||| 1UL) s
        | c -> failwithf "Must be 0 or 1, was %s" (string c)
    try
        List.ofSeq (s.Trim())
        |> work 0UL
        |> Some
    with _ -> None

let (|AsPos|_|) (s : string) =
    match s.Split ',' with
    | [| AsFloat x; AsFloat y; AsFloat z |] -> Some (x, y, z)
    | _ -> None

let reBase = Regex(@"^T:(\b\d+) AType:(\d+) (.*)")
let reMissionStart = Regex(@"GDate:([\d\.]+) GTime:([\d:]+) MFile:(.+) MID:(\w*) GType:(\d+) CNTRS:([\d:,]+) SETTS:(\d+) MODS:(\d) PRESET:(\d) AQMID:(\d+)")
let reTake = Regex(@"PLID:([\d\-]+) PID:([\d\-]+) BUL:([\d\-]+) SH:([\d\-]+) BOMB:([\d\-]+) RCT:([\d\-]+) \((.*)\) IDS:([\d\-a-fA-f]{36}) LOGIN:([\d\-a-fA-f]{36}) NAME:(.+) TYPE:(.+) COUNTRY:(\d+) FORM:(\d+) FIELD:([\d\-]+) INAIR:(\d+) PARENT:([\d\-]+) ISPL:[\d]+ ISTSTART:[\d]+ PAYLOAD:(\d+) FUEL:([\d\.#QO\-]+) SKIN:(.*) WM:(\d+)")
let reBind = Regex(@"ID:(?<id>[\d\-]+) TYPE:(?<type>[^[]+)(\[.+,(?<sub>.+)\])? COUNTRY:(?<country>\d+) NAME:(?<name>.+) PID:(?<pid>[\d\-]+)")
let reJoins = Regex(@"USERID:([\d\-a-fA-F]{36}) USERNICKID:([\d\-a-fA-f]{36})")
let reLeaves = reJoins
let reTakeOff = Regex(@"PID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)")
let reLand = reTakeOff
let reHit = Regex(@"AMMO:(.+) AID:([\d\-]+) TID:([\d\-]+)")
let reDamage = Regex(@"DMG:([\d\.\-]+) AID:([\d\-]+) TID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)")
let reKill = Regex(@"AID:([\d\-]+) TID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)")
let reEndFlight = Regex(@"PLID:([\d\-]+) PID:([\d\-]+) BUL:([\d\-]+) SH:([\d\-]+) BOMB:([\d\-]+) RCT:([\d\-]+) \(([\d,\.\s\-#QO]+)\)")

type System.TimeSpan with
    static member OfGameTicks(ticks : int) =
        let gameTicksPerSecond = 50L
        let netTicksPerSecond = 10000000L
        System.TimeSpan(int64 ticks * netTicksPerSecond / gameTicksPerSecond)

let (|MissionEvent|ObjectEvent|PlayerEvent|OtherEvent|InvalidLine|) (line : string) =
    match line with
    | MatchesRegex reBase (GroupList [AsInt ticks; AsInt eventType; eventData]) ->
        let timeStamp = System.TimeSpan.OfGameTicks(ticks)
        match eventType with
        | 0 ->
            match eventData with
            | MatchesRegex reMissionStart (GroupList (AsDate date :: AsTime(h, m, s) :: mFile :: _)) ->
                let date = date.AddHours(float h).AddMinutes(float m).AddSeconds(float s)
                MissionEvent(timeStamp, MissionStarts {| GameDate = date; MissionFile = mFile |})
            | _ ->
                InvalidLine
        | 1 ->
            match eventData with
            | MatchesRegex reHit (GroupList [ammo; AsInt attackerId; AsInt targetId]) ->
                ObjectEvent(timeStamp, ObjectHit {| AttackerId = attackerId; TargetId = targetId; Ammo = ammo |})
            | _ ->
                 InvalidLine
        | 2 ->
            match eventData with
            | MatchesRegex reDamage (GroupList [AsFloat dmg; AsInt attackerId; AsInt targetId; AsPos position]) ->
                ObjectEvent(timeStamp, ObjectDamaged {| AttackerId = attackerId; TargetId = targetId; Position = position; Damage = dmg |})
            | _ ->
                InvalidLine
        | 3 ->
            match eventData with
            | MatchesRegex reKill (GroupList [AsInt attackerId; AsInt targetId; AsPos position ]) ->
                ObjectEvent(timeStamp, ObjectKilled {| AttackerId = attackerId; TargetId = targetId; Position = position |})
            | _ ->
                InvalidLine
        | 4 ->
            match eventData with
            | MatchesRegex reEndFlight (GroupList [AsInt vehId; AsInt pilotId; AsInt bullets; AsInt shells; AsInt bombs; AsInt rockets; AsPos position]) ->
                PlayerEvent(timeStamp, PlayerEndsMission {| VehicleId = vehId; PilotId = pilotId; Bullets = bullets; Shells = shells; Bombs = bombs; Rockets = rockets; Position = position |})
            | _ ->
                InvalidLine
        | 5 ->
            match eventData with
            | MatchesRegex reTakeOff (GroupList [AsInt id; AsPos pos]) ->
                ObjectEvent(timeStamp, ObjectTakesOff {| Id = id; Position = pos |})
            | _ ->
                InvalidLine
        | 6 ->
            match eventData with
            | MatchesRegex reLand (GroupList [AsInt id; AsPos pos]) ->
                ObjectEvent(timeStamp, ObjectLands {| Id = id; Position = pos |})
            | _ ->
                InvalidLine
        | 7 ->
            MissionEvent(timeStamp, MissionEnds)
        | 10 ->
            match eventData with
            | MatchesRegex reTake (GroupList [AsInt vehId; AsInt pilotId; AsInt bullets; AsInt shells; AsInt bombs; AsInt rockets; AsPos position; userNickId; userId; name; typ; AsInt country; _; _; AsInt spawnType; AsInt parent; AsInt payload; AsFloat fuel; skin; AsInt modMask]) ->
                let taken = {
                    VehicleId = vehId
                    PilotId = pilotId
                    Bullets = bullets
                    Shells = shells
                    Bombs = bombs
                    Rockets = rockets
                    Position = position
                    UserNickId = userNickId
                    UserId = userId
                    Name = name
                    Typ = typ
                    Country = country
                    Payload = payload
                    Fuel = fuel
                    Skin = skin
                    WeaponMods = uint64 modMask
                }
                PlayerEvent(timeStamp, PlayerTakesObject taken)
            | _ ->
                InvalidLine
        | 12 ->
            match eventData with
            | MatchesRegex reBind m ->
                match m.Groups.["id"].Value, m.Groups.["country"].Value, m.Groups.["pid"].Value with
                | AsInt id, AsInt country, AsInt parent ->
                    let sub =
                        match m.Groups.["sub"].Value with
                        | AsInt sub -> Some sub
                        | _ -> None
                    ObjectEvent(timeStamp, ObjectBound { Id = id; Typ = m.Groups.["type"].Value; Sub = sub; Country = country; Name = m.Groups.["name"].Value; Parent = parent })
                | _ ->
                    InvalidLine
            | _ ->
                InvalidLine
        | 20 ->
            match eventData with
            | MatchesRegex reJoins (GroupList [userId; userNickId]) ->
                PlayerEvent(timeStamp, PlayerJoins { UserId = userId; UserNickId = userNickId })
            | _ -> InvalidLine
        | 21 ->
            match eventData with
            | MatchesRegex reLeaves (GroupList [userId; userNickId]) ->
                PlayerEvent(timeStamp, PlayerLeaves { UserId = userId; UserNickId = userNickId })
            | _ -> InvalidLine
        | _ -> OtherEvent
    | _ ->
        InvalidLine