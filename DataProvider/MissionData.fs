module SturmovikMission.DataProvider.MissionData

open SturmovikMission.DataProvider.Ast

module ShortCuts =
    let Int = ValueType.Integer
    let Float = ValueType.Float
    let Str = ValueType.String
    let Set = ValueType.Set
    let Vec = ValueType.IntVector
    let Date = ValueType.Date
    let Composite = ValueType.Composite
    let Mapping = ValueType.Mapping
    let Pair = ValueType.Pair
    let Triplet = ValueType.Triplet

open ShortCuts

let optionsType =
    "Options",
    Composite [
        ("LCName", Int)
        ("LCDesc", Int)
        ("LCAuthor", Int)
        ("PlayerConfig", Str)
        ("MultiplayerPlaneConfig", Str)
        ("Time", Triplet(Int, Int, Int))
        ("Date", Date)
        ("HMap", Str)
        ("Textures", Str)
        ("Forests", Str)
        ("Layers", Str)
        ("GuiMap", Str)
        ("SeasonPrefix", Str)
        ("MissionType", Int)
        ("AqmId", Int)
        ("CloudLevel", Int)
        ("CloudHeight", Int)
        ("PrecLevel", Int)
        ("PrecType", Int)
        ("CloudConfig", Str)
        ("SeaState", Int)
        ("Turbulence", Int)
        ("TempPressLevel", Int)
        ("Temperature", Int)
        ("Pressure", Int)
        ("WindLayers", Set(Triplet(Int, Int, Int)))
        ("Countries", Set(Pair(Int, Int)))
    ]

let blockBase = [
    ("Name", Str)
    ("Index", Int)
    ("LinkTrId", Int)
    ("XPos", Float)
    ("YPos", Float)
    ("ZPos", Float)
    ("XOri", Float)
    ("YOri", Float)
    ("ZOri", Float)
    ("Model", Str)
    ("Script", Str)
    ("Country", Int)
    ("Desc", Str)
    ("DamageReport", Int)
    ("DamageThreshold", Int)
    ("DeleteAfterDeath", Int)
    ("Damaged", Set(Mapping(Float)))
]

let blockType =
    "Block",
    Composite (("Durability", Int) :: blockBase)

let bridgeType =
    "Bridge",
    Composite (("Durability", Int) :: blockBase)

let mcuBase = [
    ("Index", Int)
    ("Targets", Vec)
    ("Objects", Vec)
    ("XPos", Float)
    ("YPos", Float)
    ("ZPos", Float)
    ("XOri", Float)
    ("YOri", Float)
    ("ZOri", Float)
]

let mcuBase2 =
    mcuBase @ [
        ("Name", Str)
        ("Desc", Str)
    ]

let iconType =
    "MCU_Icon",
    Composite (
        mcuBase @ [
            ("IconId", Int)
            ("RColor", Int)
            ("GColor", Int)
            ("BColor", Int)
            ("LineType", Int)
            ("Coalitions", Vec)
            ("LCName", Int)
            ("LCDesc", Int)
            ("Enabled", Int)
        ]
    )

let timerType =
    "MCU_Timer",
    Composite (
        mcuBase2 @ [
            ("Time", Int)
            ("Random", Int)
        ]
    )

let vehicleType =
    "Vehicle",
    Composite (
        blockBase @ [
            ("NumberInFormation", Int)
            ("Vulnerable", Int)
            ("Engageable", Int)
            ("LimitAmmo", Int)
            ("AILevel", Int)
            ("CoopStart", Int)
            ("Spotter", Int)
            ("BeaconChannel", Int)
            ("Callsign", Int)
        ]
    )

let entityType =
    "MCU_TR_Entity",
    Composite (
        mcuBase2 @ [
            ("Enabled", Str)
            ("MisObjID", Int)
            ("OnEvents", Composite [
                ("OnEvent", Composite [
                    ("Type", Int)
                    ("TarId", Int)])
                ])
        ]
    )

let spawnerType =
    "MCU_Spawner",
    Composite (
        mcuBase2 @ [
            ("SpawnAtMe", Int)
        ]
    )

let deactivateType =
    "MCU_Deactivate",
    Composite mcuBase2

let activateType =
    "MCU_Activate",
    Composite mcuBase2

let deleteType =
    "MCU_Delete",
    Composite mcuBase2

let counterType =
    "MCU_Counter",
    Composite (
        mcuBase2 @ [
            ("Counter", Int)
            ("Dropcount", Int)
        ]
    )

let subtitleType =
    "MCU_TR_Subtitle",
    Composite (
        mcuBase2 @ [
            ("Enabled", Int)
            ("SubtitleInfo",
                Composite [
                    ("Duration", Int)
                    ("FontSize", Int)
                    ("HAlign", Int)
                    ("VAlign", Int)
                    ("RColor", Int)
                    ("GColor", Int)
                    ("BColor", Int)
                    ("LCText", Int)
                ])
            ("Coalitions", Vec)
        ]
    )

let missionBeginType =
    "MCU_TR_MissionBegin",
    Composite (
        mcuBase2 @ [
            ("Enabled", Int)
        ]
    )

let airfieldType =
    "Airfield",
    Composite (
        blockBase @ [
            ("Durability", Int)
            ("Callsign", Int)
            ("Callnum", Int)
            ("Planes",
                Composite [
                    ("Plane",
                        Composite [
                            ("SetIndex", Int)
                            ("Number", Int)
                            ("AILevel", Int)
                            ("StartInAir", Int)
                            ("Engageable", Int)
                            ("Vulnerable", Int)
                            ("LimitAmmo", Int)
                            ("AIRTBDecision", Int)
                            ("Renewable", Int)
                            ("PayloadId", Int)
                            ("WMMask", Int)
                            ("Fuel", Int)
                            ("RouteTime", Int)
                            ("RenewTime", Int)
                            ("Altitude", Int)
                            ("Model", Str)
                            ("Script", Str)
                            ("Name", Str)
                            ("Skin", Str)
                            ("Callsign", Int)
                            ("Callnum", Int)
                        ]
                    )
                ]
            )
            ("Chart",
                Composite [
                    ("Point",
                        Composite [
                            ("Type", Int)
                            ("X", Float)
                            ("Y", Float)
                        ]
                    )
                ]
            )
            ("ReturnPlanes", Int)
            ("Hydrodrome", Int)
            ("RepairFriendlies", Int)
            ("RepairTime", Int)
            ("RearmTime", Int)
            ("RefuelTime", Int)
            ("MaintenanceRadius", Int)
        ]
    )

