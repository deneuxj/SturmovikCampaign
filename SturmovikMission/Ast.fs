module SturmovikMission.Ast

type Time =
    { Hour : int
      Minute : int
      Second : int
    }

type Date =
    { Year : int
      Month : int
      Day : int
    }

type Wind = 
    { Direction : int
      Speed : int
    }

type WindLayers =
    { Alt0 : Wind
      Alt500 : Wind
      Alt1000 : Wind
      Alt2000 : Wind
      Alt5000 : Wind
    }

type Countries = Countries of Map<int, int>

type MissionOptions = MissionOptions of Map<string, obj>

type Coord3d =
    { X : float
      Y : float
      Z : float
    }

type BlockInfo =
    { Name : string
      Index : int
      LinkTrId : int
      Position : Coord3d
      Orientation : Coord3d
      Model : string
      Script : string
      Country : int
      Description : string
      Durability : int
      DamageReport : int option
      DeleteAfterDeath : bool
    }

type MissionItem =
    Block of BlockInfo

type Mission =
    { Options : MissionOptions
      Items : MissionItem list
    }
