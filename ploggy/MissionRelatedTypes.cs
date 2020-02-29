using System;

namespace ploggy
{
  public enum Country
  {
    None = 0,
    Neutral = 50,
    USSR = 101,
    OtherAllies = 102,
    Germany = 201,
    OtherAxis = 202
  }

  public struct Position3d
  {
    public float X;
    public float Y;
    public float Z;

    public Position3d(float x, float y, float z)
    {
      X = x;
      Y = y;
      Z = z;
    }

    public float Distance(Position3d pos)
    {
      float dx = X - pos.X;
      float dy = Y - pos.Y;
      float dz = Z - pos.Z;

      return (float)Math.Sqrt(dx * dx + dy * dy + dz * dz);
    }

    public Position3d ToZYX()
    {
      return new Position3d(Z, Y, X);
    }

    public Position3d ToYZX()
    {
      return new Position3d(Y, Z, X);
    }

    public override string ToString()
    {
      return string.Format(Utility.floatNumbersCulture, "({0:f2}, {1:f2}, {2:f2})", X, Y, Z);
    }

    public bool IsZero()
    {
      return (X == 0.0f) && (Y == 0.0f) && (Z == 0.0f);
    }

    public float Height()
    {
      switch (Utility.AxisOrder)
      {
        case AxisOrder.XYZ:
        case AxisOrder.ZYX:
          return Y;

        case AxisOrder.YZX:
          return Z;
      }

      return Y;
    }

    public static Position3d Zero = new Position3d(0.0f, 0.0f, 0.0f);
  }

  public enum PlaneStartType
  {
    InAir = 0,
    Runway = 1,
    Parking = 2
  }

  public enum Coalition
  {
    Neutral = 0,
    Allies = 1,
    Axis = 2,
    All = 3
  }

  public enum SettingsPreset
  {
    Custom = 0,
    Normal = 1,
    Expert = 2
  }

  public enum MissionType
  {
    Single = 0,
    Cooperative = 1,
    Dogfight = 2,
    Campaign = 3,
    CaptureTheFlag = 4,


    InterceptBombers = 101,
    InterceptAttackers = 102,
    InterceptCargo = 103,

    EscortBombers = 151,
    EscortAttackers = 152,
    EscortCargo = 153,

    AttackColumn = 201,
    AttackArtilleryPosition = 202,
    AttackEnemyTrain = 203,
    AttackEnemyAirfield = 204,
    AttackShips = 205,

    SupportGroundTroops = 301,
    SupportShips = 302,
    CoverShips = 303,

    BombEnemyArtillery = 401,
    BombEnemyRailwayStation = 402,
    BombEnemyAirfield = 403,
    BombEnemySPDump = 404,
    BombEnemyBridge = 405,


    QuickSkirmish = 701,
    QuickDuel = 702,
    QuickSurvive = 703,

    None = -1
  }

  public struct CoalitionBind
  {
    public Country country;
    public Coalition coalition;

    public CoalitionBind(Country country, Coalition coalition)
    {
      this.country = country;
      this.coalition = coalition;
    }
  }

  public enum ObjectiveType
  {
    Primary = 0,
    Secondary = 1,
    Secondary2 = 2,
    Secondary3 = 3,
    Secondary4 = 4,
    Secondary5 = 5,
    Secondary6 = 6,
    Secondary7 = 7,
    Secondary8 = 8,
    Secondary9 = 9,
    Secondary10 = 10,
    Secondary11 = 11,
    Secondary12 = 12,
    Secondary13 = 13,
    Secondary14 = 14,
    Secondary15 = 15
  }
}

