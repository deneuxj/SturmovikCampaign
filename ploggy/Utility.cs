using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace ploggy
{
  public enum Relationship
  {
    Neutral,
    Friendly,
    Enemy
  }

  public enum AxisOrder
  {
    XYZ,
    YZX,
    ZYX
  }

  public class Utility
  {
    public static CultureInfo floatNumbersCulture = CultureInfo.InvariantCulture;
    public static char[] alphabet = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' };
    private static AxisOrder axisOrder = AxisOrder.XYZ;

    public static AxisOrder AxisOrder {
      get { return axisOrder; }
      set { axisOrder = value; }
    }

    static Utility()
    {
      //            Plogger.Init();
    }

    public static string GetMapSquareFromPosition(Position3d pos)
    {
      int X = (int)(pos.X / 10000);
      int Y = (int)(pos.Z / 10000);

      if ((Y >= alphabet.Length) || (Y < 0))
        return string.Format("AA{0}", X);

      return string.Format("{0}{1}", alphabet[Y], X);
    }

    public static Country GetCountry(int countryId)
    {
      switch (countryId)
      {
        case 50:
          return Country.Neutral;
        case 101:
          return Country.USSR;
        case 102:
          return Country.OtherAllies;
        case 201:
          return Country.Germany;
        case 202:
          return Country.OtherAxis;
      }

      return Country.None;
    }


    public static Coalition GetCountryCoalition(CoalitionBind[] countries, Country country)
    {
      if (countries == null)
        return Coalition.Neutral;

      for (int i = 0; i < countries.Length; ++i)
        if (countries[i].country == country)
          return countries[i].coalition;

      return Coalition.Neutral;
    }

    public static Relationship GetRelationship(CoalitionBind[] countries, Country a, Country b)
    {
      Coalition A = GetCountryCoalition(countries, a);
      Coalition B = GetCountryCoalition(countries, b);

      if ((A == Coalition.Neutral) ||
          (B == Coalition.Neutral))
        return Relationship.Neutral;

      if (A == B)
        return Relationship.Friendly;

      return Relationship.Enemy;
    }

    public static Coalition GetCoalition(int coalitionId)
    {
      switch (coalitionId)
      {
        case 1:
          return Coalition.Allies;

        case 2:
          return Coalition.Axis;
      }

      return Coalition.Neutral;
    }

    public static Coalition ParseCoalition(string str)
    {
      int coalitionId = ParseInt(str, 0);
      return GetCoalition(coalitionId);
    }

    public static int CountryIndex(CoalitionBind[] countries, Country country)
    {
      if (countries == null)
        return -1;

      for (int i = 0; i < countries.Length; ++i)
        if (countries[i].country == country)
          return i;

      return -1;
    }

    public static Country ParseCountry(string str)
    {
      int countryId = ParseInt(str, 0);
      return GetCountry(countryId);
    }

    public static ObjectiveType ParseObjectiveType(string str)
    {
      int objectiveId = ParseInt(str, 0);
      return GetObjectiveType(objectiveId);
    }

    public static bool ParseBoolInt(string str)
    {
      int intVal = ParseInt(str, 0);
      return intVal == 1;
    }

    public static MissionType GetMissionType(int typeId)
    {
      switch (typeId)
      {
        case 0:
          return MissionType.Single;
        case 1:
          return MissionType.Cooperative;
        case 2:
          return MissionType.Dogfight;
        case 3:
          return MissionType.Campaign;
        case 4:
          return MissionType.CaptureTheFlag;

        case 101:
          return MissionType.InterceptBombers;
        case 102:
          return MissionType.InterceptAttackers;
        case 103:
          return MissionType.InterceptCargo;

        case 151:
          return MissionType.EscortBombers;
        case 152:
          return MissionType.EscortAttackers;
        case 153:
          return MissionType.EscortCargo;

        case 201:
          return MissionType.AttackColumn;
        case 202:
          return MissionType.AttackArtilleryPosition;
        case 203:
          return MissionType.AttackEnemyTrain;
        case 204:
          return MissionType.AttackEnemyAirfield;
        case 205:
          return MissionType.AttackShips;

        case 301:
          return MissionType.SupportGroundTroops;
        case 302:
          return MissionType.SupportShips;
        case 303:
          return MissionType.CoverShips;

        case 401:
          return MissionType.BombEnemyArtillery;
        case 402:
          return MissionType.BombEnemyRailwayStation;
        case 403:
          return MissionType.BombEnemyAirfield;
        case 404:
          return MissionType.BombEnemySPDump;
        case 405:
          return MissionType.BombEnemyBridge;


        case 701:
          return MissionType.QuickSkirmish;
        case 702:
          return MissionType.QuickDuel;
        case 703:
          return MissionType.QuickSurvive;
      }

      return MissionType.None;
    }

    public static bool IsSingleMissionType(MissionType missionType)
    {
      switch (missionType)
      {
        case MissionType.Cooperative:
        case MissionType.Dogfight:
        case MissionType.CaptureTheFlag:
          return false;
      }

      return true;
    }

    public static int ParseInt(string str, int defaultValue)
    {
      int result = defaultValue;

      if (!int.TryParse(str, out result))
        return defaultValue;

      return result;
    }

    public static float ParseFloat(string str, float defaultValue)
    {
      float result = defaultValue;

      if (!float.TryParse(str, NumberStyles.AllowDecimalPoint, floatNumbersCulture, out result))
        return defaultValue;

      return result;
    }

    public static SettingsPreset GetSettingsPreset(int settingsId)
    {
      switch (settingsId)
      {
        case 1:
          return SettingsPreset.Normal;

        case 2:
          return SettingsPreset.Expert;
      }

      return SettingsPreset.Custom;
    }

    private static char[] commaSeparator = { ',' };

    public static int[] ParseIntList(string ids)
    {
      int[] result = null;

      if (string.IsNullOrEmpty(ids))
        return null;

      string[] strings = ids.Split(commaSeparator);

      if (strings == null)
        return null;

      int count = strings.Length;

      result = new int[count];

      for (int i = 0; i < count; ++i)
        result[i] = ParseInt(strings[i], -1);

      return result;
    }

    private static Regex regexForPosition = new Regex(@"([\d\.]+),\s*([\d\.]+),\s*([\d\.]+)");

    public static Position3d ParsePosition3d(string positionStr)
    {
      Position3d pos = Position3d.Zero;

      if (string.IsNullOrEmpty(positionStr))
        return pos;

      Match match = regexForPosition.Match(positionStr);

      if (!match.Success)
        return pos;

      switch (axisOrder)
      {
        case AxisOrder.YZX:
          pos.Y = ParseFloat(match.Groups[1].Value, 0.0f);
          pos.Z = ParseFloat(match.Groups[2].Value, 0.0f);
          pos.X = ParseFloat(match.Groups[3].Value, 0.0f);
          break;

        case AxisOrder.ZYX:
          pos.Z = ParseFloat(match.Groups[1].Value, 0.0f);
          pos.Y = ParseFloat(match.Groups[2].Value, 0.0f);
          pos.X = ParseFloat(match.Groups[3].Value, 0.0f);
          break;

        case AxisOrder.XYZ:
        default:
          pos.X = ParseFloat(match.Groups[1].Value, 0.0f);
          pos.Y = ParseFloat(match.Groups[2].Value, 0.0f);
          pos.Z = ParseFloat(match.Groups[3].Value, 0.0f);
          break;

      }


      return pos;
    }

    public static string YesNo(bool val)
    {
      return val ? @"yes" : @"no";
    }

    public static ObjectiveType GetObjectiveType(int objectiveId)
    {
      if ((objectiveId < 0) || (objectiveId > (int)ObjectiveType.Secondary15))
        return ObjectiveType.Secondary;

      return (ObjectiveType)objectiveId;
    }
  }
}
