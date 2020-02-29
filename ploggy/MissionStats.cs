using System;
using System.Collections.Generic;

namespace ploggy
{
  public class FragInfo
  {
    private Player player = null;
    private Player killed = null;
    private Coalition coalition = Coalition.Neutral;
    private Country country = Country.None;
    private string ammoType = null;
    private Position3d position = Position3d.Zero;
    private TimeSpan timestamp = TimeSpan.Zero;

    public string Player { get { return MissionObject.GetNameSafe(player); } }
    public string Killed { get { return MissionObject.GetNameSafe(killed); } }
    public TimeSpan Timestamp { get { return timestamp; } }
    public Coalition Coalition { get { return coalition; } }
    public Country Country { get { return country; } }
    public string AmmoType { get { return ammoType; } }
    public Position3d Position { get { return position; } }

    public FragInfo(Player player, Player killed, Position3d position, TimeSpan timestamp, Country country, Coalition coalition, string ammoType)
    {
      this.player = player;
      this.killed = killed;
      this.country = country;
      this.coalition = coalition;
      this.ammoType = ammoType;
      this.position = position;
      this.timestamp = timestamp;
    }

    public override string ToString()
    {
      return string.Format("[FragInfo: Player={0}, Killed={1}, Coalition={2}, Country={3}, AmmoType={4}, Position={5}, Timestamp={6}]",
          Player, Killed, Coalition, Country, AmmoType, Position, Timestamp);
    }
  }

  public class KilledUnitInfo
  {
    public int ObjectId = -1;
    public string Type = null;
    public Position3d Position = Position3d.Zero;
    public TimeSpan Timestamp = TimeSpan.Zero;

    public KilledUnitInfo(int objectId, string type, Position3d position, TimeSpan timestamp)
    {
      ObjectId = objectId;
      Type = type;
      Position = position;
      Timestamp = timestamp;
    }
  }

  public class KilledByPlayerUnitInfo : KilledUnitInfo
  {
    public string Killer = null;

    public KilledByPlayerUnitInfo(int objectId, string type, Position3d position, MissionObject killer, TimeSpan timestamp) :
        base(objectId, type, position, timestamp)
    {
      Killer = killer != null ? killer.Name : null;
    }
  }

  public class UnitTypeFrags
  {
    public UnitType UnitType = UnitType.Unknown;
    public int Count { get { return Units.Count; } }

    public List<KilledUnitInfo> Units = new List<KilledUnitInfo>(16);

    public UnitTypeFrags(UnitType unitType)
    {
      UnitType = unitType;
    }

    public void Add(MissionObject obj, MissionObject killer)
    {
      KilledUnitInfo kennyInfo = killer != null ?
          new KilledByPlayerUnitInfo(obj.ObjectId, obj.Type, obj.Position, killer, obj.LastEventTime()) :
          new KilledUnitInfo(obj.ObjectId, obj.Type, obj.Position, obj.LastEventTime());

      Units.Add(kennyInfo);
    }
  }

  public class CountryFrags
  {
    public Country Country = Country.None;
    public int Frags = 0;
    public UnitTypeFrags[] UnitTypes = new UnitTypeFrags[(int)UnitType.All];

    public CountryFrags(Country country)
    {
      this.Country = country;

      for (int i = 0; i < (int)UnitType.All; ++i)
        UnitTypes[i] = new UnitTypeFrags((UnitType)i);
    }
  }


  public class CoalitionFrags
  {
    public Coalition Coalition = Coalition.Neutral;
    public UnitTypeFrags[] UnitTypes = new UnitTypeFrags[(int)UnitType.All];
    public int Frags = 0;

    public CoalitionFrags(Coalition coalition)
    {
      this.Coalition = coalition;

      for (int i = 0; i < (int)UnitType.All; ++i)
        UnitTypes[i] = new UnitTypeFrags((UnitType)i);
    }
  }

  public class MissionStats
  {
    private List<FragInfo> fragList = new List<FragInfo>(32);

    private CountryFrags[] fragPerCountry = null;
    private CoalitionFrags[] fragPerCoalition = new CoalitionFrags[(int)Coalition.All];
    private CoalitionBind[] countries = null;


    public CountryFrags[] FragPerCountry { get { return fragPerCountry; } }
    public CoalitionFrags[] FragPerCoalition { get { return fragPerCoalition; } }
    public List<FragInfo> Frags { get { return fragList; } }

    public MissionStats(CoalitionBind[] countries)
    {
      this.countries = countries;
      if (countries == null)
        return;

      fragPerCountry = new CountryFrags[countries.Length];

      for (int i = 0; i < (int)Coalition.All; ++i)
        fragPerCoalition[i] = new CoalitionFrags((Coalition)i);

      for (int i = 0; i < countries.Length; ++i)
        fragPerCountry[i] = new CountryFrags(countries[i].country);
    }

    public void AddFrag(FragInfo info)
    {
      if (countries == null)
        return;

      fragList.Add(info);

      ++fragPerCoalition[(int)info.Coalition].Frags;

      int index = Utility.CountryIndex(countries, info.Country);

      if (index == -1)
      {
        Plogger.Warn("Can't updates stats by frag, because killer's country {0} is not in registered mission countries.", info.Country);
        Plogger.Debug(info);
        return;
      }

      ++fragPerCountry[index].Frags;
    }

    public void AddKilledUnit(Coalition killerCoalition, Country killerCountry, MissionObject obj, MissionObject killer)
    {
      if (countries == null)
        return;

      int index = Utility.CountryIndex(countries, killerCountry);

      if (index == -1)
      {
        Plogger.Warn("Can't updates stats for unit, because killer's country {0} is not in registered mission countries.", killerCountry);
        Plogger.Debug(obj);
        return;
      }

      ++fragPerCoalition[(int)killerCoalition].Frags;
      ++fragPerCountry[index].Frags;

      fragPerCoalition[(int)killerCoalition].UnitTypes[(int)obj.UnitType].Add(obj, killer);
      fragPerCountry[index].UnitTypes[(int)obj.UnitType].Add(obj, killer);
    }
  }
}

