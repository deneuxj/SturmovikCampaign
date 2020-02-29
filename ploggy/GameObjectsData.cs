using System;
using System.IO;
using System.Collections.Generic;

namespace ploggy
{
  public enum UnitType
  {
    Unknown,
    Fighter,
    Attacker,
    Bomber,
    AAA,
    FiringPoint,
    Artillery,
    Static,
    Tank,
    Vehicle,
    Turret,
    Bot,
    All
  }

  public class GameObjectsData
  {
    private Dictionary<string, UnitType> nameToType = new Dictionary<string, UnitType>();

    public GameObjectsData(Dictionary<string, UnitType> data)
    {
      nameToType = data;
    }

    public UnitType GetUnitType(string name)
    {
      UnitType type = UnitType.Unknown;

      if (nameToType.TryGetValue(name, out type))
        return type;

      return UnitType.Unknown;
    }
  }


}

