using System;
using System.Linq;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace ploggy
{
  public enum MissionObjectType
  {
    None,
    Block,
    Pilot,
    Player,
    Vehicle
  }

  public class MissionObject
  {
    public static MissionObject None = new MissionObject(MissionObjectType.None, UnitType.Unknown, "Other");
    protected UnitType unitType = UnitType.Unknown;
    protected MissionObjectType missionObjectType = MissionObjectType.None;
    protected int id = -1;
    protected int parentId = -1;
    protected MissionObject parent = null;
    protected string type = null;
    protected string name = null;
    protected Country country = Country.None;
    protected Position3d position = Position3d.Zero;
    protected bool alive = true;

    private static Regex regexForBlockArray = new Regex(@"(.+)\[.+\]$");

    protected List<MissionEvent> eventList = new List<MissionEvent>(16);

    public int ObjectId { get { return id; } }
    public int ParentId { get { return parentId; } }
    public string ParentType { get { return parent != null ? parent.Type : null; } }
    public string Name { get { return name; } }

    public string Type { get { return type; } }
    public UnitType UnitType { get { return unitType; } }
    public virtual Country Country { get { return country; } }
    public MissionObjectType MissionObjectType { get { return missionObjectType; } }
    public Position3d Position { get { return position; } }

    protected List<MissionObject> childObjects = new List<MissionObject>(4);

    public MissionObject(MissionObjectType missionObjectType, UnitType unitType)
    {
      this.missionObjectType = missionObjectType;
      this.unitType = unitType;
    }

    public void RegisterChild(MissionObject child)
    {
      childObjects.Add(child);
    }

    public List<MissionObject> GetChildren()
    {
      return childObjects;
    }

    public MissionObject(MissionObjectType missionObjectType, UnitType unitType, string name)
    {
      this.missionObjectType = missionObjectType;
      this.name = name;
      this.unitType = unitType;
    }

    public MissionObject(ObjectSpawnedEntry entry, MissionLog mlog, MissionObjectType missionObjectType, UnitType unitType)
    {
      id = entry.ObjectId;
      parentId = entry.ParentId;
      parent = mlog.GetObject(parentId);
      country = entry.Country;
      name = entry.ObjectName;
      type = ParseTypeStr(entry.ObjectType);
      this.missionObjectType = missionObjectType;
      this.unitType = unitType;
    }

    public void UpdateDeathPosition()
    {
      if (!position.IsZero())
        return;

      foreach (MissionEvent evt in eventList)
      {
        DeathEvent deathEvent = evt as DeathEvent;

        if (deathEvent == null)
          continue;

        position = deathEvent.Position;
        break;
      }
    }

    public void UpdatePosition(Position3d pos)
    {
      position = pos;
    }

    public static MissionObject Create(ObjectSpawnedEntry entry, MissionLog mlog)
    {
      // XXX: remove these handicaps
      if (entry.ObjectType != ParseTypeStr(entry.ObjectType))
        return new Block(entry, mlog);

      if (entry.ObjectType.StartsWith("Bot"))
        return new MissionObject(entry, mlog, MissionObjectType.Pilot, UnitType.Bot);

      UnitType unitType = mlog.GetUnitType(entry.ObjectType);


      switch (unitType)
      {
        case UnitType.Bot:
          return new MissionObject(entry, mlog, MissionObjectType.Pilot, UnitType.Bot);

        case UnitType.Fighter:
        case UnitType.Attacker:
        case UnitType.Bomber:
        case UnitType.AAA:
        case UnitType.FiringPoint:
        case UnitType.Artillery:
        case UnitType.Tank:
        case UnitType.Vehicle:
        case UnitType.Turret:
          return new Vehicle(entry, mlog, unitType);

        case UnitType.Unknown:
          break;

        case UnitType.Static:
          break;
      }

      return new MissionObject(entry, mlog, MissionObjectType.None, unitType);
    }

    public static string ParseTypeStr(string nameToParse)
    {
      Match match = regexForBlockArray.Match(nameToParse);

      if (!match.Success)
        return nameToParse;

      return match.Groups[1].Value;
    }

    public virtual void AddEvent(MissionEvent evt)
    {
      eventList.Add(evt);
    }

    public List<MissionEvent> GetEvents()
    {
      return eventList;
    }

    public MissionObject GetParent()
    {
      return parent;
    }

    public static string GetObjectTypeStrSafe(MissionObject obj)
    {
      if (obj == null)
        return null;

      return obj.Type;
    }

    public static string GetNameSafe(MissionObject obj)
    {
      if (obj == null)
        return null;

      return obj.Name;
    }

    public static string GetSimpleIdPreferPlayerName(MissionObject obj)
    {
      if (obj == null)
        return null;

      Player player = GetPlayer(obj);

      return player != null ? player.Name : obj.type;
    }

    public static string GetPlayerName(MissionObject obj)
    {
      if (obj == null)
        return null;

      Player player = GetPlayer(obj);

      return player != null ? player.Name : null;
    }

    public static Player GetPlayer(MissionObject obj)
    {
      if (obj == null)
        return null;

      switch (obj.MissionObjectType)
      {
        case MissionObjectType.Vehicle:
          {
            Player player = obj.GetPlayerFromChildren();

            if (player != null)
              return player;

            if (obj.UnitType != UnitType.Turret)
              return null;

            MissionObject parent = obj.GetParent();

            if (parent != null)
              player = parent.GetPlayerFromChildren();

            return player;
          }

        case MissionObjectType.Pilot:
          {
            Player player = obj.GetPlayerFromChildren();

            if (player != null)
              return player;

            MissionObject parent = obj.GetParent();

            if ((parent == null) || (parent.UnitType != UnitType.Turret))
              return null;

            MissionObject grandParent = parent.GetParent();

            if (grandParent == null)
              return null;

            return grandParent.GetPlayerFromChildren();
          }

        case MissionObjectType.Player:
          return obj as Player;
      }

      return null;
    }

    public override string ToString()
    {
      return string.Format("[MissionObject UnitType={0}, Type={1}, Name={2}, ObjectId={3}]",
          UnitType, Type, Name, ObjectId);
    }

    public void SetDead()
    {
      alive = false;
    }

    public bool IsAlive()
    {
      return alive;
    }

    public bool HaveEvent(MissionEventType evtType)
    {
      foreach (MissionEvent evt in eventList)
      {
        if (evt.Event == evtType)
          return true;
      }

      return false;
    }

    public bool HavePilotDeathEvents()
    {
      foreach (MissionEvent evt in eventList)
      {
        switch (evt.Event)
        {
          case MissionEventType.BotDeath:
            {
              UnitType unitType = BotDeathEvent.GetBotType(evt as BotDeathEvent);

              if (unitType == UnitType.Vehicle)
                return true;
            }
            break;

          case MissionEventType.Suicide:
            return true;
        }
      }

      return false;
    }

    public TimeSpan LastEventTime()
    {
      if (eventList.Count == 0)
        return TimeSpan.Zero;

      return eventList[eventList.Count - 1].Timestamp;
    }

    public Player GetPlayerFromChildren()
    {
      foreach (MissionObject obj in childObjects)
        if (obj.MissionObjectType == MissionObjectType.Player)
          return obj as Player;

      return null;
    }

    public static UnitType GetBotType(MissionObject bot)
    {
      if (bot == null)
        return UnitType.Unknown;

      MissionObject parent = bot.parent;

      if (parent == null)
        return UnitType.Unknown;

      switch (parent.UnitType)
      {
        case UnitType.Fighter:
        case UnitType.Attacker:
        case UnitType.Bomber:
          return UnitType.Vehicle;

        case UnitType.Turret:
          return UnitType.Turret;
      }

      return parent.UnitType;
    }
  }

  public class Vehicle : MissionObject
  {
    public Vehicle(ObjectSpawnedEntry entry, MissionLog mlog, UnitType unitType) : base(entry, mlog, MissionObjectType.Vehicle, unitType)
    {
    }

    public Vehicle(PlayerPlaneEntry entry, MissionLog mlog, UnitType unitType) : base(MissionObjectType.Vehicle, unitType)
    {
      id = entry.VehicleId;
      country = entry.Country;
      type = ParseTypeStr(entry.VehicleType);
    }

    public void Update(ObjectSpawnedEntry entry, MissionLog mlog)
    {
      parentId = entry.ParentId;
      parent = mlog.GetObject(parentId);
      country = entry.Country;
      name = entry.ObjectName;
    }

    public IEnumerable<MissionObject> GetTurrets()
    {
      return from child in childObjects where child.UnitType == UnitType.Turret select child;
    }
  }

  public class Block : MissionObject
  {
    private int items = 1;
    // XXX: probably move to MissionObject.childObjects
    private List<MissionObject> blockObjects = new List<MissionObject>(32);

    public int AliveObjects { get { return items; } }

    public Block(ObjectSpawnedEntry entry, MissionLog mlog) : base(entry, mlog, MissionObjectType.Block, UnitType.Static)
    {
    }

    public void AddObject(MissionObject obj)
    {
      blockObjects.Add(obj);
    }

    public MissionObject KillOne(string name)
    {
      if (blockObjects.Count == 0)
        return null;

      MissionObject foundObject = null;

      foreach (MissionObject obj in blockObjects)
      {
        if (obj.Name == name)
        {
          foundObject = obj;
          break;
        }
      }

      if (foundObject == null)
        foundObject = blockObjects[0];

      blockObjects.Remove(foundObject);

      return foundObject;
    }
  }

  public enum SessionFinishState
  {
    None,
    Crashed,    // vehicle was destoyed or killer unknown
    Killed,     // was killed
    Landed,     // successfully landed
    Ejected,    // left plane, ejected
    LeftMission, // logout for any reason
    Suicide,
    Survived,
    SurvivedInCrash,
    DiedAfterLanding
  }

  public class PlayerGameSession
  {
    public static PlayerGameSession None = new PlayerGameSession(null, null, Country.None, Position3d.Zero, null);

    private List<MissionEvent> eventList = new List<MissionEvent>(32);
    private MissionObject pilot = null;
    private Vehicle vehicle = null;
    private Country country = Country.None;
    private string skin = null;
    private Position3d startPosition = Position3d.Zero;
    private SessionFinishState finishState = SessionFinishState.None;
    private float vehicleDamage = 0.0f;
    private float pilotDamage = 0.0f;

    public int VehicleId { get { return vehicle != null ? vehicle.ObjectId : -1; } }
    public string Vehicle { get { return vehicle != null ? vehicle.Type : null; } }
    public int PilotId { get { return pilot != null ? pilot.ObjectId : -1; } }
    public virtual Country Country { get { return country; } }
    public string Skin { get { return skin; } }
    public Position3d StartPosition { get { return startPosition; } }
    public SessionFinishState FinishState { get { return finishState; } }

    public List<MissionEvent> AllEvents { get { return eventList; } }
    public List<MissionEvent> VehicleEvents { get { return vehicle != null ? vehicle.GetEvents() : null; } }
    public List<MissionEvent> PilotEvents { get { return pilot != null ? pilot.GetEvents() : null; } }
    public float VehicleDamage { get { return vehicleDamage; } }
    public float PilotDamage { get { return pilotDamage; } }
    public TimeSpan Duration = TimeSpan.Zero;
    public int HitRatio { get { return ammoUsed != 0 ? 100 * hitCount / ammoUsed : -1; } }

    private bool havePlayerEndEvent = false;
    private int hitCount = 0;
    private int ammoUsed = 0;

    public void AddEvent(MissionEvent evt)
    {
      eventList.Add(evt);

      switch (evt.Event)
      {
        case MissionEventType.PlayerEnd:
          havePlayerEndEvent = true;
          break;

        case MissionEventType.Hit:
          if (!havePlayerEndEvent)
            ++hitCount;
          break;
      }
    }

    public bool HavePlayerEndEvent()
    {
      return havePlayerEndEvent;
    }

    public Vehicle GetVehicleObject()
    {
      return vehicle;
    }

    public PlayerGameSession(Vehicle vehicle, MissionObject pilot, Country country, Position3d startPosition, string skin)
    {
      this.vehicle = vehicle;
      this.pilot = pilot;
      this.country = country;
      this.skin = skin;
      this.startPosition = startPosition;
    }

    public void FinishSession(Player player)
    {
      BotDeathEvent pilotDeathEvent = null;
      VehicleDestroyedEvent vehicleDestroyedEvent = null;
      int pilotDeathIndex = -1;
      int vehicleDestroyedIndex = -1;
      int playerEndIndex = -1;
      int landingIndex = -1;
      int ejectIndex = -1;

      finishState = SessionFinishState.None;

      player.IncreaseKillDeathsCounter(KillDeathType.FlightCount);

      // calculate pilot damage and update variables
      int count = eventList.Count;

      for (int i = 0; i < count; ++i)
      {
        MissionEvent evt = eventList[i];

        switch (evt.Event)
        {
          case MissionEventType.GotDamage:
            {
              GotDamageEvent damageEvent = evt as GotDamageEvent;
              MissionObject target = damageEvent.GetTargetObject();

              switch (target.MissionObjectType)
              {
                case MissionObjectType.Pilot:

                  if (target == pilot)
                    pilotDamage += damageEvent.Damage;

                  break;

                case MissionObjectType.Vehicle:
                  vehicleDamage += damageEvent.Damage;
                  break;
              }
            }
            break;

          case MissionEventType.BotDeath:
            {
              if (pilotDeathIndex != -1)
                break;

              BotDeathEvent deathEvent = evt as BotDeathEvent;
              UnitType botType = deathEvent.GetBotType();

              if ((botType != UnitType.Vehicle) &&
                  (botType != UnitType.Turret)) // accept only dead pilots & gunners
                break;

              pilotDeathEvent = deathEvent;
              pilotDeathIndex = i;
            }
            break;

          case MissionEventType.Suicide:
            // something to do here
            finishState = SessionFinishState.Suicide;
            break;

          case MissionEventType.VehicleDestroyed:
            if (vehicleDestroyedIndex == -1)
            {
              vehicleDestroyedEvent = evt as VehicleDestroyedEvent;
              vehicleDestroyedIndex = i;
            }
            break;

          case MissionEventType.Landing:

            if ((playerEndIndex == -1) &&
                ((vehicleDestroyedIndex == -1) || (ejectIndex != -1)))
              landingIndex = i;

            break;

          case MissionEventType.Ejected:
            if ((playerEndIndex == -1) && (pilotDeathIndex == -1))
              ejectIndex = i;
            break;

          case MissionEventType.PlayerEnd:
            playerEndIndex = i;
            break;
        }
      }

      if (finishState == SessionFinishState.Suicide)
        player.IncreaseKillDeathsCounter(KillDeathType.Suicide);

      // normalize damage
      if (vehicleDamage > 1.0f)
        vehicleDamage = 1.0f;

      if (pilotDamage > 1.0f)
        pilotDamage = 1.0f;

      if (count > 1)
        Duration = (eventList[count - 1].Timestamp - eventList[0].Timestamp).Duration();


      if ((vehicleDestroyedEvent == null) &&
          (pilotDeathEvent == null))
      {
        // can be LeftMission or Landed

        // if we got Landed, and got it earler than player end => Landed
        // otherwise => LeftMission

        if ((landingIndex != -1) &&
            ((landingIndex < playerEndIndex) || (playerEndIndex == -1)))
        {
          player.IncreaseKillDeathsCounter(KillDeathType.Landed);
          finishState = SessionFinishState.Landed;
          return;
        }

        player.IncreaseKillDeathsCounter(KillDeathType.LeftMission);
        finishState = SessionFinishState.LeftMission;
        return;
      }

      // have vehicle destroy event, have no pilot death event
      if ((vehicleDestroyedEvent != null) &&
          (pilotDeathEvent == null))
      {
        // TODO: check in different situations
        if ((ejectIndex != -1) &&
            (ejectIndex < landingIndex)) // ejected and landed
        {
          finishState = SessionFinishState.Ejected;
          player.IncreaseKillDeathsCounter(KillDeathType.VehicleLost);
          player.IncreaseKillDeathsCounter(KillDeathType.Eject);
          return;
        }

        // can be ejected or crashed
        if ((ejectIndex != -1) &&
            (ejectIndex < playerEndIndex))
        {
          finishState = SessionFinishState.Ejected;
          player.IncreaseKillDeathsCounter(KillDeathType.Eject);
          return;
        }


        player.IncreaseKillDeathsCounter(KillDeathType.VehicleLost);

        if (vehicleDestroyedEvent.AttackerId != -1)
          finishState = SessionFinishState.Survived;
        else
        {
          finishState = SessionFinishState.SurvivedInCrash;
          player.IncreaseKillDeathsCounter(KillDeathType.Crash);
        }
        return;
      }

      if ((pilotDeathEvent != null) &&
          (vehicleDestroyedEvent == null))
      {
        player.IncreaseKillDeathsCounter(KillDeathType.Death);

        if ((landingIndex != -1) &&
            (pilotDeathIndex > landingIndex))
        {
          finishState = SessionFinishState.DiedAfterLanding;
          return;
        }

        finishState = SessionFinishState.Killed;
        return;
      }

      // both pilot and vehicle are killed
      player.IncreaseKillDeathsCounter(KillDeathType.Death);
      player.IncreaseKillDeathsCounter(KillDeathType.VehicleLost);

      if ((vehicleDestroyedIndex < pilotDeathIndex) &&
          (vehicleDestroyedEvent.AttackerId == -1))
      {
        player.IncreaseKillDeathsCounter(KillDeathType.Crash);
        finishState = SessionFinishState.Crashed;
        return;
      }

      finishState = SessionFinishState.Killed;
    }
  }

  public struct UnitKillCount
  {
    public UnitType UnitType;
    public int Count;
    public List<KilledByPlayerUnitInfo> KilledList;
  }

  public enum KillDeathType
  {
    PilotKill = 0,
    GunnerKill,
    VehicleKill,
    OtherKill, // paratrooper?
    Death,
    Eject,
    Suicide,
    VehicleLost,
    Crash,
    //
    FlightCount,
    Landed,
    LeftMission,
    All
  }

  public class PlayerStatsInfo
  {
    public string Name = null;

    public int Kills { get { return killDeathCounters[(int)KillDeathType.PilotKill]; } }
    public int VehicleKills { get { return killDeathCounters[(int)KillDeathType.VehicleKill]; } }
    public int GunnerKills { get { return killDeathCounters[(int)KillDeathType.GunnerKill]; } }
    public int Deaths { get { return killDeathCounters[(int)KillDeathType.Death]; } }
    public int VehiclesLost { get { return killDeathCounters[(int)KillDeathType.VehicleLost]; } }

    // seems not needed
    //public int OtherKills { get { return killDeathCounters[(int)KillDeathType.OtherKill]; } }
    public int Ejects { get { return killDeathCounters[(int)KillDeathType.Eject]; } }
    public int Flights { get { return killDeathCounters[(int)KillDeathType.FlightCount]; } }
    public int Crashes { get { return killDeathCounters[(int)KillDeathType.Crash]; } }
    public int Landings { get { return killDeathCounters[(int)KillDeathType.Landed]; } }
    public int LeftMissions { get { return killDeathCounters[(int)KillDeathType.LeftMission]; } }

    public Country PriorityCountry = Country.None;

    private int[] killDeathCounters = null;

    public UnitKillCount[] KilledUnitsCount = new UnitKillCount[(int)UnitType.All];

    public List<SessionFinishState> SessionResults = new List<SessionFinishState>(4);

    private Player player = null;

    public PlayerStatsInfo(int[] killDeathCounters, string Name, Country country, Player player)
    {
      for (int i = 0; i < (int)UnitType.All; ++i)
      {
        KilledUnitsCount[i].UnitType = (UnitType)i;
        KilledUnitsCount[i].KilledList = new List<KilledByPlayerUnitInfo>(8);
      }

      this.killDeathCounters = killDeathCounters;
      this.Name = Name;
      this.PriorityCountry = country;
      this.player = player;
    }

    public int[] GetKillDeathCounters()
    {
      return killDeathCounters;
    }

    public Player GetPlayer()
    {
      return player;
    }
  }


  public class Player : MissionObject
  {
    private List<PlayerGameSession> sessionList = new List<PlayerGameSession>(8);
    private PlayerGameSession currentGameSession = PlayerGameSession.None;

    private int[] kills = new int[(int)KillDeathType.All];

    public int VehicleId { get { return currentGameSession.VehicleId; } }
    public int PilotId { get { return currentGameSession.PilotId; } }
    public string Vehicle { get { return currentGameSession.Vehicle; } }
    public int Kills { get { return kills[(int)KillDeathType.PilotKill]; } }
    public int VehicleKills { get { return kills[(int)KillDeathType.PilotKill]; } }
    public int GunnerKills { get { return kills[(int)KillDeathType.GunnerKill]; } }
    public int Deaths { get { return kills[(int)KillDeathType.Death]; } }
    public bool ChangedCountry = false;
    public override Country Country { get { return currentGameSession.Country; } }

    public List<PlayerGameSession> GameSessions { get { return sessionList; } }

    private void UpdateStatsWithEvents(List<MissionEvent> events, PlayerStatsInfo psi)
    {
      bool stopFlag = false;

      foreach (MissionEvent evt in events)
      {
        switch (evt.Event)
        {
          case MissionEventType.BotDeath:
            if (BotDeathEvent.GetBotType(evt as BotDeathEvent) == UnitType.Vehicle)
            {
              stopFlag = true;
            }
            break;

          case MissionEventType.Death:
          case MissionEventType.VehicleDestroyed:
            stopFlag = true;
            continue;

          case MissionEventType.Kill:
            break;

          default:
            continue;
        }

        KillEvent killEvent = evt as KillEvent;

        if (killEvent == null)
          continue;

        if (stopFlag) // we can't kill anymore
        {
          killEvent.SetIgnore();
          continue;
        }

        MissionObject target = killEvent.GetTargetObject();

        if (target == null)
          continue;

        MissionObject attacker = killEvent.GetAttackerObject();

        // ignore self killing
        if (target == attacker)
          continue;

        ++psi.KilledUnitsCount[(int)target.UnitType].Count;
        psi.KilledUnitsCount[(int)target.UnitType].KilledList.Add(
            new KilledByPlayerUnitInfo(target.ObjectId, target.Type, target.Position, attacker, killEvent.Timestamp));
      }
    }

    public PlayerStatsInfo GetStats(CoalitionBind[] countries)
    {
      PlayerStatsInfo psi = new PlayerStatsInfo(kills, Name, GetPriorityCountry(countries), this);

      foreach (PlayerGameSession session in sessionList)
        UpdateStatsWithEvents(session.AllEvents, psi);

      return psi;
    }

    public Player(PlayerPlaneEntry entry, MissionLog mlog) : base(MissionObjectType.Player, UnitType.Bot)
    {
      id = -1;

      parentId = entry.ParentId;
      parent = mlog.GetObject(parentId);
      name = entry.Name;
    }

    public PlayerGameSession GetCurrentGameSession()
    {
      return currentGameSession;
    }

    public Vehicle GetCurrentVehicle()
    {
      return currentGameSession.GetVehicleObject();
    }

    public void IncreaseKillDeathsCounter(KillDeathType type)
    {
      // unsafe
      ++kills[(int)type];
    }

    public override void AddEvent(MissionEvent evt)
    {
      base.AddEvent(evt);
      currentGameSession.AddEvent(evt);
    }

    public void StartNewGameSession(PlayerPlaneEntry entry, Vehicle vehicle, MissionObject pilot)
    {
      currentGameSession = new PlayerGameSession(vehicle, pilot, entry.Country, entry.Position, entry.Skin);
      sessionList.Add(currentGameSession);
    }

    public bool HadVehicleId(int vehicleId)
    {
      foreach (PlayerGameSession session in sessionList)
        if (session.VehicleId == vehicleId)
          return true;

      return false;
    }

    public Country GetPriorityCountry(CoalitionBind[] countries)
    {
      if (!ChangedCountry)
        return Country;

      int[] counters = new int[countries.Length];
      int max = 0;
      int maxIndex = 0;
      bool same = false;

      foreach (PlayerGameSession session in sessionList)
      {
        int index = Utility.CountryIndex(countries, session.Country);

        if (index == -1)
          continue;

        ++counters[index];

        if (counters[index] < max)
          continue;
        else if (counters[index] == max)
        {
          same = true;
          continue;
        }
        else
          same = false;

        maxIndex = index;
        max = counters[maxIndex];
      }


      return same ? Country.Neutral : countries[maxIndex].country;
    }

    public void FinishSessions()
    {
      Country country = currentGameSession.Country;

      foreach (PlayerGameSession session in sessionList)
      {
        session.FinishSession(this);

        if (session.Country != country)
          ChangedCountry = true;
      }
    }

    public bool HavePlayerEndEvent()
    {
      return currentGameSession.HavePlayerEndEvent();
    }
  }
}

