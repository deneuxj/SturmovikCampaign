using System;

namespace ploggy
{
  public enum MissionEventType
  {
    None,

    GotHit,
    Hit,
    GotDamage,
    Damage,

    Kill,
    Death,
    VehicleDestroyed,
    BotDeath,
    Suicide,

    AmmunitionStats,
    TakeOff,
    Ejected,
    Landing,

    PlayerBegin,
    PlayerEnd,

    MissionStart,
    MissionObjective,
    RoundEnd,
    MissionEnd
  }

  public class MissionEvent
  {
    private MissionEventType eventType = MissionEventType.None;
    private TimeSpan timestamp;

    public TimeSpan Timestamp { get { return timestamp; } }
    public MissionEventType Event { get { return eventType; } }

    public MissionEvent(LogEntry entry, MissionEventType eventType)
    {
      this.timestamp = entry.Timestamp;
      this.eventType = eventType;
    }
  }

  public class GotHitEvent : MissionEvent
  {
    private string ammoType = null;
    private int attackerId = -1;
    private MissionObject attacker = null;
    private MissionObject target = null;
    private Player attackerPlayer = null;

    public string AmmoType { get { return ammoType; } }
    public int AttackerId { get { return attackerId; } }
    public string Attacker { get { return MissionObject.GetNameSafe(attacker); } }
    public string AttackerType { get { return MissionObject.GetObjectTypeStrSafe(attacker); } }
    public string AttackerPlayer { get { return MissionObject.GetNameSafe(attackerPlayer); } }

    public GotHitEvent(HitEntry entry, MissionObject attacker, Player attackerPlayer, MissionObject target) :
        base(entry, MissionEventType.GotHit)
    {
      ammoType = entry.AmmoType;
      attackerId = entry.AttackerId;
      this.attacker = attacker;
      this.attackerPlayer = attackerPlayer;
      this.target = target;
    }

    public MissionObject GetAttackerObject()
    {
      return attacker;
    }

    public MissionObject GetTargetObject()
    {
      return target;
    }
  }

  public class HitEvent : MissionEvent
  {
    private string ammoType = null;
    private int targetId = -1;
    private MissionObject target = null;
    private MissionObject attacker = null;
    private Player targetPlayer = null;

    public string AmmoType { get { return ammoType; } }
    public int TargetId { get { return targetId; } }
    public string Target { get { return MissionObject.GetNameSafe(target); } }
    public string TargetType { get { return MissionObject.GetObjectTypeStrSafe(target); } }
    public string TargetPlayer { get { return MissionObject.GetNameSafe(targetPlayer); } }

    public HitEvent(HitEntry entry, MissionObject target, Player targetPlayer, MissionObject attacker) :
        base(entry, MissionEventType.Hit)
    {
      ammoType = entry.AmmoType;
      targetId = entry.TargetId;
      this.target = target;
      this.targetPlayer = targetPlayer;
      this.attacker = attacker;
    }

    public MissionObject GetAttackerObject()
    {
      return attacker;
    }

    public MissionObject GetTargetObject()
    {
      return target;
    }
  }

  public class GotDamageEvent : MissionEvent
  {
    private float damage = 0.0f;
    private int attackerId = -1;
    private MissionObject attacker = null;
    private MissionObject target = null;
    private Position3d position = Position3d.Zero;
    private Player attackerPlayer = null;

    public Position3d Position { get { return position; } }
    public float Damage { get { return damage; } }

    public int AttackerId { get { return attackerId; } }
    public string Attacker { get { return MissionObject.GetNameSafe(attacker); } }
    public string AttackerType { get { return MissionObject.GetObjectTypeStrSafe(attacker); } }
    public string AttackerPlayer { get { return MissionObject.GetNameSafe(attackerPlayer); } }

    public GotDamageEvent(DamageEntry entry, MissionObject attacker, Player attackerPlayer, MissionObject target) :
        base(entry, MissionEventType.GotDamage)
    {
      attackerId = entry.AttackerId;
      this.attacker = attacker;
      this.attackerPlayer = attackerPlayer;
      this.target = target;
      position = entry.Position;
      damage = entry.Damage;
    }

    public MissionObject GetAttackerObject()
    {
      return attacker;
    }

    public MissionObject GetTargetObject()
    {
      return target;
    }
  }

  public class DamageEvent : MissionEvent
  {
    private float damage = 0.0f;
    private int targetId = -1;
    private MissionObject target = null;
    private MissionObject attacker = null;
    private Position3d position = Position3d.Zero;
    private Player targetPlayer = null;

    public Position3d Position { get { return position; } }
    public float Damage { get { return damage; } }

    public string Target { get { return MissionObject.GetNameSafe(target); } }
    public int TargetId { get { return targetId; } }
    public string TargetType { get { return MissionObject.GetObjectTypeStrSafe(target); } }
    public string TargetPlayer { get { return MissionObject.GetNameSafe(targetPlayer); } }

    public string Attacker { get { return MissionObject.GetNameSafe(attacker); } }
    public string AttackerType { get { return MissionObject.GetObjectTypeStrSafe(attacker); } }
    public string AttackerPlayer { get { return MissionObject.GetPlayerName(attacker); } }



    public DamageEvent(DamageEntry entry, MissionObject target, Player targetPlayer, MissionObject attacker) :
        base(entry, MissionEventType.Damage)
    {
      targetId = entry.TargetId;
      this.target = target;
      this.targetPlayer = targetPlayer;
      damage = entry.Damage;
      position = entry.Position;
    }

    public MissionObject GetTargetObject()
    {
      return target;
    }

    public MissionObject GetAttackerObject()
    {
      return attacker;
    }
  }

  public class KillEvent : MissionEvent
  {
    private int targetId = -1;
    private MissionObject target = null;
    private int attackerId = -1;
    private MissionObject attacker = null;
    private Position3d position = Position3d.Zero;
    private Country country = Country.None;
    private bool ignore = false;

    public Country Country { get { return country; } }

    public string Target { get { return MissionObject.GetNameSafe(target); } }
    public string TargetPlayer { get { return MissionObject.GetPlayerName(target); } }
    public int TargetId { get { return targetId; } }
    public string TargetType { get { return target != null ? target.Type : null; } }

    public string Attacker { get { return MissionObject.GetNameSafe(attacker); } }
    public string AttackerPlayer { get { return MissionObject.GetPlayerName(attacker); } }
    public int AtackerId { get { return attackerId; } }
    public string AttackerType { get { return MissionObject.GetObjectTypeStrSafe(attacker); } }

    public Position3d Position { get { return position; } }

    public KillEvent(KillEntry entry, MissionObject attacker, MissionObject target, Country country) : base(entry, MissionEventType.Kill)
    {
      this.target = target;
      targetId = entry.TargetId;
      this.attacker = attacker;
      attackerId = entry.AttackerId;
      position = entry.Position;
      this.country = country;
    }

    public bool IsIgnored()
    {
      return ignore;
    }

    public void SetIgnore()
    {
      ignore = true;
    }

    public MissionObject GetTargetObject()
    {
      return target;
    }

    public MissionObject GetAttackerObject()
    {
      return attacker;
    }
  }

  public class DeathEvent : MissionEvent
  {
    protected int attackerId = -1;
    protected MissionObject attacker = null;
    protected Position3d position = Position3d.Zero;
    protected int targetId = -1;
    protected MissionObject target = null;
    protected Player targetPlayer = null;
    protected bool ignore = false;

    public string Attacker { get { return MissionObject.GetNameSafe(attacker); } }
    public int AttackerId { get { return attackerId; } }
    public string AttackerType { get { return MissionObject.GetObjectTypeStrSafe(attacker); } }
    public string AttackerPlayer { get { return MissionObject.GetPlayerName(attacker); } }

    public string Target { get { return MissionObject.GetNameSafe(target); } }
    public int TargetId { get { return targetId; } }
    public string TargetType { get { return MissionObject.GetObjectTypeStrSafe(target); } }
    public string TargetPlayer { get { return MissionObject.GetNameSafe(targetPlayer); } }

    public Position3d Position { get { return position; } }

    public DeathEvent(KillEntry entry, MissionObject attacker, MissionObject target, Player targetPlayer) : base(entry, MissionEventType.Death)
    {
      this.attacker = attacker;
      attackerId = entry.AttackerId;
      position = entry.Position;
      this.target = target;
      this.targetPlayer = targetPlayer;
      targetId = entry.TargetId;
    }

    public void SetIgnore()
    {
      ignore = true;
    }

    public bool IsIgnored()
    {
      return ignore;
    }

    public DeathEvent(KillEntry entry, MissionObject attacker, MissionObject target, Player targetPlayer, MissionEventType evtType)
        : base(entry, evtType)
    {
      this.attacker = attacker;
      attackerId = entry.AttackerId;
      position = entry.Position;
      this.target = target;
      this.targetPlayer = targetPlayer;
      targetId = entry.TargetId;
    }

    public MissionObject GetAttackerObject()
    {
      return attacker;
    }

    public MissionObject GetTargetObject()
    {
      return target;
    }

    public Player GetTargetPlayer()
    {
      return targetPlayer;
    }
  }

  public class BotDeathEvent : DeathEvent
  {
    public BotDeathEvent(KillEntry entry, MissionObject attacker, MissionObject target, Player targetPlayer)
        : base(entry, attacker, target, targetPlayer, MissionEventType.BotDeath)
    {
    }

    public static UnitType GetBotType(BotDeathEvent bde)
    {
      if (bde == null)
        return UnitType.Unknown;

      return MissionObject.GetBotType(bde.target);
    }

    public UnitType GetBotType()
    {
      return MissionObject.GetBotType(target);
    }
  }

  public class VehicleDestroyedEvent : DeathEvent
  {
    public VehicleDestroyedEvent(KillEntry entry, MissionObject attacker, MissionObject target, Player targetPlayer)
        : base(entry, attacker, target, targetPlayer, MissionEventType.VehicleDestroyed)
    {
    }
  }

  public class SuicideEvent : DeathEvent
  {
    public SuicideEvent(KillEntry entry, MissionObject target, Player targetPlayer)
        : base(entry, target, target, targetPlayer, MissionEventType.Suicide)
    {
    }
  }

  public class AmmunitionStatsEvent : MissionEvent
  {
    #region Instance Vars
    private int bullets = 0;
    private int shells = 0;
    private int bombs = 0;
    private int rockets = 0;
    private Position3d position = Position3d.Zero;
    private float fuel = -1.0f;
    #endregion

    #region Properties
    public int Bullets { get { return bullets; } }
    public int Shells { get { return shells; } }
    public int Bombs { get { return bombs; } }
    public int Rockets { get { return rockets; } }
    public Position3d Position { get { return position; } }
    public float Fuel { get { return fuel; } }
    #endregion

    public AmmunitionStatsEvent(PlayerMissionEndEntry entry) : base(entry, MissionEventType.AmmunitionStats)
    {
      bullets = entry.Bullets;
      bombs = entry.Bombs;
      shells = entry.Shells;
      rockets = entry.Rockets;
      position = entry.Position;
      fuel = -1.0f;
    }

    public AmmunitionStatsEvent(PlayerPlaneEntry entry) : base(entry, MissionEventType.AmmunitionStats)
    {
      bullets = entry.Bullets;
      bombs = entry.Bombs;
      shells = entry.Shells;
      rockets = entry.Rockets;
      position = entry.Position;
      fuel = entry.Fuel;
    }
  }

  public class TakeOffEvent : MissionEvent
  {
    private Position3d position = Position3d.Zero;
    private int vehicleId = -1;
    private MissionObject vehicle = null;
    private Player player = null;

    public Position3d Position { get { return position; } }
    public int VehicleId { get { return vehicleId; } }
    public string Vehicle { get { return MissionObject.GetNameSafe(vehicle); } }
    public string VehicleType { get { return MissionObject.GetObjectTypeStrSafe(vehicle); } }
    public string Player { get { return MissionObject.GetNameSafe(player); } }

    public TakeOffEvent(TakeOffEntry entry, MissionObject vehicle, Player player) : base(entry, MissionEventType.TakeOff)
    {
      position = entry.Position;
      vehicleId = entry.VehicleId;
      this.vehicle = vehicle;
      this.player = player;
    }
  }

  public class LandingEvent : MissionEvent
  {
    private Position3d position = Position3d.Zero;
    private int vehicleId = -1;
    private Vehicle vehicle = null;
    private Player player = null;

    public Position3d Position { get { return position; } }
    public int VehicleId { get { return vehicleId; } }
    public string Vehicle { get { return MissionObject.GetNameSafe(vehicle); } }
    public string VehicleType { get { return MissionObject.GetObjectTypeStrSafe(vehicle); } }
    public string Player { get { return MissionObject.GetNameSafe(player); } }

    public LandingEvent(LandingEntry entry, Vehicle vehicle, Player player) : base(entry, MissionEventType.Landing)
    {
      position = entry.Position;
      vehicleId = entry.VehicleId;
      this.vehicle = vehicle;
      this.player = player;
    }

    public MissionObject GetVehicle()
    {
      return vehicle;
    }
  }

  public class PlayerEnd : MissionEvent
  {
    private Position3d position = Position3d.Zero;
    private int vehicleId = -1;
    private MissionObject vehicle = null;

    public Position3d Position { get { return position; } }
    public int VehicleId { get { return vehicleId; } }
    public string Vehicle { get { return MissionObject.GetNameSafe(vehicle); } }
    public string VehicleType { get { return MissionObject.GetObjectTypeStrSafe(vehicle); } }

    public PlayerEnd(PlayerMissionEndEntry entry, MissionObject vehicle) : base(entry, MissionEventType.PlayerEnd)
    {
      position = entry.Position;
      vehicleId = entry.VehicleId;
      this.vehicle = vehicle;
    }
  }

  public class MissionObjectiveEvent : MissionEvent
  {
    private int objectiveId = -1;
    private Position3d position = Position3d.Zero;
    private Coalition coalition = Coalition.Neutral;
    private ObjectiveType objectiveType = ObjectiveType.Secondary;
    private bool objectiveCompleted = false;
    private int iconType = -1;

    public int MissionObjectiveId { get { return objectiveId; } }
    public ObjectiveType ObjectiveType { get { return objectiveType; } }
    public Coalition Coalition { get { return coalition; } }
    public bool Completed { get { return objectiveCompleted; } }
    public Position3d Position { get { return position; } }
    public int IconType { get { return iconType; } }

    public MissionObjectiveEvent(MissionObjectiveEntry objEntry) : base(objEntry, MissionEventType.MissionObjective)
    {
      objectiveId = objEntry.MissionObjectiveId;
      position = objEntry.Position;
      coalition = objEntry.Coalition;
      objectiveType = objEntry.ObjectiveType;
      objectiveCompleted = objEntry.Completed;
      iconType = objEntry.IconType;
    }
  }

  public class EjectEvent : MissionEvent
  {
    private int botId = -1;
    private MissionObject bot = null;
    private Vehicle vehicle = null;
    private Position3d position = Position3d.Zero;

    public int BotId { get { return botId; } }
    public string Bot { get { return MissionObject.GetNameSafe(bot); } }
    public string VehicleType { get { return MissionObject.GetObjectTypeStrSafe(vehicle); } }
    public Position3d Position { get { return position; } }
    public string Player { get { return MissionObject.GetPlayerName(bot); } }

    public EjectEvent(BotEjectEntry entry, MissionObject bot, Vehicle vehicle) : base(entry, MissionEventType.Ejected)
    {
      botId = entry.BotId;
      this.vehicle = vehicle;
      this.bot = bot;
      position = entry.Position;
    }
  }

  public class PlayerBeginEvent : MissionEvent
  {
    private int vehicleId = -1;
    private int pilotId = -1;
    private int bullets = 0;
    private int shells = 0;
    private int bombs = 0;
    private int rockets = 0;

    private PlaneStartType startType = 0;
    private string name = null;
    private Position3d position = Position3d.Zero;
    private Guid userId = Guid.Empty;
    private string type = null;
    private Country country = Country.None;
    private int positionInFormation = 0;
    private int fieldId = -1;
    private int parentId = -1;
    private int payload = 0;
    private float fuel = 0.0f;
    private int weaponModMask = 0;
    private string skin = null;

    private MissionObject vehicle = null;
    private MissionObject pilot = null;
    private MissionObject parent = null;

    public int VehicleId { get { return vehicleId; } }
    public string Vehicle { get { return MissionObject.GetNameSafe(vehicle); } }
    public string VehicleType { get { return type; } }

    public int PilotId { get { return pilotId; } }
    public string Pilot { get { return MissionObject.GetNameSafe(pilot); } }

    public int ParentId { get { return parentId; } }
    public string Parent { get { return MissionObject.GetNameSafe(parent); } }

    public Position3d Position { get { return position; } }

    public Guid UserId { get { return userId; } }
    public string Name { get { return name; } }

    public int Bullets { get { return bullets; } }
    public int Shells { get { return shells; } }
    public int Bombs { get { return bombs; } }
    public int Rockets { get { return rockets; } }

    public string Skin { get { return skin; } }

    public Country Country { get { return country; } }
    public int FieldId { get { return fieldId; } }
    public int Formation { get { return positionInFormation; } }

    public PlaneStartType StartType { get { return startType; } }
    public float Fuel { get { return fuel; } }
    public int Payload { get { return payload; } }
    public int WeaponModMask { get { return weaponModMask; } }


    public PlayerBeginEvent(PlayerPlaneEntry entry, MissionLog mlog) : base(entry, MissionEventType.PlayerBegin)
    {
      startType = entry.StartType;
      name = entry.Name;
      userId = entry.UserId;

      vehicleId = entry.VehicleId;
      pilotId = entry.PilotId;
      bullets = entry.Bullets;
      shells = entry.Shells;
      bombs = entry.Bombs;
      rockets = entry.Rockets;
      position = entry.Position;
      type = entry.VehicleType;

      country = entry.Country;
      positionInFormation = entry.Formation;
      fieldId = entry.FieldId;

      parentId = entry.ParentId;
      payload = entry.Payload;
      fuel = entry.Fuel;
      weaponModMask = entry.WeaponModMask;
      skin = entry.Skin;

      vehicle = mlog.GetObject(vehicleId);
      parent = mlog.GetObject(parentId);
      pilot = mlog.GetObject(pilotId);
    }
  }
}

