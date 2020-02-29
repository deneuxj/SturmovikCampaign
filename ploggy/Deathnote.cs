using System;
using System.Collections.Generic;

namespace ploggy
{
  /// <summary>
  /// Guessed death reason for destroyed object.
  /// Предполагаемая причина смерти уничтоженного объекта
  /// </summary>
  public enum DeathReason
  {
    /// <summary>
    /// Object was destroyed by enemy unit
    /// 
    /// Объект был уничтожен вражеским юнитом
    /// </summary>
    EnemyUnit,

    /// <summary>
    /// Object was destroyed by friendly unit
    /// 
    /// Объект был уничтожен дружественным юнитом
    /// </summary>
    FriendlyUnit,

    /// <summary>
    /// Object was destroyed by neutral unit
    /// 
    /// Объект был уничтожен нейтральным юнитом
    /// </summary>
    NeutralUnit,

    /// <summary>
    /// Reserved for event of death from suffocation
    /// 
    /// Зарезервировно для смерти от удушия
    /// </summary>
    Suffocated,

    /// <summary>
    /// Unit crashed
    /// 
    /// Объект разбился
    /// </summary>
    Crashed,

    /// <summary>
    /// Unit was killed without damage events, probably by mission logics
    /// 
    /// Объект был уничтожен без событий о повреждениях, скорее всего удален логикой миссии
    /// </summary>
    MissonLogics,

    /// <summary>
    /// Unknown reason is set, when others are no applicable
    /// 
    /// Устанавливается, если остальные причины не подходят
    /// </summary>
    Unknown,

    /// <summary>
    /// Parent object is dead
    /// 
    /// Родительский объект, например, турель для бота уничтожен
    /// </summary>
    DeadParent,

    /// <summary>
    /// object killed himself
    /// </summary>
    Suicide

  }

  /// <summary>
  /// Accumulates total damage by one attacking object to victim
  /// 
  /// Общий урон, нанесенный атакующим данном объекту
  /// </summary>
  public struct DamageRecord
  {
    private MissionObject damager;
    private float damage;
    private float share;

    /// <summary>
    /// Human readable id of MisisonObject, which total dealed damage is stored in this record
    /// 
    /// Человекочитаемый текст
    /// </summary>
    public string Damager { get { return MissionObject.GetSimpleIdPreferPlayerName(damager); } }
    public float Share { get { return share; } }
    public float Damage { get { return damage; } }

    public void Set(MissionObject o, float damage, float share)
    {
      damager = o;
      this.share = share;
      this.damage = damage;
    }

    public MissionObject GetWhoKilled()
    {
      return damager;
    }
  }

  public interface DeathSourceClassifier
  {
    Player GetKillerPlayer();
    Player GetTargetPlayer();
    MissionObject GetKillerObject();
    MissionObject GetTargetObject();
    string GetAmmoType();
    DeathSourceClassifier Create(MissionObject obj, MissionLog missionLog);
  }

  public class Deathnote : DeathSourceClassifier
  {
    private static int maxSeconds = 30;
    private static float minValidDamage = 0.35f;

    private DamageRecord[] damageRecord = null;
    private MissionObject deadObject = MissionObject.None;
    private MissionObject lastDamager = MissionObject.None;
    private MissionObject maximumDamager = MissionObject.None;
    private Position3d lastPosition = Position3d.Zero;
    private DeathReason deathReason = DeathReason.Unknown;
    private string ammoType = null;
    private float maxShare = 0.0f;
    private Player maximumDamagerPlayer = null;
    private Player targetPlayer = null;

    public static int GroundFilterMaxSeconds { get { return maxSeconds; } set { maxSeconds = value; } }
    public static float MinValidDamage { get { return minValidDamage; } set { minValidDamage = value; } }

    public TimeSpan Timestamp;
    public int ObjectId { get { return deadObject.ObjectId; } }
    public string ObjectName { get { return MissionObject.GetNameSafe(deadObject); } }
    public string ObjectType { get { return MissionObject.GetObjectTypeStrSafe(deadObject); } }
    public string Player { get { return MissionObject.GetNameSafe(targetPlayer); } }

    public DeathReason DeathReason { get { return deathReason; } }
    public Position3d DeathPosition { get { return lastPosition; } }
    public DamageRecord[] DamageRecords { get { return damageRecord; } }

    public string MaximumDamager { get { return MissionObject.GetSimpleIdPreferPlayerName(maximumDamager); } }
    public string FinalBlow { get { return MissionObject.GetSimpleIdPreferPlayerName(lastDamager); } }
    public string AmmoType { get { return ammoType; } }

    #region interface implementaion
    public MissionObject GetKillerObject()
    {
      return maximumDamager;
    }

    public MissionObject GetTargetObject()
    {
      return deadObject;
    }

    public Player GetKillerPlayer()
    {
      return maximumDamagerPlayer;
    }

    public Player GetTargetPlayer()
    {
      return targetPlayer;
    }

    public string GetAmmoType()
    {
      return ammoType;
    }

    public DeathSourceClassifier Create(MissionObject obj, MissionLog missionLog)
    {
      return new Deathnote(obj, missionLog.MissionCountries);
    }
    #endregion

    private void SetAmmo(List<MissionEvent> events, MissionObject attacker)
    {
      if (attacker == null)
        return;

      int count = events.Count;

      for (int i = count - 1; i >= 0; --i)
      {
        MissionEvent evt = events[i];

        if (evt.Event != MissionEventType.GotHit)
          continue;

        GotHitEvent gotHitEvent = (GotHitEvent)evt;

        if (gotHitEvent.GetAttackerObject() != attacker)
          continue;

        ammoType = gotHitEvent.AmmoType;
        break;
      }
    }

    private void SetDeathReason(CoalitionBind[] coalitions, MissionObject a, MissionObject b)
    {
      switch (Utility.GetRelationship(coalitions, a.Country, b.Country))
      {
        case Relationship.Enemy:
          deathReason = DeathReason.EnemyUnit;
          break;

        case Relationship.Friendly:
          deathReason = DeathReason.FriendlyUnit;
          break;

        default:
          deathReason = DeathReason.NeutralUnit;
          break;
      }
    }

    private void GuessDeathReason(CoalitionBind[] coalitions)
    {
      if (maximumDamager == deadObject)
      {
        deathReason = DeathReason.Suicide;
        return;
      }

      if (lastDamager != MissionObject.None)
      {
        SetDeathReason(coalitions, lastDamager, deadObject);
        return;
      }

      if ((maximumDamager != null) && (maxShare > 70.0f))
      {
        SetDeathReason(coalitions, maximumDamager, deadObject);
        return;
      }

      if (deadObject.MissionObjectType != MissionObjectType.Pilot)
      {
        deathReason = DeathReason.Crashed;
        return;
      }

      MissionObject parent = deadObject.GetParent();

      if ((parent != null) && !parent.IsAlive())
        deathReason = DeathReason.DeadParent;

      float height = lastPosition.Height();

      if (height > 3000.0f)
        deathReason = DeathReason.Suffocated;
      else if (height < 100.0f)
        deathReason = DeathReason.Crashed;
    }

    private Deathnote(MissionObject obj, CoalitionBind[] coalitions)
    {
      deadObject = obj;
      List<MissionEvent> events = obj.GetEvents();
      Dictionary<MissionObject, float> attackedBy = new Dictionary<MissionObject, float>(8);
      GotDamageEvent lastDamageEvent = null;
      TimeSpan lastIdentifiedTime = new TimeSpan(0);

      foreach (MissionEvent evt in events)
      {
        switch (evt.Event)
        {
          case MissionEventType.GotDamage:
            break;

          case MissionEventType.Death:
          case MissionEventType.BotDeath:
          case MissionEventType.VehicleDestroyed:
          case MissionEventType.Suicide:
            Timestamp = evt.Timestamp;
            lastPosition = ((DeathEvent)evt).Position;
            continue;

          default:
            continue;
        }

        GotDamageEvent damageEvent = evt as GotDamageEvent;
        lastDamageEvent = damageEvent;

        float curTotal = 0.0f;
        MissionObject key = damageEvent.AttackerId != -1 ? damageEvent.GetAttackerObject() : MissionObject.None;

        if (key == null)
          key = MissionObject.None;
        else
          lastIdentifiedTime = evt.Timestamp;

        if (!attackedBy.TryGetValue(key, out curTotal))
        {
          attackedBy.Add(key, damageEvent.Damage);
          continue;
        }

        curTotal += damageEvent.Damage;
        attackedBy[key] = curTotal;
        lastDamager = key;
        lastPosition = damageEvent.Position;

      }

      if (attackedBy.Count == 0)
      {
        if (events.Count == 0) // XXX: need to log this
          return;

        MissionEvent evt = events[0];

        MissionObject deathAttacker = null;

        switch (evt.Event)
        {
          case MissionEventType.BotDeath:
          case MissionEventType.VehicleDestroyed:
          case MissionEventType.Death:
          case MissionEventType.Suicide:
            {
              DeathEvent deathEvent = (DeathEvent)evt;
              deathAttacker = deathEvent.GetAttackerObject();
            }
            break;
        }

        lastDamager = deathAttacker;
        maximumDamager = deathAttacker;
        deathReason = DeathReason.MissonLogics;

        MissionObject parent = deadObject.GetParent();

        if ((parent != null) && !parent.IsAlive())
          deathReason = DeathReason.DeadParent;
      }

      damageRecord = new DamageRecord[attackedBy.Count];
      int count = 0;

      // calculate share and maximum damage
      float total = 0.0f;
      foreach (var kvp in attackedBy)
        total += kvp.Value;

      if (total == 0.0f)
        return;

      maxShare = 0.0f;

      foreach (var kvp in attackedBy)
      {
        damageRecord[count].Set(kvp.Key != MissionObject.None ? kvp.Key : null,
            kvp.Value,
            100.0f * kvp.Value / total);

        if (damageRecord[count].Share > maxShare)
        {
          maxShare = damageRecord[count].Share;
          maximumDamager = damageRecord[count].GetWhoKilled();
        }

        ++count;
      }

      if (maxSeconds > 0)
        CorrectDamager(lastDamageEvent, lastIdentifiedTime);

      maximumDamagerPlayer = MissionObject.GetPlayer(maximumDamager);
      targetPlayer = MissionObject.GetPlayer(deadObject);
      GuessDeathReason(coalitions);
      SetAmmo(events, maximumDamager);
    }

    private void CorrectDamager(GotDamageEvent lastDamageEvent, TimeSpan lastIdentifiedTime)
    {
      if (maximumDamager != null)
        return;

      // no any damagers, no need to change anything
      if (damageRecord == null)
        return;

      // no other damagers
      if (damageRecord.Length == 1)
        return;

      // got damage after maximum damage by unknown source
      // means unknown source probably was not crash, caused by earlier received damage
      if (lastDamageEvent.AttackerId != -1)
        return;

      // if much time passed, then probably it is crash not erlated to previous damage
      int seconds = (lastDamageEvent.Timestamp - lastIdentifiedTime).Duration().Seconds;

      if (seconds > maxSeconds)
        return;

      // if we're here, then one of other damagers is the reason of death
      float maxDamage = 0.0f;
      MissionObject newMaximumDamager = null;

      for (int i = 0; i < damageRecord.Length; ++i)
      {
        if (damageRecord[i].Damage < maxDamage)
          continue;

        if (damageRecord[i].Damage < minValidDamage)
          continue;

        MissionObject damager = damageRecord[i].GetWhoKilled();

        if (damager == null)
          continue;

        newMaximumDamager = damager;
        maxDamage = damageRecord[i].Damage;
      }

      if (newMaximumDamager == null)
        return;

      maximumDamager = newMaximumDamager;
    }

    public Deathnote()
    {
    }
  }
}

