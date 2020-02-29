using System;
using System.Collections.Generic;

namespace ploggy
{
  public class MissionLog
  {
#if DEBUG_INTERESTING
        public struct ObjectMapItem
        {
            public int Id;
            public string Name;
            public string Type;

            public ObjectMapItem(int Id, string Name, string Type)
            {
                this.Id = Id;
                this.Name = Name;
                this.Type = Type;
            }
        }

        private List<ObjectMapItem> AllObjects = new List<ObjectMapItem>();
        private List<LogEntry>      interestingObjectLog = new List<LogEntry>(32);
        public List<LogEntry>       InterestingObject { get { return interestingObjectLog; } }
        private int[] interestingId = { 407551 };
#endif

    private string missionFile = null;
    private DateTime missionTime;
    private CoalitionBind[] missionCountries = null;
    private MissionType missionType = MissionType.None;
    private GameSettings missionSettings = null;
    private Player[] playerArray = null;
    private List<MissionObjectiveEvent> objectiveEvents = new List<MissionObjectiveEvent>(4);

    public MissionType MissionType { get { return missionType; } }
    public string MissionFile { get { return missionFile; } }
    public DateTime MissionTime { get { return missionTime; } }
    public CoalitionBind[] MissionCountries { get { return missionCountries; } }
    public GameSettings MissionSettings { get { return missionSettings; } }

    public MissionStats MissionStats { get { return missionStats; } }
    public List<MissionObjectiveEvent> MissionObjectives { get { return objectiveEvents; } }
    public List<MissionEvent> MissionFlow { get { return missionFlow; } }
    public List<DeathSourceClassifier> DeadStats { get { return deathLog; } }
    public Player[] AllPlayers { get { return playerArray; } }

    private Dictionary<int, MissionObject> objectMap = new Dictionary<int, MissionObject>(1024);
    private Dictionary<int, MissionObject> inactiveObjectsMap = new Dictionary<int, MissionObject>(1024);
    private Dictionary<Guid, Player> playerMap = new Dictionary<Guid, Player>(64);
    private Dictionary<int, Player> pilotToPlayerMap = new Dictionary<int, Player>(64);
    private List<DeathSourceClassifier> deathLog = new List<DeathSourceClassifier>(128);
    private GameObjectsData gameObjectsData = null;

    private List<MissionObject> inactiveObjects = new List<MissionObject>(256);
    private List<LogEntry> log = new List<LogEntry>(1024);

    private int ignoredEntries = 0;

    private List<MissionEvent> missionFlow = new List<MissionEvent>(128);
    private MissionStats missionStats = null;

    private int[] logEntries = new int[(int)LogEntryType.All];
    private DeathSourceClassifier deathnoteProto = null;

    public MissionLog(LogFile logFile, GameObjectsData gameObjectsData = null, DeathSourceClassifier deathnoteProto = null)
    {
      this.gameObjectsData = gameObjectsData;
      this.deathnoteProto = deathnoteProto != null ? deathnoteProto : new Deathnote();

      Plogger.Info("Processing log.");
      foreach (LogEntry entry in logFile.Entries)
        AddLogEntry(entry);

      Plogger.Info("Processed entries:");

      for (int i = 0; i < (int)LogEntryType.All; ++i)
      {
        Plogger.Info("\t{0}: {1}", (LogEntryType)i, logEntries[i]);
      }

      Plogger.Info("Total log entries {0}, ignored {1}.", logFile.Entries.Count, ignoredEntries);

      playerArray = new Player[playerMap.Values.Count];
      playerMap.Values.CopyTo(playerArray, 0);

      Analyze();
    }

    private void Analyze()
    {
      foreach (Player player in playerMap.Values)
        player.FinishSessions();

      missionStats = new MissionStats(missionCountries);

      foreach (DeathSourceClassifier note in DeadStats)
      {
        // ignore self killed from statistics
        /*
         if ((note.DeathReason == DeathReason.Suicide) ||
            (note.DeathReason == DeathReason.DeadParent))
            continue;
        */

        MissionObject killerObject = note.GetKillerObject();
        MissionObject target = note.GetTargetObject();

        if (killerObject != null)
        {
          Coalition killerCoalition = Utility.GetCountryCoalition(missionCountries, killerObject.Country);

          missionStats.AddKilledUnit(killerCoalition, killerObject.Country, target, GetPlayerFromObject(killerObject) ?? killerObject);
        }

        Player killer = note.GetKillerPlayer();

        if (killer == null)
          continue;

        // count here obly man operated
        Player targetPlayer = note.GetTargetPlayer();

        if (targetPlayer == null)
          continue;

        // count vehicles independently
        if (target.MissionObjectType == MissionObjectType.Vehicle)
        {
          if (targetPlayer != killer)
            killer.IncreaseKillDeathsCounter(KillDeathType.VehicleKill);
          continue;
        }

        if (target.MissionObjectType != MissionObjectType.Pilot)
          continue;

        // there may be gunners and pilots
        // need to check parent object
        MissionObject parent = GetObjectEvenIfInactive(target.ParentId);

        if (parent == null)
        {
          // XXX: may be killed paratrooper
          killer.IncreaseKillDeathsCounter(KillDeathType.OtherKill);
          continue;
        }

        switch (parent.UnitType)
        {
          // count as dead pilot
          case UnitType.Fighter:
          case UnitType.Attacker:
          case UnitType.Bomber:
            {
              Coalition coalition = Utility.GetCountryCoalition(missionCountries, killer.Country);
              FragInfo frag = new FragInfo(killer, targetPlayer, target.Position, target.LastEventTime(), killer.Country, coalition, note.GetAmmoType());
              missionStats.AddFrag(frag);
              killer.IncreaseKillDeathsCounter(KillDeathType.PilotKill);
            }
            break;

          // count as gunner
          case UnitType.Turret:
            killer.IncreaseKillDeathsCounter(KillDeathType.GunnerKill);
            break;
        }
      }
    }

#if DEBUG_INTERESTING
        public bool IsInteresting(int id)
        {
            if (id == -1)
                return false;

            for (int i = 0; i < interestingId.Length; ++i)
            {
                if (interestingId[i] == id)
                    return true;


                MissionObject obj = GetObjectEvenIfInactive(id);

                while (obj != null)
                {
                    if (obj.ParentId == interestingId[i])
                        return true;

                    obj = GetObjectEvenIfInactive(obj.ParentId);
                }
            }

            return false;
        }
#endif

    public Player GetSinglePlayer()
    {
      if (AllPlayers.Length == 0)
        return null;

      return AllPlayers[0];
    }

    public List<PlayerStatsInfo> GetAllPlayerStats()
    {
      List<PlayerStatsInfo> result = new List<PlayerStatsInfo>(AllPlayers.Length);

      for (int i = 0; i < AllPlayers.Length; ++i)
      {
        PlayerStatsInfo psi = AllPlayers[i].GetStats(missionCountries);
        result.Add(psi);
      }

      return result;
    }

    #region Utility
    public UnitType GetUnitType(string name)
    {
      UnitType unitType = gameObjectsData.GetUnitType(name);

      if (unitType == UnitType.Unknown)
        Plogger.Warn("Type of object '{0}' is unknown, will be accounted as Unknown.", name);

      return unitType;
    }

    public Player GetPlayerByGuid(Guid guid)
    {
      Player result = null;

      if (playerMap.TryGetValue(guid, out result))
        return result;

      return null;
    }

    public Player GetPlayerByPilotId(int pilotId)
    {
      Player player = null;

      if (pilotToPlayerMap.TryGetValue(pilotId, out player))
        return player;

      return null;
    }

    public Player GetPlayerByVehicleId(int vehicleId)
    {
      foreach (Player player in playerMap.Values)
      {
        if (player.VehicleId == vehicleId)
          return player;

        if (player.HadVehicleId(vehicleId))
          return player;
      }

      return null;
    }

    public Player GetPlayerByVehicle(Vehicle vehicle)
    {

      Player result = vehicle.GetPlayerFromChildren();

      return result ?? GetPlayerByVehicleId(vehicle.ObjectId);
    }

    public Player GetPlayerFromObject(MissionObject obj)
    {
      Player player = null;

      if (obj == null)
        return null;

      switch (obj.MissionObjectType)
      {
        case MissionObjectType.Pilot:
          player = GetPlayerByPilotId(obj.ObjectId);
          break;

        case MissionObjectType.Vehicle:
          player = GetPlayerByVehicle((Vehicle)obj);
          break;

        default:
          //player = GetPlayerByVehicleId(obj.ObjectId);
          break;
      }

      return player;
    }

    public MissionObject GetObject(int id)
    {
      if (id == -1)
        return null;

      MissionObject result = null;
      if (objectMap.TryGetValue(id, out result))
        return result;

      return null;
    }

    public MissionObject GetObjectEvenIfInactive(int id)
    {
      if (id == -1)
        return null;

      MissionObject result = null;
      if (objectMap.TryGetValue(id, out result))
        return result;

      if (inactiveObjectsMap.TryGetValue(id, out result))
      {
        if (!result.IsAlive())
          Plogger.Debug("Dead object id {0} ({1}) was referenced.", id, result.Type);
        else
          Plogger.Debug("Inactive object id {0} ({1}) was referenced.", id, result.Type);

        return result;
      }

      return null;
    }

    public MissionObject CheckInactiveObject(int id)
    {
      if (id == -1)
        return null;

      MissionObject result = null;

      if (!inactiveObjectsMap.TryGetValue(id, out result))
        return null;

      if (!result.IsAlive())
        Plogger.Debug("Dead object id {0} ({1}) was referenced.", id, result.Type);
      else
        Plogger.Debug("Inactive object id {0} ({1}) was referenced.", id, result.Type);

      return result;
    }

    public void AddToInactiveMap(MissionObject obj)
    {
      if (obj.ObjectId == -1)
        return;

      MissionObject prevObj = null;

      if (inactiveObjectsMap.TryGetValue(obj.ObjectId, out prevObj))
      {
        if (obj != prevObj)
        {
          Plogger.Debug("Replacing {0}/{1} with {2}/{3} in recent dead objects map.",
              prevObj.Type, prevObj.Name, obj.Type, obj.Name);

          inactiveObjectsMap[obj.ObjectId] = obj;
        }

        return;
      }
      Plogger.Debug("Added object id {0} to inactive map.", obj.ObjectId);
      inactiveObjectsMap.Add(obj.ObjectId, obj);
    }

    private void AddToInactive(MissionObject obj)
    {
      if (obj == null)
        return;

      inactiveObjects.Add(obj);
      AddToInactiveMap(obj);
      bool isDead = !obj.IsAlive();

      foreach (MissionObject child in obj.GetChildren())
      {
        switch (child.MissionObjectType)
        {
          case MissionObjectType.Pilot:   // ignoring bots, they should be added personally
          case MissionObjectType.Player:  // ignoring players, they are persistent
            continue;
        }

        Plogger.Debug("Moving child object {0} ({1}) to inactive objects.", child.ObjectId, child.Type);
        inactiveObjects.Add(child);

        AddToInactiveMap(child);

        if (isDead)
          child.SetDead();

        objectMap.Remove(child.ObjectId);
      }
    }

    private void UpdatePilotMap(Player player, int pilotId)
    {
      Player other = GetPlayerByPilotId(player.PilotId);

      if (other != null) // for current player id there is some entry
      {
        if (other != player)
        {
          Plogger.Error("Internal error: player map contains other player info for pilot id {0}", player.PilotId);
          return;
        }

        if (pilotId == player.PilotId) // nothing to update
          return;

        // removing previous entry for player
        pilotToPlayerMap.Remove(player.PilotId);
      }

      // checking if there is entry for pilotId
      other = GetPlayerByPilotId(pilotId);

      if (other == null)
      {
        pilotToPlayerMap.Add(pilotId, player);
        return;
      }

      if (other == player) // nothing to update
        return;

      Plogger.Error("Internal error: player map contains other player info for pilot id {0}", pilotId);
    }
    #endregion

    #region Log entries handlers

    private void ProcessMissionStart(MissionStartEntry entry)
    {
      missionFile = entry.MissionFile;
      missionTime = entry.MissionTime;
      missionCountries = entry.MissionCountries;
      missionType = entry.MissionType;
      missionSettings = entry.GameSettings;

      missionFlow.Add(new MissionEvent(entry, MissionEventType.MissionStart));
    }

    private void RegisterObject(MissionObject obj)
    {
      objectMap.Add(obj.ObjectId, obj);

      if (obj.ParentId == -1)
        return;

      // move only vehicle parts, not bots

      MissionObject parent = GetObject(obj.ParentId);

      if (parent != null) // Good parent
      {
        parent.RegisterChild(obj);
        return;
      }

      // workaround for vehicles, for which turret can be created after death of aircraft,
      // but before player bot is dead

      parent = CheckInactiveObject(obj.ParentId);

      if (parent == null)
      {
        Plogger.Error("We've just registered object {0} ({1}) with parent id {2}, but no such parent was registered.",
            obj.ObjectId, obj.MissionObjectType, obj.ParentId);

        return;
      }

      if (parent.MissionObjectType != MissionObjectType.Vehicle)
      {
        Plogger.Error("We've just registered object {0} ({1}) with parent id {2}, but no such parent with type Vehicle.",
            obj.ObjectId, obj.MissionObjectType, obj.ParentId);
      }
    }

    private bool AcceptableSameIdSituation(ObjectSpawnedEntry entry, MissionObject prevObj)
    {
      // XXX: something is defenitely wrong here, too many nested blocks, needs refactoring

      string objType = MissionObject.ParseTypeStr(entry.ObjectType);

      if ((prevObj.MissionObjectType != MissionObjectType.Block) ||                 // prev object is not block
          (objType == entry.ObjectType) ||  // new object is same type
          (objType != prevObj.Type))            // sanity check: 
      {
        if ((prevObj.Name != entry.ObjectName) ||
            (prevObj.MissionObjectType != MissionObjectType.Block))
        {
          if (entry.ParentId != -1)
          {
            MissionObject parent = GetObjectEvenIfInactive(entry.ParentId);
            Player player = GetPlayerFromObject(parent);

            if ((player != null) && player.HavePlayerEndEvent())
            {
              Plogger.Warn("Object id {0} (type: {1}) is already created, ignoring creation of object with type: {2}.",
                  entry.ObjectId, prevObj.Type, objType);

              Plogger.Warn(entry);

              ++ignoredEntries;
              return false;
            }
          }


          Plogger.Error("Object id {0} (type: {1}) is already created, ignoring creation of object with type: {2}.",
              entry.ObjectId, prevObj.Type, objType);

          Plogger.Error(entry);

          ++ignoredEntries;

          return false;
        }

        Plogger.Warn("Object id {0} (type: {1}) is already created, but creating object with type: {2} as child, because of same name.",
                entry.ObjectId, prevObj.Type, objType);

        Plogger.Warn(entry);
      }

      return true;
    }

    private void ProcessObjectSpawn(ObjectSpawnedEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.ObjectId))
                interestingObjectLog.Add(entry);

            AllObjects.Add(new ObjectMapItem(entry.ObjectId, entry.ObjectName, entry.ObjectType));
#endif
      MissionObject prevObj = GetObject(entry.ObjectId);

      if (prevObj == null)
      {
        MissionObject obj = MissionObject.Create(entry, this);
        Plogger.Debug("Spawned object {0} ({1})", obj.ObjectId, obj.Type);

        if (obj.UnitType == UnitType.Unknown)
        {
          MissionObject parent = GetObjectEvenIfInactive(entry.ParentId);
          Plogger.Warn("Spawned object {0} ({1}), Parent: {2} ({3}) with unknown unit type",
              entry.ObjectId, entry.ObjectType, entry.ParentId, parent != null ? parent.Type : "<invalid>");
        }

        RegisterObject(obj);
        return;
      }

      // workaround for vehicles created for, but after PlayerPlane log entry
      if ((prevObj.MissionObjectType == MissionObjectType.Vehicle) && (prevObj.Name == null))
      {
        Plogger.Debug("Updating earlier spawned vehicle {0} ({1}):\nParentId = {2}, Country = {3}, Name = {4}",
            entry.ObjectId, prevObj.Type, entry.ParentId, entry.Country, entry.ObjectName);

        if (prevObj.Type == entry.ObjectType)
        {
          Vehicle prevVehicle = (Vehicle)prevObj;
          prevVehicle.Update(entry, this);
          return;
        }

        Plogger.Error("Earlier spawned vehicle {0} ({1}) has different type than requested {2).",
                entry.ObjectId, prevObj.Type, entry.ObjectType);
      }

      // XXX: something is wrong here, needs refactoring
      Block block = prevObj as Block;
      string objType = MissionObject.ParseTypeStr(entry.ObjectType);

      if (!AcceptableSameIdSituation(entry, prevObj))
        return;
      /*
      if ((block == null) ||					// prev object is not block
          (objType == entry.ObjectType) ||	// new object is not block
          (objType != block.Type))			// sanity check: block types should be same
      {

          if ((prevObj.Name != entry.ObjectName) || (block == null))
          {
              Plogger.Error("Object id {0} (type: {1}) is already created, ignoring creation of object with type: {2}.",
                  entry.ObjectId, prevObj.Type, objType);

              Plogger.Error(entry);

              ++ignoredEntries;

              return;
          }

          Plogger.Warn("Object id {0} (type: {1}) is already created, but creating object with type: {2} as child, because of same name.",
                  entry.ObjectId, prevObj.Type, objType);

          Plogger.Warn(entry);
      }
      */
      Plogger.Debug("Adding new item to Block Object Id {0}", entry.ObjectId);
      MissionObject newObj = MissionObject.Create(entry, this);

      if (newObj.UnitType == UnitType.Unknown)
      {
        MissionObject parent = GetObjectEvenIfInactive(entry.ParentId);
        Plogger.Warn("Spawned object {0} ({1}), Parent: {2} ({3}) with unknown unit type",
            entry.ObjectId, entry.ObjectType, entry.ParentId, parent != null ? parent.Type : "<invalid>");
      }

      block.AddObject(newObj);
    }

    private void ProcessPlayerPlaneEntry(PlayerPlaneEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.VehicleId) ||
                IsInteresting(entry.PilotId) ||
                IsInteresting(entry.ParentId))
                interestingObjectLog.Add(entry);
#endif
      Player player = GetPlayerByGuid(entry.UserId);
      MissionObject objVehicle = GetObject(entry.VehicleId);
      Vehicle vehicle = objVehicle as Vehicle;
      MissionObject bot = GetObject(entry.PilotId);

      if (vehicle == null)
      {
        if (objVehicle != null)
        {
          Plogger.Error("Vehicle type {0} has wrong binding, check GameObjectsData.json. Results will be wrong.",
              objVehicle.Type);
          // XXX: return here to avoid crash
          return;
        }

        // workaround for situations when plane created after plane entry
        UnitType unitType = GetUnitType(entry.VehicleType);
        vehicle = new Vehicle(entry, this, unitType);

        if (unitType == UnitType.Unknown)
        {
          Plogger.Warn("Unit created by PlayerPlaneEntry for player {0} is of Unknown unit type.",
              player.Name);
        }
        // other fields, including name should be updated when it spawned.
      }


      if (player == null)
      {
        player = new Player(entry, this);
        playerMap.Add(entry.UserId, player);
      }

      player.StartNewGameSession(entry, vehicle, bot);
      UpdatePilotMap(player, entry.PilotId);

      vehicle.RegisterChild(player);

      MissionObject pilot = GetObject(entry.PilotId);

      if (pilot != null)
        pilot.RegisterChild(player);

      PlayerBeginEvent evt = new PlayerBeginEvent(entry, this);
      player.AddEvent(evt);
      player.AddEvent(new AmmunitionStatsEvent(entry));
      missionFlow.Add(evt);
    }

    private void ProcessPlayerMissionEndEntry(PlayerMissionEndEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.VehicleId) ||
                IsInteresting(entry.PilotId))
                interestingObjectLog.Add(entry);
#endif
      Player player = GetPlayerByPilotId(entry.PilotId);
      Vehicle vehicle = GetObjectEvenIfInactive(entry.VehicleId) as Vehicle;
      MissionEvent evt = new PlayerEnd(entry, vehicle);

      if (player == null)
      {
        Plogger.Warn("No player registered for pilot id {0}", entry.PilotId);
        return;
      }
      else
      {
        player.AddEvent(new AmmunitionStatsEvent(entry));
        player.AddEvent(evt);
      }

      missionFlow.Add(evt);
    }

    private void ProcessTakeOffEntry(TakeOffEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.VehicleId))
                interestingObjectLog.Add(entry);
#endif
      Player player = GetPlayerByVehicleId(entry.VehicleId);
      MissionObject vehicle = GetObject(entry.VehicleId);
      TakeOffEvent evt = new TakeOffEvent(entry, vehicle, player);

      if (vehicle == null)
      {
        Plogger.Error("Can't find vehicle id {0} to handle Take off event", entry.VehicleId);
        return;
      }

      vehicle.AddEvent(evt);
      missionFlow.Add(evt);

      if (player == null)
      {
        Plogger.Debug("Can't find player with vehicle id {0} to handle Take off event", entry.VehicleId);
        return;
      }

      player.AddEvent(evt);
    }

    private void ProcessMissionObjectiveEntry(MissionObjectiveEntry entry)
    {
      MissionObjectiveEvent evt = new MissionObjectiveEvent(entry);
      objectiveEvents.Add(evt);
      missionFlow.Add(evt);
    }

    private void ProcessLandingEntry(LandingEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.VehicleId))
                interestingObjectLog.Add(entry);
#endif

      Player player = GetPlayerByVehicleId(entry.VehicleId);
      Vehicle vehicle = GetObject(entry.VehicleId) as Vehicle;

      if ((vehicle == null) && (player != null)) // trying to use current vehicle
        vehicle = player.GetCurrentVehicle();

      LandingEvent evt = new LandingEvent(entry, vehicle, player);

      missionFlow.Add(evt);

      vehicle = evt.GetVehicle() as Vehicle;

      if (vehicle == null)
        Plogger.Error("Can't find vehicle id {0} to handle Landing event", entry.VehicleId);
      else
        vehicle.AddEvent(evt);

      if (player == null)
      {
        Plogger.Debug("Can't find player with vehicle id {0} to handle Landing event", entry.VehicleId);
        return;
      }

      player.AddEvent(evt);
    }

    private Player NotifyPlayerOfDeath(KillEntry entry, MissionObject attacker, Player attackerPlayer, MissionObject target, Player targetPlayer)
    {
      if (targetPlayer == null) // fall back to old way of getting player
      {
        targetPlayer = target.MissionObjectType == MissionObjectType.Pilot ?
            GetPlayerByPilotId(target.ObjectId) : GetPlayerByVehicleId(target.ObjectId);

        if (targetPlayer == null)
          return null;
      }

      MissionObject attackerObject = attackerPlayer ?? attacker;

      MissionEvent evt = null;

      switch (target.MissionObjectType)
      {
        case MissionObjectType.Pilot:
          evt = new BotDeathEvent(entry, attackerObject, target, targetPlayer);
          break;

        case MissionObjectType.Vehicle:
        default:
          evt = new VehicleDestroyedEvent(entry, attackerObject, target, targetPlayer);
          break;
      }

      if (evt != null)
        targetPlayer.AddEvent(evt);

      return targetPlayer;
    }

    private void ProcessBotUninit(BotUninitEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.BotId))
                interestingObjectLog.Add(entry);
#endif
      MissionObject target = GetObject(entry.BotId);

      if (target == null)
      {
        Plogger.Debug("Got Bot Uninit entry with bot id {0}, but don't have such active target.", entry.BotId);
        Plogger.Debug(entry);

        MissionObject missionObject = CheckInactiveObject(entry.BotId);

        if (missionObject != null)
        {
          missionObject.UpdatePosition(entry.Position);
          return;
        }

        ++ignoredEntries;
        return;
      }

      Plogger.Debug("Moving {0} ({1}) to inactive objects, because it is uninited.",
          target.ObjectId, target.Type);

      target.UpdatePosition(entry.Position);
      objectMap.Remove(entry.BotId);
      AddToInactive(target);
    }

    private void ProcessBotEject(BotEjectEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.BotId) ||
                IsInteresting(entry.ParentId))
                interestingObjectLog.Add(entry);
#endif
      MissionObject target = GetObjectEvenIfInactive(entry.BotId);

      if (target == null)
      {
        Plogger.Warn("Got Bot Eject entry with bot id {0}, but don't have such target.", entry.BotId);
        Plogger.Debug(entry);
        ++ignoredEntries;
        return;
      }

      target.UpdatePosition(entry.Position);

      Vehicle vehicle = GetObjectEvenIfInactive(entry.ParentId) as Vehicle;

      EjectEvent evt = new EjectEvent(entry, target, vehicle);
      target.AddEvent(evt);

      Player player = GetPlayerByPilotId(entry.BotId);

      if (player == null)
        return;

      missionFlow.Add(evt);
      player.AddEvent(evt);
    }

    private void ProcessKillEntry(KillEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.TargetId) ||
                IsInteresting(entry.AttackerId))
                interestingObjectLog.Add(entry);
#endif
      MissionObject target = GetObject(entry.TargetId);

      if (target == null)
      {
        Plogger.Debug("Got kill entry with target {0}, but don't have such active target.", entry.TargetId);
        Plogger.Debug(entry);
        CheckInactiveObject(entry.TargetId);
        ++ignoredEntries;
        return;
      }

      target.SetDead();

      MissionObject attacker = GetObjectEvenIfInactive(entry.AttackerId);
      Player targetPlayer = MissionObject.GetPlayer(target);

      DeathEvent deathEvent = attacker != target ?
              new DeathEvent(entry, attacker, target, targetPlayer) :
              new SuicideEvent(entry, attacker, targetPlayer);

      target.AddEvent(deathEvent);

      DeathSourceClassifier note = deathnoteProto.Create(target, this);

      if (note != null)
        deathLog.Add(note);

      Player killerPlayer = note != null ? note.GetKillerPlayer() : MissionObject.GetPlayer(attacker);

      Country accountFor = (attacker != null) ? attacker.Country : Country.None;
      accountFor = (killerPlayer != null) ? killerPlayer.Country : accountFor;

      KillEvent killEvent = new KillEvent(entry, attacker, target, accountFor);

      if (attacker != null)
        attacker.AddEvent(killEvent);

      if (killerPlayer != null)
      {
        killerPlayer.AddEvent(killEvent);
        missionFlow.Add(killEvent);
      }

      //targetPlayer = note != null ? note.GetTargetPlayer() : MissionObject.GetPlayer(target);

      if (NotifyPlayerOfDeath(entry, attacker, killerPlayer, target, targetPlayer) != null)
        missionFlow.Add(deathEvent);

      // XXX: block logic is not good enough
      Block block = target as Block;

      if (block != null)
      {
        MissionObject killedObject = block.KillOne(target.Name);

        if (killedObject != null)
        {
          DeathEvent blockDeathEvent = new DeathEvent(entry, attacker, killedObject, targetPlayer);
          killedObject.AddEvent(blockDeathEvent);
          killedObject.UpdateDeathPosition();
          killedObject.SetDead();
          AddToInactive(killedObject);
          Plogger.Debug("One object in Block {0} ({1}) was destroyed, moving to inactive {2} ({3}).",
              block.ObjectId, block.Type, killedObject.ObjectId, killedObject.Type);
          return;
        }
      }

      Plogger.Debug("Moving {0} ({1}) to inactive objects, because it is dead.",
          target.ObjectId, target.Type);

      target.UpdateDeathPosition();
      objectMap.Remove(entry.TargetId);
      AddToInactive(target);
    }

    private void ProcessHitEntry(HitEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.TargetId) ||
                IsInteresting(entry.AttackerId))
                interestingObjectLog.Add(entry);
#endif

      MissionObject attacker = GetObject(entry.AttackerId);
      MissionObject target = GetObject(entry.TargetId);

      if (target == null)
      {
        Plogger.Debug("Got hit entry with target {0}, but don't have such active target. Ignored.", entry.TargetId);
        Plogger.Debug(entry);
        CheckInactiveObject(entry.TargetId);
        ++ignoredEntries;
        return;
      }

      Player targetPlayer = GetPlayerFromObject(target);
      Player attackerPlayer = GetPlayerFromObject(attacker);

      if (attacker != null)
      {
        HitEvent hitEvent = new HitEvent(entry, target, targetPlayer, attacker);
        attacker.AddEvent(hitEvent);

        if (attackerPlayer != null)
          attackerPlayer.AddEvent(hitEvent);
      }

      GotHitEvent gotHitEvent = new GotHitEvent(entry, attacker, attackerPlayer, target);
      target.AddEvent(gotHitEvent);

      if (targetPlayer != null)
        targetPlayer.AddEvent(gotHitEvent);
    }

    private void ProcessDamageEntry(DamageEntry entry)
    {
#if DEBUG_INTERESTING
            if (IsInteresting(entry.TargetId) ||
                IsInteresting(entry.AttackerId))
                interestingObjectLog.Add(entry);
#endif
      MissionObject attacker = GetObject(entry.AttackerId);
      MissionObject target = GetObject(entry.TargetId);

      if (target == null)
      {
        Plogger.Debug("Got damage entry with target {0}, but don't have such active target. Ignored.", entry.TargetId);
        Plogger.Debug(entry);
        CheckInactiveObject(entry.TargetId);
        ++ignoredEntries;
        return;
      }

      Player targetPlayer = GetPlayerFromObject(target);
      Player attackerPlayer = GetPlayerFromObject(attacker);

      if (attacker != null)
      {
        DamageEvent damageEvent = new DamageEvent(entry, target, targetPlayer, attacker);
        attacker.AddEvent(damageEvent);

        if (attackerPlayer != null)
          attackerPlayer.AddEvent(damageEvent);
      }

      GotDamageEvent gotDamageEvent = new GotDamageEvent(entry, attacker, attackerPlayer, target);
      target.AddEvent(gotDamageEvent);

      if (targetPlayer != null)
        targetPlayer.AddEvent(gotDamageEvent);
    }

    public void AddLogEntry(LogEntry entry)
    {
      ++logEntries[(int)entry.EntryType];

      if (entry.EntryType == LogEntryType.LogVersion)
      {
        ++ignoredEntries;
        return;
      }

      log.Add(entry);

      switch (entry.EntryType)
      {
        case LogEntryType.MissionStart:
          ProcessMissionStart((MissionStartEntry)entry);
          break;

        case LogEntryType.Hit:
          ProcessHitEntry((HitEntry)entry);
          break;

        case LogEntryType.Damage:
          ProcessDamageEntry((DamageEntry)entry);
          break;

        case LogEntryType.Kill:
          ProcessKillEntry((KillEntry)entry);
          break;

        case LogEntryType.PlayerMissionEnd:
          ProcessPlayerMissionEndEntry((PlayerMissionEndEntry)entry);
          break;

        case LogEntryType.TakeOff:
          ProcessTakeOffEntry((TakeOffEntry)entry);
          break;

        case LogEntryType.Landing:
          ProcessLandingEntry((LandingEntry)entry);
          break;

        case LogEntryType.MissionEnd:
          missionFlow.Add(new MissionEvent(entry, MissionEventType.MissionEnd));
          break;

        case LogEntryType.MissionObjective:
          ProcessMissionObjectiveEntry((MissionObjectiveEntry)entry);
          break;

        case LogEntryType.AirfieldInfo:
          ++ignoredEntries;
          break;

        case LogEntryType.PlayerPlane:
          ProcessPlayerPlaneEntry((PlayerPlaneEntry)entry);
          break;

        case LogEntryType.GroupInit:
          ++ignoredEntries;
          break;

        case LogEntryType.ObjectSpawned:
          ProcessObjectSpawn((ObjectSpawnedEntry)entry);
          break;

        case LogEntryType.InfluenceAreaHeader:
        case LogEntryType.InfluenceAreaBoundary:
          break;

        case LogEntryType.LogVersion: // never goes here
          break;

        case LogEntryType.BotUninit:
          ProcessBotUninit((BotUninitEntry)entry);
          break;

        case LogEntryType.PosChanged: // unused
          ++ignoredEntries;
          break;

        case LogEntryType.BotEjectLeave:
          ProcessBotEject((BotEjectEntry)entry);
          break;

        case LogEntryType.RoundEnd:
          missionFlow.Add(new MissionEvent(entry, MissionEventType.RoundEnd));
          break;

        case LogEntryType.Join:
        case LogEntryType.Leave:
          ++ignoredEntries;
          break;
      }
    }
    #endregion

    public Dictionary<string, object> GetExportData(ExportOptions exportOptions)
    {
      Dictionary<string, object> result = new Dictionary<string, object>(8);

#if DEBUG_INTERESTING
            if (exportOptions.ExportInteresting)
                result.Add("Interesting", interestingObjectLog);
#endif
      if (exportOptions.ExportMissionInfo)
      {
        var missionInfo = new
        {
          MissionFile = MissionFile,
          MissionSettings = MissionSettings,
          MissionCountries = MissionCountries,
          MissionTime = MissionTime
        };

        result.Add("MissionInfo", missionInfo);
      }

      if (exportOptions.ExportMissionObjectives)
        result.Add("MissionObjectives", MissionObjectives);

      if (exportOptions.ExportMissionStats)
        result.Add("MissionStats", MissionStats);

      if (exportOptions.ExportMissionFlow)
        result.Add("MissionFlow", MissionFlow);

      if (exportOptions.ExportDeadStats)
        result.Add("DeadStats", deathLog);

      if (exportOptions.ExportAllPlayers)
        result.Add("AllPlayers", AllPlayers);

      return result;
    }
  }
}

