using System;
using System.Text.RegularExpressions;
using System.Text;
using System.Collections.Generic;

namespace ploggy
{
  public enum LogEntryType
  {
    MissionStart = 0,       //
    Hit = 1,                //
    Damage = 2,             //
    Kill = 3,               //
    PlayerMissionEnd = 4,   //
    TakeOff = 5,            //
    Landing = 6,            //
    MissionEnd = 7,
    MissionObjective = 8,   //
    AirfieldInfo = 9,       //
    PlayerPlane = 10,       //
    GroupInit = 11,         //
    ObjectSpawned = 12,     //
    InfluenceAreaHeader = 13,   //
    InfluenceAreaBoundary = 14, //
    LogVersion = 15,            //
    BotUninit = 16,             //
    PosChanged = 17,            // unused
    BotEjectLeave = 18,         //
    RoundEnd = 19,              //
    Join = 20,                  //
    Leave = 21,                 //
    None = 22,
    All = 23,
    Artificial = 65536 // Artificial entries injected by Coconut's dynamic campaign system
  }

  public class LogEntry
  {
    // T:0 AType:15
    private static Regex regexForType = new Regex(@"^T:(\b\d+) AType:(\d+)", RegexOptions.Compiled);

    #region Instance Vars
    private LogEntryType eventType = LogEntryType.None;

    private string originalString = null;

    private bool isValid = false;

    private long ticks = 0;
    #endregion

    #region Properties
    public TimeSpan Timestamp { get { return new TimeSpan(ticks * 200000); } }

    public string OriginalString { get { return originalString; } }

    public bool IsValid() { return isValid; }
    public void SetValid(bool value) { isValid = value; }

    public LogEntryType EntryType { get { return eventType; } }

    #endregion

    private LogEntry()
    {
    }

    public LogEntry(LogEntryType eventType)
    {
      this.eventType = eventType;
    }

    private static LogEntryType ConvertToEventType(int value)
    {
      if (value == (int)LogEntryType.Artificial)
        return LogEntryType.Artificial;

      if (value > (int)LogEntryType.Leave)
        return LogEntryType.None;

      if (value < 0)
        return LogEntryType.None;

      return (LogEntryType)value;
    }

    public static LogEntry CreateNoneLogEntry()
    {
      return new NoneLogEntry();
    }

    public static LogEntry CreateLogEntry(LogEntryType eventType)
    {
      switch (eventType)
      {
        case LogEntryType.MissionStart:
          return new MissionStartEntry();

        case LogEntryType.Hit:
          return new HitEntry();

        case LogEntryType.Damage:
          return new DamageEntry();

        case LogEntryType.Kill:
          return new KillEntry();

        case LogEntryType.PlayerMissionEnd:
          return new PlayerMissionEndEntry();

        case LogEntryType.TakeOff:
          return new TakeOffEntry();

        case LogEntryType.Landing:
          return new LandingEntry();

        case LogEntryType.MissionEnd:
          return new MissionEndEntry();

        case LogEntryType.MissionObjective:
          return new MissionObjectiveEntry();

        case LogEntryType.AirfieldInfo:
          return new AirfieldInfoEntry();

        case LogEntryType.PlayerPlane:
          return new PlayerPlaneEntry();

        case LogEntryType.GroupInit:
          return new GroupInitEntry();

        case LogEntryType.ObjectSpawned:
          return new ObjectSpawnedEntry();

        case LogEntryType.InfluenceAreaHeader:
          return new InfluenceAreaHeaderEntry();

        case LogEntryType.InfluenceAreaBoundary:
          return new InfluenceAreaBoundaryEntry();

        case LogEntryType.LogVersion:
          return new VersionEntry();

        case LogEntryType.BotUninit:
          return new BotUninitEntry();

        case LogEntryType.PosChanged:
          break;

        case LogEntryType.BotEjectLeave:
          return new BotEjectEntry();

        case LogEntryType.RoundEnd:
          return new RoundEndEntry();

        case LogEntryType.Join:
          return new JoinEntry();

        case LogEntryType.Leave:
          return new LeaveEntry();

        case LogEntryType.Artificial:
          return new ArtificialEntry();
      }

      return new LogEntry(eventType);
    }

    public static LogEntry Parse(string stringToParse)
    {
      Match eventHeader = regexForType.Match(stringToParse);

      // something is wrong
      if (!eventHeader.Success)
        return CreateNoneLogEntry();

      int pticks = 0;

      if (!int.TryParse(eventHeader.Groups[1].Value, out pticks))
        return CreateNoneLogEntry();

      int type = -1;

      int.TryParse(eventHeader.Groups[2].Value, out type);

      LogEntryType eventType = ConvertToEventType(type);

      LogEntry newEvent = CreateLogEntry(eventType);

      if (newEvent != null)
      {
        bool result = newEvent.Initialize(stringToParse, pticks);
        newEvent.SetValid(result);
      }

      return newEvent;
    }

    public virtual void Dump()
    {
      Plogger.Debug("{0}: {1}", Timestamp, eventType);
    }

    public virtual bool Initialize(string originalString, long ticks)
    {
      this.originalString = originalString;
      this.ticks = ticks;
      return true;
    }
  }

  public class NoneLogEntry : LogEntry
  {
    public NoneLogEntry() : base(LogEntryType.None)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      return false; // always invalid
    }
  }

  public class VersionEntry : LogEntry
  {
    // VER:17
    private static Regex regexForVersion = new Regex(@"VER:(\d+)");
    private int logVersion = -1;

    public int Version { get { return logVersion; } }

    public VersionEntry() : base(LogEntryType.LogVersion)
    {

    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForVersion.Match(originalString);

      if (!match.Success)
        return false;

      string versionStr = match.Groups[1].Value;

      if (!int.TryParse(versionStr, out logVersion))
      {
        Plogger.Error("Failed to parse log version, expected integer number, but got '{0}'", versionStr);
        return false;
      }

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tlog format version: {0}", Version);
    }
  }

  public class JoinEntry : LogEntry
  {
    //USERID:GUID USERNICKID:GUID
    private static Regex regexForGuids = new Regex(@"USERID:([\d\-a-fA-F]{36}) USERNICKID:([\d\-a-fA-f]{36})", RegexOptions.Compiled);
    private Guid userId;
    private Guid nickId;

    #region Properties
    public Guid UserId { get { return userId; } }
    public Guid NickId { get { return nickId; } }
    #endregion

    public JoinEntry() : base(LogEntryType.Join)
    {

    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForGuids.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse UserId and UserNickId, expected 2 GUID strings, got {0}", originalString);
        return false;
      }

      userId = new Guid(match.Groups[1].Value);
      nickId = new Guid(match.Groups[2].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tUser Id: {0}\n\tNick Id: {1}", UserId, NickId);
    }
  }

  public class LeaveEntry : LogEntry
  {
    private static Regex regexForGuids = new Regex(@"USERID:([\d\-a-fA-F]{36}) USERNICKID:([\d\-a-fA-f]{36})", RegexOptions.Compiled);
    private Guid userId;
    private Guid nickId;

    #region Properties
    public Guid UserId { get { return userId; } }
    public Guid NickId { get { return nickId; } }
    #endregion

    public LeaveEntry() : base(LogEntryType.Leave)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForGuids.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse UserId and UserNickId, expected 2 GUID strings, got {0}", originalString);
        return false;
      }

      userId = new Guid(match.Groups[1].Value);
      nickId = new Guid(match.Groups[2].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tUser Id: {0}\n\tNick Id: {1}", UserId, NickId);
    }
  }

  public class MissionStartEntry : LogEntry
  {
    // GDate:1942.11.30 GTime:11:56:0 MFile:Missions\_gen.msnbin MID: GType:202 CNTRS:0:0,101:1,201:2 SETTS:1111 1101 1110 1001 0000 0001 110 MODS:0 PRESET:1 AQMID:259
    // GDate:1942.11.19 GTime:13:0:0 MFile:Multiplayer/Dogfight\FirstMap-WL-010-07.msnbin MID: GType:2 CNTRS:0:0,50:0,101:1,201:2,202:2 SETTS:0100000000100001000000001 MODS:0 PRESET:0 AQMID:0'
    private static Regex regexForMissionStart =
        new Regex(@"GDate:([\d\.]+) GTime:([\d:]+) MFile:(.+) MID:(\w*) GType:(\d+) CNTRS:([\d:,]+) SETTS:(\d+) MODS:(\d) PRESET:(\d) AQMID:(\d+)", RegexOptions.Compiled);

    private static Regex regexForDate = new Regex(@"^(\d+)\.(\d+)\.(\d+)", RegexOptions.Compiled);
    private static Regex regexForTime = new Regex(@"^(\d+):(\d+):(\d+)", RegexOptions.Compiled);
    private static Regex regexForCountries = new Regex(@"(\d+):(\d+)", RegexOptions.Compiled);

    #region Instance vars
    private DateTime missionTime;
    private string missionId = null;
    private MissionType missionType = MissionType.None;
    private int aqmId = 0;
    private GameSettings gameSettings = null;
    private CoalitionBind[] missionCountries = null;
    #endregion

    #region Properies
    private string missionFile = null;
    public string MissionFile { get { return missionFile; } }
    public DateTime MissionTime { get { return missionTime; } }

    public MissionType MissionType { get { return missionType; } }
    public GameSettings GameSettings { get { return gameSettings; } }
    public CoalitionBind[] MissionCountries { get { return missionCountries; } }
    #endregion

    private void ParseDate(string dateString, string timeString)
    {
      Match dateMatch = regexForDate.Match(dateString);

      int year = 1812;
      int month = 10;
      int day = 26;

      if (!dateMatch.Success)
        Plogger.Warn("Failed to parse mission date, expected YYYY.MM.YY, got '{0}'", dateString);
      else
      {
        year = Utility.ParseInt(dateMatch.Groups[1].Value, year);
        month = Utility.ParseInt(dateMatch.Groups[2].Value, month);
        day = Utility.ParseInt(dateMatch.Groups[3].Value, day);
      }

      // parsing time
      Match timeMatch = regexForTime.Match(timeString);

      int hour = 12;
      int minutes = 0;
      int seconds = 0;

      if (!timeMatch.Success)
        Plogger.Warn("Failed to parse mission time, expected HH:MM:SS, got '{0}'", timeString);
      else
      {
        hour = Utility.ParseInt(timeMatch.Groups[1].Value, hour);
        minutes = Utility.ParseInt(timeMatch.Groups[2].Value, minutes);
        seconds = Utility.ParseInt(timeMatch.Groups[3].Value, seconds);
      }

      missionTime = new DateTime(year, month, day, hour, minutes, seconds);
    }

    private void ParseCountries(string countriesStr)
    {
      MatchCollection matches = regexForCountries.Matches(countriesStr);
      int count = matches.Count;
      missionCountries = count != 0 ? new CoalitionBind[count] : null;

      for (int i = 0; i < count; ++i)
      {
        Match match = matches[i];
        Country country = Utility.ParseCountry(match.Groups[1].Value);
        Coalition coalition = Utility.ParseCoalition(match.Groups[2].Value);
        CoalitionBind newBind = new CoalitionBind(country, coalition);

        missionCountries[i] = newBind;
      }
    }

    public MissionStartEntry() : base(LogEntryType.MissionStart)
    {

    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForMissionStart.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse mission related data: '{0}'", originalString);
        return false;
      }

      string dateString = match.Groups[1].Value;
      string timeString = match.Groups[2].Value;
      ParseDate(dateString, timeString);

      missionFile = match.Groups[3].Value;
      missionId = match.Groups[4].Value;
      missionType = Utility.GetMissionType(Utility.ParseInt(match.Groups[5].Value, 202));

      ParseCountries(match.Groups[6].Value);

      bool mods = Utility.ParseBoolInt(match.Groups[8].Value);
      SettingsPreset preset = Utility.GetSettingsPreset(Utility.ParseInt(match.Groups[9].Value, 2));

      gameSettings = new GameSettings(match.Groups[7].Value, preset, mods);

      aqmId = Utility.ParseInt(match.Groups[10].Value, 0);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tMission: {0}\n\tDate: {1}\n\tType: {2}, Preset: {3}, Mods: {4}\n\tCoalitions:",
          MissionFile, MissionTime,
          MissionType,
          gameSettings.Preset, Utility.YesNo(gameSettings.ModsEnabled));

      if (missionCountries != null)
      {
        for (int i = 0; i < missionCountries.Length; ++i)
          Plogger.Debug("\t\t{0} in {1}", missionCountries[i].country, missionCountries[i].coalition);
      }
    }
  }

  public class GroupInitEntry : LogEntry
  {
    //AType:11
    //Planes group information
    //T:1 AType:11 GID:926720 IDS:532480,538624,547840,557056,563200,569344,575488 LID:532480
    //T:1 AType:11 GID:927744 IDS:640000,646144,655360,664576,670720,676864,683008 LID:640000
    private static Regex regexForGroupInit = new Regex(@"GID:([\d\-]+) IDS:([\d,\-]+) LID:([\d\-]+)", RegexOptions.Compiled);

    #region Instance vars
    private int groupId = -1;
    private int leaderId = -1;
    private int[] ids = null;
    #endregion

    #region Properties
    public int GroupId { get { return groupId; } }
    public int LeaderId { get { return leaderId; } }
    public int[] GroupIds { get { return ids; } }
    #endregion

    public GroupInitEntry() : base(LogEntryType.GroupInit)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForGroupInit.Match(originalString);

      if (!match.Success)
        return false;

      groupId = Utility.ParseInt(match.Groups[1].Value, -1);
      ids = Utility.ParseIntList(match.Groups[2].Value);
      leaderId = Utility.ParseInt(match.Groups[3].Value, -1);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tGroup ID: {0}, Leader ID: {1}", groupId, leaderId);

      if (ids == null)
        return;

      for (int i = 0; i < ids.Length; ++i)
        Plogger.Debug("\t\t{0}: {1}", i, ids[i]);
    }
  }

  public class ObjectSpawnedEntry : LogEntry
  {
    // T:5 AType:12 ID:755711 TYPE:Il-2 mod.1942 COUNTRY:101 NAME:Player PID:-1
    // T:5 AType:12 ID:756735 TYPE:BotPilot_Il2 COUNTRY:101 NAME:BotPilot_Il2 PID:755711
    private static Regex regexForSpawned = new Regex(@"ID:(?<id>[\d\-]+) TYPE:(?<type>[^[]+)(\[.+,(?<sub>.+)\])? COUNTRY:(?<country>\d+) NAME:(?<name>.+) PID:(?<pid>[\d\-]+)", RegexOptions.Compiled);

    #region Instance Vars
    private int id = -1;
    private int parentId = -1;
    private string type = null;
    private Country country = Country.None;
    private string name = null;
    private int subGroup = -1;
    #endregion

    #region Properties
    public int ObjectId { get { return id; } }
    public int ParentId { get { return parentId; } }
    public string ObjectType { get { return type; } }
    public Country Country { get { return country; } }
    public string ObjectName { get { return name; } }
    public int SubGroup { get { return subGroup; } }
    #endregion

    public ObjectSpawnedEntry() : base(LogEntryType.ObjectSpawned)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForSpawned.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse spawned object: '{0}'", originalString);
        return false;
      }

      id = Utility.ParseInt(match.Groups["id"].Value, -1);
      country = Utility.ParseCountry(match.Groups["country"].Value);
      type = match.Groups["type"].Value;
      name = match.Groups["name"].Value;
      parentId = Utility.ParseInt(match.Groups["pid"].Value, -1);
      if (match.Groups["sub"].Success)
      {
        subGroup = Utility.ParseInt(match.Groups["sub"].Value, -1);
      }
      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tId: {0}, Country: {1}, Type: {2}\n\tName: {3}, Parent Id: {4}", id, country, type, name, parentId);
    }

    public override string ToString()
    {
      return string.Format("\t[ObjectSpawnedEntry Id: {0}, Country: {1}, Type: {2}\n\tName: {3}, Parent Id: {4}, Timestamp: {5}]",
          id, country, type, name, parentId, Timestamp);
    }
  }

  public class PlayerPlaneEntry : LogEntry
  {
    // AType:10 PLID:755711 PID:756735 BUL:2298 SH:0 BOMB:0 RCT:0 (187125.453,96.071,278790.156) IDS:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx LOGIN:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx NAME:XXXXX TYPE:Il-2 mod.1942 COUNTRY:101 FORM:1 FIELD:676863 INAIR:1 PARENT:-1 ISPL:1 ISTSTART:1 PAYLOAD:49 FUEL:0.850 SKIN:il2m42/il2m42_unique_01.dds WM:43
    // AType:10 PLID:904199 PID:905223 BUL:2996 SH:0 BOMB:0 RCT:0 (162681.281,54.328,187733.656) IDS:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx LOGIN:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx NAME:=XX=X TYPE:Ju 87 D-3 COUNTRY:201 FORM:0 FIELD:1753088 INAIR:2 PARENT:-1 ISPL:1 ISTSTART:1 PAYLOAD:7 FUEL:0.500 SKIN: WM:5
    // AType:10 PLID:161844 PID:587828 BUL:340 SH:0 BOMB:0 RCT:0 (1.#QO,1.#QO,1.#QO) IDS:1c3aa157-8fdf-418a-80a0-0b9afe511d16 LOGIN:8b832e85-d764-4d13-a5e8-028f37c6b57a NAME:=UAF=inor TYPE:La-5 ser.8 COUNTRY:101 FORM:0 FIELD:0 INAIR:0 PARENT:-1 ISPL:1 ISTSTART:1 PAYLOAD:0 FUEL:-1.#QO SKIN:la5s8/la5s8_skin_01.dds WM:1
    private static Regex regexForSpawned = new Regex(@"PLID:([\d\-]+) PID:([\d\-]+) BUL:([\d\-]+) SH:([\d\-]+) BOMB:([\d\-]+) RCT:([\d\-]+) \((.*)\) IDS:([\d\-a-fA-f]{36}) LOGIN:([\d\-a-fA-f]{36}) NAME:(.+) TYPE:(.+) COUNTRY:(\d+) FORM:(\d+) FIELD:([\d\-]+) INAIR:(\d+) PARENT:([\d\-]+) ISPL:[\d]+ ISTSTART:[\d]+ PAYLOAD:(\d+) FUEL:([\d\.#QO\-]+) SKIN:(.*) WM:(\d+)", RegexOptions.Compiled);

    #region Instance Vars
    private int vehicleId = -1;
    private int pilotId = -1;
    private int bullets = 0;
    private int shells = 0;
    private int bombs = 0;
    private int rockets = 0;
    private Position3d position = Position3d.Zero;
    private Guid nickId = Guid.Empty;
    private Guid userId = Guid.Empty;
    private string name = null;
    private string type = null;
    private Country country = Country.None;
    private int positionInFormation = 0;
    private int fieldId = -1;
    private PlaneStartType startType = PlaneStartType.InAir;
    private int parentId = -1;
    private int payload = 0;
    private float fuel = 0.0f;
    private int weaponModMask = 0;
    private string skin = null;
    #endregion

    #region Properties
    public int VehicleId { get { return vehicleId; } }
    public int PilotId { get { return pilotId; } }
    public int Bullets { get { return bullets; } }
    public int Shells { get { return shells; } }
    public int Bombs { get { return bombs; } }
    public int Rockets { get { return rockets; } }
    public Position3d Position { get { return position; } }
    public Guid NickId { get { return nickId; } }
    public Guid UserId { get { return userId; } }

    public string Name { get { return name; } }
    public string VehicleType { get { return type; } }

    public Country Country { get { return country; } }
    public int Formation { get { return positionInFormation; } }
    public int FieldId { get { return fieldId; } }
    public PlaneStartType StartType { get { return startType; } }
    public int ParentId { get { return parentId; } }
    public int Payload { get { return payload; } }
    public float Fuel { get { return fuel; } }
    public int WeaponModMask { get { return weaponModMask; } }
    public string Skin { get { return skin; } }
    #endregion

    public PlayerPlaneEntry() : base(LogEntryType.PlayerPlane)
    {

    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForSpawned.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse PlayerPlane event: '{0}'", originalString);
        return false;
      }

      vehicleId = Utility.ParseInt(match.Groups[1].Value, -1);
      pilotId = Utility.ParseInt(match.Groups[2].Value, -1);
      bullets = Utility.ParseInt(match.Groups[3].Value, 0);
      shells = Utility.ParseInt(match.Groups[4].Value, 0);
      bombs = Utility.ParseInt(match.Groups[5].Value, 0);
      rockets = Utility.ParseInt(match.Groups[6].Value, 0);
      position = Utility.ParsePosition3d(match.Groups[7].Value);
      nickId = new Guid(match.Groups[8].Value);
      userId = new Guid(match.Groups[9].Value);
      name = match.Groups[10].Value;
      type = match.Groups[11].Value;
      country = Utility.ParseCountry(match.Groups[12].Value);
      positionInFormation = Utility.ParseInt(match.Groups[13].Value, 0);
      fieldId = Utility.ParseInt(match.Groups[14].Value, -1);
      // XXX: will be wrong if new start types added
      startType = (PlaneStartType)Utility.ParseInt(match.Groups[15].Value, (int)startType);
      parentId = Utility.ParseInt(match.Groups[16].Value, -1);
      payload = Utility.ParseInt(match.Groups[17].Value, 0);
      fuel = Utility.ParseFloat(match.Groups[18].Value, 0.0f);
      skin = match.Groups[19].Value;
      weaponModMask = Utility.ParseInt(match.Groups[20].Value, 0);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tId: {0}, Pilot Id: {1}, Parent Id: {2}\n\tName: {3}, User Id: {4}, Nick Id: {5}\n\tCountry: {6}, Field Id: {7}",
                   vehicleId, pilotId, parentId,
                   name, userId, nickId,
                   country, fieldId);

      Plogger.Debug("\tPosition: {0}, Formation: {1}", position, positionInFormation);

      Plogger.Debug("\tType: {0}, Skin: {1}\n\tWeaponMods: {2}, Payload: {3}\n\tFuel: {4}, In Air: {5}\n\tBullets: {6}, Shells: {7}, Bombs: {8}, Rockets: {9}",
              type, skin,
              weaponModMask, payload,
              fuel, startType, /* Utility.YesNo(inAir), */
              bullets, shells, bombs, rockets
          );
    }
  }

  public class AirfieldInfoEntry : LogEntry
  {
    /*
     * T:10 AType:9 AID:37887 COUNTRY:101 POS(189790.516, 31.930, 292127.094) IDS()
        T:10 AType:9 AID:275455 COUNTRY:201 POS(140597.156, 132.626, 241827.594) IDS(-1,-1,-1,-1,-1,-1)
     */
    private static Regex regexForAirfield = new Regex(@"AID:([\d\-]+) COUNTRY:(\d+) POS\(([\d,\.\s\-#QO]+)\) ");// IDS\(([\d,]+\))", RegexOptions.Compiled);

    #region Instance vars
    private int airfieldId = -1;
    private Country country = Country.None;
    private Position3d position = Position3d.Zero;
    private int[] ids = null;
    #endregion

    #region Properties
    public int AirfieldId { get { return airfieldId; } }
    public Country Country { get { return country; } }
    public Position3d Position { get { return position; } }
    public int[] AirfieldIds { get { return ids; } }

    #endregion

    public AirfieldInfoEntry() : base(LogEntryType.AirfieldInfo)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForAirfield.Match(originalString);

      if (!match.Success)
        return false;

      airfieldId = Utility.ParseInt(match.Groups[1].Value, -1);
      country = Utility.ParseCountry(match.Groups[2].Value);
      position = Utility.ParsePosition3d(match.Groups[3].Value);
      ids = Utility.ParseIntList(match.Groups[4].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tAirfield Id: {0}, Position: {1}, Country: {2}", airfieldId, position, country);

      if (ids == null)
        return;

      for (int i = 0; i < ids.Length; ++i)
        Plogger.Debug("\t{0}: {1}", i, ids[i]);

    }
  }

  public class TakeOffEntry : LogEntry
  {
    // T:1320 AType:5 PID:411647 POS(185560.047, 2499.558, 263659.313)
    private static Regex regexForTakeOff = new Regex(@"PID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)", RegexOptions.Compiled);

    private int vehicleId = -1;
    private Position3d position3d = Position3d.Zero;

    public Position3d Position { get { return position3d; } }
    public int VehicleId { get { return vehicleId; } }

    public TakeOffEntry() : base(LogEntryType.TakeOff)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForTakeOff.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse Take-Off event: '{0}'", originalString);
        return false;
      }

      vehicleId = Utility.ParseInt(match.Groups[1].Value, -1);
      position3d = Utility.ParsePosition3d(match.Groups[2].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tPlane Id: {0}, Position: {1}", VehicleId, Position);
    }
  }

  public class MissionObjectiveEntry : LogEntry
  {
    // T:2828 AType:8 OBJID:4416 POS(187135.281,94.093,278819.531) COAL:1 TYPE:1 RES:1 ICTYPE:0
    private static Regex regexForObjective = new Regex(@"OBJID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\) COAL:(\d+) TYPE:(\d+) RES:(\d+) ICTYPE:(\d+)", RegexOptions.Compiled);

    private int objectiveId = -1;
    private Position3d position = Position3d.Zero;
    private Coalition coalition = Coalition.Neutral;
    private ObjectiveType objectiveType = ObjectiveType.Secondary;
    private bool objectiveCompleted = false;
    private int iconType = -1;

    public Position3d Position { get { return position; } }
    public Coalition Coalition { get { return coalition; } }
    public ObjectiveType ObjectiveType { get { return objectiveType; } }
    public bool Completed { get { return objectiveCompleted; } }
    public int IconType { get { return iconType; } }
    public int MissionObjectiveId { get { return objectiveId; } }

    public MissionObjectiveEntry() : base(LogEntryType.MissionObjective)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForObjective.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse mission objective: '{0}'", originalString);
        return false;
      }

      objectiveId = Utility.ParseInt(match.Groups[1].Value, -1);
      position = Utility.ParsePosition3d(match.Groups[2].Value);
      coalition = Utility.ParseCoalition(match.Groups[3].Value);
      objectiveType = Utility.ParseObjectiveType(match.Groups[4].Value);
      objectiveCompleted = Utility.ParseBoolInt(match.Groups[5].Value);
      iconType = Utility.ParseInt(match.Groups[6].Value, 0);


      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tObjective Id: {0}, Position: {1}, IconType: {2}\n\tType: {3}, Coalition: {4}, Completed: {5}",
          MissionObjectiveId, Position, IconType,
          ObjectiveType, Coalition, Completed);
    }
  }

  public class HitEntry : LogEntry
  {
    // T:4196 AType:1 AMMO:explosion AID:647167 TID:411647
    private static Regex regexForHit = new Regex(@"AMMO:(.+) AID:([\d\-]+) TID:([\d\-]+)", RegexOptions.Compiled);

    private string ammo = null;
    private int attackerId = -1;
    private int targetId = -1;

    public string AmmoType { get { return ammo; } }
    public int AttackerId { get { return attackerId; } }
    public int TargetId { get { return targetId; } }

    public HitEntry() : base(LogEntryType.Hit)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForHit.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse hit event: '{0}'", originalString);
        return false;
      }

      ammo = match.Groups[1].Value;
      attackerId = Utility.ParseInt(match.Groups[2].Value, -1);
      targetId = Utility.ParseInt(match.Groups[3].Value, -1);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tAmmo: {0}, Attaker Id: {1}, Target Id: {2}", AmmoType, AttackerId, TargetId);
    }

    public override string ToString()
    {
      return string.Format("[HitEntry: AmmoType={0}, AttackerId={1}, TargetId={2}]\n\toriginalString:{3}",
          AmmoType, AttackerId, TargetId, OriginalString);
    }
  }

  public class DamageEntry : LogEntry
  {
    // T:4195 AType:2 DMG:0.010 AID:647167 TID:411647 POS(188725.156,2611.606,264585.063)
    private static Regex regexForDamage = new Regex(@"DMG:([\d\.\-]+) AID:([\d\-]+) TID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)", RegexOptions.Compiled);
    private float damage = 0.0f;
    private int attackerId = -1;
    private int targetId = -1;
    private Position3d position = Position3d.Zero;

    public int AttackerId { get { return attackerId; } }
    public int TargetId { get { return targetId; } }
    public float Damage { get { return damage; } }
    public Position3d Position { get { return position; } }

    public DamageEntry() : base(LogEntryType.Damage)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForDamage.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse damage event: '{0}'", originalString);
        return false;
      }

      damage = Utility.ParseFloat(match.Groups[1].Value, 0.0f);
      attackerId = Utility.ParseInt(match.Groups[2].Value, -1);
      targetId = Utility.ParseInt(match.Groups[3].Value, -1);
      position = Utility.ParsePosition3d(match.Groups[4].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tDamage: {0}, Attacker Id: {1}, Target Id: {2}, Position: {3}",
          Damage, AttackerId, TargetId, Position);
    }

    public override string ToString()
    {
      return string.Format("[DamageEntry: AttackerId={0}, TargetId={1}, Damage={2}, Position={3}]\n\toriginalString: {4}]",
          AttackerId, TargetId, Damage, Position, OriginalString);
    }
  }

  public class KillEntry : LogEntry
  {
    // T:40187 AType:3 AID:755711 TID:731135 POS(149129.281,134.210,236242.875)
    private static Regex regexForDamage = new Regex(@"AID:([\d\-]+) TID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)", RegexOptions.Compiled);

    private int attackerId = -1;
    private int targetId = -1;
    private Position3d position = Position3d.Zero;

    public int AttackerId { get { return attackerId; } }
    public int TargetId { get { return targetId; } }
    public Position3d Position { get { return position; } }

    public KillEntry() : base(LogEntryType.Kill)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForDamage.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse kill event: '{0}'", originalString);
        return false;
      }

      attackerId = Utility.ParseInt(match.Groups[1].Value, -1);
      targetId = Utility.ParseInt(match.Groups[2].Value, -1);
      position = Utility.ParsePosition3d(match.Groups[3].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tAttacker Id: {0}, Target Id: {1}, Position: {2}",
          AttackerId, TargetId, Position);
    }

    public override string ToString()
    {
      return string.Format("[KillEntry: AttackerId={0}, TargetId={1}, Position={2}]\n\toriginalString:{3}",
          AttackerId, TargetId, Position, OriginalString);
    }
  }

  public class InfluenceAreaHeaderEntry : LogEntry
  {
    // T:0 AType:13 AID:26624 COUNTRY:201 ENABLED:1 BC(0,0,0)
    private static Regex regexForAreaHeader = new Regex(@"AID:([\d\-]+) COUNTRY:(\d+) ENABLED:(\d) BC\(([\d,]+)\)", RegexOptions.Compiled);

    private int areaId = -1;
    private Country country = Country.None;
    private bool enabled = false;
    private int[] bodyCount = null;

    public int AreaId { get { return areaId; } }
    public Country Country { get { return country; } }
    public bool Enabled { get { return enabled; } }
    public int[] BodyCountPerCoalition { get { return bodyCount; } }

    public InfluenceAreaHeaderEntry() : base(LogEntryType.InfluenceAreaHeader)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForAreaHeader.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse influence area header event: '{0}'", originalString);
        return false;
      }

      areaId = Utility.ParseInt(match.Groups[1].Value, -1);
      country = Utility.ParseCountry(match.Groups[2].Value);
      enabled = Utility.ParseBoolInt(match.Groups[3].Value);
      bodyCount = Utility.ParseIntList(match.Groups[4].Value);

      if ((bodyCount == null) || (bodyCount.Length != (int)Coalition.All))
        return false;

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tArea Id: {0}, Country: {1}, Enabled: {2}",
          AreaId, Country, Enabled);

      if (BodyCountPerCoalition == null)
        return;

      StringBuilder sb = new StringBuilder();
      int[] bc = BodyCountPerCoalition;
      sb.AppendFormat("\tBody Count: ({0}", bc[0]);

      for (int i = 1; i < bc.Length; ++i)
        sb.AppendFormat(", {0}", bc[i]);

      sb.AppendFormat(")");
      Plogger.Debug(sb.ToString());
    }
  }

  public class InfluenceAreaBoundaryEntry : LogEntry
  {
    // T:1 AType:14 AID:26624 BP((111047.0,112.3,246804.0),...)
    private static Regex regexForBoundary = new Regex(@"AID:([\d\-]+) BP\((.+)\)$", RegexOptions.Compiled);
    private static Regex regexForPoint = new Regex(@"\(([\d\.,\-\s#QO]+)\)", RegexOptions.Compiled);

    private int areaId = -1;
    private List<Position3d> boundaryList = new List<Position3d>(16);

    public int AreaId { get { return areaId; } }
    public Position3d[] BoundaryPoints { get { return boundaryList.ToArray(); } }

    public InfluenceAreaBoundaryEntry() : base(LogEntryType.InfluenceAreaBoundary)
    {
    }

    private bool ParseBoundaryPoints(string strToParse)
    {
      MatchCollection matches = regexForPoint.Matches(strToParse);

      foreach (Match match in matches)
      {
        if (!match.Success)
        {
          Plogger.Warn("Ignoring point, because failed to parse it");
          continue;
        }

        Position3d pos = Utility.ParsePosition3d(match.Groups[1].Value);
        boundaryList.Add(pos);
      }

      return true;
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForBoundary.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse influence area boundary entry: '{0}'", originalString);
        return false;
      }

      areaId = Utility.ParseInt(match.Groups[1].Value, -1);

      if (!ParseBoundaryPoints(match.Groups[2].Value))
      {
        Plogger.Error("Failed to parse influence area boundary points: '{0}'", match.Groups[2].Value);
        return false;
      }

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Position3d[] points = BoundaryPoints;

      Plogger.Debug("\tArea Id: {0}, Points: {1}",
          AreaId, points != null ? points.Length : 0);

      if (points == null)
        return;

      for (int i = 0; i < points.Length; ++i)
      {
        Position3d pos = points[i];

        Plogger.Debug("\t\t{0}: {1}", i, pos);
      }
    }
  }

  public class LandingEntry : LogEntry
  {
    // T:420710 AType:6 PID:733185 POS(159283.922, 103.844, 234791.250)
    private static Regex regexForLanding = new Regex(@"PID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)", RegexOptions.Compiled);

    private int vehicleId = -1;
    private Position3d position3d = Position3d.Zero;

    public Position3d Position { get { return position3d; } }
    public int VehicleId { get { return vehicleId; } }

    public LandingEntry() : base(LogEntryType.Landing)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForLanding.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse Landing event: '{0}'", originalString);
        return false;
      }

      vehicleId = Utility.ParseInt(match.Groups[1].Value, -1);
      position3d = Utility.ParsePosition3d(match.Groups[2].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tPlane Id: {0}, Position: {1}", VehicleId, Position);
    }
  }

  public class PlayerMissionEndEntry : LogEntry
  {
    // T:421028 AType:4 PLID:733185 PID:734209 BUL:2970 SH:0 BOMB:0 RCT:0 (159287.406,103.819,234770.250)
    private static Regex regexForPlayerEnd = new Regex(@"PLID:([\d\-]+) PID:([\d\-]+) BUL:([\d\-]+) SH:([\d\-]+) BOMB:([\d\-]+) RCT:([\d\-]+) \(([\d,\.\s\-#QO]+)\)", RegexOptions.Compiled);

    #region Instance Vars
    private int vehicleId = -1;
    private int pilotId = -1;
    private int bullets = 0;
    private int shells = 0;
    private int bombs = 0;
    private int rockets = 0;
    private Position3d position = Position3d.Zero;
    #endregion

    #region Properties
    public int VehicleId { get { return vehicleId; } }
    public int PilotId { get { return pilotId; } }
    public int Bullets { get { return bullets; } }
    public int Shells { get { return shells; } }
    public int Bombs { get { return bombs; } }
    public int Rockets { get { return rockets; } }
    public Position3d Position { get { return position; } }
    #endregion

    public PlayerMissionEndEntry() : base(LogEntryType.PlayerMissionEnd)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForPlayerEnd.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse Player Mission End event: '{0}'", originalString);
        return false;
      }

      vehicleId = Utility.ParseInt(match.Groups[1].Value, -1);
      pilotId = Utility.ParseInt(match.Groups[2].Value, -1);
      bullets = Utility.ParseInt(match.Groups[3].Value, 0);
      shells = Utility.ParseInt(match.Groups[4].Value, 0);
      bombs = Utility.ParseInt(match.Groups[5].Value, 0);
      rockets = Utility.ParseInt(match.Groups[6].Value, 0);
      position = Utility.ParsePosition3d(match.Groups[7].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tId: {0}, Pilot Id: {1}, Position: {2}",
          vehicleId, pilotId, position);


      Plogger.Debug("\tBullets: {0}, Shells: {1}, Bombs: {2}, Rockets: {3}",
          bullets, shells, bombs, rockets);
    }
  }

  public class BotUninitEntry : LogEntry
  {
    // T:421033 AType:16 BOTID:734209 POS(159287.422,103.986,234770.172)
    private static Regex regexForLanding = new Regex(@"BOTID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)", RegexOptions.Compiled);

    private int botId = -1;
    private Position3d position3d = Position3d.Zero;

    public Position3d Position { get { return position3d; } }
    public int BotId { get { return botId; } }

    public BotUninitEntry() : base(LogEntryType.BotUninit)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForLanding.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse Landing event: '{0}'", originalString);
        return false;
      }

      botId = Utility.ParseInt(match.Groups[1].Value, -1);
      position3d = Utility.ParsePosition3d(match.Groups[2].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tBot Id: {0}, Position: {1}", BotId, Position);
    }

    public override string ToString()
    {
      return string.Format("[BotUninitEntry BotId={0} Position: {1}]\n\toriginalString: {2}",
          BotId, Position, OriginalString);
    }
  }

  public class BotEjectEntry : LogEntry
  {
    //T:439433 AType:18 BOTID:768002 PARENTID:774146 POS(121058.375,72.294,272324.188)
    private static Regex regexForEject = new Regex(@"BOTID:([\d\-]+) PARENTID:([\d\-]+) POS\(([\d,\.\s\-#QO]+)\)", RegexOptions.Compiled);

    private int botId = -1;
    private int parentId = -1;
    private Position3d position3d = Position3d.Zero;

    public Position3d Position { get { return position3d; } }
    public int BotId { get { return botId; } }
    public int ParentId { get { return parentId; } }

    public BotEjectEntry() : base(LogEntryType.BotEjectLeave)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);
      Match match = regexForEject.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse Bot Eject event: '{0}'", originalString);
        return false;
      }

      botId = Utility.ParseInt(match.Groups[1].Value, -1);
      parentId = Utility.ParseInt(match.Groups[2].Value, -1);
      position3d = Utility.ParsePosition3d(match.Groups[3].Value);

      return true;
    }

    public override void Dump()
    {
      base.Dump();
      Plogger.Debug("\tBot Id: {0}, Parent Id: {1}, Position: {1}", BotId, ParentId, Position);
    }

    public override string ToString()
    {
      return string.Format("[BotEjectEntry: Position={0}, BotId={1}, ParentId={2}]\n\toriginalString:{3}",
          Position, BotId, ParentId, OriginalString);
    }
  }

  public class RoundEndEntry : LogEntry
  {
    public RoundEndEntry() : base(LogEntryType.RoundEnd)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      return base.Initialize(originalString, ticks);
    }
  }

  public class MissionEndEntry : LogEntry
  {
    public MissionEndEntry() : base(LogEntryType.MissionEnd)
    {
    }

    public override bool Initialize(string originalString, long ticks)
    {
      return base.Initialize(originalString, ticks);
    }
  }

  public class ArtificialEntry : LogEntry
  {
    private static Regex regexForArtificial = new Regex(@"DATA:(.*)", RegexOptions.Compiled);
    private string data;

    public string Data { get { return data; } }

    public ArtificialEntry() : base(LogEntryType.Artificial)
    {
    }

    public ArtificialEntry(long ticks, string data) : this()
    {
      var originalString = $"T:{ticks} AType:{(int)this.EntryType} DATA:{data}";
      this.Initialize(originalString, ticks);
    }

    public override bool Initialize(string originalString, long ticks)
    {
      base.Initialize(originalString, ticks);

      Match match = regexForArtificial.Match(originalString);

      if (!match.Success)
      {
        Plogger.Error("Failed to parse Data");
        return false;
      }

      data = match.Groups[1].Value;
      return true;
    }
  }
}

