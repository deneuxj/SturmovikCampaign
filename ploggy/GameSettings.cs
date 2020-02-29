using System;

namespace ploggy
{
  public class GameSettings
  {
    private bool[] boolArray = null;
    private bool mods = true;
    private SettingsPreset preset = SettingsPreset.Custom;

    public bool ModsEnabled { get { return mods; } }
    public SettingsPreset Preset { get { return preset; } }

    public GameSettings(string settingStr, SettingsPreset preset, bool mods)
    {
      if (settingStr == null)
        return;

      this.mods = mods;
      this.preset = preset;

      ParseSettings(settingStr);
    }

    private void ParseSettings(string settingsStr)
    {
      int count = settingsStr.Length;
      boolArray = new bool[count];

      for (int i = 0; i < count; ++i)
        boolArray[i] = settingsStr[i] == '1';
    }

    private bool GetSettingsFlag(int index)
    {
      if (boolArray == null)
        return false;

      if (boolArray.Length < index)
        return false;

      if (index < 0)
        return false;

      return boolArray[index];
    }

    public bool ObjectIcons { get { return GetSettingsFlag(0); } }
    public bool NavigationIcons { get { return GetSettingsFlag(1); } }
    public bool UnlimitedMapIcons { get { return GetSettingsFlag(2); } }
    public bool SimpleDevices { get { return GetSettingsFlag(3); } }
    public bool AimingHelp { get { return GetSettingsFlag(4); } }
    public bool PadLock { get { return GetSettingsFlag(5); } }
    public bool AutoCoordination { get { return GetSettingsFlag(6); } }
    public bool AutoMixture { get { return GetSettingsFlag(7); } }
    public bool AutoPilot { get { return GetSettingsFlag(8); } }
    public bool AutoRadiator { get { return GetSettingsFlag(9); } }
    public bool AutoStart { get { return GetSettingsFlag(10); } }
    public bool AutoThrottle { get { return GetSettingsFlag(11); } }
    public bool AutoThrottleLimit { get { return GetSettingsFlag(12); } }
    public bool AutoStabilize { get { return GetSettingsFlag(13); } }
    public bool NoEngineStops { get { return GetSettingsFlag(14); } }
    public bool WarmedUpEngine { get { return GetSettingsFlag(15); } }
    public bool Invulnerability { get { return GetSettingsFlag(16); } }
    public bool NoBreak { get { return GetSettingsFlag(17); } }
    public bool NoMisfire { get { return GetSettingsFlag(18); } }
    public bool NoMoment { get { return GetSettingsFlag(19); } }
    public bool NoWind { get { return GetSettingsFlag(20); } }
    public bool UnlimitedAmmo { get { return GetSettingsFlag(21); } }
    public bool UnlimitedFuel { get { return GetSettingsFlag(22); } }
    public bool AllowSpectators { get { return GetSettingsFlag(23); } }
    public bool Subtitles { get { return GetSettingsFlag(24); } }
    public bool CourseWeaponAimimg { get { return GetSettingsFlag(26); } }
  }
}

