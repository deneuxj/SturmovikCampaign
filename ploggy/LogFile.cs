using System;
using System.Text.RegularExpressions;
using System.IO;
using System.Collections.Generic;

namespace ploggy
{
  public class LogFile
  {
    private List<LogEntry> entryList = new List<LogEntry>(1024);
    private string idString = null;

    public string LogId { get { return !string.IsNullOrEmpty(idString) ? idString : "mission"; } }
    public List<LogEntry> Entries { get { return entryList; } }

    private LogFile()
    {
    }

    public void ParseFile(string filePath)
    {
      TimeSpan last = new TimeSpan(0);

      using (StreamReader reader = new StreamReader(filePath))
      {
        while (true)
        {
          string line = reader.ReadLine();

          if (line == null)
            return;

          LogEntry logEntry = LogEntry.Parse(line);

          if (logEntry == null)
            continue;

          if (logEntry.IsValid())
            logEntry.Dump();

          if (logEntry.Timestamp < last)
            continue;

          last = logEntry.Timestamp;
          entryList.Add(logEntry);
        }
      }
    }

    public bool HaveLogEntries()
    {
      return entryList.Count > 0;
    }

    public static LogFile Parse(string pathToFile, bool follow = true)
    {
      Regex pathRegex = new Regex(@"(.+)\((.+)\)\[(\d+)\](.+)", RegexOptions.Compiled);
      Match match = pathRegex.Match(pathToFile);

      LogFile file = new LogFile();

      if (!follow || !match.Success)
      {
        if (!File.Exists(pathToFile))
          Plogger.Error("File '{0}' doesn't exist.", pathToFile);
        else
          file.ParseFile(pathToFile);

        return file;
      }

      string pathStart = match.Groups[1].Value;
      file.idString = match.Groups[2].Value;
      int counter = Utility.ParseInt(match.Groups[3].Value, 0);
      string pathFinish = match.Groups[4].Value;

      while (true)
      {
        string filename = string.Format("{0}({1})[{2}]{3}", pathStart, file.idString, counter, pathFinish);

        if (!File.Exists(filename))
          break;

        Plogger.Debug("Parsing file {0}", filename);
        ++counter;

        try
        {
          file.ParseFile(filename);
        }

        catch (Exception e)
        {
          Plogger.Error("Failed to read file '{0}', got exception: {1}", filename, e.Message);
          Plogger.Debug(e.StackTrace);
        }
      }

      return file;
    }
  }
}

