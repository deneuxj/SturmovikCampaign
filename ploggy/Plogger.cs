using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

#if NO_NLOG
#else
using NLog;
#endif

namespace ploggy
{
#if NO_NLOG
    public class Plogger
    {
        public static void Init()
        {
        }

        public static void Debug(string format, params object[] objs)
        {
        }

        public static void Debug(string str)
        {
        }

        public static void Debug(object obj)
        {
        }

        public static void Info(string format, params object[] objs)
        {
        }

        public static void Info(string str)
        {
        }

        public static void Info(object obj)
        {
        }

        public static void Warn(string format, params object[] objs)
        {
        }

        public static void Warn(string str)
        {
        }

        public static void Warn(object obj)
        {
        }

        public static void Error(string format, params object[] objs)
        {
        }

        public static void Error(string str)
        {
        }

        public static void Error(object obj)
        {
        }
    }
#else
  public class Plogger
  {
    private static Logger nlogLogger = LogManager.GetCurrentClassLogger();

    public static void Init()
    {
      LogManager.Configuration.DefaultCultureInfo = Utility.floatNumbersCulture;
    }

    public static void Debug(string format, params object[] objs)
    {
      nlogLogger.Debug(format, objs);
    }

    public static void Debug(string str)
    {
      nlogLogger.Debug(str);
    }

    public static void Debug(object obj)
    {
      nlogLogger.Debug(obj);
    }

    public static void Info(string format, params object[] objs)
    {
      nlogLogger.Info(format, objs);
    }

    public static void Info(string str)
    {
      nlogLogger.Info(str);
    }

    public static void Info(object obj)
    {
      nlogLogger.Info(obj);
    }

    public static void Warn(string format, params object[] objs)
    {
      nlogLogger.Warn(format, objs);
    }

    public static void Warn(string str)
    {
      nlogLogger.Warn(str);
    }

    public static void Warn(object obj)
    {
      nlogLogger.Warn(obj);
    }

    public static void Error(string format, params object[] objs)
    {
      nlogLogger.Error(format, objs);
    }

    public static void Error(string str)
    {
      nlogLogger.Error(str);
    }

    public static void Error(object obj)
    {
      nlogLogger.Error(obj);
    }
  }
#endif
}
