#I "bin/Debug"

#r "Campaign.dll"
#r "NLog"

open Campaign.Run
open Campaign.PlaneModel
open Campaign.Configuration

NLog.LogManager.Configuration <- NLog.Config.LoggingConfiguration()

let config =
    { Configuration.Default with
        PlaneSet = PlaneSet.EarlyAccess
        StrategyFile = "KubanAutumn.mission"
        OutputDir = @"C:\Users\johann\Documents\FromServer\Expert"
        //OutputDir = @"C:\Users\johann\Documents\AutoMoscow"
        //ServerDataDir = @"C:\Users\johann\Documents\FromServer"
        ServerDataDir = @"E:\dserver\data"
        ServerBinDir = @"E:\dserver\bin"
        ServerSdsFile = @"sds\devserver.sds"
        ScriptPath = @"C:\Users\johann\Documents\SturmovikCampaign\Campaign\bin\Debug"
    }