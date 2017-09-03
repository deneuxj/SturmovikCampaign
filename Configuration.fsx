#I "bin/Debug"

#r "Campaign.dll"

open Campaign.Run
open Campaign.PlaneModel
open Campaign.Configuration

let config =
    { Configuration.Default with
        PlaneSet = PlaneSet.Stalingrad
        StrategyFile = "StalingradSummer.mission"
        //OutputDir = @"C:\Users\johann\Documents\FromServer\AutoMoscow"
        OutputDir = @"C:\Users\johann\Documents\AutoMoscow"
        //ServerDataDir = @"C:\Users\johann\Documents\FromServer"
        ServerDataDir = @"E:\dserver\data"
        ServerBinDir = @"E:\dserver\bin"
        ServerSdsFile = @"sds\devserver.sds"
        ScriptPath = @"C:\Users\johann\Documents\SturmovikCampaign\bin\Debug"
    }