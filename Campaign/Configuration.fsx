#I "bin/Debug"

#r "Campaign.dll"

open Campaign.Run
open Campaign.PlaneModel

let config =
    { Configuration.Default with
        PlaneSet = PlaneSet.Moscow
        StrategyFile = "StrategySmall1.mission"
        //OutputDir = @"C:\Users\johann\Documents\FromServer\AutoMoscow"
        OutputDir = @"C:\Users\johann\Documents\AutoMoscow"
        //ServerDataDir = @"C:\Users\johann\Documents\FromServer"
        ServerDataDir = @"E:\dserver\data"
        ServerBinDir = @"E:\dserver\bin"
        ServerSdsFile = @"sds\devserver.sds"
        ScriptPath = @"C:\Users\johann\Documents\SturmovikMission-git\Campaign\bin\Debug"
    }