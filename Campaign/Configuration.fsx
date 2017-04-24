#I "bin/Debug"

#r "Campaign.dll"

open Campaign.Run

let config =
    {
        StrategyFile = "StrategySmall1.mission"
        Seed = None // Some 0
        WeatherDayMaxOffset = 15
        MaxConvoys = 10
        MaxSimultaneousConvoys = 2
        MaxInvasionsInPlanning = 3
        MaxInvasions = 1
        MaxReinforcements = 1
        MissionName = "AutoGenMission2"
        MissionLength = 180
        ConvoyInterval = 60
        OutputDir = @"C:\Users\johann\Documents\AutoMoscow"
        ServerDataDir = @"E:\dserver\data"
        ServerBinDir = @"E:\dserver\bin"
        ServerSdsFile = @"sds\devserver.sds"
        ScriptPath = @"C:\Users\johann\Documents\SturmovikMission-git\Campaign\bin\Debug"
        ThinkTime = 30
        Briefing = @"
Let the battle begin!

This mission is part of a dynamic campaign, where the events from one mission are carried over to the next mission.

Large cities with factories produce tanks, planes, ammo for the anti-air and anti-tank canons and supplies to repair damage storage facilities.
Damaging factories reduces the overall production output of their region.

Each region has a number of storage facilities in the form of dugouts and baraks. Damaging these reduces the amount of ammo potentially available to the region's defenses.

Supplies are transfered via truck convoys and trains. Trains have a greater capacity than truck convoys.

Regions are captured by tank columns. If a tank column reaches the capital of a region, a battle is played after the mission's end with the defense forces.
If victorious, the invaders gain control over the region, including its airfields and all the planes parked there!

Airplanes can be transfered to new airfields (including enemy ones!) by landing at a different airfield than the original one.
Transfered planes, if undamaged, are available at the destination airfield in the next mission.
Damages to planes, including parked ones, are tracked and affect the availability of planes. For example, it takes two identical planes with 50% damage to make one plane available.
"
    }