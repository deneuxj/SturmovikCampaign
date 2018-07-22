# SturmovikCampaign #

A dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad and other episodes in the series.
Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>.

### What is this repository for? ###

This reporitory contains the source code for SturmovikCampaign. It compiles to an assembly, which can be loaded by [SturmovikServerControl](https://bitbucket.org/johdex/sturmovikservercontrol).
This repository also contains the source code for a small app that can also load SturmovikCampaign.
It's a simpler replacement for SturmovikServerControl which does not provide any means to control DServer.exe from IL-2 BOS.

### How do I get set up? ###

To build from source, you will need to acquire a number of dependencies manually:
* [TickDelayMonitoring](https://bitbucket.org/johdex/tickdelaymonitoring)
* [SturmovikServerControl](https://bitbucket.org/johdex/sturmovikservercontrol)
* [SturmovikMission](https://github.com/deneuxj/SturmovikMission)
* [plog](https://bitbucket.org/johdex/plog)

Other dependencies are managed via NuGet, and should be retrieved automatically by Visual Studio 2017. You'll need to have F# support enabled in Visual Studio, by the way.

To get the campaign running, see Campaign/Installation.txt.
To add or modify a campaigns scenario, see Campaign/howto-create-campaign-map.txt.
