# SturmovikCampaign #

A dynamic persistent campaign for the IL-2 Sturmovik: Great Battles series of games.

### What is this repository for? ###

This reporitory contains the source code for the dynamic campaign and related tools:
- BanEnforcer: A web service to manage player bans. Not yet usable.
- FitArea: A console utility to find areas clear from woods, cities and water.
- GroupInstantiator: A console utility to create complex mission logic from simple groups from the mission editor.
- RoadExtractor: A console utility to extract road and railway graphs from maps.
- WebController: A web service to run and control a multiplayer dynamic campaign.
- WorldCheck: A console utility to validate campaign scenario files.

The code makes use of https://github.com/deneuxj/SturmovikMission to process and combine fragments of missions.

An html/javascript front-end for WebController is in https://github.com/deneuxj/SturmovikCampaignWeb.

### How do I get set up? ###

Dependencies are managed via NuGet, and should be retrieved automatically by Visual Studio. You'll need to have F# support enabled in Visual Studio, by the way.

To get the campaign running, see Campaign/Installation.txt.
To add or modify a campaigns scenario, see Campaign/howto-create-campaign-map.txt.
