CreateTankColumn, Copyright (C) 2015 Johann Deneux
Licensed under the GNU Public License, see COPYING.txt.

This is a command-line tool to create tank columns for the game IL-2 Sturmovik: Battle of Stalingrad.

Tank Column Specification
-------------------------

Create a group file with the mission editor containing a vehicle and a number of waypoints. The vehicle and the waypoints will be replaced by multiple groups of vehicles.
Each group is called a platoon, and is composed of a number of tanks (currently 2, although that might change in the future) and escort vehicles (a single AA truck).
A unit is composed of a number of platoons (2 by default).
A column is composed of a number of platoons (6 by default, which corresponds to 3 full units).

The name of a waypoint is used to control the behaviour of the column:
- CombatZone: Leave the road and prepare to spread out
- Spread: The generated waypoint is moved laterally by a random amount (currently hard-coded to 400m)
- Tighten: The generated waypoint is moved laterally by a random amount (currently hard-coded to 80m)
- Destination: Last waypoint. When reached, the column is considered to be successful.
- OnRoad: To be used after a CombatZone to get back on the road (not fully implemented, tanks from multiple platoons tend to collide)

The orientation and location of the vehicle define the location and orientation of the vehicles when they are spawned.

The country of the vehicle decides the type and country of the generated vehicles.

Generated group
---------------

The tool generates a group file ready to be imported in the mission editor.
The tank groups in a column and their logic are all located on top of each other.
As an exception, the following nodes make the interface of the column and are moved a bit away from the rest of the mess (north-east of the spawn point):
- Init: A timer which spawns a platoon.
  All platoons in a column a linked in such a way that platoons are spawned on a regular basis (fixed time interval).
  The Init node of the first platoon must be dragged away and connected (as target) to logic that activates the platoon (e.g. a MissionBegin MCU)
- ShutdownAll: Deactivate all vehicles and their logic.
- SomeSuccess: At least one vehicle reached the destination.
- AllFailure: All platoons were destroyed. 99% reliable (you'll probably want to use a timeout for those 1% when complete destruction was not detected).

Behaviour of the column
-----------------------

Beside what has been described in the tank column specification regarding waypoints, logic is also generated to implement a number of interesting behaviours.

When a platoon is approached by an enemy plane, a green flare is fired by the platoon to request air cover, and the platoon performs an emergency stop.

When a platoon reaches the destination, it fires a yellow flare.

When a platoon approaches another platoon on the road, it stops until the other platoon moves ahead or is destroyed.

When a platoon in a unit reaches the first spread point after the combat zone entry, it waits for the other platoons in the unit before proceeding.

Customizing platoons
--------------------

To change the types of vehicles, or change their numbers, add to or modify their behaviour, modify the file TankPlatoon.Group.
There are limitations to what you can and can't do:
- Do not rename existing nodes that have names.
- Do not remove existing nodes that have a name.
- Changing links between existing nodes may brake the behaviour of the generated platoons.
- All vehicles should be German ground vehicles. This means their country should be Axis/Germany, and the vehicle models and scripts should be German ones.
- If you change the number of vehicles, remember to update the count value of the InternalAllKilled counter.

It's hard describing exactly what can be done, and what will give broken results. Simply try it and look at the result in the mission editor.
