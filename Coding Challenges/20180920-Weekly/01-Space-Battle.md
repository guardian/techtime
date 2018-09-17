
## Introduction

So... last week (phase 1) we did the travelling salesman problem. This week (phase 2) we are doing space exploration with quantum jumps between energy points (a sort of travelling optimization problem). Phase 1 and 2 had the characteristic that players were playing independently of each other, the only "interaction" between players was the ordering of path values, leading to the scoring.

Phase 3 is going to be a real step up in game mechanics, as Phase 3 will have players play directly against each other.

There will be one battle per hour. (At each hour a new battle starts.)

First let me share the game elements. There will be **Capital Ships**, each player will have one of those. **Battle Cruisers** (you will have zero or more of those) and **Energy Carriers** (you will have zero or more of those).

The principle of the game will be very simple. You want to inflict damages and possibly destroy other players ships and ideally their Capital Ship. The more destroying you do, the better. 

## Game actions

The map is a set of randomly generated points (pure coordinates, not energy points).

At the beginning of the hour you get one capital ship. It comes with a certain initial amount of **energy**. 

You can move the Capital Ship around by quantum jumping between points. This costs energy (taken from the Capital Ship's own energy tank).

You can build new battle cruisers. This costs energy (taken from the Capital Ship's own energy tank). 

You can move the battle cruisers around by quantum jumping between points. This costs energy (taken from the battle cruiser's own energy tank).

You can build energy carriers. This costs energy (taken from the Capital Ship's own energy tank).

You can move energy carriers around by quantum jumping between points. This costs energy (taken from the energy carrier's own energy tank).

When a battle cuiser and an energy carrier are at the same location, the energy carrier can transfer its energy to the battle cruiser. Hence the name "energy carrier".

You can scan some part of the space around the Battle Cruisers and retrieve the location of ships belonging to other players. This costs energy (taken from the Battle Cruiser's own energy tank). Your battle cruisers are how you probe space and retrieve the location of enemy ships

You can instruct a Battle Cruiser to shoot at another ship possibly located at another location. How much damage you produce to the target ship depends on the distance between you and the target. This costs energy (taken from the Battle Cruiser's own energy tank).

## Game winning conditions

Just blow stuff up. Points will be awarded depending how big the stuff you destroyed is. 

## The big question

At that point your main question should be "Pascal, given that all those actions need energy to perform, where do I find more of it ?". Answer: there will be a special action you can perform (that require CPU cycles on your own computer) that when completed tops up your Capital Ship energy tank. After which you can use energy carriers to distribute it to your battle cruisers, so that they carry on moving and blowing things up, or build more ships.

## Basic play actions and time limitations

You will be able to retrieve, in one single JSON object, the complete status of your entire fleet (types, locations, energy capabilities, log of recently inflicted damages and last known locations of the shooter), as well as the latest space probings performed by your battle cruisers.

Other actions are moving your ships and shooting at things.

You will be limited to one action per second (this is not to prevent overloading the server, but simply to prevent faster bots to have an unfair advantage on slower ones).