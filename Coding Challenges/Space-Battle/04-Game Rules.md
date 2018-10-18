## Game Rules


### Rule 1

The game happens within a square of size **1000x1000**. ( The map's points have their 2D coordinates chosen between **0** and **1000** ).


### Rule 2

Even though any unit could be used we will measure distances in **kilometres**. So the map has a size of 1000 kilometers by 1000 kilometers. 

### Rule 3

Maps have **500** possible jump points, randomly positioned on the game canvas.

### Rule 4

As a player you must get a `userkey`. There is one userkey per username. Getting that key is your first step when starting to play. User keys prevent cheating by impersonating other players. Keep that key secret. To get your userkey, talk to Pascal.

### Rule 5

As for the two previous weeks, a new map / game is started every hour. You can carry your userkey over from one game to another (you only need to query it once).

### Rule 6

Ships attack each other through **wormhole bombs**. They are self contained energy packets that you can send through hyperspace at particular jump points. Note that those bombs are pure energy and therefore will affect and possibly destroy your own ships if you shoot at yourself. This can happen if you shoot at another ship and by accident one of your own ships was present at the same location. We usually refer to wormhole bombs simply as **bombs**.

### Rule 7

Energy Carriers die as soon as they are hit by a bomb. When a bomb hit a battle cruiser or a capital ship, those ships are affected proportionaly to the **effective energy** of the bomb.

### Rule 8

Bombs have a **nominal power** (the amount of energy packet into them when they are built), but they also have an **effective power** which is the energy they have once they explode. The effective power is computed by a formula that uses the nominal power and _distance to target_ as inputs. The effective power is the energy that is substracted from the ship's energy level and if that energy level become negative, the ship dies.

### Rule 9

When a new map is created, a special fleet, belonging to user called "The BBC" is created and put into the game. There is the Capital Ship and a dozen battle cruisers. They are immobile and do not shoot at anything. Use them as target practice, for free points :)

### Rule 10

When your Capital ship is dead, you can still control your Battle Cruisers, but you cannot control the energy carriers (in particular they cannot jump). If you have active energy carriers and active cruisers at the time your Capital dies. You can still move your battle cruisers where the energy carriers and perform energy transfers.

### Rule 11

The scoring is simple. Your score is increased either

- when you destroy a ship. The increment depends on the type of the ship you've destroyed, or 
- when you move one of your ships. The increment depends on the type of the ship you moved.

The relative ranking of players (for the Guardian leaderboard) is the same as it's been the last few weeks before this game: best player gets 0.1 point per hour, second player 70% of that, etc..

With `7*24=168` hours in a week, you can make up to **16.8** points per week.

### Rule 12

Unlike in the first version of the game where players actions were throttled by the server, there no longer is a restriction on the rate of queries players may send per unit of time. Players queries happen as fast that server hardware and network latency allow.

The Capital Ship top up challenge's proof of work is what slows the game down. You should control your energy levels carefully and be sure not to run out of it, otherwise your ships will be vulnerable to attacks until you top up (plus the time it will take you to distribute that energy around).

### Rule 13: Server IP address

The server's IP addresses are given in **02-Game URLs (quick look).txt**. We try to keep them stable. (Otherwise any changes will be communicated on the Weekly Coding Channel chat room.) 

### Rule 14: game-parameters.json

Some aspects of the game are driven by the file `game-parameters.json`, which is a config file. When a new hour starts a copy of this file is stored together with the map and user data. This file is essentially a key value store and this rule helps clarifying the meaning of some of the keys

- **bombsEffectMultiplier** (current value: **3**). Bombing work as follows: Your battle cruiser makes an energy bomb from its own energy tank, then wormholes that bomb to another location on the map, the bomb explodes and makes damages to ships at the target location. The damage that is applied to target ships manifests itself as a decrease of the target ships' energy reserve. In this sense the bombs are *anti-energy* bombs.
	- The building of bombs is standardised. It cost **10** energy units to build (this is the value of `fleetBattleCruiserBombBuildingCost`) and then they carry **50** energy units (this is the value of `fleetBattleCruiserBombNominalEnergy`). This value of **50** is referred to as their **nominal energy**. 
	- After having been transported across space the bomb has lost a bit of energy. The energy it carries after transport is given by the following formula 
		```
		nominalEnergy*Math.exp(-distanceToTarget.to_f/300)
		``` 
	- The amount of energy it will take from targetted ships is the above value multiplied by **3**, which is the current value of `bombsEffectMultiplier`. 
	- Example: if you want to hit a ship located 300 kilometers  away, this will cost you `10 + 50 = 60` energy units and the targetted ships will lose `50*Math.exp(-100/300)*3 = 107.47` energy units. The same bombs from 5 kilometers away would cost them `50*Math.exp(-5/300)*3 = 147.52` energy units

- **fleetShipNomenclature2JumpCostCoefficient`** (current value ):
	
	```
	 {
        "energyCarrier": 0.01,
        "battleCruiser": 0.1,
        "capitalShip"  : 0.5
    }
	``` 
	- Depending on the ship the same jump costs differently. Almost nothing for the energy carrier, 10 times more for the battle cruiser, and 5 times the latter for the capital ship. The formula is `(distanceBetweenPoints**1.1)*coefficient`. 
	- Example: jumping 200 kilometers costs `3.39` energy units to an energy carrier, but will cost `169.86` if the capital ship wanted to make the same jump.
	
- **serverThrottlingPausingPeriodInSeconds** (current value: 0.1). The server pause 100 miliseconds before executing your next request if your last request hit it less than 100 milliseconds ago.
 	