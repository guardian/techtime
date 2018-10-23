# Space Battle: Game API


## Game Data

### Say Hello

```
curl http://10.249.16.173:14561
```

## Server Answers

With the exception of 
	
- `/game/v1/map`, and
- `/game/v1/parameters`

All answers from the API are JSON objects. Either

```
{
    "status"    => 200
    "answer"    => ({data relevant, answer} to the request itself)
    "userFleet" => FleetReport
}
```

or

```
{
    "status"  => HTTP Error Code
    "error"   => String # hexadecimal, length 8
    "message" => String
}
```

Which indicate, respectively, correct call, or a server 4XX kind of error. The server always returns HTTP 200 `application/json` but indicate in the JSON answer itself what the HTTP 4XX error is. This means that the client only needs to manipulate the JSON payload, and not HTTP error codes and JSON. 

The error is an hexadecimal string of length 8. This is what you should be pattern matching against if your code needs to handle error types. Do not match against the messages themselves, are they are not stable.

### Get your user key

A user key used to be available using self service end point, but due to greater exposure you should now request your key directly from Pascal. 

Be careful not to leak your userkey on chat if you copy paste urls.

### Get the current map

```
curl http://10.249.16.173:14561/game/v1/map
```

The map has the following structure 

```
Map: {
  "mapId"     : "0fa91da4-788e-458e-b8b0-4e57d31ffbee"
  "timestamp" : "2018-09-15-21"
  "points"    : Array[MapPoint]
}

MapPoint: {
  "label": "4420b065",
  "coordinates": [
    58.21,
    599.61
  ]
}
```

### Game parameters

```
curl http://10.249.16.173:14561/game/v1/parameters
```

This JSON object contains almost all of the contants / parameters of the game: from the size of the canvas and number of jump points to how much energy various operations cost etc.

## Game DataTypes

In this section I describe the various basic data types

### Capital Ship

This is your main ship. It is the one that receives your energy mining codes and builds new ships. If you lose it (meaning it gets destroyed), you have lost that game, even if you have other ships (destroyers and energy carriers) still alive. 

```
CapitalShip 
{
	"nomenclature" : "capitalShip"
    "uuid"         : UUID
	"location"     : MapPoint
	"energyLevel"  : Float
	"alive"        : Boolean
}
```

- `location` is the same as a MapPoint.
- `alive` indicates whether your ship is still alive. In this current version of the game the only reason it would not be alive is that it was hit by a bomb after its shield went down.

### Battle Cruiser

Battle Cruisers are your main offensive units. They are the only ships that can deploy wormhole bombs. The are created by Capitals. 

```
BattleCruiser
{
	"nomenclature" : "battleCruiser"
	"uuid"         : UUID
	"location"     : MapPoint
	"energyLevel"  : Float
	"alive"        : Boolean
}
```

- shipUUID: is a completely unique uuid of this ship. It's a length 8 hexadecimal string. You use it to refer to the ship when sending commands to it. 

### Ship

`Ship` is the union type 

```
CapitalShip | BattleCruiser | EnergyCarrier
```

### Energy Carriers

Energy Carriers are special purpose ships essentially used to transfer energy between your Capital Ship and the Battle Cruisers. They are created by capitals

```
EnergyCarrier
{
	"nomenclature" : "energyCarrier"
	"uuid"         : UUID
	"location"     : MapPoint
	"energyLevel"  : Float
	"alive"        : Boolean
}
```

### Fleet Report

The fleet report contains all information about your entire fleet

```
FleetReport
{
    "username"          : YOUR-USERNAME
    "capitalEnergyTopUpChallenge" : CapitalShipTopUpChallenge
    "gameScore"         : Float
    "ships"             : Array[Ship] # Ship is CapitalShip, BattleCruiser or EnergyCarrier
    "mapExploration"    : Array[MapPointLabel]
    "spaceProbeResults" : Map[UUID, SpaceProbeResults]
    "logWarnings"       : Array[WarningLogItem]
}
``` 

- `capitalEnergyTopUpChallenge` is used for the Capital Ship energy top up. See the section "Capital Ship top up" below for details.

- The score indicates how many points you have accumulated so far. 

- `mapExploration` records the map point labels that your ships have visited (was introduced as part of the expansion of game scoring to encourage exploration). 

- `spaceProbeResults` are the results of a battle cruiser scanning space and retriving the location of enemy ships. The report is dated, with a timestamp expressed in Unixtime, and the type and location of enemy ships are given. Note that ships probing capabilities only extend to a disc of radius 300 kilometers from the current location of the ship. The `location` is the location from which the scan was performed (the center of the scan disc).

    ```
    SpaceProbeResults
    {
        "unixtime" : Unixtime
        "datetime" : DateTime # same instant as the unixtime, given for user friendliness
        "location" : MapPoint
        "results"  : Array[SpaceProbeResultItem]
    }
    
    SpaceProbeResultItem 
    {
        "location"     : MapPoint
        "nomenclature" : ShipNomenclature
        "username"     : USERNAME # who owns this ship
    }
    ```

- The `logWarnings` indicates even that have occured that you might (should) be interested in. Mostly that enemy Battle Cruisers are shooting at you. `logWarnings` is an aray of `WarningLogItem`, the latter is a union type, which version it is is given by the value of `eventType`. In this version of the game there is only one type in this  union: `WarningLogItemWormholeBomb` 

	```
	WarningLogItemWormholeBomb
	{
		"unixtime"  : Unixtime
		"eventUUID" : UUID
		"eventType" : "WormholeBomb"
		"eventData" : {
			"source" : WarningLogItemWormholeBombEventDataSource
			"target" : Ship
		}
	}
	
	WarningLogItemWormholeBombEventDataSource
	{
		"location"     : MapPoint
		"nomenclature" : String # ship nomenclature
		"username"     : String # enemy username
	}
	
	```

For the moment the game only reports wormholes bomb. Note that the target is always one of your ships, so the report gives you the uuid of that ship, you might want to move it, if it's not destroyed yet, and possibly shoot back if you have Battle Cruisers nearby.

## Game API

### Generic Errors

The following errors can be returned by any API calls.

- 401, "c26b7c33", "Invalid userkey"
- 404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"
- 403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"
- 403, "86877586", "Your capital ship for this hour is dead"

### Capital Ship Initialization and Fleet Reports

```
/game/v1/:userkey/:mapid/capital-ship/init
```

This can only be done once per game. And in particular, you cannot do it again after your Capital Ship has been destroyed for the current game.

The return value is your initial fleet report.

Errors:

- 403, "3b6f4992", "You cannot init a Capital Ship, you already have one for this hour"

### Fleet

At any point you can query your fleet report with

```
/game/v1/:userkey/:mapid/fleet
```

### Create Battle Cruiser

```
/game/v1/:userkey/:mapid/capital-ship/create-battle-cruiser
```

Creates a battle cruiser and returns the corresponding BattleCruiser object. The call fails if your Capital didn't have enough energy to create the ship and fill it with that much energy.

Errors:

- 403, "36be6a8b", "Your capital ship doesn't have enough energy (...)"

### Space Probes

Are performed by Battle Cruisers and are used to attempt to detect enemy ships. Note that a standard probe only covers a 300 kilometers radius.

```
/game/v1/:userkey/:mapid/space-probe/:battlecruisershipuuid
```

Errors:

- 404, "a0ce7e39", "Your fleet has no ship with uuid #{battleCruiserShipUUID}"
- 403, "051366e2", "The probing battle cruiser is dead"

### Create Energy Carrier

```
/game/v1/:userkey/:mapid/capital-ship/create-energy-carrier/:energyamount
```

Creates an energy carrier and returns the corresponding EnergyCarrier object. Note that in this command you need to specify the amount of energy you want the carrier to carry. The call fails if your Capital didn't have enough energy to create the ship and fill it with that much energy.

Errors:

- 403, "fc31efd0", "Your capital ship doesn't have enough energy (...)"
- 403, "b68c3046", "You are creating a carrier with too much energy. (...)"

### Moving ships around

You can move your ship from any MapPoint to any other assuming you have enough energy to make the jump. You specify the ship by giving its uuid (the ship must obviously belong to you) and you specify the target point by giving it label.

```
/game/v1/:userkey/:mapid/jump/:shipuuid/:targetpointlabel
```

The energy expenditure for a jump depends on the distance you want to travel (the energy needed is proportional to the distance), but also the type of the ship. In increasing energy expenditure per ship type you have: carriers, battle cruisers and the Capital Ship (meaning that it is cheaper to move a carrier across a given distance than a battle cruiser). Capital Ships tend to be, by design, a bit expensive to move around.

Errors:

- 404, "34d25d8a", "The specified point doesn't exist"
- 403, "f7a8dee2", "The ship is dead"
- 403, "03717296", "Your capital ship is dead. You cannot jump energy carriers in that case."
- 403, "c36b1859", "The ship doesn't have enough energy for this jump. ()"

### Energy transfer

You can transfer energy from any ship to any other, the computation will automatically prevent overflow (meaning we respect ships total capacities)

For the former the call is 

```
/game/v1/:userkey/:mapid/energy-transfer/:ship1uuid/:ship2uuid/:energylevel
```

**:energylevel** is how much you want to transfer.

Errors:

- 403, "66474ae3", "You are transferring energy from a ship to itself."
- 404, "7b680a12", "Your fleet has no ship with uuid #{ship1uuid}"
- 404, "1c5436b9", "Your fleet has no ship with uuid #{ship2uuid}"
- 403, "391388ae", "The source ship, #{ship1uuid}, is dead"
- 403, "a9e028ed", "The target ship, #{ship2uuid}, is dead"
- 403, "a9971906", "You cannot transfer energy between the two ships, they are not at the same map location"
- 403, "cf1e71c1", "The source ship has no energy to transfer"

### Shooting at things

Only battle cruisers can shoot at things and the action simply consists in instructing a cruiser to send a standard wormhole bomb at a location.

```
/game/v1/:userkey/:mapid/bomb/:shipuuid/:targetpointlabel
```

The answer is a AttackerBombDamageReport

```
AttackerBombDamageReport = Array[AttackerBombDamageReportItem]

AttackerBombDamageReportItem
{
	"username"     : USERNAME
	"nomenclature" : SHIP-NOMENCLATURE
	"destroyed"    : Boolean
}
```
Note that since an energy wormhole bomb is indiscriminate, you can hit your worn ships and the username can be your username. You do not get points for destroying your own fleet. 

Errors:

- 404, "88bb18fd", "The specified point doesn't exist"
- 404, "1a0ddb98", "Your fleet has no ship with uuid #{attackerBattleCruiserShipUUID}"
- 403, "bc0bb00f", "Your attacking battle cruiser is dead"
- 403, "943802d8", "Your attacking battle cruiser doesn't have enough energy to complete the construction of a bomb"

### Capital Ship top up

```
/game/v1/:userkey/:mapid/capital-ship/top-up/:code
```

The process of mining energy for a Capital Ship top up is simple. It's just a proof of work. You just need to find a code a given hash. 

The datatype CapitalShipTopUpChallenge is like this

```
CapitalShipTopUpChallenge 
{
	"input" : "flkjadu"
	"difficulty" : 1
}
```

You will need to find an **alphabetical string** (and other string will be rejected), for instance "nnaywpahgahl", such that the input concatenated with your solution has a `sha1` hash in hexadecimal form ending with the number of zeros indicated by the difficulty.

In this case we have 

```
$ echo -n "flkjadunnaywpahgahl" | sha1-stdin 
cf97896ca2dfb9185cf62f4574cbc067b7b95b20
```

Therefore you could use "nnaywpahgahl" as your top up code. How much energy is added to your Capital Ship energy level is defined by the game parameters.

When you complete a top-up challenge and commit your top-up code, a new challenge is written into your fleet's "capitalEnergyTopUpChallenge" key, allowing you to start working on a new top up.

Errors:

- 403, "d7713626", "Your code is correct, please keep it (!), but you cannot submit it at this time. Your ship has too much energy in reserve."
- 403, "d07feb9c", "Your code is not a solution to the challenge"

## Scores

To get the scores, call

```
/game/v1/scores/:hourcode1/:hourcode2
```

`hourcode1` and `hourcode2` are timestamps in the form `YYYY-MM-DD-HH`. You specify the datetime range you want the scoring to run for.

If you call 

```
/game/v1/scores
```

Then you get the scores for the current hour.

Text output, results per hour followed by summary.
