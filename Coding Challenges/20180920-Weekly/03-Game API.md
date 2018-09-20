# Space Battle: Game API


## Game Data

### Say Hello

```
curl http://10.249.16.173:14561
```

### Get your user key

I order to play you need to have a user key. It is issued once and you need to keep it secret. In order to get one, perform the two following steps:

1. Choose your username. I recommend something like `pascal.honore`. Note that your username cannot have a colon (`:`).

1. Call 
	
	```
	curl http://10.249.16.173:14561/game/v1/get-userkey/<YOUR-USERNAME>
	```

Be careful not to leak your userkey on chat when copy pasting commands.

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
curl http://10.249.16.173:14561/game/v1/game-parameters
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
    "spaceProbeResults" : Map[UUID, SpaceProbeResults]
    "logWarnings"       : Array[WarningLogItem]
}
``` 

- `capitalEnergyTopUpChallenge` is used for the Capital Ship energy top up. See the section "Capital Ship top up" below for details.

- The score indicates how many points you have accumulated so far. 

- `spaceProbeResults` are the results of a battle cruiser scanning space and retriving the location of enemy ships. The report is dated, with a timestamp expressed in Unixtime, and the type and location of enemy ships are given. Note that ships probing capabilities only extend to a disc of radius 300 kilometers from the current location of the ship. 

    ```
    SpaceProbeResults
    {
        "unixtime" : Unixtime
        "datetime" : DateTime # same instant as the unixtime, given for user friendliness
        "results"  : Array[SpaceProbeResultItem]
    }
    
    SpaceProbeResultItem 
    {
        "location"     : MapPoint
        "nomenclature" : ShipNomenclature
        "username"     : USERNAME # who owns this ship
    }
    ```

- The `logWarnings` indicates even that have occured that you might (should) be interested in. Mostly that enemy Battle Cruisers are shooting at you.

	```
	WarningLogItem
	{
		"unixtime"  : Unixtime
		"eventUUID" : UUID
		"eventType" : "WormholeBomb"
		"eventData" : {
			"source" : {
				"location"     : MapPoint
				"nomenclature" : "BattleCruiser"
				"username"     : ENEMY-USERNAME
			}
			"target" : Ship # CapitalShip, BattleCruiser or EnergyCarrier
		}
	}
	```

For the moment the game only reports wormholes bomb. Note that the target is always one of your ships, so the report gives you the uuid of that ship, you might want to move it, if it's not destroyed yet, and possibly shoot back if you have Battle Cruisers nearby.

## Game API

### Capital Ship Initialization

```
curl /game/v1/:userkey/:mapid/capital-ship/init
```

This can only be done once per game. And in particular, you cannot do it again after your Capital Ship has been destroyed for the current game.

The return value is your initial fleet report.

### Create Battle Cruiser

```
curl /game/v1/:userkey/:mapid/capital-ship/create-battle-cruiser
```

Creates a battle cruiser and returns the corresponding BattleCruiser object. The call fails if your Capital didn't have enough energy to create the ship and fill it with that much energy.

### Space Probes

Are performed by Battle Cruisers and are used to attempt to detect enemy ships. Note that a standard probe only covers a 300 kilometers radius.

```
/game/v1/:userkey/:mapid/space-probe/:battlecruisershipuuid
```

### Create Energy Carrier

```
curl /game/v1/:userkey/:mapid/capital-ship/create-energy-carrier/:energyamount
```

Creates an energy carrier and returns the corresponding EnergyCarrier object. Note that in this command you need to specify the amount of energy you want the carrier to carry. The call fails if your Capital didn't have enough energy to create the ship and fill it with that much energy.

### Moving ships around

You can move your ship from any MapPoint to any other assuming you have enough energy to make the jump. You specify the ship by giving its uuid (the ship must obviously belong to you) and you specify the target point by giving it label.

```
curl /game/v1/:userkey/:mapid/jump/:shipuuid/:targetpointlabel
```

The energy expenditure for a jump depends on the distance you want to travel (the energy needed is proportional to the distance), but also the type of the ship. In increasing energy expenditure per ship type you have: carriers, battle cruisers and the Capital Ship (meaning that it is cheaper to move a carrier across a given distance than a battle cruiser). Capital Ships tend to be, by design, a bit expensive to move around.

### Energy transfer

You can transfer energy from any ship to any other, the computation will automatically prevent overflow (meaning we respect ships total capacities)

For the former the call is 

```
/game/v1/:userkey/:mapid/energy-transfer/:ship1uuid/:ship2uuid/:energylevel
```

**:energylevel** is how much you want to transfer.

### Shooting at things

Only battle cruisers can shoot at things and the action simply consists in instructing a cruiser to send a standard wormhole bomb at a location.

```
curl /game/v1/:userkey/:mapid/bomb/:shipuuid/:targetpointlabel
```

The answer is a BombReport

```
BombReport
{
	"shipsHit" : Array[BombReportShipDetail]
}

BombReportShipDetail
{
	"username"     : USERNAME
	"nomenclature" : SHIP-NOMENCLATURE
	"destroyed"    : Boolean
}
```
Note that since an energy wormhole bomb is indiscriminate, you can hit your worn ships and the username can be your username. You do not get points for destroying your own fleet. 

### Capital Ship top up

```
curl /game/v1/:userkey/:mapid/capital-ship/top-up/:code
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

The top up call return `[true]` when successful, otherwise you get a 403.

## Standard Answers

Answers from the API are either 

**Error Conditions**

```
{
    "status"  => HTTP Error Code
    "message" => String
}
```

**200**

```
{
    "status" => 200
    "answer" => ({data relevant, answer} to the request itself)
    "userFleet" => FleetReport
}
```

In particluar you get a full FleetReport at every query.

## Scores

```
/game/v1/scores
```

Text output, results per hour followed by summary.