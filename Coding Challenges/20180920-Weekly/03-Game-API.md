## Space Battle: Game API


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
  "label": "4420b0",
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

This JSON object contains almost all of the contants/parameters of the game: from the size of the canvas and number of jump points to how much energy various operations cost etc.

### Capital Ship Initialization



### Capital Ship top up

```
/game/v1/:username/:userkey/:mapid/capital-ship/top-up/:code
```