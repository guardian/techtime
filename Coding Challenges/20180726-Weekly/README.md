### Get a personal key

In order to play, and because everybody interacts with the same server, you need to ask Pascal for a personal key. This avoid cheating by impersonation.

### Game server IP address and Port

The IP address of the game server is currently **lucille19.local**. This machine is Pascal's iMac in the office. The IP should be stable but if the computer needs a restart the address might change. If that happen the new address will be broadcasted on the channel. The port will always be **14001**

### Game

The purpose of the game is to let players (humans) play the Iterated Prisoner's Dilemma against each other. For background information about the Prisoner's Dilemma see: [https://en.wikipedia.org/wiki/Prisoner%27s_dilemma](https://en.wikipedia.org/wiki/Prisoner%27s_dilemma)

The game server contains the entire state of the game and comes with a HTTP interface for user interaction. You can play using `curl` at the command line or write a client to play for you (in the programming language of your choice).

### Glossary

A **game step** is when two players each play "cooperate" or "betray". The points for a step are

```
cooperate & cooperate   : 3 & 3
cooperate & betray      : 0 & 5
betray    & cooperate   : 5 & 0
betray    & betray      : 1 & 1
```

A **game sequence** is 10 game steps. Once a game is completed, both players get points according to the above matrix. 

### User Interface Principles

Assuming IP address **lucille19.local** you can test that the server is running with 

```
curl http://lucille19.local:14001/server/ping
```

with returns `["pong"]`. 

The following request returns the game scores (one score per player)

```
curl http://lucille19.local:14001/game-board
```

The two above commands do not require an API key. The remaining commands have the form

```
curl http://lucille19.local:14001/game/<your-api-key>/<command-name>/argument1/argument2/<etc...>
```

Lookup requests are GET and action request are POST (see below for details). All game server answers are valid JSON objects.

### Game Actions

The first action you might want to do is to look up the list of players

```
curl http://lucille19.local:14001/game/<your-api-key>/players
```

Once you have identified a player you want to play with, you can start a game with 

```
curl http://lucille19.local:14001/game/<your-api-key>/start/<player-name>
```

The answer is either 

```
{
    "event" => "Starting a game between you (#{partyName}) and #{counterpartyName}",
    "party" => partyName,
    "cuonterparty" => counterpartyName,
    "gameId" => gameId
}
``` 

or `[null]`. You should record the `game-id` as you will need it later on. The answer `[null]` happens if a new game with that player could not start (mostly because there is already an on-going game with that player).

Once you have a `game-id` you can make a move with either

```
curl http://lucille19.local:14001/game/<your-api-key>/play/<game-id>/cooperate
```

or 

```
curl http://lucille19.local:14001/game/<your-api-key>/play/<game-id>/betray
```

The game is set up so that you do not need the other player to have completed their side of a game step to make another step. In this sense, playing with somebody happens in a completely asynchronous fashion. If you make several steps (and possibly all 10 steps of a game sequence), your opponent with only see the moves you made for already completed steps. In other words, you can safely "pre-play" your moves if you want to.

The two above requests both return `[true]` if the move was valid and `[false]` if it wasn't. A move is invalid if the player has already made 10 moves in that game.

You can see the state of a game with 

```
curl http://lucille19.local:14001/game/<your-api-key>/game-status/<game-id>
```

This returns an object of the form

```
{
	"Alice"  : Array[Digits],
	"Bob"    : Array[Digits],
	"scores" : null,
	"game_metadata" : {
		"game_id"       : String
		"starting_date" : DateTime,
		"players"       : [Name1, Name2]
		"status"        : "on-going"
	}
}
```

Where Alice and Bob are playing and assuming Alice sent the request she will see all her moves so far but only the moves made by Bob on completed steps. In other words, her array in always longer (or equal size) than Bob's array. The digits are either 0 or 1, where 0 means "betrayal" and 1 means "cooperative".
 
When a game has been completed, the answer is

```
{
	"Alice"  : Array[Digits],
	"Bob"    : Array[Digits],
	"scores" : {
		"Alice" : Integer,
		"Bob"   : Integer
	}
	"game_metadata" : {
		"game_id"       : String
		"starting_date" : DateTime
		"players"       : [Name1, Name2]
		"status"        : "completed"
	}
}
```

Your score for this week's challenge is the sum of all your scores across all your completed games devided by the number of completed games. In other words, your game sequence score average. If you have no completed games, your score is 0.

You can get all your games with

```
curl http://lucille19.local:14001/game/<your-api-key>/my-games
```

In particular this will let me extract your game ids.

### Points

The points you get on the weekly challenge leaderboard will be: **10 points** for the best, 70% that for the second best, and so on (similar scoring system of last week).

### Deadlines

The first phase of the game will end on **Monday 30th July at 6pm**. The leaderboard will then be updated. 

Then the entire game state will be totally reset, and another phase will start ending **Monday 6th August 6pm**, followed by another update of the leaderboard. 

Each phase carries 10 points for the player in first position. This really means that somebody winning both phases can grab up to **20 points**. 
