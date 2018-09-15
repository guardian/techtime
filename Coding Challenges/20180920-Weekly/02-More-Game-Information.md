## Additional game information

This document is a complement to the introduction and contains more details about the game, given in a completely random order.

- The game happens within a square of size **1000x1000**. ( The map's points have their 2D coordinates chosen between **0** and **1000** ).

- Maps have **500** possible jump points, randomly positioned on the game canvas.
	
	```
	Math.sqrt(500) = 44.72 # number of jump points on a line. 
	1000.to_f/44.72 = 22.36 # kilometers between jump points if in a square lattice. 
	```

- As a player you must get a user key. There is one key per username. Getting that key is your first step when starting to play Space Battle. User keys prevent cheating by impersonating other players. Keep that key secret. See **03-Game-API.md** for details.

- As for the two previous weeks, a new map / game is started every hour. You can carry your userkey over (you only need to query it once)

- Lost of information can be found in the game parameters JSON object. See API documentation for details. 