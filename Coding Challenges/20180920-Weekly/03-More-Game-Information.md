## Additional game information

This document is a complement to the introduction and contains more details about the game, given in a completely random order.

- The game happens within a square of size **1000x1000**. ( The map's points have their 2D coordinates chosen between **0** and **1000** ).

- Even though any unit could be used we will measure distances in **kilometres**. So the map has a size of 1000 kilometers by 1000 kilometers. 

- Maps have **500** possible jump points, randomly positioned on the game canvas.
	
	```
	Math.sqrt(500) = 44.72 # number of jump points on a line. 
	1000.to_f/44.72 = 22.36 # kilometers between jump points if in a square lattice. 
	```

- As a player you must get a user key. There is one key per username. Getting that key is your first step when starting to play Space Battle. User keys prevent cheating by impersonating other players. Keep that key secret. See **03-Game-API.md** for details.

- As for the two previous weeks, a new map / game is started every hour. You can carry your userkey over (you only need to query it once)

- Lost of information can be found in the game parameters JSON object. See API documentation for details. 

- Ships attack each other through **wormhole bombs**. They are self contained energy packets that you can send through hyperspace at particular jump points. Note that those bombs are pure energy and therefore will affect and possibly destroy your own ships if you shoot at yourself. This can happen if you shoot at another ship and by accident one of your own ships was present at the same location. We usually refer to wormhole bombs simply as **bombs**.

- Capital Ships and Battle Cruisers come with a **shield**. The shield protects from attack from other ships (meaning bomb impacts). It is represented as a number between 0 and 1. A value of 0 means that the shield is totally down and the next impact will totally destroy it. Any ship can top up its shield by converting energy from its own energy reserve. Note that energy carriers do not have shields. They are destroyed (the the energy they carry lost) as soon as a bomb explodes at the location they are.