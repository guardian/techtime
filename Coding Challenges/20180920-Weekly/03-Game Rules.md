## Game Rules

1. The game happens within a square of size **1000x1000**. ( The map's points have their 2D coordinates chosen between **0** and **1000** ).

1. Even though any unit could be used we will measure distances in **kilometres**. So the map has a size of 1000 kilometers by 1000 kilometers. 

1. Maps have **500** possible jump points, randomly positioned on the game canvas.

1. As a player you must get a `userkey`. There is one user key per username. Getting that key is your first step when starting to play. User keys prevent cheating by impersonating other players. Keep that key secret. See **Game API.md** for details.

1. As for the two previous weeks, a new map / game is started every hour. You can carry your userkey over from one game to another (you only need to query it once).

1. Ships attack each other through **wormhole bombs**. They are self contained energy packets that you can send through hyperspace at particular jump points. Note that those bombs are pure energy and therefore will affect and possibly destroy your own ships if you shoot at yourself. This can happen if you shoot at another ship and by accident one of your own ships was present at the same location. We usually refer to wormhole bombs simply as **bombs**.

1. Energy Carriers die as soon as they are hit by a bomb. When a bomb hit a battle cruiser or a capital ship, those ships are affected to the amount of the effective energy of the bomb.

1. Bombs have a **nominal power** (the amount of energy packet into them when they are built), but they also have an **effective power** which is the energy they have once they explode. The effective power is the energy that is substracted from the ship's energy level and if that energy level become negative, the ship dies.

1. When a new map is created, a special fleet, belonging to user called "TheBBC" is created and put into the game. There is the Capital Ship and a dozen battle cruisers. They are immobile and do not shoot at anything. Use them as target practice, for free points :)

1. When your Capital ship is dead, you can still control your Battle Cruisers, but you cannot control the energy carriers (in particular they cannot jump). If you have active energy carriers and active cruisers at the time your Capital dies. You can still move your battle cruisers where the energy carriers and perform the energy transfer.

 