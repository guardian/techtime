## Game Rules


### Rule 1

The game happens within a square of size **1000x1000**. ( The map's points have their 2D coordinates chosen between **0** and **1000** ).


### Rule 2

Even though any unit could be used we will measure distances in **kilometres**. So the map has a size of 1000 kilometers by 1000 kilometers. 

### Rule 3

Maps have **500** possible jump points, randomly positioned on the game canvas.

### Rule 4

As a player you must get a `userkey`. There is one user key per username. Getting that key is your first step when starting to play. User keys prevent cheating by impersonating other players. Keep that key secret. See **Game API.md** for details.

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

The scoring is simple. Everytime you destroy a ship, your score is increased by a amount determined by the ship type. The relative ranking of players (for the Guardian leaderboard is the same as it's been the last few weeks before this game: best player gets 0.1 point per hour, second player 70% of that, etc..)

### Rule 12

Unlike in the first version of the game where players actions were throttled by the server, there no longer is a restriction on the rate of queries players may send per unit of time. Players queries happen as fast that server hardware and network latency allow.

The Capital Ship top up challenge's proof of work is what slows the game down. You should control your energy levels carefully and be sure not to run out of it, otherwise your ships will be vulnerable to attacks until you top up (plus the time it will take you to distribute that energy around).

### Rule 13: Server IP address

The server's IP addresses are given in **02-Game URLs (quick look).txt**. We try to keep them stable. (Otherwise any changes will be communicated on the Weekly Coding Channel chat room.) 


