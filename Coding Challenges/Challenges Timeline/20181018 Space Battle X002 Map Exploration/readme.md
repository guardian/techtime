## Space Battle X002 Map Exploration

This week the challenge is again based on the game Space Battle and is the next baby step towards playing. The game's logic around scoring has been expanded and in addition of the points you get for destroying other ships, everytime you move one of your ships some points are also added to the fleet's game score (which determines your ranking for the hour and consequently your leaderboard increase at the end of the week). This means that exploration is now rewarded.

The increment is driven by the following map

```
{
    "energyCarrier" : 1,
    "battleCruiser" : 10,
    "capitalShip"   : 50
}
```

The map and scoring logic will remain, but the values of the above map are only valid for this wee (more pernament values will be given from next week).

In order to make your first points, you just need to

1. Make sure you have your userkey (ask Pascal if not).
2. Init your fleet.
3. Look up a map location and move your capital ship (or make another ship and move the latter one) to that location. Goto 3.

If you ever run out of energy for the jumps, remember that you can top-up your capital ship with additional energy, by calling the `top-up` end-point.



 