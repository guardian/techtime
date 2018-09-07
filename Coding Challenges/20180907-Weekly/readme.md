
This weeks challenge contains

- A game server to submit your solutions and see your score in real time.
- No need for API keys.
- No need to compute sha1 hashes.
- An interesting scoring mechanism. 

In other words, everything you've ever dreamt of.

## Introduction

We play in the cartesian plan, meaning the 2D space where each point has two coordinates. Such a pair of coordinates is written `(2, -2.17)`, for the point whose first coordinate is `2` and the second coordinate is `-2.17`.

The game server will generate 12 points. We will be referring to such a choice as a **map**.

Your mission, should you choose to accept it, is to solve the [travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem) on that map.

## Game mechanics

### Game API and datatypes

To get the current map:

```
curl http://10.249.16.173:14361/game/v1/map
```

This will return a JSON object looking like this

```
{
    "mapId"  : "796b1df1-b0e9-4138-ae2d-9aa681dd2ed6",
    "points" : [
        {
            "label" : "e1",
            "coordinates": [2, -2.17]
        }
        ...
    ]
}
```

Each point has a label and coordinates. The label is unique to that point and will be used by you to refer to the point later on. 

You will then want to submit a solution. A solution is an ordered sequence of points that the salesman should go through. From the first point to the last. 

Assuming that you think the salesman should visit the points in this order: `e1` -> `f4` -> ... -> `21`, you would then call

```
curl http://10.249.16.173:14361/game/v1/submit/<yourname>/<mapId>/e1,f4,...,21
```

After `submit`, you indicate your name, like, say, "pascal.honore". Try and use the same name during the entire duration of the game. Then you specify the `mapId`, this to ensure that you do not submit a solution for the wrong map. Then you list the points by giving their labels separated by comas.

When you submit a solution the server will compute the length of the path specified by your sequence. You should try and minimise that path.

### Scoring

Here I need to introduce the scoring. There won't be one winner (and everybody else is a loser). We are going to use the same geometric scoring we used in the past. The best player gets the maximum number of points and the second player gets 70% of that etc...

### Scheduling

A new map is going to be generated each hour. Moreover you need to submit your best solution before the end of the hour because once a new map is generated you can't submit a solution for an old one. You can submit more than one solution for each map, and the server will just keep your best solution, so in doubt just submit.

### Scoring (again)

The points attribution mechanism is independent between maps. In other words, the points you got in the previous maps (aka in the previous hour) are permanently awarded and will go to the leaderboard. 

Since we generate a new map every hour, each map carries a maximum of 0.1 points. If you are the best player of each hour for an entire week in a row, then you will make 0.1 * 24 * 7 = 16.8 points. Which makes this the best rewarded challenge so far. 

### Versions

The API urls have a fragment `v1`. This is because we want this game to run more than one week (with updates between weeks). This is then the version of the challenge you are playing. The next version (either an update next week or simply an update mid-week will be at `v2` etc). Each version will have its own documented url scheme, but all against the same game server and all variations of the same problem. 

### Game score report

The game has got its own scoring which can bee seen at

```
curl http://10.249.16.173:14361/game/v1/scores
```

There is an automatic feed from the game engine to the leaderboard, therefore the leaderboard is also real-time up to date. If you submit something to the game or submit an improvement to an existing solution, both the game score and the leaderboard will reflect it. Note though that the leaderboard will report the name you have chosen for the game, so you may appear in two places in the leaderboard. This discrepancy will be fixed at the end of the week when Pascal moves us from one variant to another one (essentially when game scores are manually permanently committed to the leaderboard).


