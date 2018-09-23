# Travelling Optimization on Map of Energy Points


This week's challenge is the follow up of that week's which was based on the [travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem).

## Introduction

This week we again play with points on a 2D surface, but unlike last week where the problem was to minimise a path's length, this week the problem is to maximize it, but with a twist: in order to travel, you need **energy**. If at any point of your journey you find yourself running out of it, then your path stops where you stand.

## Game mechanics

Say hello to the server. Note that the port is different from last week.

```
curl http://10.249.16.173:14462
```

### Game API and datatypes

To get the current map:

```
curl http://10.249.16.173:14462/game/v1/map
```

This will return a JSON object looking like this

```
{
    "mapId"  : "796b1df1-b0e9-4138-ae2d-9aa681dd2ed6",
    "timestamp": "2018-09-07-11",
    "points" : [
        {
            "label" : "e167",
            "coordinates": [2, -2.17],
            "energy" => 2.341
        }
        ...
    ]
}
```

**Same from last week**

The `timestamp` is the the date and the hour. The value `2018-09-07-11` says that the map lives until 11:59 on 2018-09-07.

Each point has a label and coordinates. The label is unique to that point and will be used by you to refer to the point later on. 

You will then want to submit a path. A path is an ordered sequence of points that you go through. From the first point to the last.

Assuming that you visit the points in this order: `e167` -> `f432` -> ... -> `21dd`, you would then call

```
curl http://10.249.16.173:14462/game/v1/submit/<yourname>/<mapId>/e167,f432,...,21dd
```

After `submit`, you indicate your name, like, say, "pascal.honore". Try and use the same name during the entire duration of the game. Then you specify the `mapId`, this to ensure that you do not submit a solution for the wrong map. Then you list the points by giving their labels separated by comas.

**Different from last week**

Unlike last week the number of points in the map is not fixed, but will be changing from hour to hour. (Currently set up to be randomly chosen between 20 and 40.)

There is an additional field to a point from last week, called `energy`. 

```
{
    "label" : "e167",
    "coordinates": [2, -2.17],
    "energy" => 2.341
}
```

This is the amount of energy that will be added to your energy tank once you reach the corresponding point. This value is not necessarily stable from one point to another. Also note that **this value can be negative**, meaning that when you reach that point, no energy is added to your energy tank but you actually experience a small leak.

The paths that you submit **do not need** to contain all the points. You can even submit a path of length 0 (which contains only one point -- meaning that you started at that point and stayed there). But paths **cannot contain the same point twice**.

When you submit a solution the server will compute the length of the path specified by your sequence. This total will be the value of your submission and your ranking for this map will be computed accordingly. And again, bigger values are ranked first. And again, bigger values are ranked first.

The initial value of your energy tank is the energy value of the first point you decide to visit. Then to be accepted your path **needs** to have the following property. To move to point **n** to point **n+1**, the value of your energy tank must be at least equal to the square of the distance between the two points. Then when you arrive at point **n+1**, then the square of the distance is substracted from your energy tank and then the energy value of the point **n+1** is added to it.  

The above implies that if you start at a point with negative energy, then you will definitively not be able to go anywhere else, move over your value of that submission will be negative :)  
 
### Scoring and Scheduling

Same as last week. Every player gets some points and a new map is generated every hour.

Because the system is more complex, I encourage you to play on the command line first to get a feel of the problem (use short paths to see how things work), before you start writing your client or start modifying your client from last week. 


### Game score report

The game has got its own scoring which can bee seen at

```
curl http://10.249.16.173:14462/game/v1/scores
```