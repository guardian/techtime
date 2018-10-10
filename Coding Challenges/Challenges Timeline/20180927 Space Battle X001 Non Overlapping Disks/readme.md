Challenge is now closed. It ran from Thursday October 4th 2018 to Wednesday October 10th 2018.


## Non Overlapping Disks

The challenge this week is an instance of a very general class of optimization problems usually known as **packing problems**. This is the kind of problem that most companies shipping products  needs to solve to save space (and therefore money).

### Introduction

The space in which we are operating this week is the _Space Battle_ map, but this challenge doesn't at all interacts with the existing game. 

In order to state the aim of the challenge in an easy way, let's introduce three definitions.

1. A **disk** is a pair `(MapPointLabel, Radius: Float)`. The label has to be a valid map label, otherwise the disk itself will not be defined.

1. A (well defined) disk will be said to be **valid**, if it contains no other point of the map than its center. In other words, to be valid, the radius of the disk has to be smaller than the distance between the center of the disk and any other point of the map.

1. A collection of disks is said to be **valid** if each of its disks is valid and the disks do not overlap (pairwise). In other words the distance between the center of any two disks of the collection has to be smaller than the sum of their radii.

1. We also add the additional condition that your collection can have at most 50 disks (half of the number of points on the map).

The aim of the challenge is to come up with the *biggest* collection of valid disks. To make that more precise, we need two more definitions

1. The **value** of a disk is the square of its radius. (This was chosen so that the value is proportional to the surface area of the disk).

1. The **value** of a valid collection is the sum of values of the disks of the collection. 

You are after a collection of maximal value.

### Challenge details 

The map for the current hour can be downloaded from here

```
curl http://52.19.203.87:14561/challenge-20180927/map
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

Each map for this challenge will have 100 points (a subset of the points of the game's map). 

When you have valid collection, you can submit it to the server with

```
curl http://52.19.203.87:14561/challenge-20180927/submit/<mapid>/<yourname>/<label1>,<radius1>,<label2>,<radius2>,(etc)
```

Essentially, giving your collection of disks (each of them is a label together with a radius), you just linearise the collection as a sequence of alternating labels and radii. 

**Advice**: when playing with it at first, try just one point with a small radius (1 kilometers) and then increase it slight to see how far you can push that one disk. Then try with two disks... This alone will give you a good idea of the problem you are trying to solve. 

The value of `yourname` should be something nice and sweet, like `pascal.honore`.

When the server receives your submission it will perform the following tasks

1. Record the name.
1. Turn the last URL fragment into a collection of disks
1. Check that you are submitting no more than 50 disks
1. Check that no disk intersects any point of the map
1. Check that the disks do not intersect each other
1. Compute the value of each disk and the value of the collection. Record that against the name of the user 

Scores per hour will be available here

```
curl http://52.19.203.87:14561/challenge-20180927/scores
```