# Space Battle

## Introduction

**Space Battle** is a game born out of (move exactly evolved from) the weekly Guardian engineering coding challenges of Summer 2018. 

The game has three kind of objects: your **Capital Ships**, **Battle Cruisers** and **Energy Carriers**. The main idea is simple: you control a space fleet and your objective is to destroy enemy space ships before they destroy yours.

The most striking aspect of this game, relatively to other games you may have played, is that it comes without a graphical user interface. It only comes with an HTTP interface. You could play on the command line, as all the actions (from generating energy, to building or moving your ships, to shooting at enemies) are simple curl+url commands. Obviously, you may also want, as it's been done with challenges of previous weeks that came with an HTTP interface, just build a small client, in the programming language of your choice, and let it play for you. In this sense it's a game made for developers. 

We carry on with this game the same scheduling that has shown to be great for previous weekly challenges: a new game starts at the top of every hour. This means that if you make some mistakes, don't worry, you get a fresh start at the next hour. 

For Guardian staff: the points you make each hour are accumulated throughout the week and added to the leaderboard every Wednesday evening.

## Playing

### How do I join (or just keep an eye on it) ?

For Guardian staff: join us at the Hangouts Chat "Weekly Coding Challenge" channel. For external players: just drop Pascal a line (github handle: **shtukas**).

Players need a userkey that is given away upon request (there isn't a self service userkey end point for the moment).

### Where can I find the server ?

The two IP addresses at which the game can be found are: 

1. Pascal's iMac in the office (available through VPN): **10.249.16.20**, and 
2. External IP: **52.19.203.87**.

### What should I do first ?

First you can just test that the server is up and running by calling the current map (you do not need a userkey for that)

```
curl http://52.19.203.87:14561/game/v1/map
```

Then, assuming you have a userkey, look up the **mapId** in the answer of the previous call, and you can already **init** your fleet

```
curl http://52.19.203.87:14561/game/v1/:userkey/:mapid/capital-ship/init
```

Replace `:userkey` and `:mapid` by the correct values in the previous URL. A new map, and in fact an entire new game, starts at the top of each hour (with a new **mapId**). 

Everything else you need to know to play can be found in the game manuals.

## Game manuals

- [02-Game URLs](02-Game-URLs.txt): quick look up of all the end points.

- [03-Game API.md](03-Game-API.md): Read this if you intend to play. It contains everything you need to drive your fleet, either manually from the command line or through a programmatic client (written in the language of your choice since the interface is just HTTP).

- [04-Game Rules](04-Game-Rules.md): More details about the game.

## Roadmap

We don't really have a roadmap for the moment, but the API and game rules can evolve through user requests (in backwards compatible ways), so don't hesitate to keep in touch with a request or an idea.

## Acknowledgements

The game is the result of two things that were brought together:

- [Challenge 20180913](https://github.com/guardian/techtime/tree/master/Coding%20Challenges/Challenges%20Timeline/20180913%20Travelling%20Optimization%20on%20Map%20of%20Energy%20Points) (Travelling Optimization on Map of Energy Points), and 
- Pascal's love for [Eve Online](https://en.wikipedia.org/wiki/Eve_Online).
