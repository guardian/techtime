## Coding Conventions

1. The classes at `game-server/library` should not know about each other. If there is a need for orchestration, do it at `GameLibrary`.

1. The classes at `game-server/library` should only have _mostly pure_ functions. Keep the IO in `GameLibrary`. (Note: A "_mostly pure_ function" doesn't do IO, but may not necessarily be totally deterministic, if it makes a random choice. An example is `UserFleet::spawnCapitalTopUpChallenge(difficulty)`, also those functions may use the global variable `$GAME_PARAMETERS`.)
