# A small experient with genetic algorithms

... work in progress 

## World

The world exists of field in three states:
* free
* marked
* blocked

The challenge for the bot ist to turn all free fields to marked fields.

## Operations

A bot's programm consists of different operations:
* nop : Do nothing
* set : Set current field to marked
* {skip, Count} : Skip the next N (= Count) Operations
* {move, Direction} : Move one field to the given operation. If the field is blocked, nothing is done.
* {skip_next_if, Direction, Status} : Look at field in given direction and skip the next operation if this field has the given state.

Directions are
* left
* right
* up
* down
