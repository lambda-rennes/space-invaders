# Space Invaders

This code is the starting point of a coding Dojo held by the Lambda Rennes
functional programming meetup. The goal is to implement a clone of all-time
video game classic: SPACE INVADERS !

We use the Haskell programming language, which embraces pure functional
programming. We rely on the Gloss library, which exposes an Elm-ish API for
writing interactive applications using 2D graphics.

## Your tasks

As it is, the code compiles and displays:
- The spaceship.
- A monster.
- A trippy background image.

The game is entirely static and does not respond to user input. Your are asked
to fill in the blanks and implement the missing features.

### TASK 1: Render spaceship and monsters from game state

Current status: the rendering function displays a single monster at a fixed
position, irrelevant of the game state. The same holds for the spaceship.

Intended status: monsters and spaceships are rendered based on the state of the
game (`Game`).

### TASK 2: Allow the spaceship to move

The player should be able to move the spaceship with left and right arrow keys.
The spaceship should not be allowed to move past window boundaries.

### TASK 3: Missiles

Allow the user to send missiles by hitting spacebar. Make sure to remove
missiles out of the drawing area !

### TASK 4: Monster / missile collision

When a missile hits a monster, both should be destroyed.

### TASK 5: Scoring

Keep track of and render the player's score.

### TASK 6: Invasion

Monsters should move downwards row-by-row.

## Project structure

The project is composed of:
- A library, which contains all of the type definitions and application logic.
  Its code is found in `src/`.
- An executable that performs all the necessary plumbing for starting up the
  game. Its code is found in `app/`.

The interesting part of the code, where most of your work will take place, is
found in the `SpaceInvaders` module of the library (`src/SpaceInvaders.hs`)

## Compiling your code

To compile your code, type:

```
stack build
```

To run the program, type:

```
stack exec space-invaders
```
