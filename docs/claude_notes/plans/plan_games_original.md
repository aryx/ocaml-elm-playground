# Plan: an original game, something no game in the catalogue (or elsewhere) did

Every game in `CATALOG.md` is there for what it brought: a new
mechanic, a higher model of physics (ragdolls, the gravity gun), a
graphics trick (the raycaster, Mode 7, the BSP), a smarter AI
(alpha-beta, flow fields), or a new way to play with time (Braid) or
space (Portal, Monument Valley). Read in order, the 2.5D games and
their 3D twins are a history of how games *cheated*: each rendering
trick came with limits (the raycaster cannot show a stair, Doom cannot
put a room over a room, Mode 7 is one flat plane, Voxel Space cannot
roll, the isometric view throws the height away), and each 3D twin
says what the z-buffer made unnecessary.

The question: what game could be *original*, invent something the
catalogue's originals did not? The ideas below come from a discussion
on what the project can do that others can't: the model is a value,
one model can have many views, the software rasterizer owns every
pixel, and the kits already hold most of graphics history. (Names and
dates of the games cited from memory, to check.) Ideas 1 to 4 came
first; the "Second pass" section reads every game's header, axis by
axis, and adds Ideas 5 to 8 from the gaps it finds.

## Idea 1: TinyRetcon, the renderer is the laws of physics (the pick)

The world is only what the current renderer can show. The player
switches between the renderers of graphics history, and each switch
changes what exists:

- **Wolfenstein mode** (raycaster, one height per cell): stairs and
  pits flatten; the pit you could not cross is now floor, the ledge
  you could not reach is a wall.
- **Doom mode** (sectors, BSP): heights come back, but a room cannot
  sit over another room; where a bridge crosses a corridor, one of
  the two stops existing, and the renderer's rule says which.
- **Mode 7**: the world is one textured plane; walls become painted
  lines you drive over.
- **Isometric**: height is lost, so Monument Valley's ambiguity: blocks
  that look adjacent are adjacent.
- **Painter's algorithm, no z-buffer**: a wrong sort draws a far wall
  in front of a near one, and what is drawn in front *is* in front --
  the sorting bug is a door (`PaintersAlgorithmFail3d` already shows
  the artifact).
- **Voxel Space**: no roll, so no falling over; a narrow ridge becomes
  walkable.

Why here:

- One model, many views is the Elm promise `TinyTron3d` shows; here
  the switch changes the *level*, not only the look.
- Every renderer exists already as a kit or a game: `Sectors`, the
  raycaster of `TinyWolfenstein`, `Isometric`, `Heightmap`, the Mode 7
  floor of `TinyMarioKart`.
- The teaching payoff is the project's thesis: to solve a level, the
  player has to understand what each algorithm cannot represent.

The design problem at its core: one canonical world (full 3D, in the
style of `Segments` or `Sectors`), and per mode a *projection* into
what that mode can represent. Collision runs on the projected world;
each mode's lossy projection is the mechanic.

### Prior art: games that switch 3D/2D or projection as a puzzle

Idea 1 belongs to a known family. What those games do:

Switching between 3D and 2D:

- **Crush** (Zoë Mode / Sega, PSP, 2007; *Crush3D* on 3DS, 2012): a 3D
  level "crushed" flat along the camera's axis; platforms far apart in
  depth become one 2D platform. The most literal 3D-to-2D puzzle game.
  (Now in the catalogue: `TinyCrush.ml`, a stack of 2D slices and their
  union, the same platformer on either; the first of this family here,
  and a first step towards Idea 1's projections.)
- **Fez** (Phil Fish / Polytron, 2012): a 3D world played in a 2D
  projection, turned a quarter at a time; depth is ignored, so what
  lines up in the projection is connected. (Now in the catalogue:
  `TinyFez.ml`, with Fez's own rules -- any top a floor, only what is
  at your depth a wall -- and its pixel art, from XPM files.)
- **Super Paper Mario** (Intelligent Systems, Wii, 2007): a 2D
  side-scroller flipped into 3D to walk around obstacles, then back.
- **Perspective** (DigiPen student game, 2012): move a 3D camera, then
  drop into a 2D platformer played on what the camera shows.

The picture is the truth:

- **Echochrome** (Sony, 2008, after Jun Fujiki's *OLE Coordinate
  System*): Escher architecture; what looks connected is connected, a
  gap hidden behind a pillar is closed. `TinyMonumentValley` is in
  this line.
- **Monument Valley** (ustwo, 2014): impossible isometric architecture.
- **Superliminal** (Pillow Castle, 2019): forced perspective; an object
  keeps the size it appears to have on screen.
- **Viewfinder** (Sad Owl, 2023): a photo placed in the world becomes
  real 3D geometry.

Walking on projections (shadows):

- **Lost in Shadow** (Hudson, Wii, 2010) and **Contrast** (Compulsion
  Games, 2013): walking on shadows cast on walls; moving the light
  changes the level.
- **Shadowmatic** (Triada, 2015): turn an object until its shadow
  makes a shape.

Changing the era of the graphics:

- **Evoland** (Shiro Games, 2012): the game goes through graphics
  history (monochrome, 16 colours, 3D), but mostly as a joke; the
  graphics do not make the puzzles.

What is left new: all of these use *one geometric projection*
(flattening along an axis, a camera's angle, a shadow). Evoland alone
changes rendering technology, and there it does not change the rules.
So Idea 1 is in Crush's and Fez's family; its new part is narrower:
each mode is a real historical algorithm, and its *specific* limits
are the rules. The raycaster's one height per cell, Doom's no room
over a room, Mode 7's single plane and the painter's wrong sort each
behave differently, which a flattening axis doesn't. The levels
should avoid what Crush and Fez already do (lining things up in
depth) and build their puzzles on those algorithm-specific limits.

## Idea 2: the frame is the world

Collision against the last rendered framebuffer, not the model.
`TinyLemmings` already reads pixels for its terrain; this goes
further: anything drawn is solid -- the Hershey score digits, the HUD,
a particle trail, a shadow, a motion-blur smear. Scoring grows the
platform your score is; turning a debug overlay off removes a bridge.
Only possible because the software rasterizer owns every pixel.

## Idea 3: honest eyes, stealth where the guards render the scene

In `TinyMetalGearSolid`, sight is a cone. Here each guard renders the
scene from its own camera into a small ID buffer and reacts to how
many of your pixels it sees and how much they stand out from what is
around them. Real lighting, colour camouflage, standing behind a
billboard, hiding in the fog or past the far clip plane: all work
with no special case, because the guard sees what a renderer would.
It continues `AiBots`' question, what makes a bot fair. Thief and
Splinter Cell have a light meter; no game known to use a real render,
with colour matching, as a guard's sight.

The headers show a gap this idea fills: the computer never obeys the
player's limits. `TinyCivilization`'s rival "sees the whole map (no fog
of its own)"; in `TinyWarcraft2` the fog is "a drawing rule, not a rule
of the world", since the enemy ignores it. Only `TinySoldat`'s bots
(`ai=engine`, `Sense`) are limited in what they perceive. Here the
guards' sight is the player's sight, rendered from where the guard is.

## Idea 4: the model as a value, shown to the player

The player sees the `'model` record and can edit a few of its fields
per level; the level is designed so the invariants the game relies on
break. Baba Is You rewrites the rules, this rewrites the *state*.
Closer to `TinyCoreWar` and `TinyBabaIsYou`, so less new than 1 to 3.
Prior art: Hack 'n' Slash (Double Fine, 2014), in which the player's
sword edits the game's variables and later its code.

## Second pass: what the headers say, axis by axis

Each game's header says what its original broke, and most say what the
trick still could not do. Read together, they make a few ladders, one
per axis, and they show the gaps: the next rung that no game climbs,
or a limit that no game turns into play.

### The ladders

- **Physics**: rules instead of physics (Pong) -> inertia and gravity
  (Spacewar!, Asteroids, Lunar Lander's tolerances) -> Newton's third
  law by hand (XPilot's rope) -> speed along a surface (Sonic) ->
  stacking and breaking by impulse (Slingshot) -> tunnelling fought by
  substeps (Pinball) then by sweeping (Pinball3d, Soldat's bullets) ->
  physics as the weapon (Half-Life 2) -> the world itself breaking
  (Teardown). Left as exercises: materials of different strength, fire,
  fluids, a rope that burns. The most explored axis.
- **Graphics**: vectors (Battlezone, Elite: "you see through
  everything") -> one projection faked a row, a column or an object at
  a time (Out Run, Mode 7, Wolfenstein, Doom, Voxel Space, Zaxxon) ->
  work moved before the game (Quake's vis and light) -> the z-buffer
  (every 3D twin). `README-2.5d.md`'s thesis: "Nothing can be above
  something else, and that restriction is what makes each trick
  possible." Wolfenstein's header: "Its limits are its rules."
- **AI**: patterns and targets (Pac-Man's ghosts, Invaders) -> the
  deliberately stupid crowd (Robotron, Gauntlet's "THE MONSTERS ARE
  DELIBERATELY STUPID", Lode Runner) -> search (A* in Dune 2, flow
  fields in Warcraft 2, Tower Defense's search "as a referee") -> game
  trees (Othello, Chess, Connect 4) -> giving up knowledge (Go's
  Monte Carlo playouts) -> bots limited in what they sense (Soldat).
  No game has an opponent that learns or models the player; learning
  (`ai/`'s phases 10-11: MENACE, Q-learning) is still a plan.
- **Time**: the frame is the clock (everything) -> time as turns
  (Rogue: "a game's clock doesn't have to be the display's") -> turns
  taken out (Dungeon Master, Diablo) -> time units and reaction fire
  (XCOM) -> a world as a function of t (Frogger) -> frames as the unit
  of a move (Street Fighter II, hitstop) -> the music's clock, not the
  frame's (DDR) -> rewind and time as position (Braid) -> death paying
  for the next run (Hades). Left undone: Braid's World 6, "time as a
  field over space, each object stepped by its own clock".
- **Space and dimensions**: one screen -> a world bigger than the
  screen (Defender's cylinder, Mario's scrolling) -> a world in any
  order (Zelda, Metroid's locks and keys) -> depth on a belt (Final
  Fight) -> height lost and given back by a shadow (Zaxxon, Marble
  Madness, Sensible Soccer, Mario 64, Boomerang Fu) -> six degrees of
  freedom (Elite, Descent) -> space as a transform (Portal) -> the
  projection's lie as the rule (Monument Valley). Recurring "cannot":
  rooms above rooms, overhangs, tunnels, roll.
- **Audio**: the pitch of a thing's speed (Pong's blip, Gran Trak's
  engine) -> the music as the clock (DDR) -> you hear what you play
  (Guitar Hero) -> Doppler and panning (Star Fox). Sound carries
  information in two places only: Gran Trak's pitch, to time a gear,
  and Shuffle Puck's *clack*, which gave back the one number its
  perspective threw away ("how far away the puck is"). Everything else
  is feedback. The audio library (`audio/`: synthesis, ABC tunes,
  `Audio3d` with interaural delay, air and Doppler, `Spectrum`) can do
  much more than the games ask of it. No microphone yet.
- **Input**: a knob per player (Tennis for Two) -> a trackball and a
  point to aim at (Missile Command) -> two directions at once
  (Robotron) -> input kept acting after the kick (Kick Off's
  aftertouch) -> motion read from an input history (Street Fighter II)
  -> the mouse as the whole interface (Solitaire, Diablo, Maniac
  Mansion) -> sentences (Zork) -> indirect orders (Lemmings) ->
  a program as the input (Core War) -> small lies told in the player's
  favour (Celeste). Left undone everywhere: analog input (Lunar
  Lander's throttle), the network (Maze War, XPilot, rollback).
- **Information**: what is hidden (Solitaire against FreeCell, Rick's
  traps "hidden until it kills you once", the grue that is never seen)
  -> what has been seen (Rogue, Civilization, Warcraft 2's two
  bitmaps) -> what each unit sees (XCOM's cones, Metal Gear's radar,
  jammed "when you most need it").

### The pattern: a limit turned into a rule

Almost every "what it brought" is the same move: a machine could not
do something, so the designers constrained the player, and the
constraint became the game. Space Invaders' processor moved one alien a
frame, and the speed-up was kept. Star Fox could not afford free
flight, so rails, "two numbers instead of six". Alone in the Dark could
not draw its rooms, so fixed cameras, so tank controls. Metal Gear
could not show many enemies, so avoid them. Dungeon Master's painted
slots: "The price of the trick is in the rules". Rick Dangerous could
not scroll, so flip-screens. Frogger had no memory, so lanes as
functions of t. The kept accidents are the same move by chance:
Invaders' speed-up, Street Fighter II's combos, Metroid's sequence
breaks.

Idea 1 applies this move on purpose, to rendering. The ideas below
apply it on the other axes, or climb the missing rung of a ladder.

### The gaps

1. **Sound is never how the player knows something**, outside the
   rhythm games and two single cues.
2. **The computer never learns the player**, and never obeys the
   player's limits (the rivals see through the fog).
3. **Time is always one clock for everybody.** Braid's World 6 is an
   exercise.
4. **The physics engine's own limits (the time step, tunnelling) are
   fought with substeps, never played with.** Only the renderers' limits
   became games.
5. **The ambiguity of a projection** is patched with a shadow
   everywhere except Monument Valley (Idea 1 continues it).
6. **Analog input, the microphone and the network** are left undone
   in every header that mentions them.

## Idea 5: TinyTimestep, the physics engine's limits are the rules

The physics twin of Idea 1. `TinyPinball`'s header says it: at 1/60 s a
fast ball goes through a wall, "and even a wall can be the fast one";
the cure is four substeps, and `TinyPinball3d`'s sweeping is the next
one. Here the player holds the dial. With few steps a second, you
tunnel through thin walls, but you can no longer land on a thin ledge
or stop against a spring. With many steps you are exact, and the walls
hold. Other engine settings can join the dial one level at a time:
the solver's iterations (too few, and a stack of boxes sinks into
itself and lets you through), the restitution Pong's port pushes to
1.05 (energy made from nothing), Tennis for Two's switch between the
Moon's and Jupiter's gravity.

A lean variant on the same idea: Celeste's "small named lies" (coyote
time, jump buffering, corner correction) as an inventory, collected
and spent, instead of Assist Mode's menu.

Prior art: speedrunners play with these limits (Mario 64's
backwards long jump, clipping through walls), but as accidents found
in a game, not as its design. None known that hands the time step to
the player.

## Idea 6: time as a field over space

Braid's World 6, left undone in `TinyBraid`, made into a whole game.
Each place has its own rate of time (a slow pool, a fast corridor),
and each object is stepped by the clock of where it is. Frogger's
trick makes this cheap: an object whose position is a pure function of
its own time needs no state, so a lane that crosses a slow zone just
bends. The player's weapon is to carry or move a zone: a car slowed
where you want to cross, a door held open by the slow air around it,
a platform on a fast loop.

A stranger variant: a finite speed of information. You see each thing
where it was when its light left it, and the enemies see you the same
way, so you shoot at where something was and hide from where they
will look.

Prior art: Braid's ring (one zone around one object); Superhot (2016,
time moves when you move: one clock, driven by the player); A Slower
Speed of Light (MIT Game Lab, 2012, relativistic effects to look at).
None known that gives each place its own clock and makes the level
out of it.

## Idea 7: the oracle, an opponent that learns you and shows it

The one AI rung no game climbs. The opponent predicts your next move
from your own input history, which `TinyStreetFighter` already keeps
to read special moves, with the simplest learner (counts of what
followed your last few moves, like MENACE's beads), and it *shows* its
guess: a ghost of the move it expects. You win by being unpredictable,
or by feeding it a habit and breaking it at the right moment. On
`Frame_data`: a fight at the level of the guess, not the reflexes.

Prior art: Aaronson's oracle (predicting a person's keypresses, the
game of rock-paper-scissors against it); Killer Instinct's Shadow AI
(2014) and Tekken's ghosts, which copy a player's style; Forza's
Drivatars. They learn quietly; none known that makes the prediction
visible and the whole game about beating it.

## Idea 8: sound as the renderer, a raycaster for the ears

What Shuffle Puck's *clack* did for one number, for the whole world:
a level played by ear, the screen dark or nearly. The 2.5D renderers
turned into sound renderers: Wolfenstein's DDA casts rays, but each
ray returns an echo (a delay, a loudness, a colour of sound from the
wall's material) instead of a screen column; Doppler gives the speed
of what moves (`Audio3d` has interaural delay, air and Doppler
already). A bat's game: a click to echolocate, which the monsters
hear too (`Sense.audible` exists on the AI side). And the Idea 1
move on audio: switch the "sound renderer" (plain stereo pan, then
interaural delay, then echoes), each one making a different part of
the level perceivable.

Prior art: audio games exist (Papa Sangre, 2010; A Blind Legend,
2016; The Vale, 2021), and Devil's Tuning Fork (DePaul, 2009) draws
echoes as light. The new part here is narrower: the echo computed by
the same ray casting as Wolfenstein's picture, and the renderers of
sound as rules.

## Order

1. Idea 1 first: it gathers most of the catalogue and has no exact
   precedent. First step: the canonical world's representation, and
   the projection of each mode into what it can represent.
2. Idea 5 next: the same move on physics, and small (a 2D level on the
   physics engine, with the time step in the model).
3. Idea 3 or Idea 7, in the AI line: 3 needs a renderer per guard, 7
   needs only a count table and `TinyStreetFighter`'s kit.
4. Ideas 6 and 8 when the playground can afford them (8 wants
   `Audio3d` driven by the game).
