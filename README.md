# Erltrek: Star Trek Game in Erlang

* Requirement: R16B03-1
* License: BSD 3-clause (Note: tinymt-erlang has its own BSD license, compatible with this software)

## Changes in this version

Contributed by Andreas Stenius:

* The galaxy is a process (see `erltrek_galaxy`)
* The ships (Enterprise and Klingons) are processes
* The game no longer depends on internal time synchronization

## Goals

* Incorporate true concurrency (= game proceeds in real-time)
* Keep following the traditional Star Trek model
    * Coordinate system: 2D and dual layer (Quadrants + Sectors)
    * Weapons: phaser (torpedo is optional)
    * Not too fast but not too slow
* Start from a simplified model but make it extendable
* Write in pure Erlang/OTP

# Non-goals

* Using NIFs (unless absolutely necessary)

## Functions implemented at tag `baselevel_20140318`

* Game field setup
* Impulse engine for Enterprise
* Enterprise firing phaser to Klingon
* Enterprise can dock/undock to/from the starbase
* The Game is now a proper Erlang application (by Andreas Stenius)
* Game message handled by an gen\_event server (by Andreas Stenius)
* Dedicated command shell (by Andreas Stenius)

## Functions under development

* Klingon firing to Enterprise

## How to run (will invoke a dedicated shell)

    ./game.sh

## On random number initialization

Seeding of tinymt32 module is intentionally omitted to make casual testing
easier. The processes using tinymt32 module will begin with the same internal state.
See `erltrek_setup:seed/0` for the details.
*This feature will surely be removed in later versions.*

## Related YouTube Talk at Erlang Factory SF Bay 2014

(Courtesy Erlang Solutions)

* <http://youtu.be/1mJUhUDipuo>

## TODO

* Documentation in the source code (edoc or edown)
* Eunit test cases
* Porting to 17.0 (maps may replace most of dict functionality)
* Note: dialyzer remote type issues incompatibility between R16B03-1 and 17.0-rc2 reported as in
<http://erlang.org/pipermail/erlang-questions/2014-February/077945.html> and <http://erlang.org/pipermail/erlang-questions/2014-February/077955.html>; all -spec entries must be rewritten.

## Authors

* Andreas Stenius
* Kenji Rikitake

## Acknowledgments

* Cimarron Taylor
* Eric Allman
* Francesco Cesarini
* Fréd Hébert
* Kyoko Rikitake
* Loïc Hoguin
* Robert Virding
