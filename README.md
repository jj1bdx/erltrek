# Erltrek: Star Trek Game in Erlang

* Requirement: Erlang/OTP 17.1
* Tested on: OS X 10.9.3, FreeBSD 10-STABLE
* License: BSD 3-clause (Note: tinymt-erlang has its own BSD license, compatible with this software)
* *Note well: this program is still in the alpha level. Please report bugs to the GitHub issues and contribute through the pull requests.*

## Travis CI build status for the master branch

[![Build Status](https://travis-ci.org/jj1bdx/erltrek.svg?branch=master)](https://travis-ci.org/jj1bdx/erltrek)

## Goals

* Incorporate true concurrency (= game proceeds in real-time)
* Keep following the traditional Star Trek model
    * Coordinate system: 2D and dual layer (Quadrants + Sectors)
    * Weapons: phaser (torpedo is optional)
    * Not too fast but not too slow
* Start from a simplified model but make it extendable
* Write in pure Erlang/OTP

## Non-goals

* Using NIFs (unless absolutely necessary)

## Functions implemented at tag `baselevel_20140416`

(Most of code files are revised and rewritten by Andreas Stenius)

* Game field setup
* Impulse engine for Enterprise
* Enterprise firing phaser to Klingon
* Klingon firing to Enterprise
* Enterprise can dock/undock to/from the starbase
* The Game is now a proper Erlang application
* Game message handled by an gen\_event server
* Dedicated command shell
* The galaxy is a process (see `erltrek_galaxy`)
* The ships (Enterprise and Klingons) are processes
* The game no longer depends on internal time synchronization
* Type spec for functions are now 17.0-compatible

## Functions under development

* Torpedoes

## Make options (of erlang.mk)

* `Makefile` works on both BSD/GNU make
* `Makefile.erltrek` is the real GNU make file; edit this file for modification
* Building: `make`
* Documentation: `make docs` (not yet ready)
* Testing: `make tests` (not yet ready)
* See also [erlang.mk](https://github.com/extend/erlang.mk) for the details

## How to run (will invoke a dedicated shell)

    ./game.sh

## On random number initialization

The processes using tinymt32 module will begin with the same internal state,
for an easy debugging.  See `erltrek_setup:seed/0` for the details.  *This
feature will surely be removed in later versions.* Also, the players should be
aware that all ships are *concurrently* running, so the sequence of execution
will *not* be guaranteed.

## Related YouTube Talk at Erlang Factory SF Bay 2014

(Courtesy Erlang Solutions)

* <http://youtu.be/1mJUhUDipuo>

## TODO

* Documentation in the source code (edoc or edown)
* Eunit test cases
* Refactoring with maps (maps may replace most of dict functionality)
* Running on Windows

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
