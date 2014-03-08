# Erltrek: Star Trek Game in Erlang

* Requirement: R16B03-1
* License: BSD 3-clause (Note: tinymt-erlang has its own BSD license, compatible with this software)

## Goals

* Incorporating true concurrency (= game proceeds in real-time)
* Not altering the traditional Star Trek model
    * Coordinate system: 2D and dual layer (Quadrants + Sectors)
    * Weapons: phaser (torpedo is optional)
    * Not too fast but not too slow
* Start from a simplified model but make it extendable

## Functions implemented at tag `baselevel_20140302`

* Game field setup
* Impulse engine for Enterprise
* Klingon firing to Enterprise
* Enterprise firing phaser to Klingon

## How to run

    erl -pa ebin -run erltrek_game start_link -run erltrek_game start_game

## Another way to run

    erl -args_file ./initgame.txt
    % Discover src/e.erl for all the shortcuts
   
## Related YouTube Talk

(Courtesy Erlang Solutions)

* <http://youtu.be/1mJUhUDipuo>

## TODO

* Documentation in the source code (edoc or edown)
* Eunit test cases
* Porting to 17.0 (maps may replace most of dict functionality)
* Note: dialyzer remote type issues incompatibility between R16B03-1 and 17.0-rc2 reported as in
<http://erlang.org/pipermail/erlang-questions/2014-February/077945.html> and <http://erlang.org/pipermail/erlang-questions/2014-February/077955.html>; all -spec entries must be rewritten.

