# Erltrek: Star Trek Game in Erlang

* Requirement: R16B03-1
* License: BSD 3-clause (Note: tinymt-erlang has its own license)
* *Note well: no workable code yet*

## Goals

* Incorporating true concurrency (= game proceeds in real-time)
* Not altering the traditional Star Trek model
    * Coordinate system: 2D and dual layer (Quadrants + Sectors)
    * Weapons: phaser (torpedo is optional)
    * Not too fast but not too slow
* Start from a simplified model but make it extendable

## TODO

* Dialyzer remote type issues incompatibility between R16B03-1 and 17.0-rc2 reported as in
<http://erlang.org/pipermail/erlang-questions/2014-February/077945.html> and <http://erlang.org/pipermail/erlang-questions/2014-February/077955.html>.
