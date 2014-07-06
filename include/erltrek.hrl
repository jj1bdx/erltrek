%%% --------------------------------------------------------------------
%%% Erltrek ("this software") is covered under the BSD 3-clause
%%% license.
%%% 
%%% This product includes software developed by the University of
%%% California, Berkeley and its contributors.
%%% 
%%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%% 
%%% * Redistributions in binary form must reproduce the above
%%%   copyright notice, this list of conditions and the following
%%%   disclaimer in the documentation and/or other materials provided
%%%   with the distribution.
%%% 
%%% * Neither the name of Kenji Rikitake, k2r.org, nor the names of
%%%   its contributors may be used to endorse or promote products
%%%   derived from this software without specific prior written
%%%   permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
%%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
%%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%% 
%%% This software incorporates portions of the BSD Star Trek source
%%% code, distributed under the following license:
%%% 
%%% Copyright (c) 1980, 1993
%%%      The Regents of the University of California.
%%%      All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above
%%%    copyright notice, this list of conditions and the following
%%%    disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%% 3. All advertising materials mentioning features or use of this
%%%    software must display the following acknowledgement:
%%%      This product includes software developed by the University of
%%%      California, Berkeley and its contributors.
%%% 4. Neither the name of the University nor the names of its
%%%    contributors may be used to endorse or promote products derived
%%%    from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS
%%% IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
%%% REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% 
%%% [End of LICENSE]
%%% --------------------------------------------------------------------

%% dimensions of quadrant in sectors
-define(NSECTS, 10).
%% dimension of galaxy in quadrants
-define(NQUADS, 8).
%% maximum stars per quadrant
-define(MAXSTARQUAD, 9).
%% maximum klingons per quadrant
-define(MAXKLQUAD, 9).
%% maximum number of starbases in galaxy
-define(MAXBASES, 9).
%% max number of concurrently pending events
-define(MAXEVENTS, 25).
%% maximum concurrent distress calls
-define(MAXDISTR, 5).
%% Version number string
-define(ERLTREK_VERSION, "0.1").
%% Debug trace flag
-define(DEBUG_TRACE, true).
%% Initial Enterprise energy
-define(SHIPENERGY, 5000).
%% Initial Enterprise shield level
-define(SHIPSHIELD, 1000).
%% Initial klingon energy
-define(KLINGONENERGY, 400).
%% Interval in milliseconds per tick
-define(TICK_INTERVAL, 1000).
%% Initial Stardate * 100
-define(INITTICK, 200000).

%%% types and records

%% see OTP lib/stdlib/src/orddict.erl

-type orddict() :: [{Key :: term(), Value :: term()}].

%% see OTP lib/stdlib/src/supervisor.erl

-type startlink_err() :: {'already_started', pid()} | {'shutdown', term()} | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

%% quadrant and sector coordinates

-type quadcoord() :: 0..(?NQUADS - 1).
-type sectcoord() :: 0..(?NSECTS - 1).
-type galacoord() :: float(). % 0..(?NQUADS * ?NSECTS - 1)

-record(quadxy, { x :: quadcoord(), y :: quadcoord()}).
-record(sectxy, { x :: sectcoord(), y :: sectcoord()}).
-record(galaxy, { x :: galacoord(), y :: galacoord()}).

%% entities in the sector array

-type sector_atoms() ::
    's_empty' | 's_star' | 's_enterprise' | 's_base' | 's_inhabited' |
    's_klingon' | 's_hole'.
-type sector_entity() :: sector_atoms() | {ship_class(), undefined | pid()}.
-type sector_array() :: array:array(sector_entity()).

%% record for entities

-record(base_info, { xy :: #sectxy{} }).
-record(inhabited_info, { xy :: #sectxy{}, systemname :: string()}).

%% Enterprise status
-type ship_condition() :: 'cond_green' | 'cond_yellow' | 'cond_red' | 'cond_docked'.

%% Class of ships
-type ship_class() :: s_enterprise | s_klingon.

%% Ship data used by erltrek_ship
-record(ship_def, {
          class :: ship_class(),
          commander :: atom(), %% commander module
          max_energy=1 :: pos_integer(),
          max_shield=0 :: non_neg_integer(),
          max_speed=1.5 :: float(), %% for impulse engines
          initial_speed=0.8 :: float(), %% initial speed for impulse engines
          %% TODO: cost based on actual speed.. faster costs more
          engine_cost=10 :: pos_integer(), %% consumed energy / sector travel
          durability :: fun((body | shield, integer()) -> integer())
         }).

-define(enterprise_ship,
        #ship_def{
           class = s_enterprise,
           commander = erltrek_enterprise_commander,
           max_energy = ?SHIPENERGY,
           max_shield = ?SHIPSHIELD,
           initial_speed = 0.9,
           durability = fun (body, D) when D > 0 -> trunc(D * 1.3) + 10;
                            (_, D) -> D
                        end
          }).

-define(klingon_ship,
        #ship_def{
           class = s_klingon,
           commander = erltrek_klingon_commander,
           max_energy = ?KLINGONENERGY,
           max_shield = 0,
           initial_speed = 0.3,
           durability = fun (_, D) -> D end
          }).

%% Ship state used by erltrek_ship (also used in some event notifications)
-record(ship_state, {
          ship=#ship_def{} :: #ship_def{},
          commander :: pid(), %% commander process
          energy=1 :: non_neg_integer(),
          shield=0 :: non_neg_integer(),
          condition=cond_green :: ship_condition(),
          docked=false :: boolean(),

          %% speed setting to use when moving (not current speed, that
          %% is in #ship_data{})
          speed=0.3 :: number(),

          %% traveling target coordinates
          tquad :: #quadxy{},
          tsect :: #sectxy{}
         }).


%% galaxy data about ship used by erltrek_galaxy
-record(ship_data, {
          class :: ship_class(),
          pos :: #galaxy{},
          quad :: #quadxy{},
          sect :: #sectxy{},
          course=0.0 :: number(), % 0..360
          speed=0.0 :: number()
         }).

%% shell command record
-record(command, {
          name :: atom() | list(atom()),
          expand = true :: boolean(), %% tab complete command? (default: yes)
          desc = "(no arguments)" :: string(), %% printed on expand command
          help = "No help available for this command." :: string(),
          dispatch :: fun((list()) -> ok),
          result = ok :: fun ((term()) -> ok) | term()
         }).


%% vim: set ts=4 sw=4 sts=4 et :
%% emacs: -*- mode:erlang; tab-width:4; indent-tabs-mode:nil;  -*-
