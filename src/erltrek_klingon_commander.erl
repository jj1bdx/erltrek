%%% -------------------------------------------------------------------
%%% Erltrek ("this software") is covered under the BSD 3-clause
%%% license.
%%%
%%% This product includes software developed by the University of
%%% California, Berkeley and its contributors.
%%%
%%% Copyright (c) 2014 Kenji Rikitake and Andreas Stenius.
%%% All rights reserved.
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
%%% * Neither the name of Kenji Rikitake, Andreas Stenius, k2r.org,
%%%   nor the names of its contributors may be used to endorse or
%%%   promote products derived from this software without specific
%%%   prior written permission.
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

-module(erltrek_klingon_commander).
-behaviour(gen_fsm).

%% API
-export([start/1]).

%% States
-export([idle/2, scout/2, aggressive/2, evasive/2]).

%% Callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-include("erltrek.hrl").

%% klingon will start getting away when energy drops below this level
-define(BADLY_HURT, 200).

-record(state, {
          ship :: pid(),
          skill=?TICK_INTERVAL
         }).


%%% --------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------

-spec start(pid()) -> {ok, pid()}.
start(Ship) ->
    gen_fsm:start(?MODULE, Ship, []).


%%% --------------------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------------------

init(Ship) ->
    monitor(process, Ship),
    State = #state{ ship=Ship },
    {ok, idle, State, State#state.skill}.

handle_event(_Event, StateName, StateData) ->
    next_state(StateName, StateData).

handle_sync_event(_Event, _From, StateName, StateData) ->
    next_state(StateName, StateData).

handle_info({enter_quadrant, _QC, _SC}, evasive, State) ->
    ok = erltrek_ship:command(ship(State), stop),
    %% TODO: is there any way for klingon ships to recharge their energy.. ?
    %% now would be the time for it
    next_state(idle, State);
handle_info({event, _}, StateName, StateData) ->
    next_state(StateName, StateData);
handle_info({sync_event, {Pid, Ref}, _}, StateName, StateData) ->
    Pid ! {Ref, ok},
    next_state(StateName, StateData);
handle_info({'DOWN', _Ref, process, _Ship, _Info}, _StateName, StateData) ->
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    next_state(StateName, StateData).

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%% --------------------------------------------------------------------
%%% States
%%% --------------------------------------------------------------------

idle(timeout, State) ->
    %% wait for enterprise to show up
    case erltrek_ship:count_nearby_enemies(ship(State)) of
        0 -> next_state(idle, State);
        _ -> next_state(scout, State)
    end.

scout(timeout, State) ->
    %% depending on our energy level, we'll be evasive or aggressive
    case status(State) of
        ok ->
            next_state(aggressive, State);
        badly_hurt ->
            next_state(evasive, State)
    end.

evasive(timeout, State) ->
    %% TODO: check where enterprise are, and go in other direction
    %% (also check for closest nearby quadrant)
    Course = tinymt32:uniform(360) - 1,
    ok = erltrek_ship:command(ship(State), {impulse, Course}),
    %% will stop when we reach new quadrant
    %% TODO: (not here) we may collide, and need to find another direction..
    next_state(evasive, State, infinity).

aggressive(timeout, State) ->
    case status(State) of
        ok ->
            %% TODO: attack!
            next_state(aggressive, State);
        badly_hurt ->
            %% go straight to evasive (fast reaction)
            evasive(timeout, State)
    end.


%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

next_state(StateName, #state{ skill=Timeout }=StateData) ->
    next_state(StateName, StateData, Timeout).

next_state(StateName, StateData, Timeout) ->
    {next_state, StateName, StateData, Timeout}.

ship(#state{ ship=Ship }) -> Ship.

-spec status(#state{}) -> ok | badly_hurt.
status(State) ->
    case erltrek_ship:status(ship(State)) of
        #ship_state{ energy=E } when E > ?BADLY_HURT ->
            ok;
        _ ->
            badly_hurt
    end.
