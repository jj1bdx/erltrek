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

-module(erltrek_game).
-behaviour(gen_server).

-export([
         code_change/3,
         enterprise_command/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         lost/1,
         start_game/0,
         start_link/0,
         start_link/1,
         stop/0,
         terminate/2,
         won/1
     ]).

-include("erltrek.hrl").

%% public APIs

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start_game() ->
    gen_server:call(?MODULE, start_game).

stop() ->
    gen_server:cast(?MODULE, {stop, stop}).

lost(Message) ->
    gen_server:cast(?MODULE, {stop, {lost, Message}}).

won(Message) ->
    gen_server:cast(?MODULE, {stop, {won, Message}}).

enterprise_command(Command) ->
    gen_server:call(?MODULE, {enterprise_command, Command}).

%% Callbacks

init([]) ->
    {ok, []}.

handle_call(start_game, _From, _State) ->
    % Initialize {Tick,SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} 
    InitState = erltrek_setup:setup_state(),
    % wait one second to start the game
    Timer = erlang:send_after(1000, self(), tick_event),
    % build gen_server State
    GameStateAndTimer = {Timer, InitState},
    {reply, ok, GameStateAndTimer};
handle_call({enterprise_command, Command}, _From, GameStateAndTimer) ->
    {Timer, GameState} = GameStateAndTimer,
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    case SHIP#enterprise_status.next_command =:= {} of
        true ->
            SHIP2 = SHIP#enterprise_status{next_command = Command},
            NewGameState = {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS},
            NewGameStateAndTimer = {Timer, NewGameState},
            {reply, ok, NewGameStateAndTimer};
        false ->
            {reply, command_refused, GameStateAndTimer}
    end;
handle_call(get_state, _From, State) ->
    {reply, State, State}.

terminate(normal, {Timer, _GameState}) ->
    erlang:cancel_timer(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

handle_cast({stop, Event}, State) ->
    ok = erltrek_event:sync_notify(Event),
    {stop, normal, State}.

handle_info(tick_event, GameStateAndTimer) ->
    {OldTimer, GameState} = GameStateAndTimer,
    erlang:cancel_timer(OldTimer),
    % do interval timer task here
    % CAUTION: DO NOT change Tick value inside!
    NewGameState = erltrek_event:timer_tasks(GameState),
    % increment tick counter and restart timer
    NewTimer = erlang:send_after(?TICK_INTERVAL, self(), tick_event),
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = NewGameState,
    NewGameState2 = {Tick + 1, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS},
    NewGameStateAndTimer = {NewTimer, NewGameState2},
    {noreply, NewGameStateAndTimer}.

