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
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         lose/1,
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start_game() ->
    gen_server:call(?MODULE, start_game).

stop() ->
    gen_server:cast(?MODULE, stop).

lose(Message) ->
    gen_server:cast(?MODULE, {lose, Message}).

won(Message) ->
    gen_server:cast(?MODULE, {won, Message}).

%% Callbacks

init([]) ->
    {ok, []}.

handle_call(start_game, _From, _State) ->
    % initialize the stardate clock,
    Tick = ?INITTICK,
    % {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} 
    InitState = erltrek_setup:setup_state(),
    Timer = erlang:send_after(1, self(), tick_event),
    GameTimeState = {Tick, Timer, InitState},
    {reply, ok, GameTimeState}.

terminate(normal, State) ->
    ok.

handle_cast({lose, Message}, State) ->
    io:format("~s: Game lost: ~s~n", [?MODULE, Message]),
    {stop, normal, State};
handle_cast({won, Message}, State) ->
    io:format("~s: Game won: ~s~n", [?MODULE, Message]),
    {stop, normal, State};
handle_cast(stop, State) ->
    io:format("~s: handle_cast(stop, State) received~n", [?MODULE]),
    {stop, normal, State}.

handle_info(tick_event, GameTimeState) ->
    {Tick, OldTimer, GameState} = GameTimeState,
    erlang:cancel_timer(OldTimer),
    % do interval timer task here,
    {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} = GameState,
    % warping test
    QC = #quadxy{x = tinymt32:uniform(?NQUADS) - 1,
                 y = tinymt32:uniform(?NQUADS) - 1},
    {SECT2, DKS2} = erltrek_setup:setup_sector(QC, DS, DI, DB, DH, DKQ),
    % put enterprise in the current quadrant
    SC = erltrek_setup:rand_sect(SECT2),
    SECT3 = array:set(erltrek_setup:sectxy_index(SC), s_enterprise, SECT2), 
    SHIP2 = SHIP#enterprise_status{quadxy = QC, sectxy = SC},
    % displaying the status
    erltrek_scan:srscan(Tick, SHIP2, SECT3, DI, DKQ),
    % Set new game state
    % NOTE WELL ON THE VARIABLES!
    NewGameState = {SHIP2,NK,DS,DI,DB,DH,DKQ,SECT3,DKS},
    % increment tick counter and restart timer
    NewTick = Tick + 1,
    NewTimer = erlang:send_after(?TICK_INTERVAL, self(), tick_event),
    NewGameTimeState = {NewTick, NewTimer, NewGameState},
    {noreply, NewGameTimeState}.

