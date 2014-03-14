%%% -------------------------------------------------------------------
%%% Erltrek ("this software") is covered under the BSD 3-clause
%%% license.
%%%
%%% This product includes software developed by the University of
%%% California, Berkeley and its contributors.
%%%
%%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
%%% Copyright (c) 2014 Andreas Stenius. All rights reserved.
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

-module(erltrek_terminal).

-export([init/1, handle_event/2, terminate/2]).

-include("erltrek.hrl").

init(_Args) -> {ok, []}.

terminate(_Arg, _State) -> ok.

handle_event({lost, Message}, State) ->
    ok = io:format("Game lost: ~s~n", [Message]),
    {ok, State};
handle_event({won, Message}, State) ->
    ok = io:format("Game won: ~s~n", [Message]),
    {ok, State};
handle_event(stop, State) ->
    ok = io:format("Game stopped~n"),
    {ok, State};
handle_event({move, out_of_bound}, State) ->
    ok = io:format("impulse move: course out of bound~n"),
    {ok, State};
handle_event({move, CDEG, DISTSD}, State) ->
    ok = io:format("impulse move: course = ~.1f, distance = ~.1f~n",
                   [CDEG, DISTSD]),
    {ok, State};
handle_event({move_quad, QC, SC}, State) ->
    ok = io:format("impulse move cross-quadrant to ~b,~b/~b,~b~n",
                   [QC#quadxy.x, QC#quadxy.y,
                    SC#sectxy.x, SC#sectxy.y]),
    {ok, State};
handle_event({move_quad, failed}, State) ->
    ok = io:format("impulse move: cross-quadrant step move failed, stop~n"),
    {ok, State};
handle_event({move_sect, QC, SC}, State) ->
    ok = io:format("impulse move to ~b,~b/~b,~b~n",
                   [QC#quadxy.x, QC#quadxy.y,
                    SC#sectxy.x, SC#sectxy.y]),
    {ok, State};
handle_event({move_sect, failed}, State) ->
    ok = io:format("impulse move: step move failed, stop~n"),
    {ok, State};
handle_event(move_done, State) ->
    ok = io:format("impulse move done~n"),
    {ok, State};
handle_event({display_position, GameState}, State) ->
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, _DKS} = GameState,
    ok = io:format("Current position: ~b,~b/~b,~b~n",
                   [SHIP#enterprise_status.quadxy#quadxy.x,
                    SHIP#enterprise_status.quadxy#quadxy.y,
                    SHIP#enterprise_status.sectxy#sectxy.x,
                    SHIP#enterprise_status.sectxy#sectxy.y]),
    {ok, State};
handle_event({lrscan, GameState}, State) ->
    ok = io:format("~s", [erltrek_scan:lrscan_string(GameState)]),
    {ok, State};
handle_event({srscan, GameState}, State) ->
    ok = io:format("~s", [erltrek_scan:srscan_string(GameState)]),
    {ok, State};
handle_event({hit, SK, KHIT}, State) ->
    ok = io:format("Klingon hit from sector ~b,~b level ~b~n",
                   [SK#sectxy.x, SK#sectxy.y, KHIT]),
    {ok, State};
handle_event({phaser_hit, SK, HIT}, State) ->
    ok = io:format("Phaser hit to Klingon at sector ~b,~b level ~b~n",
                   [SK#sectxy.x, SK#sectxy.y, HIT]),
    {ok, State};
handle_event({killed, SK}, State) ->
    ok = io:format("Klingon at sector ~b,~b killed~n",
                   [SK#sectxy.x, SK#sectxy.y]),
    {ok, State};
handle_event(shields_gone, State) ->
    ok = io:format("Shield gone~n"),
    {ok, State};
handle_event({damage_level, DAMAGELEVEL}, State) ->
    ok = io:format("Damage level up to ~b~n", [DAMAGELEVEL]),
    {ok, State};
handle_event({shield_level, NSHIELD}, State) ->
    ok = io:format("Shield level down to ~b~n", [NSHIELD]),
    {ok, State};
handle_event({condition, Condition}, State) ->
    ok = io:format("Condition changed to: ~s~n",
                   [erltrek_scan:condition_string(Condition)]),
    {ok, State};
handle_event({unknown_command, Command}, State) ->
    ok = io:format("enterprise_command: unknown command: ~p~n", [Command]),
    ok = io:format("enterprise_command: status cleared~n", []),
    {ok, State};
handle_event(no_target, State) ->
    ok = io:format("No Klingon in sector, phaser not fired~n"),
    {ok, State};
handle_event(fire_level, State) ->
    ok = io:format("Firing level exceeds availably energy, phaser not fired~n"),
    {ok, State};
handle_event(Event, State) ->
    ok = io:format("~p: unknown event: ~p~n", [?MODULE, Event]),
    {ok, State}.
