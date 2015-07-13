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

-module(erltrek_terminal).

-export([init/1, handle_event/2, handle_info/2, terminate/2]).
-export([describe_object/1]).

-include("erltrek.hrl").

-spec init(term()) -> {'ok', []}.

init(_Args) ->
    {ok, []}.

-spec terminate(term(), term()) -> 'ok'.

terminate(_Arg, _State) -> ok.

-spec handle_event(term(), term()) -> {ok, term()}.

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
handle_event({enter_quadrant, QC, SC}, State) ->
    ok = io:format("impulse move cross-quadrant to ~b,~b/~b,~b~n",
                   [QC#quadxy.x, QC#quadxy.y,
                    SC#sectxy.x, SC#sectxy.y]),
    {ok, State};
handle_event({move_quad, failed}, State) ->
    ok = io:format("impulse move: cross-quadrant step move failed, stop~n"),
    {ok, State};
handle_event({enter_sector, QC, SC}, State) ->
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
handle_event({collision, Object, {_, QC, SC}}, State) ->
    ok = io:format("Collision with ~s at ~b,~b/~b,~b~n",
                   [describe_object(Object),
                    QC#quadxy.x, QC#quadxy.y,
                    SC#sectxy.x, SC#sectxy.y]),
    {ok, State};
handle_event({hit, SK, KHIT}, State) ->
    ok = io:format("Klingon hit from sector ~b,~b level ~b~n",
                   [SK#sectxy.x, SK#sectxy.y, KHIT]),
    {ok, State};
handle_event({hit, protected_by_starbase}, State) ->
    ok = io:format("Starbase shields protect the ship~n"),
    {ok, State};
handle_event({klingon_move, SK, SKM}, State) ->
    ok = io:format("Klingon moved from sector ~b,~b to ~b,~b~n",
                   [SK#sectxy.x, SK#sectxy.y,
                       SKM#sectxy.x, SKM#sectxy.y]),
    {ok, State};
handle_event({phaser_hit, Level, {Class, SC}}, State) ->
    ok = io:format("~s hit with phasers from sector ~b,~b level ~b~n",
                   [describe_object(Class),
                    SC#sectxy.x, SC#sectxy.y,
                    Level]),
    {ok, State};
handle_event({killed, s_klingon, QC, SC}, State) ->
    ok = io:format("Klingon at quadrant/sector ~b,~b/~b,~b killed~n",
                   [QC#quadxy.x, QC#quadxy.y, SC#sectxy.x, SC#sectxy.y]),
    {ok, State};
handle_event({killed, s_enterprise, _QC, _SC}, State) ->
    erltrek_game:lost("Enterprise was destroyed"),
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
handle_event(energy_refilled, State) ->
    ok = io:format("Ship energy and shield replenished~n"),
    {ok, State};
handle_event({unknown_command, Command}, State) ->
    ok = io:format("enterprise_command: unknown command: ~p~n", [Command]),
    ok = io:format("enterprise_command: status cleared~n", []),
    {ok, State};
handle_event(Event, State) ->
    ok = io:format("~p: unknown event: ~p~n", [?MODULE, Event]),
    {ok, State}.

-spec handle_info(term(), term()) -> {ok, term()}.

%% This happens when CTRL/D is received
handle_info({'EXIT', Pid, normal}, State) ->
    ok = io:format("~p: normally exited, Pid: ~p~n", [?MODULE, Pid]),
    {ok, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    ok = io:format("~p: 'EXIT' received, Pid: ~p, Reason:~p~n",
                   [?MODULE, Pid, Reason]),
    {ok, State}.

-spec describe_object(atom() | term()) -> string().

describe_object(s_klingon) -> "Klingon";
describe_object(s_base) -> "Starbase";
describe_object(s_star) -> "Star";
describe_object(s_inhabited) -> "Star";
describe_object(s_hole) -> "Black hole";
describe_object(s_empty) -> "Empty";
describe_object(Other) ->
    io_lib:format("<unknown object: ~p>", [Other]).
