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

-module(erltrek_event).

-export([
        clear_command_buffer/1,
        timer_tasks/1
        ]).

-include("erltrek.hrl").

%% Do timer tasks
%% Input and output:
%% {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState

-spec timer_tasks(game_state()) -> game_state().

timer_tasks(GameState) ->
    % Klingon actions
    GameState2 = klingon_actions(GameState),
    % Enterprise action
    GameState3 = enterprise_command(GameState2),
    % Monitoring status
    GameState4 = monitoring_game(GameState3),
    % Set new game state
    GameState4.

%% Monitoring current status

-spec monitoring_game(game_state()) -> game_state().

monitoring_game(GameState) ->
    {Tick, SHIP, _NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    % update number of Klingons in the galaxy
    NK2 = dict:fold(fun(_K, V, A) -> A + V end, 0, DKQ),
    % update ship condition
    OldCondition = SHIP#enterprise_status.condition,
    case {dict:size(DKS) > 0,
          SHIP#enterprise_status.energy < 1000,
          SHIP#enterprise_status.docked} of
        {_, _, true} ->
            Condition = cond_docked;
        {false, true, false} ->
            Condition = cond_yellow;
        {true, _, false} ->
            Condition = cond_red;
        {false, false, false} ->
            Condition = cond_green
    end,
    case OldCondition =/= Condition of
        true ->
            io:format("Condition changed to: ~s~n",
                [erltrek_scan:condition_string(Condition)]);
        false ->
            ok % do nothing
    end,
    SHIP2 = SHIP#enterprise_status{condition = Condition},
    {Tick, SHIP2, NK2, DS, DI, DB, DH, DKQ, SECT, DKS}.

%% Clear Enterprise command buffer

-spec clear_command_buffer(game_state()) -> game_state().

clear_command_buffer(GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    % clear command buffer
    SHIP2 = SHIP#enterprise_status{next_command = {}},
    {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS}.

%% Do commands for Enterprise

-spec enterprise_command(game_state()) -> game_state().

enterprise_command(GameState) ->
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, _DKS} = GameState,
    Command = SHIP#enterprise_status.next_command,
    case Command of
        {lrscan} -> % long range scanner
            ok = erltrek_scan:lrscan(GameState),
            clear_command_buffer(GameState);
        {srscan} -> % short range scanner
            ok = erltrek_scan:srscan(GameState),
            clear_command_buffer(GameState);
        {impulse, SX, SY} -> % impulse moving in the same quadrant
            % does NOT clear command buffer
            erltrek_move:impulse(SX, SY, GameState);
        {impulse, QX, QY, SX, SY} -> % impulse moving to the different quadrant
            % does NOT clear command buffer
            erltrek_move:impulse(QX, QY, SX, SY, GameState);
        {} -> % do nothing
            GameState;
        _ -> % do nothing if something strange comes
            io:format("~s:enterprise_command: unknown command: ~p~n",
                [?MODULE, Command]),
            io:format("~s:enterprise_command: status cleared~n", [?MODULE]),
            clear_command_buffer(GameState)
    end.

%% Do Klingon actions

-spec klingon_actions(game_state()) -> game_state().

klingon_actions(GameState) ->
    GameState2 = erltrek_klingon:move(GameState),
    GameState3 = erltrek_klingon:attack(GameState2),
    GameState3.
