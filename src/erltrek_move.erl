%%% -------------------------------------------------------------------
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

-module(erltrek_move).

-export([
        impulse/3,
        impulse/5
        ]).

-include("erltrek.hrl").

%% impulse move

-spec impulse(non_neg_integer(), non_neg_integer(), game_state()) -> game_state().

impulse(SX, SY, GameState) ->
    {_, SHIP, _, _, _, _, _, _, _, _} = GameState,
    impulse(SHIP#enterprise_status.quadxy#quadxy.x,
            SHIP#enterprise_status.quadxy#quadxy.y,
            SX, SY, GameState).

-spec impulse(non_neg_integer(), non_neg_integer(),
    non_neg_integer(), non_neg_integer(), game_state()) -> game_state().

impulse(QX, QY, SX, SY, GameState) ->
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, _DKS} = GameState,
    case SHIP#enterprise_status.impulse_move of
        false -> % course initialization needed
            GameState2 = course_init(QX, QY, SX, SY, GameState),
            % Do the first move
            course_onmove(GameState2);
        true -> % it's already moving
            course_onmove(GameState)
    end.

%% initialize impulse drive course information

-spec course_init(non_neg_integer(), non_neg_integer(),
    non_neg_integer(), non_neg_integer(), game_state()) -> game_state().

course_init(QX, QY, SX, SY, GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    case erltrek_calc:track_course(SHIP#enterprise_status.quadxy,
            SHIP#enterprise_status.sectxy,
            #quadxy{x = QX, y = QY},
            #sectxy{x = SX, y = SY}) of
        out_of_bound ->
            erltrek_event:notify({move, out_of_bound}),
            % clear command buffer
            erltrek_event:clear_command_buffer(GameState);
        {ok, _DIFFX, _DIFFY, CDEG, DISTSD, LQC} ->
            erltrek_event:notify({move, CDEG, DISTSD}),
            % decrease energy at this point
            E = SHIP#enterprise_status.energy -
                trunc(DISTSD * 10 + 0.5),
            % set course and moving flag
            SHIP2 = SHIP#enterprise_status{
                impulse_course = LQC, impulse_move = true,
                energy = E},
            {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS}
    end.

%% clear moving flag, course info, and next command buffer

-spec clear_status(game_state()) -> game_state().

clear_status(GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    SHIP2 = SHIP#enterprise_status{impulse_move = false, impulse_course = []},
    erltrek_event:clear_command_buffer(
        {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS}).

%% display current position (return unaltered state)

-spec display_position(game_state()) -> game_state().

display_position(GameState) ->
    erltrek_event:notify({display_position, GameState}),
    GameState.

%% check whether moving path remains

-spec course_checkend(game_state()) -> {boolean(), game_state()}.

course_checkend(GameState) ->
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, _DKS} = GameState,
    LQC = SHIP#enterprise_status.impulse_course,
    case LQC of
        [] -> % no more moving needed
            erltrek_event:notify(move_done),
            display_position(GameState),
            GameState2 = clear_status(GameState),
            {true, GameState2};
        [_H|_T] -> % do nothing if
            {false, GameState}
    end.

%% check whether moving path remains
%% if ends, terminate
%% if remains, do nothing

-spec course_checkend_noaction(game_state()) -> game_state().

course_checkend_noaction(GameState) ->
    {_, GameState2} = course_checkend(GameState),
    GameState2.

%% on move: check whether moving path remains
%% if ends, terminate
%% if remains, move to next sector

-spec course_onmove(game_state()) -> game_state().

course_onmove(GameState) ->
    case course_checkend(GameState) of
        {true, GameState2} -> % no more moving needed
            GameState2;
        {false, GameState3} -> % move to next sector
            course_onmove_next(GameState3)
    end.

%% Force halt

-spec force_halt(game_state()) -> game_state().

force_halt(GameState) ->
    display_position(GameState),
    clear_status(GameState).

%% pick up next move

-spec course_onmove_next(game_state()) -> game_state().

course_onmove_next(GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    LQC = SHIP#enterprise_status.impulse_course,
    [LQCH | LQCT] = LQC,
    {QC, SC} = LQCH,
    % Update ship status for moved state
    SHIP2 = SHIP#enterprise_status{
        quadxy = QC, sectxy = SC, impulse_course = LQCT},
    case QC =/= SHIP#enterprise_status.quadxy of
        true -> % change current quadrant
            {SECT2, DKS2} = erltrek_setup:setup_sector(QC, DS, DI, DB, DH, DKQ),
            ENT = array:get(erltrek_setup:sectxy_index(SC), SECT2),
            case ENT =:= s_empty of
                true -> % sector empty, move in
                    erltrek_event:notify({move_quad, QC, SC}),
                    % fill Enterprise in the new sector array
                    SECT3 = array:set(erltrek_setup:sectxy_index(SC), s_enterprise, SECT2),
                    GameState2 = {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT3, DKS2},
                    course_checkend_noaction(GameState2);
                false -> % sector already filled, fail to move
                    erltrek_event:notify({move_quad, failed}),
                    force_halt(GameState)
            end;
        false -> % in the same quadrant
            ENT2 = array:get(erltrek_setup:sectxy_index(SC), SECT),
            case ENT2 =:= s_empty of
                true -> % sector empty, move in
                    erltrek_event:notify({move_sect, QC, SC}),
                    % clear Enterprise in the current sector array
                    SECT4 = array:set(erltrek_setup:sectxy_index(
                                        SHIP#enterprise_status.sectxy),
                                        s_empty, SECT),
                    % fill Enterprise in the current sector array
                    SECT5 = array:set(erltrek_setup:sectxy_index(SC),
                                        s_enterprise, SECT4),
                    GameState3 = {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT5, DKS},
                    course_checkend_noaction(GameState3);
                false -> % sector already filled, fail to move
                    erltrek_event:notify({move_sect, failed}),
                    force_halt(GameState)
            end
    end.
