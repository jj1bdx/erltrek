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
        course_distance/4,
        destination/4,
        impulse/3,
        impulse/5,
        track_course/4
     ]).

-include("erltrek.hrl").

%% Calculate course and distance between two quad/sect coordinates
%% Input: source #quadxy, #sectxy, dest #quadxy, #sectxy
%% Output:
%%   difference of X,
%%   difference of Y,
%%   course (0-360 degrees, 0: -X direction, clockwise (e.g., 90: +Y direction)),
%%   distance (unit: sector, number of sectors for a quadrant = ?NSECTS )

-spec course_distance(#quadxy{}, #sectxy{}, #quadxy{}, #sectxy{}) ->
    {ok, integer(), integer(), float(), float()} | out_of_bound.

course_distance(SQC, SSC, DQC, DSC) ->
    case ?INQUADQC(SQC) andalso ?INQUADQC(DQC) andalso
        ?INSECTSC(SSC) andalso ?INSECTSC(DSC) of
        true ->
            DIFFX = ((DQC#quadxy.x * ?NSECTS) + DSC#sectxy.x) -
                    ((SQC#quadxy.x * ?NSECTS) + SSC#sectxy.x),
            DIFFY = ((DQC#quadxy.y * ?NSECTS) + DSC#sectxy.y) -
                    ((SQC#quadxy.y * ?NSECTS) + SSC#sectxy.y),
            CRAD = math:atan2(DIFFY, -DIFFX),
            case CRAD < 0 of
                true ->
                    CRAD2 = (CRAD + (2 * math:pi()));
                false ->
                    CRAD2 = CRAD
            end,
            CDEG = CRAD2 * 180 / math:pi(),
            DISTSD = math:sqrt((DIFFX * DIFFX) + (DIFFY * DIFFY)),
            {ok, DIFFX, DIFFY, CDEG, DISTSD};
        false ->
            out_of_bound
    end.

%% Calculate course and distance between two quad/sect coordinates
%% and output the per-sector coordinate pair list
%% Input: source #quadxy, #sectxy, dest #quadxy, #sectxy
%% Output:
%%   difference of X,
%%   difference of Y,
%%   course (0-360 degrees)
%%   distance (unit: sector),
%%   list of {#quadxy, #sectxy} in the path

-spec track_course(#quadxy{}, #sectxy{}, #quadxy{}, #sectxy{}) ->
    {ok, integer(), integer(), float(), float(), [{#quadxy{}, #sectxy{}}]} | out_of_bound.

track_course(SQC, SSC, DQC, DSC) ->
    case course_distance(SQC, SSC, DQC, DSC) of
        out_of_bound ->
            out_of_bound;
        {ok, DIFFX, DIFFY, CDEG, DISTSD} ->
            case DIFFX == 0 andalso DIFFY == 0 of
                true ->
                    LQC = [];
                false ->
                    LQC = list_track(SQC, SSC, DIFFX, DIFFY)
            end,
            {ok, DIFFX, DIFFY, CDEG, DISTSD, LQC}
    end.

-spec list_track(#quadxy{}, #sectxy{}, integer(), integer()) -> [{#quadxy{}, #sectxy{}}].

list_track(SQC, SSC, DIFFX, DIFFY) ->
    case abs(DIFFX) > abs(DIFFY) of
        true ->
            list_track_x(SQC, SSC, DIFFX, DIFFY);
        false ->
            list_track_y(SQC, SSC, DIFFX, DIFFY)
    end.

-spec list_track_x(#quadxy{}, #sectxy{}, integer(), integer()) -> [{#quadxy{}, #sectxy{}}].

list_track_x(QC, SC, DIFFX, DIFFY) ->
    IX = ((QC#quadxy.x * ?NSECTS) + SC#sectxy.x),
    Y = float((QC#quadxy.y * ?NSECTS) + SC#sectxy.y),
    N = abs(DIFFX),
    IDX = DIFFX div N,
    DY = DIFFY / N,
    list_track_x_elem(N, IX, IDX, Y, DY, []).

-spec list_track_x_elem(integer(), integer(), integer(), 
    float(), float(), [{#quadxy{}, #sectxy{}}]) -> [{#quadxy{}, #sectxy{}}].

list_track_x_elem(0, _IX, _IDX, _Y, _DY, PATH) ->
    lists:reverse(PATH);
list_track_x_elem(N, IX, IDX, Y, DY, PATH) ->
    IX2 = IX + IDX,
    Y2 = Y + DY,
    IY = trunc(Y2 + 0.5),
    NQC = #quadxy{x = IX2 div ?NSECTS, y = IY div ?NSECTS},
    NSC = #sectxy{x = IX2 rem ?NSECTS, y = IY rem ?NSECTS},
    list_track_x_elem(N - 1, IX2, IDX, Y2, DY, [{NQC, NSC}|PATH]).

-spec list_track_y(#quadxy{}, #sectxy{}, integer(), integer()) -> [{#quadxy{}, #sectxy{}}].

list_track_y(QC, SC, DIFFX, DIFFY) ->
    X = float((QC#quadxy.x * ?NSECTS) + SC#sectxy.x),
    IY = ((QC#quadxy.y * ?NSECTS) + SC#sectxy.y),
    N = abs(DIFFY),
    IDY = DIFFY div N,
    DX = DIFFX / N,
    list_track_y_elem(N, X, DX, IY, IDY, []).

-spec list_track_y_elem(integer(), float(), float(), 
    integer(), integer(), [{#quadxy{}, #sectxy{}}]) -> [{#quadxy{}, #sectxy{}}].

list_track_y_elem(0, _X, _DX, _IY, _IDY, PATH) ->
    lists:reverse(PATH);
list_track_y_elem(N, X, DX, IY, IDY, PATH) ->
    IY2 = IY + IDY,
    X2 = X + DX,
    IX = trunc(X2 + 0.5),
    NQC = #quadxy{x = IX div ?NSECTS, y = IY2 div ?NSECTS},
    NSC = #sectxy{x = IX rem ?NSECTS, y = IY2 rem ?NSECTS},
    list_track_y_elem(N - 1, X2, DX, IY2, IDY, [{NQC, NSC}|PATH]).

%% Calculate the destination coordinate from given coordinate
%% and course (0-360 degrees)
%% and distance (unit: sector)
%% Input: source #quadxy, #sectxy, course, distance
%% Output:
%% destination #quadxy, #sectxy

-spec destination(#quadxy{}, #sectxy{}, float(), float()) ->
    {ok, #quadxy{}, #sectxy{}} | out_of_bound.

destination(SQC, SSC, COURSE, DIST) ->
    SX = (SQC#quadxy.x * ?NSECTS) + SSC#sectxy.x,
    SY = (SQC#quadxy.y * ?NSECTS) + SSC#sectxy.y,
    ANGLE = COURSE / 180 * math:pi(),
    DIFFX = DIST * -math:cos(ANGLE),
    DIFFY = DIST * math:sin(ANGLE),
    DESTX = trunc(SX + DIFFX + 0.5),
    DESTY = trunc(SY + DIFFY + 0.5),
    DESTQC = #quadxy{x = DESTX div ?NSECTS, y = DESTY div ?NSECTS},
    DESTSC = #sectxy{x = DESTX rem ?NSECTS, y = DESTY rem ?NSECTS},
    case ?INQUADQC(DESTQC) andalso ?INSECTSC(DESTSC) of
        true ->
            {ok, DESTQC, DESTSC};
        false ->
            out_of_bound
    end.

%% impulse move (skeleton yet)

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
            course_init(QX, QY, SX, SY, GameState);
        true -> % it's already moving
            course_onmove(GameState)
    end.

%% initialize impulse drive course information

-spec course_init(non_neg_integer(), non_neg_integer(),
    non_neg_integer(), non_neg_integer(), game_state()) -> game_state().

course_init(QX, QY, SX, SY, GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    case track_course(SHIP#enterprise_status.quadxy,
            SHIP#enterprise_status.sectxy,
            #quadxy{x = QX, y = QY},
            #sectxy{x = SX, y = SY}) of
        out_of_bound ->
            io:format("impulse move: course out of bound~n"),
            % clear command buffer
            erltrek_event:clear_command_buffer(GameState);
        {ok, _DIFFX, _DIFFY, CDEG, DISTSD, LQC} ->
            io:format("impulse move: course = ~.1f, distance = ~.1f~n",
                [CDEG, DISTSD]),
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
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, _DKS} = GameState,
    io:format("Current position: ~b.~b/~b.~b~n",
        [SHIP#enterprise_status.quadxy#quadxy.x,
         SHIP#enterprise_status.quadxy#quadxy.y,
         SHIP#enterprise_status.sectxy#sectxy.x,
         SHIP#enterprise_status.sectxy#sectxy.y]),
    GameState.

%% on move: check whether moving path remains

-spec course_onmove(game_state()) -> game_state().

course_onmove(GameState) ->
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, _DKS} = GameState,
    LQC = SHIP#enterprise_status.impulse_course,
    case length(LQC) > 0 of
        true -> % moving to next sector
            course_onmove_next(GameState);
        false -> % no more moving needed
            io:format("impulse move done~n"),
            clear_status(GameState)
    end.

%% pick up next move

-spec course_onmove_next(game_state()) -> game_state().

course_onmove_next(GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    LQC = SHIP#enterprise_status.impulse_course,
    [LQCH | LQCT] = LQC,
    {QC, SC} = LQCH,
    % Update ship status for moved state
    SHIP2 = SHIP#enterprise_status{
        quadxy = QC, sectxy = SC, impulse_course=LQCT},
    case QC =/= SHIP#enterprise_status.quadxy of
        true -> % change current quadrant
            {SECT2, DKS2} = erltrek_setup:setup_sector(QC, DS, DI, DB, DH, DKQ),
            ENT = array:get(erltrek_setup:sectxy_index(SC), SECT2),
            case ENT =:= s_empty of
                true -> % sector empty, move in
                    io:format("impulse move cross-quadrant to ~b.~b/~b.~b~n",
                                [QC#quadxy.x, QC#quadxy.y,
                                 SC#sectxy.x, SC#sectxy.y]),
                    % fill Enterprise in the new sector array 
                    SECT3 = array:set(erltrek_setup:sectxy_index(SC), s_enterprise, SECT2),
                    {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT3, DKS2};
                false -> % sector already filled, fail to move
                    io:format("impulse move: cross-quadrant step move failed, stop~n"),
                    display_position(GameState),
                    clear_status(GameState)
            end;
        false -> % in the same quadrant
            ENT2 = array:get(erltrek_setup:sectxy_index(SC), SECT),
            case ENT2 =:= s_empty of
                true -> % sector empty, move in
                    io:format("impulse move to ~b.~b/~b.~b~n",
                                [QC#quadxy.x, QC#quadxy.y,
                                 SC#sectxy.x, SC#sectxy.y]),
                    % clear Enterprise in the current sector array 
                    SECT4 = array:set(erltrek_setup:sectxy_index(
                                        SHIP#enterprise_status.sectxy),
                                        s_empty, SECT),
                    % fill Enterprise in the current sector array 
                    SECT5 = array:set(erltrek_setup:sectxy_index(SC),
                                        s_enterprise, SECT4),
                    {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT5, DKS};
                false -> % sector already filled, fail to move
                    io:format("impulse move: step move failed, stop~n"),
                    display_position(GameState),
                    clear_status(GameState)
            end
    end.
