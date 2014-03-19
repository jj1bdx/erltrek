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

-module(erltrek_calc).

-export([
         course_distance/4,
         destination/4,
         in_quadrant/1,
         in_quadrant/2,
         in_quadxy/1,
         in_sector/1,
         in_sector/2,
         in_sectxy/1,
         sector_course/2,
         sector_course_distance/2,
         sector_distance/2,
         track_course/4,
         quadxy_index/1,
         sectxy_index/1,
         index_quadxy/1,
         index_sectxy/1
        ]).

-include("erltrek.hrl").

%% check inside the quadrant

-spec in_quadrant(quadcoord()) -> boolean().

in_quadrant(X) -> (X >= 0) andalso (X < ?NQUADS).

-spec in_quadrant(quadcoord(), quadcoord()) -> boolean().

in_quadrant(X, Y) -> in_quadrant(X) andalso in_quadrant(Y).

-spec in_quadxy(#quadxy{}) -> boolean().

in_quadxy(#quadxy{ x=X, y=Y }) -> in_quadrant(X, Y).

%% check inside the sector

-spec in_sector(sectcoord()) -> boolean().

in_sector(X) -> (X >= 0) andalso (X < ?NSECTS).

-spec in_sector(sectcoord(), sectcoord()) -> boolean().

in_sector(X, Y) -> in_sector(X) andalso in_sector(Y).

-spec in_sectxy(#sectxy{}) -> boolean().

in_sectxy(#sectxy{ x=X, y=Y }) -> in_sector(X, Y).

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
    case in_quadxy(SQC) andalso in_quadxy(DQC) andalso
        in_sectxy(SSC) andalso in_sectxy(DSC) of
        true ->
            DIFFX = ((DQC#quadxy.x * ?NSECTS) + DSC#sectxy.x) -
                    ((SQC#quadxy.x * ?NSECTS) + SSC#sectxy.x),
            DIFFY = ((DQC#quadxy.y * ?NSECTS) + DSC#sectxy.y) -
                    ((SQC#quadxy.y * ?NSECTS) + SSC#sectxy.y),
            CRAD = math:atan2(DIFFY, -DIFFX),
            CRAD2 = case CRAD < 0 of
                true -> (CRAD + (2 * math:pi()));
                false -> CRAD
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
            LQC = case DIFFX == 0 andalso DIFFY == 0 of
                true -> [];
                false -> list_track(SQC, SSC, DIFFX, DIFFY)
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
    case in_quadxy(DESTQC) andalso in_sectxy(DESTSC) of
        true ->
            {ok, DESTQC, DESTSC};
        false ->
            out_of_bound
    end.

%% Calculate course and distance between two sectors
%% course (0-360 degrees, 0: -X direction, clockwise (e.g., 90: +Y direction)),

-spec sector_course_distance(#sectxy{}, #sectxy{}) -> {float(), float()}.

sector_course_distance(SC, DC) ->
    DX = DC#sectxy.x - SC#sectxy.x,
    DY = DC#sectxy.y - SC#sectxy.y,
    CRAD = math:atan2(DY, -DX),
    CRAD2 = case CRAD < 0 of
        true -> (CRAD + (2 * math:pi()));
        false -> CRAD
    end,
    {CRAD2 * 180 / math:pi(), math:sqrt((DX*DX) + (DY*DY))}.

%% Calculate course between two sectors

-spec sector_course(#sectxy{}, #sectxy{}) -> float().

sector_course(SC, DC) ->
    {COURSE, _DISTANCE} = sector_course_distance(SC, DC),
    COURSE.

%% Calculate distance between two sectors

-spec sector_distance(#sectxy{}, #sectxy{}) -> float().

sector_distance(SC, DC) ->
    {_COURSE, DISTANCE} = sector_course_distance(SC, DC),
    DISTANCE.

%% convert quadrant coordinate record to Quad array position

-spec quadxy_index(#quadxy{}) -> non_neg_integer().

quadxy_index(QC) ->
    (QC#quadxy.x * ?NQUADS) + QC#quadxy.y.

%% convert sector coordinate record to Sect array position

-spec sectxy_index(#sectxy{}) -> non_neg_integer().

sectxy_index(QC) ->
    (QC#sectxy.x * ?NSECTS) + QC#sectxy.y.

%% convert quadrant array index to coordinate

-spec index_quadxy(non_neg_integer()) -> #quadxy{}.

index_quadxy(QI) when is_integer(QI), QI >= 0 ->
    %% make the index wrap in case it goes out of bounds
    B = QI rem (?NQUADS * ?NQUADS),
    #quadxy{ x = B div ?NQUADS, y = B rem ?NQUADS }.

%% convert sector array index to coordinate

-spec index_sectxy(non_neg_integer()) -> #sectxy{}.

index_sectxy(SI) when is_integer(SI), SI >= 0 ->
    %% make the index wrap in case it goes out of bounds
    B = SI rem (?NSECTS * ?NSECTS),
    #sectxy{ x = B div ?NSECTS, y = B rem ?NSECTS }.
