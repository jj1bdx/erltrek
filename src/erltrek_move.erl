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
        track_course/4
     ]).

-include("erltrek.hrl").

%% Calculate course and distance between two quad/sect coordinates
%% Input: source #quadxy, #sectxy, dest #quadxy, #sectxy
%% Output:
%%   difference of X,
%%   difference of Y,
%%   course (0-360 degrees, 0: -X direction, counter clockwise),
%%   distance (unit: sector)

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
%%   course (0-360 degrees, 0: -X direction, counter clockwise),
%%   distance (unit: sector),
%%   list of {#quadxy, #sectxy} in the path

-spec track_course(#quadxy{}, #sectxy{}, #quadxy{}, #sectxy{}) ->
    {ok, integer(), integer(), float(), float(), [{#quadxy{}, #sectxy{}}]} | out_of_bound.

track_course(SQC, SSC, DQC, DSC) ->
    case course_distance(SQC, SSC, DQC, DSC) of
        out_of_bound ->
            out_of_bound;
        {ok, DIFFX, DIFFY, CDEG, DISTSD} ->
            case abs(DIFFX) > abs(DIFFY) of
                true ->
                    LQC = list_track_x(SQC, SSC, DIFFX, DIFFY);
                false ->
                    LQC = list_track_y(SQC, SSC, DIFFX, DIFFY)
            end,
            {ok, DIFFX, DIFFY, CDEG, DISTSD, LQC}
    end.

-spec list_track_x(#quadxy{}, #sectxy{}, integer(), integer()) -> [{#quadxy{}, #sectxy{}}].

list_track_x(QC, SC, DIFFX, DIFFY) ->
    IX = ((QC#quadxy.x * ?NSECTS) + SC#sectxy.x),
    Y = float((QC#quadxy.y * ?NSECTS) + SC#sectxy.y),
    DY = DIFFY / DIFFX,
    list_track_x_elem(DIFFX, IX, Y, DY, []).

-spec list_track_x_elem(integer(), integer(), float(), float(), [{#quadxy{}, #sectxy{}}]) ->
    [{#quadxy{}, #sectxy{}}].

list_track_x_elem(0, _IX, _Y, _DY, PATH) ->
    lists:reverse(PATH);
list_track_x_elem(N, IX, Y, DY, PATH) ->
    IX2 = IX + 1,
    Y2 = Y + DY,
    IY = trunc(Y2 + 0.5),
    NQ = #quadxy{x = IX2 div ?NSECTS, y = IY div ?NSECTS},
    NC = #sectxy{x = IX2 rem ?NSECTS, y = IY rem ?NSECTS},
    list_track_x_elem(N - 1, IX2, Y2, DY, [{NQ, NC}|PATH]).

-spec list_track_y(#quadxy{}, #sectxy{}, integer(), integer()) -> [{#quadxy{}, #sectxy{}}].

list_track_y(QC, SC, DIFFX, DIFFY) ->
    X = float((QC#quadxy.x * ?NSECTS) + SC#sectxy.x),
    IY = ((QC#quadxy.y * ?NSECTS) + SC#sectxy.y),
    DX = DIFFX / DIFFY,
    list_track_y_elem(DIFFY, X, IY, DX, []).

-spec list_track_y_elem(integer(), float(), integer(), float(), [{#quadxy{}, #sectxy{}}]) ->
    [{#quadxy{}, #sectxy{}}].

list_track_y_elem(0, _X, _IY, _DX, PATH) ->
    lists:reverse(PATH);
list_track_y_elem(N, X, IY, DX, PATH) ->
    IY2 = IY + 1,
    X2 = X + DX,
    IX = trunc(X2 + 0.5),
    NQ = #quadxy{x = IX div ?NSECTS, y = IY2 div ?NSECTS},
    NC = #sectxy{x = IX rem ?NSECTS, y = IY2 rem ?NSECTS},
    list_track_y_elem(N - 1, X2, IY2, DX, [{NQ, NC}|PATH]).
