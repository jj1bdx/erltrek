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

-module(erltrek_scan).

-include("erltrek.hrl").

-export([lrscan/1,
         srscan/1,
         adjacent_sector_contents/2,
         %
         condition_string/1,
         lrscan_string/1,
         srscan_string/1
        ]).

-spec lrscan(game_state()) -> ok.
lrscan(GameState) ->
    erltrek_event:notify({lrscan, GameState}).

-spec srscan(game_state()) -> ok.
srscan(GameState) ->
    erltrek_event:notify({srscan, GameState}).

%% Return specified quadrant info string from
%% * Quadrant coordinate #quadxy
%% * dicts with keys of quadxy on:
%%   * stars, values of #sectxy list (of multiple stars)
%%   * inhabited systems, values of #inhabited_info list (one element per list)
%%   * bases, values of #base_info list (one element per list)
%%   * values of the number of klingons per quadrant

-spec quadstr(#quadxy{}, dict(), dict(), dict(), dict()) -> string().

quadstr(QC, DS, DI, DB, DKQ) ->
    case erltrek_calc:in_quadxy(QC) of
        true ->
            NS = case dict:is_key(QC, DS) of
                true -> length(dict:fetch(QC, DS));
                false -> 0
            end,
            NS2 = case dict:is_key(QC, DI) of
                true -> NS + 1;
                false -> NS
            end,
            NB = case dict:is_key(QC, DB) of
                true -> 1;
                false -> 0
            end,
            NK = case dict:is_key(QC, DKQ) of
                true -> dict:fetch(QC, DKQ);
                false -> 0
            end,
            [$0 + NK, $0 + NB, $0 + NS2];
        false ->
            % negative energy barrier, out of quadrant range
            " * "
    end.

%% print lrscan line for each list of X

-spec lrscan_lines([integer()], [integer()], dict(), dict(), dict(),
 dict()) -> iolist().

lrscan_lines([], _LY, _DS, _DI, _DB, _DKQ) ->
    "   -------------------\n\n";
lrscan_lines([X|LXT], LY, DS, DI, DB, DKQ) ->
    [case erltrek_calc:in_quadrant(X) of
         true -> [32, $0 + X, 32, $!];
         false -> "   !"
     end,
     [" " ++ quadstr(QC, DS, DI, DB, DKQ) ++ " !"
      || QC <- [#quadxy{x = X, y = Y} || Y <- LY]],
     "\n"
     | lrscan_lines(LXT, LY, DS, DI, DB, DKQ)].

%% Display long range sensor output from the game state

-spec lrscan_string(game_state()) -> iolist().

lrscan_string(GameState) ->
    {_Tick, SHIP, _NK, DS, DI, DB, _DH, DKQ, _SECT, _DKS} = GameState,
    QC = SHIP#enterprise_status.quadxy,
    QX = QC#quadxy.x,
    QY = QC#quadxy.y,
    LY = lists:seq(QY - 1, QY + 1),
    %% begin iolist result
    [io_lib:format("Long range scan for Quadrant ~b,~b~n", [QX, QY]),
     "   ",
     [case erltrek_calc:in_quadrant(Y) of
              true ->
                  "   " ++ [$0 + Y] ++ "  ";
              false ->
                  "      "
      end || Y <- LY],
     "\n"
     "   -------------------\n"
     | lrscan_lines(lists:seq(QX - 1, QX + 1), LY, DS, DI, DB, DKQ)].


%% Fetch condition string

-spec condition_string(cond_green | cond_yellow | cond_red | cond_docked) ->
    string().

condition_string(cond_green) -> "GREEN";
condition_string(cond_yellow) -> "YELLOW";
condition_string(cond_red) -> "RED";
condition_string(cond_docked) -> "DOCKED".

scan_char(s_empty) -> $.;
scan_char(s_star) -> $*;
scan_char(s_enterprise) -> $E;
scan_char(s_base) -> $#;
scan_char(s_inhabited) -> $@;
scan_char(s_klingon) -> $K;
scan_char(s_hole) -> $H;
scan_char({Class, _}) ->
    scan_char(Class).

%% Display current sector info and ship status from the game state

-spec srscan_string(game_state()) -> iolist();
                   ({integer(), list()}) -> iolist().

srscan_string({Stardate, Scan}) ->
    Ship = lists:keyfind(ship_state, 1, Scan),
    Data = lists:keyfind(ship_data, 1, Scan),
    Quad = proplists:get_value(quad, Scan),

    LT = integer_to_list(Stardate),
    {LT1, LT2} = lists:split(length(LT) - 2, LT),
    STATUS =
        [io_lib:format("Stardate:      ~s.~s", [LT1, LT2]),
         io_lib:format("Position:      ~b,~b/~b,~b",
                       [Data#ship_data.quad#quadxy.x,
                        Data#ship_data.quad#quadxy.y,
                        Data#ship_data.sect#sectxy.x,
                        Data#ship_data.sect#sectxy.y]),
         io_lib:format("Condition:     ~s",
                       [condition_string(Ship#ship_state.condition)]),
         io_lib:format("Energy:        ~b", [Ship#ship_state.energy]),
         io_lib:format("Shield:        ~b", [Ship#ship_state.shield]),
         io_lib:format("Klingons:      ?? (todo)", [])
        ],
    %% begin iolist result
    ["Short range sensor scan\n",
     "  0 1 2 3 4 5 6 7 8 9\n",
     srscan_xline(0, STATUS, Quad),
     "  0 1 2 3 4 5 6 7 8 9\n",
     case lists:keyfind(inhabited_info, 1, Scan) of
         #inhabited_info{ systemname=Name } ->
             io_lib:format("Starsystem ~s~n", [Name]);
         false -> [] % do nothing
     end];
    
srscan_string(GameState) ->
    {Tick, SHIP, NK, _DS, DI, _DB, _DH, _DKQ, SECT, _DKS} = GameState,
    LT = integer_to_list(Tick),
    {LT1, LT2} = lists:split(length(LT) - 2, LT),
    STATUS = [
        io_lib:format("Stardate:      ~s.~s", [LT1, LT2]),
        io_lib:format("Position:      ~b,~b/~b,~b",
            [SHIP#enterprise_status.quadxy#quadxy.x,
             SHIP#enterprise_status.quadxy#quadxy.y,
             SHIP#enterprise_status.sectxy#sectxy.x,
             SHIP#enterprise_status.sectxy#sectxy.y]),
        io_lib:format("Condition:     ~s",
            [condition_string(SHIP#enterprise_status.condition)]),
        io_lib:format("Energy:        ~b", [SHIP#enterprise_status.energy]),
        io_lib:format("Shield:        ~b", [SHIP#enterprise_status.shield]),
        io_lib:format("Klingons:      ~b", [NK])
        ],
    %% begin iolist result
    ["Short range sensor scan\n",
     "  0 1 2 3 4 5 6 7 8 9\n",
     srscan_xline(0, STATUS, SECT),
     "  0 1 2 3 4 5 6 7 8 9\n",
     case dict:is_key(SHIP#enterprise_status.quadxy, DI) of
         true ->
             LI = dict:fetch(SHIP#enterprise_status.quadxy, DI),
             [I] = LI,
             io_lib:format("Starsystem ~s~n", [I#inhabited_info.systemname]);
         false ->
             [] % do nothing
     end].

-spec srscan_xline(non_neg_integer(), [string()], array()) -> iolist().

srscan_xline(?NSECTS, _SL, _SECT) -> [];
srscan_xline(X, SL, SECT) ->
    {Status, SLT} =
        case SL of
            [H|T] -> {H, T};
            T -> {[], T}
        end,
    [io_lib:format("~c ", [X + $0]),
     srscan_ypos(0, X, SECT),
     io_lib:format("~c  ", [X + $0]),
     Status, "\n"
     | srscan_xline(X + 1, SLT, SECT)].

-spec srscan_ypos(non_neg_integer(), non_neg_integer(), array()) -> iolist().

srscan_ypos(?NSECTS, _X, _SECT) -> [];
srscan_ypos(Y, X, SECT) ->
    [io_lib:format(
       "~c ", [scan_char(
                 array:get(
                   erltrek_calc:sectxy_index(#sectxy{x = X, y = Y}),
                   SECT))])
     | srscan_ypos(Y + 1, X, SECT)].

%% Return the list of adjacent sectors
%% with the tuple of sector coordinates and contents
%% (content is out_of_bound if outside the sector)

-spec adjacent_sector_contents(#sectxy{}, array()) ->
    [{#sectxy{}, sector_entity() | out_of_bound}].

adjacent_sector_contents(SC, SECT) ->
    SX = SC#sectxy.x,
    SY = SC#sectxy.y,
    [{#sectxy{x = X, y = Y}, sector_content(X, Y, SECT)} ||
        X <- [SX - 1, SX, SX + 1], Y <- [SY - 1, SY, SY + 1]].

-spec sector_content(sectcoord(), sectcoord(), array()) ->
    sector_entity() | out_of_bound.

sector_content(SX, SY, SECT) ->
    case erltrek_calc:in_sector(SX, SY) of
        false ->
            out_of_bound;
        true ->
            array:get(erltrek_calc:sectxy_index(
                    #sectxy{x = SX, y = SY}), SECT)
    end.
