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

-export([condition_string/1,
         lrscan_string/1,
         srscan_string/1
        ]).


%% Fetch condition string

-spec condition_string(cond_green | cond_yellow | cond_red | cond_docked) ->
    string().

condition_string(cond_green) -> "GREEN";
condition_string(cond_yellow) -> "YELLOW";
condition_string(cond_red) -> "RED";
condition_string(cond_docked) -> "DOCKED".


%% Display long range sensor output from the game state

-spec lrscan_string(list()) -> iolist().

lrscan_string([Data|Scan]) ->
    #ship_data{ quad=#quadxy{ x=QX, y=QY } } = Data,
    %% begin iolist result
    [io_lib:format("Long range scan for Quadrant ~b,~b~n", [QX, QY]),
     "   ",
     [case erltrek_calc:in_quadrant(Y) of
              true ->
                  "   " ++ [$0 + Y] ++ "  ";
              false ->
                  "      "
      end || Y <- lists:seq(QY - 1, QY + 1)],
     "\n"
     "   -------------------\n",
     [lrscan_lines(X, Scan) || X <- lists:seq(QX - 1, QX + 1)],
     "   -------------------\n\n"].

%% print lrscan line for each list of X

lrscan_lines(X, Scan) ->
    [case erltrek_calc:in_quadrant(X) of
         true -> [32, $0 + X, 32, $!];
         false -> "   !"
     end,
     [if Props == negative_energy_barrier -> "  *  !";
         true -> io_lib:format(
                   " ~3.b !",
                   [lists:foldl(
                      fun ({stars, S}, C) -> C + S;
                          ({bases, B}, C) -> C + 10 * B;
                          ({enemies, K}, C) -> C + 100 * K
                      end, 0, Props)])
      end || {#quadxy{ x=QX }, Props} <- Scan,
             QX == X],
     "\n"].

%% convert sector_entity() into the corresponding character

-spec scan_char(sector_entity()) -> char().

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

-spec srscan_string({integer(), list()}) -> iolist().

srscan_string({Stardate, Scan}) ->
    Ship = lists:keyfind(ship_state, 1, Scan),
    Data = lists:keyfind(ship_data, 1, Scan),
    Quad = proplists:get_value(quad, Scan),
    Enemies = proplists:get_value(enemies, Scan, unknown),

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
         io_lib:format("Klingons:      ~p", [Enemies])
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
     end].


-spec srscan_xline(non_neg_integer(), [string()], sector_array()) -> iolist().

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

-spec srscan_ypos(non_neg_integer(), non_neg_integer(), sector_array()) -> iolist().

srscan_ypos(?NSECTS, _X, _SECT) -> [];
srscan_ypos(Y, X, SECT) ->
    [io_lib:format(
       "~c ", [scan_char(
                 array:get(
                   erltrek_calc:sectxy_index(#sectxy{x = X, y = Y}),
                   SECT))])
     | srscan_ypos(Y + 1, X, SECT)].
