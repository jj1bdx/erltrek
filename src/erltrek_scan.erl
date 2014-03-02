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

-export([
        condition_string/1,
        lrscan/1,
        srscan/1
        ]).

%% Return specified quadrant info string from
%% * Quadrant coordinate #quadxy
%% * dicts with keys of quadxy on:
%%   * stars, values of #sectxy list (of multiple stars)
%%   * inhabited systems, values of #inhabited_info list (one element per list)
%%   * bases, values of #base_info list (one element per list)
%%   * values of the number of klingons per quadrant

-spec quadstr(#quadxy{}, dict(), dict(), dict(), dict()) -> string().

quadstr(QC, DS, DI, DB, DKQ) ->
    case ?INQUADQC(QC) of
        true ->
            case dict:is_key(QC, DS) of
                true ->
                    NS = length(dict:fetch(QC, DS));
                false ->
                    NS = 0
            end,
            case dict:is_key(QC, DI) of
                true ->
                    NS2 = NS + 1;
                false ->
                    NS2 = NS
            end,
            case dict:is_key(QC, DB) of
                true ->
                    NB = 1;
                false ->
                    NB = 0
            end,
            case dict:is_key(QC, DKQ) of
                true ->
                    NK = dict:fetch(QC, DKQ);
                false ->
                    NK = 0
            end,
            [$0 + NK, $0 + NB, $0 + NS2];
        false ->
            % negative energy barrier, out of quadrant range
            " * "
    end.

%% print lrscan line for each list of X

-spec lrscan_lines([integer()], [integer()], dict(), dict(), dict(), dict()) -> ok.

lrscan_lines([], _LY, _DS, _DI, _DB, _DKQ) ->
    ok;
lrscan_lines([X|LXT], LY, DS, DI, DB, DKQ) ->
    SX = case ?INQUAD(X) of
        true ->
            [32, $0 + X, 32, $!];
        false ->
            "   !"
    end,
    SY = [ " " ++ quadstr(QC, DS, DI, DB, DKQ) ++ " !" || 
        QC <- [#quadxy{x = X, y = Y} || Y <- LY]],
    io:format("~s~s~n", [SX, lists:append(SY)]),
    lrscan_lines(LXT, LY, DS, DI, DB, DKQ).

%% Display long range sensor output from the game state

-spec lrscan(game_state()) -> ok.

lrscan(GameState) ->
    {_Tick, SHIP, _NK, DS, DI, DB, _DH, DKQ, _SECT, _DKS} = GameState,
    QC = SHIP#enterprise_status.quadxy,
    QX = QC#quadxy.x,
    QY = QC#quadxy.y,
    LY = lists:seq(QY - 1, QY + 1),
    io:format("Long range scan for Quadrant ~b,~b~n", [QX, QY]),
    SY = [case ?INQUAD(Y) of
              true ->
                  "   " ++ [$0 + Y] ++ "  ";
              false ->
                  "      "
          end || Y <- LY],
    io:format("   ~s~n", [SY]),
    io:format("   -------------------~n"),
    lrscan_lines(lists:seq(QX - 1, QX + 1), LY, DS, DI, DB, DKQ),
    io:format("   -------------------~n~n").

%% Fetch condition string

-spec condition_string(cond_green | cond_yellow | cond_red | cond_docked) ->
    string().

condition_string(Condition) ->
    CONDITION = orddict:from_list([
            {cond_green, "GREEN"}, {cond_yellow, "YELLOW"},
            {cond_red, "RED"}, {cond_docked, "DOCKED"}]),
    orddict:fetch(Condition, CONDITION).

%% Display current sector info and ship status from the game state

-spec srscan(game_state()) -> ok.

srscan(GameState) ->
    {Tick, SHIP, NK, _DS, DI, _DB, _DH, _DKQ, SECT, _DKS} = GameState,
    DISP = orddict:from_list([
            {s_empty, $.}, {s_star, $*}, {s_enterprise, $E},
            {s_base, $#}, {s_inhabited, $@}, {s_klingon, $K},
            {s_hole, $H}]),
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
    io:format("Short range sensor scan~n"),
    io:format("  0 1 2 3 4 5 6 7 8 9~n"),
    srscan_xline(0, STATUS, SECT, DISP),
    io:format("  0 1 2 3 4 5 6 7 8 9~n"),
    case dict:is_key(SHIP#enterprise_status.quadxy, DI) of
        true ->
            LI = dict:fetch(SHIP#enterprise_status.quadxy, DI),
            [I] = LI,
            io:format("Starsystem ~s~n", [I#inhabited_info.systemname]);
        false ->
            ok % do nothing
    end,
    ok.

-spec srscan_xline(non_neg_integer(), [string()], array(), orddict()) -> ok.

srscan_xline(?NSECTS, _SL, _SECT, _DISP) ->
    ok;
srscan_xline(X, SL, SECT, DISP) ->
    io:format("~c ", [X + $0]),
    srscan_ypos(0, X, SECT, DISP),
    io:format("~c  ", [X + $0]),
    case length(SL) > 0 of
        true ->
            [H|SL2] = SL,
            io:format("~s~n", [H]);
        false ->
            SL2 = SL,
            io:format("~n")
    end,
    srscan_xline(X + 1, SL2, SECT, DISP).
    
-spec srscan_ypos(non_neg_integer(), non_neg_integer(), array(), orddict()) -> ok.

srscan_ypos(?NSECTS, _X, _SECT, _DISP) ->
    ok;
srscan_ypos(Y, X, SECT, DISP) ->
    io:format("~c ", [orddict:fetch(
                array:get(erltrek_setup:sectxy_index(#sectxy{x = X, y = Y}), 
                    SECT), DISP)]),
    srscan_ypos(Y + 1, X, SECT, DISP).

