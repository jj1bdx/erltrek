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

-module(erltrek_klingon).

-export([
        attack/1,
        move/1
        ]).

-include("erltrek.hrl").

%% Klingon attacks in the sector
%% {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState

-spec attack(game_state()) -> game_state().

attack(GameState) ->
    {_Tick, _SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, DKS} = GameState,
    % Attach if at least one Klingon is in the quadrant
    % and do it only randomly once in 10 calls
    case dict:size(DKS) > 0 andalso tinymt32:uniform(10) == 1 of 
        true ->
            actual_attack(GameState);
        false -> % do nothing
            GameState
    end.

%% Klingon moves in the sector
%% {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState

-spec move(game_state()) -> game_state().

move(GameState) ->
    {_Tick, _SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, DKS} = GameState,
    % Attach if at least one Klingon is in the quadrant
    % and do it only randomly once in five calls
    case dict:size(DKS) > 0 andalso tinymt32:uniform(5) == 1 of 
        true ->
            actual_move(GameState);
        false -> % do nothing
            GameState
    end.

%% Calculate distance between two sectors

-spec sector_distance(#sectxy{}, #sectxy{}) -> float().

sector_distance(SC, DC) ->
    DX = SC#sectxy.x - DC#sectxy.x,
    DY = SC#sectxy.y - DC#sectxy.y,
    math:sqrt((DX*DX) + (DY*DY)).

%% Klingon actual attacks in the sector

-spec actual_attack(game_state()) -> game_state().

actual_attack(GameState) ->
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, DKS} = GameState,
    ShipSC = SHIP#enterprise_status.sectxy,
    LK = dict:fetch_keys(DKS),
    LDIST = [sector_distance(SC, ShipSC) || SC <- LK],
    % sorted with the distance
    {LDIST2, LK2} = lists:unzip(lists:sort(
            fun(A, B) -> {DA, _} = A, {DB, _} = B, DA =< DB end,
            lists:zip(LDIST, LK))),
    % attack from the closest one
    perform_attack(LK2, LDIST2, GameState).

%% Performing attack for each klingon

-spec perform_attack([#sectxy{}], [float()], game_state()) -> game_state().

perform_attack([], _, GameState) ->
    GameState; % do nothing if klingon list is empty
perform_attack(LK, LDIST, GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    [SK|LK2] = LK,
    [SDIST|LDIST2] = LDIST,
    [K] = dict:fetch(SK, DKS),
    KE = K#klingon_status.energy,
    case KE < 50 of
        true -> % choose next one
            perform_attack(LK2, LDIST2, GameState);
        false -> % use this one to attack
            % Use 30 - 50% of energy level
            KBLAST = trunc(float(KE) * ((tinymt32:uniform() * 0.1) + 0.4)),
            % Hit power
            KHIT = trunc(float(KBLAST) * math:pow(0.9, float(SDIST)) * 0.8),
            % Deplete energy from Klingon and update the dict
            K2 = K#klingon_status{energy = KE - KBLAST},
            % erase and append to the dict required
            DKS2 = dict:append(SK, K2, dict:erase(SK, DKS)),
            % subtract hitting energy from Enterprise
            SHIPSHIELD = SHIP#enterprise_status.shield,
            io:format("Klingon hit from sector ~b,~b level ~b~n",
                [SK#sectxy.x, SK#sectxy.y, KHIT]),
            % first subtract from shield
            NSHIELD = SHIPSHIELD - KHIT,
            case NSHIELD =< 0 of
                true ->
                    io:format("Shield gone~n"),
                    DAMAGE = trunc(float(-NSHIELD * 1.3)) + 10,
                    io:format("Damage level up to ~b~n", [DAMAGE]),
                    NSHIPSHIELD = 0;
                false ->
                    io:format("Shield level down to ~b~n", [NSHIELD]),
                    DAMAGE = 0,
                    NSHIPSHIELD = NSHIELD
            end,
            SHIPENERGY = SHIP#enterprise_status.energy,
            NENERGY = SHIPENERGY - DAMAGE,
            SHIP2 = SHIP#enterprise_status{energy = NENERGY, shield = NSHIPSHIELD},
            {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS2}
    end.

%% Klingon actual moves in the sector
%% {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState

-spec actual_move(game_state()) -> game_state().

actual_move(GameState) -> GameState. % skeleton
