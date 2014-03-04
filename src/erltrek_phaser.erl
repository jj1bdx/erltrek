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

-module(erltrek_phaser).

-export([
        phaser/4
        ]).

-include("erltrek.hrl").

%% Klingon attacks in the sector
%% {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState

-spec phaser(integer(), integer(), integer(), game_state()) -> game_state().

phaser(SX, SY, ENERGY, GameState) ->
    {_Tick, _SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, DKS} = GameState,
    % only fire phaser if at least one Klingon is in the quadrant
    case dict:size(DKS) > 0 of
        true ->
            fire_phaser(SX, SY, ENERGY, GameState);
        false -> % do nothing
            io:format("No Klingon in sector, phaser not fired~n"),
            GameState
    end.

-spec fire_phaser(integer(), integer(), integer(), game_state()) -> game_state().

fire_phaser(SX, SY, ENERGY, GameState) ->
    {_Tick, SHIP, _NK, _DS, _DI, _DB, _DH, _DKQ, _SECT, _DKS} = GameState,
    SHIPENERGY = SHIP#enterprise_status.energy,
    case SHIPENERGY > ENERGY of
        true ->
            prepare_phaser(SX, SY, ENERGY, GameState);
        false -> % cannot fire because of exceeding energy level
            io:format("Firing level exceeds availably energy, phaser not fired~n"),
            GameState
    end.

-spec prepare_phaser(integer(), integer(), integer(), game_state()) -> game_state().

prepare_phaser(SX, SY, ENERGY, GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    % Deplete energy from Enterprise
    SHIPENERGY = SHIP#enterprise_status.energy,
    SHIP2 = SHIP#enterprise_status{energy = SHIPENERGY - ENERGY},
    ShipSC = SHIP#enterprise_status.sectxy,
    % Calculate course for each Klingon
    COURSE = erltrek_calc:sector_course(ShipSC, #sectxy{x = SX, y = SY}),
    LK = dict:fetch_keys(DKS),
    {LCOURSE, LDIST} = lists:unzip(
        [erltrek_calc:sector_course_distance(ShipSC, SC) || SC <- LK]),
    NewGameState = {Tick, SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS},
    hit_phaser(LK, LDIST, LCOURSE, ENERGY, COURSE, NewGameState).

%% Calculate phaser hit for each klingon

-spec hit_phaser([#sectxy{}], [float()], [float()],
    integer(), float(), game_state()) -> game_state().

hit_phaser([], _, _, _, _, GameState) ->
    GameState; % do nothing if klingon list is empty
hit_phaser(LK, LDIST, LCOURSE, ENERGY, COURSE, GameState) ->
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    [SK|LK2] = LK,
    [SDIST|LDIST2] = LDIST,
    [SCOURSE|LCOURSE2] = LCOURSE,
    [K] = dict:fetch(SK, DKS),
    KE = K#klingon_status.energy,
    % Calculate hitting level
    io:format("ENERGY = ~b COURSE = ~.1f SDIST = ~.1f SCOURSE = ~.1f~n",
                [ENERGY, COURSE, SDIST, SCOURSE]),
    HIT = trunc(float(ENERGY) * math:pow(0.9, float(SDIST)) *
                math:exp(-0.7 * abs((SCOURSE - COURSE)/2.0))),
    % Deplete energy from Klingon and update the dict
    io:format("Phaser hit to Klingon at sector ~b,~b level ~b~n",
                [SK#sectxy.x, SK#sectxy.y, HIT]),
    NKE = KE - HIT,
    case NKE > 0 of
        true -> % klingon is alive
            K2 = K#klingon_status{energy = NKE},
            DKS2 = dict:append(SK, K2, dict:erase(SK, DKS)),
            GameState2 = {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS2},
            hit_phaser(LK2, LDIST2, LCOURSE2, ENERGY, COURSE, GameState2);
        false -> % klingon is killed
            io:format("Klingon at sector ~b,~b killed~n",
                [SK#sectxy.x, SK#sectxy.y]),
            QC = SHIP#enterprise_status.quadxy,
            DKQ2 = dict:store(QC, dict:fetch(QC, DKQ) - 1, DKQ),
            DKS3 = dict:erase(SK, DKS),
            NK2 = NK - 1,
            SECT2 = array:set(erltrek_setup:sectxy_index(SK), s_empty, SECT),
            GameState3 = {Tick, SHIP, NK2, DS, DI, DB, DH, DKQ2, SECT2, DKS3},
            hit_phaser(LK2, LDIST2, LCOURSE2, ENERGY, COURSE, GameState3)
    end.
