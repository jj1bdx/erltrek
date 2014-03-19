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

-module(erltrek_setup).

-include("erltrek.hrl").

-export([
        gen_quad_list/1,
        gen_sect_list/3,
        inhabited_names/0,
        init_quad/0,
        init_sect/0,
        rand_quad/1,
        rand_sect/1,
        setup_galaxy/0,
        setup_sector/6,
        setup_state/0
        ]).

-import(erltrek_calc, [quadxy_index/1, sectxy_index/1]).

%% return empty {sx, sy} in the Sector array

-spec rand_sect(array()) -> #sectxy{}.

rand_sect(S) ->
    rand_sect(S, #sectxy{}, false).

-spec rand_sect(array(), #sectxy{}, boolean()) -> #sectxy{}.

rand_sect(_, SC, true) ->
    SC;
rand_sect(S, _, false) ->
    SC = #sectxy{x = tinymt32:uniform(?NSECTS) - 1,
                 y = tinymt32:uniform(?NSECTS) - 1},
    rand_sect(S, SC, array:get(sectxy_index(SC), S) =:= s_empty).

%% return empty {qx, qy} in the Quadrant array

-spec rand_quad(array()) -> #quadxy{}.

rand_quad(Q) ->
    rand_quad(Q, #quadxy{}, false).

-spec rand_quad(array(), #quadxy{}, boolean()) -> #quadxy{}.

rand_quad(_, QC, true) ->
    QC;
rand_quad(Q, _, false) ->
    QC = #quadxy{x = tinymt32:uniform(?NQUADS) - 1,
                 y = tinymt32:uniform(?NQUADS) - 1},
    rand_quad(Q, QC, array:get(quadxy_index(QC), Q) =:= q_empty).

%% init Sector array

-spec init_sect() -> array().

init_sect() ->
    array:new(?NSECTS * ?NSECTS, {default, s_empty}).

%% init Quadrant array

-spec init_quad() -> array().

init_quad() ->
    array:new(?NQUADS * ?NQUADS, {default, q_empty}).

%% Generate a list of random quadrant coordinates without duplicates

-spec gen_quad_list(non_neg_integer()) -> list(#quadxy{}).

gen_quad_list(N) ->
    gen_quad_list(N, [], init_quad()).

-spec gen_quad_list(non_neg_integer(), list(#quadxy{}), array()) -> list(#quadxy{}).

gen_quad_list(0, L, _) ->
    L;
gen_quad_list(N, L, A) ->
    QC = rand_quad(A),
    gen_quad_list(N - 1, [QC|L], array:set(quadxy_index(QC), q_fill, A)).

%% Generate a list of random sector coordinates without duplicates
%% with sector state input and output

-spec gen_sect_list(non_neg_integer(), sector_entity(), array()) ->
    {array(), list(#sectxy{})}.

gen_sect_list(N, ENT, SECT) ->
    gen_sect_list(N, ENT, [], SECT).

-spec gen_sect_list(non_neg_integer(), sector_entity(), list(#sectxy{}), array()) ->
    {array(), list(#sectxy{})}.

gen_sect_list(0, _, L, SECT) ->
    {SECT, L};
gen_sect_list(N, ENT, L, A) ->
    SC = rand_sect(A),
    gen_sect_list(N - 1, ENT, [SC|L], array:set(sectxy_index(SC), ENT, A)).

%% List of names of inhabited stars

-spec inhabited_names() -> list(string()).

inhabited_names() -> [
    "Talos IV",
    "Rigel III",
    "Deneb VII",
    "Canopus V",
    "Icarus I",
    "Prometheus II",
    "Omega VII",
    "Elysium I",
    "Scalos IV",
    "Procyon IV",
    "Arachnid I",
    "Argo VIII",
    "Triad III",
    "Echo IV",
    "Nimrod III",
    "Nemisis IV",
    "Centarurus I",
    "Kronos III",
    "Spectros V",
    "Beta III",
    "Gamma Tranguli VI",
    "Pyris III",
    "Triachus",
    "Marcus XII",
    "Kaland",
    "Ardana",
    "Stratos",
    "Eden",
    "Arrikis",
    "Epsilon Eridani IV",
    "Exo III"
    ].

%% Setup galaxy, returns following info:
%% * number of Klingons
%% * dicts with keys of quadxy on:
%%   * stars, values of #sectxy list (of multiple stars)
%%   * inhabited systems, values of #inhabited_info list (one element per list)
%%   * bases, values of #base_info list (one element per list)
%%   * holes, values of #sectxy list (of multiple stars)
%%   * number of klingons, values of integer (NOT a list)

-spec setup_galaxy() -> {pos_integer(), dict(), dict(), dict(), dict(), dict()}.

setup_galaxy() ->
    % number of bases in the galaxy
    NBASES = tinymt32:uniform(?MAXBASES - 3) + 3,
    LBASEQUAD = gen_quad_list(NBASES),
    LINHABITNAME = inhabited_names(),
    LINHABITQUAD = gen_quad_list(length(LINHABITNAME)),
    DINAME = dict:from_list(lists:zip(LINHABITQUAD, LINHABITNAME)),
    {DSTAR, DINHABIT, DBASE, DHOLE} =
        setup_galaxy_sector(?NQUADS - 1, ?NQUADS - 1,
            LBASEQUAD, LINHABITQUAD, DINAME,
            dict:new(), dict:new(), dict:new(), dict:new()),
    % number of klingons in the galaxy
    NKLINGONS = (tinymt32:uniform(25) * 2) + 10,
    DKLINGON = setup_klingon_numbers(NKLINGONS, dict:new()),
    {NKLINGONS, DSTAR, DINHABIT, DBASE, DHOLE, DKLINGON}.

%% Setup galaxy sector of each quadrant X, Y,
%% list of base #quadxy, list of inhabited #quadxy,
%% pair list of {#quadxy, systemname} for inhabited systems,
%% and dicts with keys of quadxy on:
%%   * stars, values of #sectxy list (of multiple stars)
%%   * inhabited systems, values of #inhabited_info list (one element per list)
%%   * bases, values of #base_info list (one element per list)
%%   * holes, values of #sectxy list (of multiple stars)

-spec setup_galaxy_sector(integer(), integer(), list(#quadxy{}), list(#quadxy{}),
    dict(), dict(), dict(), dict(), dict()) ->
    {dict(), dict(), dict(), dict()}.

setup_galaxy_sector(-1, _QY, _LB, _LI, _DINAME, DS, DI, DB, DH) ->
    {DS, DI, DB, DH};
setup_galaxy_sector(QX, -1, LB, LI, DINAME, DS, DI, DB, DH) ->
    setup_galaxy_sector(QX - 1, ?NQUADS - 1, LB, LI, DINAME, DS, DI, DB, DH);
setup_galaxy_sector(QX, QY, LB, LI, DINAME, DS, DI, DB, DH) ->
    QC = #quadxy{x = QX, y = QY},
    SECT = init_sect(),
    {SECT2, DB2} = case lists:member(QC, LB) of
        true ->
            % fill the sector map
            SC = rand_sect(SECT),
            {array:set(sectxy_index(SC), s_base, SECT),
            % add the base info
            % @todo add attacked, etc
            dict:append(QC, #base_info{xy = SC}, DB)};
        false ->
            {SECT, DB}
    end,
    {NINHABITED, SECT3, DI2} = case lists:member(QC, LI) of
        true ->
            SC2 = rand_sect(SECT),
            SYSTEMNAME = dict:fetch(QC, DINAME),
            {1,
            array:set(sectxy_index(SC2), s_inhabited, SECT2),
            % @todo add distressed, etc
            dict:append(QC,
                #inhabited_info{xy = SC2,
                    systemname = SYSTEMNAME},
                DI)};
        false ->
            {0, SECT2, DI}
    end,
    % the number of stars includes the inhabited star
    NSTARS = tinymt32:uniform(9) - NINHABITED,
    {SECT4, DS2} = case NSTARS > 0 of
        true ->
            {SECT31, STARLIST} = gen_sect_list(NSTARS, s_star, SECT3),
            {SECT31, dict:append_list(QC, STARLIST, DS)};
        false ->
            {SECT3, DS}
    end,
    NHOLES = tinymt32:uniform(3) - 1,
    {_SECT5, HOLELIST} = gen_sect_list(NHOLES, s_hole, SECT4),
    DH2 = dict:append_list(QC, HOLELIST, DH),
    setup_galaxy_sector(QX, QY - 1, LB, LI, DINAME, DS2, DI2, DB2, DH2).

%% setup a dict with keys of quadxy on the number of klingons per quadrant
%% with given total number of klingons

-spec setup_klingon_numbers(non_neg_integer(), dict()) -> dict().

setup_klingon_numbers(0, DKQ) ->
    DKQ;
setup_klingon_numbers(NKALL, DKQ) ->
    N = tinymt32:uniform(4),
    NKADD = case N > NKALL of
        true ->
            NKALL;
        false ->
            N
    end,
    QC = #quadxy{x = tinymt32:uniform(?NQUADS) - 1,
                 y = tinymt32:uniform(?NQUADS) - 1},
    NKOLD = case dict:is_key(QC, DKQ) of
        true ->
            dict:fetch(QC, DKQ);
        false ->
            0
    end,
    NKNEW = NKOLD + NKADD,
    {DKQ2, NKALL2} = case NKNEW =< ?MAXKLQUAD of
        true ->
            {dict:store(QC, NKNEW, DKQ),
                NKALL - NKNEW};
        false ->
            {DKQ, NKALL}
    end,
    setup_klingon_numbers(NKALL2, DKQ2).

%% fill in a sector array with given sector_entity()
%% for the given list of #sectxy

-spec fill_sector([#sectxy{}], sector_entity(), array()) -> array().

fill_sector([], _E, SECT) ->
    SECT;
fill_sector(LSC, E, SECT) ->
    [H|T] = LSC,
    fill_sector(T, E, array:set(sectxy_index(H), E, SECT)).

%% fill in a sector array and a dict of key #sectxy with value #klingon_status
%% for the given number of Klingons

-spec fill_klingons(non_neg_integer(), array(), dict()) -> {array(), dict()}.

fill_klingons(0, SECT, DKS) ->
    {SECT, DKS};
fill_klingons(N, SECT, DKS) ->
    SC = rand_sect(SECT),
    fill_klingons(N - 1, array:set(sectxy_index(SC), s_klingon, SECT),
        % initial energy of klingon
        dict:append(SC, #klingon_status{energy = ?KLINGONENERGY}, DKS)).

%% Setup the sector array and a dict of key #sectxy with value #klingon_status
%% for the given Quadrant of #quadxy and
%% * dicts with keys of quadxy on:
%%   * stars, values of #sectxy list (of multiple stars)
%%   * inhabited systems, values of #inhabited_info list (one element per list)
%%   * bases, values of #base_info list (one element per list)
%%   * holes, values of #sectxy list (of multiple stars)
%%   * values of the number of klingons per quadrant

-spec setup_sector(#quadxy{}, dict(), dict(), dict(), dict(), dict()) ->
        {array(), dict()}.

setup_sector(QC, DS, DI, DB, DH, DKQ) ->
    SECT = init_sect(),
    % stars
    SECT2 = case dict:is_key(QC, DS) of
        true ->
            fill_sector(dict:fetch(QC, DS), s_star, SECT);
        false ->
            SECT
    end,
    % inhabited systems
    SECT3 = case dict:is_key(QC, DI) of
        true ->
            [TI] = dict:fetch(QC, DI),
            fill_sector([TI#inhabited_info.xy], s_inhabited, SECT2);
        false ->
            SECT2
    end,
    % bases
    SECT4 = case dict:is_key(QC, DB) of
        true ->
            [TB] = dict:fetch(QC, DB),
            fill_sector([TB#base_info.xy], s_base, SECT3);
        false ->
            SECT3
    end,
    % holes
    SECT5 = case dict:is_key(QC, DH) of
        true ->
            fill_sector(dict:fetch(QC, DH), s_hole, SECT4);
        false ->
            SECT4
    end,
    % klingons
    DKS = dict:new(),
    {SECT6, DKS2} = case dict:is_key(QC, DKQ) of
        true ->
            fill_klingons(dict:fetch(QC, DKQ), SECT5, DKS);
        false ->
            {SECT5, DKS}
    end,
    {SECT6, DKS2}.

%% setup game state, which returns:
%% * Enterprise status as #enterprise_status
%% * number of Klingons
%% * dicts with keys of quadxy on:
%%   * stars, values of #sectxy list (of multiple stars)
%%   * inhabited systems, values of #inhabited_info list (one element per list)
%%   * bases, values of #base_info list (one element per list)
%%   * holes, values of #sectxy list (of multiple stars)
%%   * number of klingons, values of integer (NOT a list)
%% * sector array for the current quadrant where Enterprise resides
%% * a dict of key #sectxy with value #klingon_status for the current quadrant

-spec setup_state() -> game_state().

setup_state() ->
    Tick = ?INITTICK,
    {NK, DS, DI, DB, DH, DKQ} = erltrek_setup:setup_galaxy(),
    % put enterprise to where a base resides if possible
    LB = dict:fetch_keys(DB),
    QC = case length(LB) > 0 of
        true ->
            hd(LB);
        false ->
            #quadxy{x = tinymt32:uniform(?NQUADS) - 1,
                    y = tinymt32:uniform(?NQUADS) - 1}
    end,
    {SECT, DKS} = erltrek_setup:setup_sector(QC, DS, DI, DB, DH, DKQ),
    % put enterprise in the current quadrant
    SC = rand_sect(SECT),
    SECT2 = array:set(sectxy_index(SC), s_enterprise, SECT),
    % setup enterprise status
    SHIP = #enterprise_status{quadxy = QC, sectxy = SC,
        energy = ?SHIPENERGY, shield = ?SHIPSHIELD,
        impulse_move = false, impulse_course = [],
        warp_move = false, warp_course = [],
        docked = false, condition = cond_green,
        next_command = {}},
    % return the values as a tuple
    {Tick, SHIP, NK, DS, DI, DB, DH, DKQ, SECT2, DKS}.
