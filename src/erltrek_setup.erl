%%% ----------------------------------------------------------------------------------
%%% Erltrek ("this software") is covered under the BSD 3-clause license.
%%% 
%%% This product includes software developed by the University of California, Berkeley
%%% and its contributors.
%%% 
%%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%% 
%%% * Redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%% 
%%% * Redistributions in binary form must reproduce the above copyright notice, this
%%%   list of conditions and the following disclaimer in the documentation and/or
%%%   other materials provided with the distribution.
%%% 
%%% * Neither the name of Kenji Rikitake, k2r.org, nor the names of its
%%%   contributors may be used to endorse or promote products derived from
%%%   this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
%%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% 
%%% This software incorporates portions of the BSD Star Trek source code,
%%% distributed under the following license:
%%% 
%%% Copyright (c) 1980, 1993
%%%      The Regents of the University of California.  All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 3. All advertising materials mentioning features or use of this software
%%%    must display the following acknowledgement:
%%%      This product includes software developed by the University of
%%%      California, Berkeley and its contributors.
%%% 4. Neither the name of the University nor the names of its contributors
%%%    may be used to endorse or promote products derived from this software
%%%    without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%% 
%%% [End of LICENSE]
%%% ----------------------------------------------------------------------------------

-module(erltrek_setup).

-include("erltrek.hrl").

-export([
        genquadlist/1,
        gensectlist/3,
        inhabitednames/0,
        initquad/0,
        initsect/0,
        randquad/1,
        randsect/1,
        setupgalaxy/0
        ]).

%% return empty {sx, sy} in the Sector array
randsect(S) ->
    randsect(S, {}, false).

randsect(_, SC, true) ->
    SC;
randsect(S, _, false) ->
    SX = tinymt32:uniform(?NSECTS) - 1,
    SY = tinymt32:uniform(?NSECTS) - 1,
    randsect(S, {SX, SY}, 
        array:get(?SECTCOORD(SX, SY), S) =:= s_empty).

%% return empty {qx, qy} in the Quadrant array
randquad(Q) ->
    randquad(Q, {}, false).

randquad(_, QC, true) ->
    QC;
randquad(Q, _, false) ->
    QX = tinymt32:uniform(?NQUADS) - 1,
    QY = tinymt32:uniform(?NQUADS) - 1,
    randquad(Q, {QX, QY}, 
        array:get(?QUADCOORD(QX, QY), Q) =:= q_empty).

%% init Sector array
initsect() ->
    array:new(?NSECTS * ?NSECTS, {default, s_empty}).

%% init Quadrant array
initquad() ->
    array:new(?NQUADS * ?NQUADS, {default, q_empty}).

%% Generate a list of random quadrant coordinates without duplicates
genquadlist(N) ->
    genquadlist(N, [], initquad()).

genquadlist(0, L, _) ->
    L;
genquadlist(N, L, A) ->
    QC = randquad(A),
    {QX, QY} = QC,
    A2 = array:set(?QUADCOORD(QX, QY), q_fill, A),
    genquadlist(N - 1, [QC|L], A2).

%%% Generate a list of random sector coordinates without duplicates
%%% with sector state input and output
gensectlist(N, ENT, SECT) ->
    gensectlist(N, ENT, [], SECT).

gensectlist(0, _, L, SECT) ->
    {SECT, L};
gensectlist(N, ENT, L, A) ->
    SC = randsect(A),
    {SX, SY} = SC,
    A2 = array:set(?SECTCOORD(SX, SY), ENT, A),
    gensectlist(N - 1, ENT, [SC|L], A2).

%% List of names of inhabited stars
inhabitednames() -> [
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

%% Setup stars (returns dict)
setupgalaxy() ->
    NBASES = tinymt32:uniform(?MAXBASES - 3) + 3,
    LBASEQUAD = genquadlist(NBASES),
    LINHABITNAME = inhabitednames(),
    LINHABITQUAD = genquadlist(length(LINHABITNAME)),
    DINAME = dict:from_list(lists:zip(LINHABITQUAD, LINHABITNAME)),
    {DSTAR, DINHABIT, DBASE, DHOLE} =
        setupgalaxypersect(?NQUADS - 1, ?NQUADS - 1,
            LBASEQUAD, LINHABITQUAD, DINAME, 
            dict:new(), dict:new(), dict:new(), dict:new()),
    NKLINGONS = (tinymt32:uniform(25) * 2) + 10,
    DKLINGON = setupnklingons(NKLINGONS, dict:new()),
    {NKLINGONS, DSTAR, DINHABIT, DBASE, DHOLE, DKLINGON}.

setupgalaxypersect(-1, _QY, _LB, _LI, _DINAME, DS, DI, DB, DH) ->
    {DS, DI, DB, DH};
setupgalaxypersect(QX, -1, LB, LI, DINAME, DS, DI, DB, DH) ->
    setupgalaxypersect(QX - 1, ?NQUADS - 1, LB, LI, DINAME, DS, DI, DB, DH);
setupgalaxypersect(QX, QY, LB, LI, DINAME, DS, DI, DB, DH) ->
    QC = {QX, QY},
    SECT = initsect(),
    case lists:member(QC, LB) of
        true ->
            {SX, SY} = randsect(SECT),
            SECT2 = array:set(?SECTCOORD(SX, SY), s_base, SECT),
            DB2 = dict:append(QC, {SX, SY}, DB); % add attacked, etc
        false ->
            SECT2 = SECT,
            DB2 = DB
    end,
    case lists:member(QC, LI) of
        true ->
            {SX2, SY2} = randsect(SECT2),
            SECT3 = array:set(?SECTCOORD(SX2, SY2), s_inhabited, SECT2),
            SYSTEMNAME = dict:fetch(QC, DINAME),
            DI2 = dict:append(QC, {SX2, SY2, SYSTEMNAME}, DI); % add distressed, etc
        false ->
            SECT3 = SECT2,
            DI2 = DI
    end,
    NSTARS = tinymt32:uniform(9),
    {SECT4, STARLIST} = gensectlist(NSTARS, s_star, SECT3),
    DS2 = dict:append(QC, STARLIST, DS),
    NHOLES = tinymt32:uniform(3) - 1,
    {_SECT5, HOLELIST} = gensectlist(NHOLES, s_hole, SECT4),
    DH2 = dict:append(QC, HOLELIST, DH),
    setupgalaxypersect(QX, QY - 1, LB, LI, DINAME, DS2, DI2, DB2, DH2).

setupnklingons(0, DKQ) ->
    DKQ;
setupnklingons(NKALL, DKQ) ->
    N = tinymt32:uniform(4),
    case N > NKALL of
        true ->
            NKADD = NKALL;
        false ->
            NKADD = N
    end,
    QX = tinymt32:uniform(?NQUADS) - 1,
    QY = tinymt32:uniform(?NQUADS) - 1,
    QC = {QX, QY},
    case dict:is_key(QC, DKQ) of
        true ->
            NKOLD = dict:fetch(QC, DKQ);
        false ->
            NKOLD = 0
    end,
    NKNEW = NKOLD + NKADD,
    case NKNEW =< ?MAXKLQUAD of
        true ->
            DKQ2 = dict:store(QC, NKNEW, DKQ),
            NKALL2 = NKALL - NKNEW;
        false ->
            DKQ2 = DKQ,
            NKALL2 = NKALL
    end,
    setupnklingons(NKALL2, DKQ2).



