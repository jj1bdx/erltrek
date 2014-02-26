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
        setup_galaxy/0
        ]).

%% return empty {sx, sy} in the Sector array
rand_sect(S) ->
    rand_sect(S, {}, false).

rand_sect(_, SC, true) ->
    SC;
rand_sect(S, _, false) ->
    SX = tinymt32:uniform(?NSECTS) - 1,
    SY = tinymt32:uniform(?NSECTS) - 1,
    rand_sect(S, {SX, SY}, 
        array:get(?SECTCOORD(SX, SY), S) =:= s_empty).

%% return empty {qx, qy} in the Quadrant array
rand_quad(Q) ->
    rand_quad(Q, {}, false).

rand_quad(_, QC, true) ->
    QC;
rand_quad(Q, _, false) ->
    QX = tinymt32:uniform(?NQUADS) - 1,
    QY = tinymt32:uniform(?NQUADS) - 1,
    rand_quad(Q, {QX, QY}, 
        array:get(?QUADCOORD(QX, QY), Q) =:= q_empty).

%% init Sector array
init_sect() ->
    array:new(?NSECTS * ?NSECTS, {default, s_empty}).

%% init Quadrant array
init_quad() ->
    array:new(?NQUADS * ?NQUADS, {default, q_empty}).

%% Generate a list of random quadrant coordinates without duplicates
gen_quad_list(N) ->
    gen_quad_list(N, [], init_quad()).

gen_quad_list(0, L, _) ->
    L;
gen_quad_list(N, L, A) ->
    QC = rand_quad(A),
    {QX, QY} = QC,
    A2 = array:set(?QUADCOORD(QX, QY), q_fill, A),
    gen_quad_list(N - 1, [QC|L], A2).

%%% Generate a list of random sector coordinates without duplicates
%%% with sector state input and output
gen_sect_list(N, ENT, SECT) ->
    gen_sect_list(N, ENT, [], SECT).

gen_sect_list(0, _, L, SECT) ->
    {SECT, L};
gen_sect_list(N, ENT, L, A) ->
    SC = rand_sect(A),
    {SX, SY} = SC,
    A2 = array:set(?SECTCOORD(SX, SY), ENT, A),
    gen_sect_list(N - 1, ENT, [SC|L], A2).

%% List of names of inhabited stars
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

%% Setup stars (returns dict)
setup_galaxy() ->
    NBASES = tinymt32:uniform(?MAXBASES - 3) + 3,
    LBASEQUAD = gen_quad_list(NBASES),
    LINHABITNAME = inhabited_names(),
    LINHABITQUAD = gen_quad_list(length(LINHABITNAME)),
    DINAME = dict:from_list(lists:zip(LINHABITQUAD, LINHABITNAME)),
    {DSTAR, DINHABIT, DBASE, DHOLE} =
        setup_galaxy_sector(?NQUADS - 1, ?NQUADS - 1,
            LBASEQUAD, LINHABITQUAD, DINAME, 
            dict:new(), dict:new(), dict:new(), dict:new()),
    NKLINGONS = (tinymt32:uniform(25) * 2) + 10,
    DKLINGON = setup_klingon_numbers(NKLINGONS, dict:new()),
    {NKLINGONS, DSTAR, DINHABIT, DBASE, DHOLE, DKLINGON}.

setup_galaxy_sector(-1, _QY, _LB, _LI, _DINAME, DS, DI, DB, DH) ->
    {DS, DI, DB, DH};
setup_galaxy_sector(QX, -1, LB, LI, DINAME, DS, DI, DB, DH) ->
    setup_galaxy_sector(QX - 1, ?NQUADS - 1, LB, LI, DINAME, DS, DI, DB, DH);
setup_galaxy_sector(QX, QY, LB, LI, DINAME, DS, DI, DB, DH) ->
    QC = {QX, QY},
    SECT = init_sect(),
    case lists:member(QC, LB) of
        true ->
            {SX, SY} = rand_sect(SECT),
            SECT2 = array:set(?SECTCOORD(SX, SY), s_base, SECT),
            DB2 = dict:append(QC, {SX, SY}, DB); % add attacked, etc
        false ->
            SECT2 = SECT,
            DB2 = DB
    end,
    case lists:member(QC, LI) of
        true ->
            {SX2, SY2} = rand_sect(SECT2),
            SECT3 = array:set(?SECTCOORD(SX2, SY2), s_inhabited, SECT2),
            SYSTEMNAME = dict:fetch(QC, DINAME),
            DI2 = dict:append(QC, {SX2, SY2, SYSTEMNAME}, DI); % add distressed, etc
        false ->
            SECT3 = SECT2,
            DI2 = DI
    end,
    NSTARS = tinymt32:uniform(9),
    {SECT4, STARLIST} = gen_sect_list(NSTARS, s_star, SECT3),
    DS2 = dict:append(QC, STARLIST, DS),
    NHOLES = tinymt32:uniform(3) - 1,
    {_SECT5, HOLELIST} = gen_sect_list(NHOLES, s_hole, SECT4),
    DH2 = dict:append(QC, HOLELIST, DH),
    setup_galaxy_sector(QX, QY - 1, LB, LI, DINAME, DS2, DI2, DB2, DH2).

setup_klingon_numbers(0, DKQ) ->
    DKQ;
setup_klingon_numbers(NKALL, DKQ) ->
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
    setup_klingon_numbers(NKALL2, DKQ2).



