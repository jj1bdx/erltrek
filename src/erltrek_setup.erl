-module(erltrek_setup).

-include("erltrek.hrl").

-export([
        genquadlist/1,
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
    setupgalaxy(?NSECTS - 1, ?NSECTS - 1,
        LBASEQUAD, LINHABITQUAD, DINAME,
        dict:new(), dict:new(), dict:new(), dict:new(), dict:new()).

setupgalaxy(-1, -1, LB, LI, DINAME, DS, DI, DB, DH, DKQ) ->
    {LB, LI, DINAME, DS, DI, DB, DH, DKQ};
setupgalaxy(QX, -1, LB, LI, DINAME, DS, DI, DB, DH, DKQ) ->
    setupgalaxy(QX - 1, ?NSECTS - 1, DINAME, LB, LI, DS, DI, DB, DH, DKQ);
setupgalaxy(QX, QY, LB, LI, DINAME, DS, DI, DB, DH, DKQ) ->
    QC = {QX, QY},
    SECT = initsect(),
    case lists:member(QC, LB) of
        true ->
            {SX, SY} = randsect(SECT),
            SECT2 = array:set(?SECTCOORD(SX, SY), s_base, SECT),
            DB2 = dict:append(QC, {SX, SY}, DB); % add attacked, etc
        _Else ->
            SECT2 = SECT,
            DB2 = DB
    end,
    case lists:member(QC, LI) of
        true ->
            {SX, SY} = randsect(SECT2),
            SECT3 = array:set(?SECTCOORD(SX, SY), s_inhabited, SECT2),
            SYSTEMNAME = dict:fetch(QC, DINAME),
            DI2 = dict:append(QC, {SX, SY, SYSTEMNAME}, DI); % add distressed, etc
        _Else ->
            SECT3 = SECT2,
            DI2 = DI
    end,
    true. % more to go
