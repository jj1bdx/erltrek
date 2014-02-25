-module(erltrek_setup).

-include("erltrek.hrl").

-export([
        genquadlist/1,
        inhabitednames/0,
        initquad/0,
        initsect/0,
        randquad/1,
        randsector/1,
        setupgalaxy/0
        ]).

%% return empty {sx, sy} in the Sector array
randsector(S) ->
    randsector(S, {}, false).

randsector(_, SC, true) ->
    SC;
randsector(S, _, false) ->
    SX = tinymt32:uniform(?NSECTS) - 1,
    SY = tinymt32:uniform(?NSECTS) - 1,
    randsector(S, {SX, SY}, 
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
    setupgalaxy(?NSECTS - 1, ?NSECTS - 1,
        LBASEQUAD, LINHABITQUAD,
        dict:new(), dict:new(), dict:new(), dict:new(), dict:new()).

setupgalaxy(-1, -1, LB, LI, DS, DI, DB, DH, DKQ) ->
    {LB, LI, DS, DI, DB, DH, DKQ};
setupgalaxy(QX, -1, LB, LI, DS, DI, DB, DH, DKQ) ->
    setupgalaxy(QX - 1, ?NSECTS - 1, LB, LI, DS, DI, DB, DH, DKQ);
setupgalaxy(QX, QY, LB, LI, DS, DI, DB, DH, DKQ) ->
    true.
