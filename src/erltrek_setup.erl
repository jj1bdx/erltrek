-module(erltrek_setup).

-include("erltrek.hrl").

-export([test/0, randsector/1]).

%% return empty {qx, qy, sx, sy} in the galaxy
randsector(_, C, true) ->
    C;
randsector(G, _, false) ->
    QX = tinymt32:uniform(?NQUADS) - 1,
    QY = tinymt32:uniform(?NQUADS) - 1,
    SX = tinymt32:uniform(?NSECTS) - 1,
    SY = tinymt32:uniform(?NSECTS) - 1,
    randsector(G, {QX, QY, SX, SY}, 
        array:get(?GALAXYCOORD(QX, QY, SX, SY), G) =:= empty).
    
randsector(G) ->
    randsector(G, {}, false).

%% test
test() -> 
    G = array:new(?NSECTS * ?NSECTS * ?NQUADS * ?NQUADS, 
                  {default, empty}),
    test(G, [], 100).

test(G, L, 0) ->
    {G, L};
test(G, L, N) ->
    C = randsector(G),
    {QX, QY, SX, SY} = C,
    G1 = array:set(?GALAXYCOORD(QX, QY, SX, SY), star, G),
    L2 = lists:append(L, [C]),
    test(G1, L2, N - 1).

