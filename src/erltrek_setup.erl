-module(erltrek_setup).

-include("erltrek.hrl").

-export([test/0,
        randsector_quad/3]).

%% return empty {qx, qy, sx, sy} in the specified quadrant
randsector_quad(G, QX, QY) ->
    randsector_quad(G, QX, QY, {}, false).

randsector_quad(_, _, _, SC, true) ->
    SC;
randsector_quad(G, QX, QY, _, false) ->
    SX = tinymt32:uniform(?NSECTS) - 1,
    SY = tinymt32:uniform(?NSECTS) - 1,
    randsector_quad(G, QX, QY, {SX, SY}, 
        array:get(?GALAXYCOORD(QX, QY, SX, SY), G) =:= s_empty).

%% test
test() -> 
    G = array:new(?NSECTS * ?NSECTS * ?NQUADS * ?NQUADS, 
                  {default, s_empty}),
    test(G, [], 100).

test(G, L, 0) ->
    {G, L};
test(G, L, N) ->
    QX = tinymt32:uniform(?NQUADS) - 1,
    QY = tinymt32:uniform(?NQUADS) - 1,
    C = randsector_quad(G, QX, QY),
    {SX, SY} = C,
    G1 = array:set(?GALAXYCOORD(QX, QY, SX, SY), s_star, G),
    L2 = lists:append(L, [{QX, QY, SX, SY}]),
    test(G1, L2, N - 1).

