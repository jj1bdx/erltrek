#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(tinymt32),
    code:load_file(erltrek_setup),
    code:load_file(erltrek_scan),
    {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} = erltrek_setup:setup_state(),
    erltrek_scan:srscan(10, SHIP, SECT, DI).
