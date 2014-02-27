#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(tinymt32),
    code:load_file(erltrek_setup),
    {NK, DS, DI, DB, DH, DKQ} = erltrek_setup:setup_galaxy(),
    QC = hd(dict:fetch_keys(DB)),
    {SECT, DKS} = erltrek_setup:setup_sector(QC, DS, DI, DB, DH, DKQ).
