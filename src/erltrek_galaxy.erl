%%% -------------------------------------------------------------------
%%% Erltrek ("this software") is covered under the BSD 3-clause
%%% license.
%%%
%%% This product includes software developed by the University of
%%% California, Berkeley and its contributors.
%%%
%%% Copyright (c) 2014 Kenji Rikitake and Andreas Stenius.
%%% All rights reserved.
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
%%% * Neither the name of Kenji Rikitake, Andreas Stenius, k2r.org,
%%%   nor the names of its contributors may be used to endorse or
%%%   promote products derived from this software without specific
%%%   prior written permission.
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

-module(erltrek_galaxy).
-behaviour(gen_server).

%% API
-export([start_link/0, spawn_ship/1,
         stardate/0, my_data/0
        ]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-import(erltrek_calc, [index_quadxy/1, index_sectxy/1]).

-include("erltrek.hrl").

-record(state, {
          stardate=?INITTICK,
          sync,
          ships,
          galaxy,
          stars, inhabited, bases, holes
         }).


%%% --------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec spawn_ship(#ship_def{}) -> {ok, pid()}.
spawn_ship(Ship) when is_record(Ship, ship_def)->
    {ok, Pid} = erltrek_ship_sup:start_ship([Ship]),
    ok = gen_server:cast(?MODULE, {new_ship, Ship#ship_def.class, Pid}),
    {ok, Pid}.

stardate() ->
    gen_server:call(?MODULE, get_stardate).

my_data() ->
    gen_server:call(?MODULE, get_my_data).

%%% --------------------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------------------

init([]) ->
    {_NK, DS, DI, DB, DH, DKQ} = erltrek_setup:setup_galaxy(),
    {ok, #state{
            sync=os:timestamp(),
            ships=orddict:new(),
            galaxy=array:map(
                     fun (QI, _) ->
                             QC = erltrek_calc:index_quadxy(QI),
                             {SECT, DKS} = erltrek_setup:setup_sector(
                                             QC, DS, DI, DB, DH, DKQ),
                             spawn_klingons(DKS, QC, SECT)
                     end,
                     erltrek_setup:init_quad()),
            stars = DS,
            inhabited = DI,
            bases = DB,
            holes = DH
           }}.

handle_call(get_stardate, _From, State) ->
    {reply, get_stardate(State), State};
handle_call(get_my_data, {Pid, _Ref}, #state{ ships=Ships }=State) ->
    case orddict:find(Pid, Ships) of
        {ok, #ship_data{ quad=QC }=Data} ->
            {reply,
             [Data,
              {quad, get_quad(QC, State)},
              {klingons, orddict:fold(
                          fun (_, #ship_data{ class=s_klingon }, Count) ->
                                  Count + 1;
                              (_, _, Count) ->
                                  Count
                          end, 0, Ships)}
              |case dict:find(QC, State#state.inhabited) of
                   {ok, Value} -> Value;
                   _ -> []
               end],
             State};
        _ ->
            {reply, [], State}
    end;
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({new_ship, Class, Pid}, State0) ->
    {QC, SC, State} = place_object({Class, Pid}, State0),
    handle_info({register_ship, Pid,
                 #ship_data{ class=Class, quad=QC, sect=SC }},
                State);
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({register_ship, Pid, Ship}, #state{ ships=Ships }=State) ->
    monitor(process, Pid),
    {noreply, State#state{ ships=orddict:store(Pid, Ship, Ships) }};
handle_info({'DOWN', _Ref, process, Pid, _Info}, #state{ ships=Ships }=State0) ->
    State = case orddict:find(Pid, Ships) of
                {ok, #ship_data{ class=Class, quad=QC, sect=SC }} ->
                    erltrek_event:notify({killed, Class, QC, SC}),
                    update_sector(QC, SC, s_empty, State0);
                _ -> State0
            end,
    {noreply, State#state{ ships=orddict:erase(Pid, Ships) }};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

quadxy_index(QI) when is_integer(QI) -> QI rem (?NQUADS * ?NQUADS);
quadxy_index(QC) -> erltrek_calc:quadxy_index(QC).

sectxy_index(SI) when is_integer(SI) -> SI rem (?NSECTS * ?NSECTS);
sectxy_index(SC) -> erltrek_calc:sectxy_index(SC).

get_quad(QC, #state{ galaxy=G }) ->
    array:get(quadxy_index(QC), G).

set_quad(QC, SECT, #state{ galaxy=G }=State) ->
    State#state{ galaxy=array:set(quadxy_index(QC), SECT, G) }.

update_sector(QC, SC, Value, State) ->
    set_quad(QC, update_sector(SC, Value, get_quad(QC, State)), State).

update_sector(SC, Value, SECT) ->
    array:set(sectxy_index(SC), Value, SECT).

lookup_sector(SC, SECT) ->
    array:get(sectxy_index(SC), SECT).

spawn_klingons(DKS, QC, SECT0) ->
    dict:fold(
      fun (SC, [#klingon_status{}], SECT) ->
              ShipDef = ?klingon_ship,
              {ok, Ship} = erltrek_ship_sup:start_ship(
                             [ShipDef,
                              [{commander, erltrek_klingon_commander}]]),
              self() ! {register_ship, Ship,
                        #ship_data{
                           class=ShipDef#ship_def.class,
                           quad=QC, sect=SC }},
              update_sector(SC, {s_klingon, Ship}, SECT)
      end, SECT0, DKS).

place_object(Object, State) ->
    {QI, SI} = find_empty_sector(State),
    {index_quadxy(QI), index_sectxy(SI),
     update_sector(QI, SI, Object, State)}.

find_empty_sector(#state{ galaxy=G }=State) ->
    S = array:size(G),
    %% make sure we eventually have searched all sectors before giving up
    %% by starting somewhere in 2*NQUADS..NQUADS, and downto 0.
    QI = tinymt32:uniform(S) + S,
    find_empty_sector(QI, State).

find_empty_sector(QI, State) ->
    SECT = get_quad(QI, State),
    SI = tinymt32:uniform(array:size(SECT)),
    find_empty_sector(QI, SI, SECT, State).

find_empty_sector(QI, SI, SECT, State) ->
    case lookup_sector(SI, SECT) of
        s_empty -> {QI, SI};
        _ when SI > 0 ->
            find_empty_sector(QI, SI - 1, SECT, State);
        _ when QI > 0 ->
            find_empty_sector(QI - 1, State);
        _ ->
            no_empty_sector_found
    end.

get_stardate(#state{ stardate=SD, sync=Sync }) ->
    SD + elapsed(os:timestamp(), Sync).

elapsed({M1, S1, _}, {M2, S2, _}) ->
    (M1 - M2) * 1000000 + (S1 - S2).
