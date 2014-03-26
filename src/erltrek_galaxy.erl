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
         stardate/0, get_position/0,
         srscan/0, lrscan/0,
         impulse/2, phaser/2,
         count_nearby_enemies/0, bases/0
        ]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-import(erltrek_calc, [index_quadxy/1, index_sectxy/1]).

-include("erltrek.hrl").

-define(GALAXY_TICK, 500).

-record(state, {
          stardate=?INITTICK,
          sync :: os:timestamp(),
          ships,
          galaxy,
          stars, inhabited, bases, holes
         }).


%%% --------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec spawn_ship(#ship_def{}) -> {ok, pid()}.
spawn_ship(Ship) when is_record(Ship, ship_def)->
    {ok, Pid} = erltrek_ship_sup:start_ship([Ship]),
    ok = gen_server:cast(?MODULE, {new_ship, Ship#ship_def.class, Pid}),
    {ok, Pid}.

-spec get_position() -> {reply, term(), #state{}}.
get_position() -> call(get_position).

-spec stardate() -> {reply, term(), #state{}}.
stardate() -> call(get_stardate).

-spec srscan() -> {reply, term(), #state{}}.
srscan() -> call(srscan).

-spec lrscan() -> {reply, term(), #state{}}.
lrscan() -> call(lrscan).

-spec impulse(non_neg_integer(), non_neg_integer()) -> {reply, term(), #state{}}.
impulse(Course, Speed) -> call({impulse, Course, Speed}).

-spec phaser(non_neg_integer(), non_neg_integer()) -> {reply, term(), #state{}}.
phaser(Course, Energy) -> call({phaser, Course, Energy}).

-spec count_nearby_enemies() -> {reply, term(), #state{}}.
count_nearby_enemies() -> call(count_nearby_enemies).

-spec bases() -> {reply, term(), #state{}}.
bases() -> call(get_bases).


%%% --------------------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------------------

-spec init([]) -> {ok, #state{}}.

init([]) ->
    erltrek_setup:seed(),
    {_NK, DS, DI, DB, DH, DKQ} = erltrek_setup:setup_galaxy(),
    erlang:send_after(?GALAXY_TICK, self(), tick),
    {ok, #state{
            sync=os:timestamp(),
            ships=orddict:new(),
            galaxy=array:map(
                     fun (QI, _) ->
                             QC = erltrek_calc:index_quadxy(QI),
                             {SECT, LKS} = erltrek_setup:setup_sector(
                                             QC, DS, DI, DB, DH, DKQ),
                             spawn_klingons(LKS, QC, SECT)
                     end,
                     erltrek_setup:init_quad()),
            stars = DS,
            inhabited = DI,
            bases = DB,
            holes = DH
           }}.

-spec handle_call(term(), term(), #state{}) -> {reply, term(), #state{}}.

handle_call(get_stardate, _From, State) ->
    {reply, trunc(State#state.stardate), State};
handle_call(get_position, {Pid, _Ref}, State) ->
    case find_ship(Pid, State) of
        {ok, #ship_data{ quad=QC, sect=SC }} ->
            {reply, {QC, SC}, State};
        _ ->
            {reply, error, State}
    end;
handle_call(srscan, {Pid, _Ref}, State) ->
    case find_ship(Pid, State) of
        {ok, Data} ->
            {reply, srscan(Data, State), State};
        _ ->
            {reply, [], State}
    end;
handle_call(lrscan, {Pid, _Ref}, State) ->
    case find_ship(Pid, State) of
        {ok, Data} ->
            {reply, lrscan(Data, State), State};
        _ ->
            {reply, [], State}
    end;
handle_call({impulse, Course, Speed}, {Pid, _Ref}, State) ->
    case find_ship(Pid, State) of
        {ok, Data} ->
            {reply, ok,
             store_ship(
               Pid, set_ship_vector(Course, Speed, Data),
               State)};
        _ ->
            {reply, error, State}
    end;
handle_call({phaser, Course, Energy}, {Pid, _Ref}, State) ->
    case find_ship(Pid, State) of
        {ok, Data} ->
            {reply,
             {phaser_hit, phaser(Course, Energy, Data, State)},
             State};
        _ ->
            {reply, error, State}
    end;
handle_call(count_nearby_enemies, {Pid, _Ref}, State) ->
    case find_ship(Pid, State) of
        {ok, #ship_data{ class=Class, quad=QC }} ->
            {reply, count_other_ships(Class, get_quad(QC, State)), State};
        _ ->
            {reply, error, State}
    end;
handle_call(get_bases, _From, State) ->
    %% TODO: should non-Enterprise ship have this info?
    {reply, State#state.bases, State};
handle_call(Call, _From, State) ->
    {reply, {error, {unknown_call, Call}}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({new_ship, Class, Pid}, State0) ->
    {QC, SC, State} = place_object({Class, Pid}, State0),
    handle_info({register_ship, Pid,
                 #ship_data{
                    class=Class,
                    pos=erltrek_calc:quadsect_to_galaxy({QC, SC})
                   }},
                State);
handle_cast(_Cast, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(tick, State) ->
    {noreply, tick(State)};
handle_info({register_ship, Ship, Data0}, State) ->
    monitor(process, Ship),
    {_, Data} = update_ship_pos(Data0),
    {noreply, store_ship(Ship, Data, State)};
handle_info({'DOWN', _Ref, process, Pid, _Info}, State0) ->
    State = case find_ship(Pid, State0) of
                {ok, #ship_data{ class=Class, quad=QC, sect=SC }} ->
                    erltrek_event:notify({killed, Class, QC, SC}),
                    erase_ship(Pid, update_sector(QC, SC, s_empty, State0));
                _ -> State0
            end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

-spec call(term()) -> {reply, term(), #state{}}.
call(Msg) -> gen_server:call(?MODULE, Msg).

-spec quadxy_index(non_neg_integer() | #quadxy{}) -> non_neg_integer().
quadxy_index(QI) when is_integer(QI) -> QI rem (?NQUADS * ?NQUADS);
quadxy_index(QC) -> erltrek_calc:quadxy_index(QC).

-spec sectxy_index(non_neg_integer() | #sectxy{}) -> non_neg_integer().
sectxy_index(SI) when is_integer(SI) -> SI rem (?NSECTS * ?NSECTS);
sectxy_index(SC) -> erltrek_calc:sectxy_index(SC).

-spec get_quad(#quadxy{}, #state{}) -> array().
get_quad(QC, #state{ galaxy=G }) ->
    array:get(quadxy_index(QC), G).

-spec set_quad(#quadxy{}, array(), #state{}) -> #state{}.
set_quad(QC, Quad, #state{ galaxy=G }=State) ->
    State#state{ galaxy=array:set(quadxy_index(QC), Quad, G) }.

-spec update_sector(#quadxy{}, #sectxy{}, sector_entity(), #state{}) -> #state{}.
update_sector(QC, SC, Value, State) ->
    set_quad(QC, update_sector(SC, Value, get_quad(QC, State)), State).

-spec update_sector(#sectxy{},
    sector_entity() | {s_klingon, undefined | pid()}, array()) -> array().
update_sector(SC, Value, Quad) ->
    array:set(sectxy_index(SC), Value, Quad).

-spec lookup_sector(#sectxy{}, array()) -> sector_entity().
lookup_sector(SC, Quad) ->
    array:get(sectxy_index(SC), Quad).

-spec lookup_sector(#quadxy{}, #sectxy{}, #state{}) -> sector_entity().
lookup_sector(QC, SC, State) ->
    lookup_sector(SC, get_quad(QC, State)).

-spec spawn_klingons([#sectxy{}], #quadxy{}, array()) -> array().
spawn_klingons(LKS, QC, SECT0) ->
    lists:foldl(
      fun (SC, SECT) ->
              ShipDef = ?klingon_ship,
              {ok, Ship} = erltrek_ship_sup:start_ship([ShipDef]),
              self() ! {register_ship, Ship,
                        #ship_data{
                           class=ShipDef#ship_def.class,
                           pos=erltrek_calc:quadsect_to_galaxy({QC, SC})
                          }},
              update_sector(SC, {s_klingon, Ship}, SECT)
      end, SECT0, LKS).

-spec place_object(sector_entity() | {ship_class, pid()}, #state{}) ->
    {#quadxy{}, #sectxy{}, #state{}}.
place_object(Object, State) ->
    {QI, SI} = find_empty_sector(State),
    {index_quadxy(QI), index_sectxy(SI),
     update_sector(QI, SI, Object, State)}.

-spec find_empty_sector(#state{}) -> {non_neg_integer(), non_neg_integer()}.
find_empty_sector(#state{ galaxy=G }=State) ->
    S = array:size(G),
    %% make sure we eventually have searched all sectors before giving up
    %% by starting somewhere in 2*NQUADS..NQUADS, and downto 0.
    QI = tinymt32:uniform(S) + S,
    find_empty_sector(QI, State).

-spec find_empty_sector(non_neg_integer(), #state{}) ->
    {non_neg_integer(), non_neg_integer()}.
find_empty_sector(QI, State) ->
    SECT = get_quad(index_quadxy(QI), State),
    SI = tinymt32:uniform(array:size(SECT)) - 1,
    find_empty_sector(QI, SI, SECT, State).

-spec find_empty_sector(non_neg_integer(), non_neg_integer(), array(), #state{}) ->
    {non_neg_integer(), non_neg_integer()}.
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

find_ship(Ship, #state{ ships=Ships }) ->
    orddict:find(Ship, Ships).

store_ship(Ship, Data, #state{ ships=Ships }=State) ->
    State#state{ ships=orddict:store(Ship, Data, Ships) }.

erase_ship(Ship, #state{ ships=Ships }=State) ->
    State#state{ ships=orddict:erase(Ship, Ships) }.

set_ship_vector(Course, Speed, Data) ->
    Data#ship_data{ course=Course / 180 * math:pi(), speed=Speed }.

update_ship_pos(#ship_data{ pos=GC, quad=QC, sect=SC }=Data) ->
    case erltrek_calc:galaxy_to_quadsect(GC) of
        {QC, SC} -> Data;
        {QC, NSC} ->
            {{enter_sector, QC, NSC},
             Data#ship_data{ sect=NSC }};
        {NQC, NSC} ->
            {{enter_quadrant, NQC, NSC},
             Data#ship_data{ quad=NQC, sect=NSC }}
    end.

update_ship_pos(Ship, Data0, State) ->
    case update_ship_pos(Data0) of
        Data0 -> store_ship(Ship, Data0, State); %% moved just a fraction within current sector
        {Event, #ship_data{ quad=QC, sect=SC }=Data} ->
            case lookup_sector(QC, SC, State) of
                s_empty ->
                    Ship ! Event,
                    #ship_data{ quad=SQC, sect=SSC } = Data0,
                    #ship_data{ quad=DQC, sect=DSC } = Data,
                    Ship ! {distance_traveled,
                            erltrek_calc:sector_distance(
                              erltrek_calc:quadsect_to_galaxy({SQC, SSC}),
                              erltrek_calc:quadsect_to_galaxy({DQC, DSC}))},
                    store_ship(
                      Ship, Data,
                      move_object(SQC, SSC, DQC, DSC, State));
                Object ->
                    Ship ! {collision,
                            if is_tuple(Object) -> element(1, Object);
                               true -> Object
                            end,
                            Event},
                    %% stop ship dead in its tracks..
                    #ship_data{ quad=SQC, sect=SSC } = Data0,
                    GC = erltrek_calc:quadsect_to_galaxy({SQC, SSC}),
                    store_ship(Ship, Data0#ship_data{ pos=GC, speed=0 }, State)
            end
    end.

move_object(SQC, SSC, DQC, DSC, State) ->
    Quad = get_quad(SQC, State),
    Object = lookup_sector(SSC, Quad),
    update_sector(
      DQC, DSC, Object,
      set_quad(
        SQC,
        update_sector(SSC, s_empty, Quad),
        State)).

move_ships(Delta, #state{ ships=Ships }=State0) ->
    Moved = orddict:fold(
              fun (_, #ship_data{ speed=0 }, Acc) -> Acc;
                  (Ship, #ship_data{
                            pos=#galaxy{ x=GX, y=GY },
                            speed=Speed, course=Course }=Data,
                   Acc) ->
                      Dist = Speed * Delta,
                      DX = Dist * -math:cos(Course),
                      DY = Dist * math:sin(Course),
                      [{Ship, Data#ship_data{ pos=#galaxy{ x=GX + DX, y=GY + DY }}}|Acc]
              end, [], Ships),
    lists:foldl(
      fun ({Ship, Data}, State) ->
              update_ship_pos(Ship, Data, State)
      end, State0, Moved).

elapsed({M1, S1, U1}, {M2, S2, U2}) ->
    ((M1 - M2) * 1000000) + (S1 - S2) + ((U1 - U2) / 1000000).

tick(#state{ stardate=Stardate, sync=Sync }=State0) ->
    Timestamp = os:timestamp(),
    Tick = elapsed(Timestamp, Sync),
    State = move_ships(Tick, State0),
    erlang:send_after(?GALAXY_TICK, self(), tick),
    State#state{ stardate=Stardate + Tick, sync=Timestamp }.

count_other_ships(Class, #state{ ships=Ships }) ->
    orddict:fold(
      fun (_, #ship_data{ class=SClass }, Count)
            when SClass =/= Class -> Count + 1;
          (_, _, Count) -> Count
      end, 0, Ships);
count_other_ships(Class, Quad) ->
    array:foldl(
      fun (_, {SClass, _}, Count)
            when SClass =/= Class -> Count + 1;
          (_, _, Count) -> Count
      end, 0, Quad).

srscan(#ship_data{ class=Class, quad=QC }=Data, State) ->
    [Data,
     {quad, get_quad(QC, State)},
     {enemies, count_other_ships(Class, State)}
     |case dict:find(QC, State#state.inhabited) of
          {ok, Value} -> Value;
          _ -> []
      end].

count_objects(QC, D) ->
    case dict:find(QC, D) of
        {ok, Value} -> length(Value);
        error -> 0
    end.

lrscan(#ship_data{ class=Class, quad=#quadxy{ x=QX, y=QY } }=Data, State) ->
    [Data
     | [lrscan(Class, #quadxy{ x=X, y=Y }, State)
        || X <- lists:seq(QX - 1, QX + 1),
           Y <- lists:seq(QY - 1, QY + 1)
       ]
    ].

lrscan(Class, QC, #state{ stars=DS, inhabited=DI, bases=DB }=State) ->
    case erltrek_calc:in_quadxy(QC) of
        true ->
            {QC,
             [{stars, count_objects(QC, DS) + count_objects(QC, DI)},
              {bases, count_objects(QC, DB)},
              {enemies, count_other_ships(Class, get_quad(QC, State))}
             ]};
        false ->
            {QC, negative_energy_barrier}
    end.

phaser(Course, Energy, #ship_data{ class=SClass, quad=QC, sect=SC }, State) ->
    SI = erltrek_calc:sectxy_index(SC),
    array:foldl(
      fun (TSI, _, Acc) when TSI == SI -> Acc;
          (TSI, {Class, Ship}, Acc) ->
              TSC = erltrek_calc:index_sectxy(TSI),
              {Angle, Dist} = erltrek_calc:sector_course_distance(SC, TSC),
              %% TODO: I think we should only consider ships within the phaser beam..
              Level = Energy * math:pow(0.9, Dist)
                  * math:exp(-0.7 * abs((Angle - Course)/10)),
              Hit = trunc(Level),
              %% notify attacked ship about damage and attacker
              Ship ! {phaser_hit, Hit, {SClass, SC}},
              %% save hit data for result
              [{TSC, Class, Hit}|Acc];
          (_, _, Acc) -> Acc
      end, [], get_quad(QC, State)).
