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

-module(erltrek_ship).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, command/2]).

%% Commander API
-export([count_nearby_enemies/1, status/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("erltrek.hrl").


%%% --------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------

-spec start_link(#ship_def{}) -> {ok, pid()} | ignore | {error, term()}.

start_link(Ship)
  when is_record(Ship, ship_def) ->
    start_link(Ship, []).


-spec start_link(#ship_def{}, list()) -> {ok, pid()} | ignore | {error, term()}.

start_link(Ship, Args)
  when is_record(Ship, ship_def), is_list(Args) ->
    gen_server:start_link(?MODULE, [{ship, Ship}|Args], []).


command(Ship, Command) ->
    gen_server:call(Ship, {command, Command}).


%% Commander api
status(Ship) ->
    gen_server:call(Ship, get_status).

count_nearby_enemies(Ship) ->
    gen_server:call(Ship, count_nearby_enemies).


%%% --------------------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------------------

init([{ship, Ship}|_Args]) ->
    Cmdr = case Ship#ship_def.commander of
               undefined ->
                   undefined;
               Commander ->
                   {ok, Pid} = Commander:start(self()),
                   Pid
           end,
    #ship_def{ max_energy=E, max_shield=S } = Ship,
    {ok, #ship_state{ ship = Ship, energy = E, shield = S,
                      commander = Cmdr }}.

handle_call({command, Command}, _From, State0) ->
    {Reply, State} = handle_command(Command, State0),
    {reply, Reply, State};
handle_call(Command, {Pid, _Ref}, #ship_state{ commander=Pid }=State0) ->
    {Reply, State} = handle_commander_request(Command, State0),
    {reply, Reply, State};
handle_call(_Call, _From, State) ->
    {reply, not_commander, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({Event, QC, SC}=Info, #ship_state{ tquad=TQC, tsect=TSC }=State)
  when Event == enter_sector; Event == enter_quadrant ->
    ok = notify(Info, State),
    if SC == TSC andalso (QC == TQC orelse TQC == undefined) ->
            ok = erltrek_galaxy:impulse(0,0),
            ok = notify(move_done, State);
       true -> nop
    end,
    {noreply, State};
handle_info({collision, _Object, _Info}=Event, State) ->
    %% todo: something ought to break when smashing into things..
    ok = notify(Event, State),
    ok = notify(move_done, State),
    {noreply, State};
handle_info({distance_traveled, Dist},
            #ship_state{ ship=#ship_def{ engine_cost=C },
                         energy=E }=State)
  when E > (Dist * C) ->
    %% TODO: check energy level and stop if we're getting dangeoursly close to empty
    {noreply, State#ship_state{ energy = trunc(E - (Dist * C)) }};
handle_info({distance_traveled, _}, State) ->
    %% TODO: Notification needed to tell killed by energy exhaustion
    {stop, normal, State#ship_state{energy = 0}};
handle_info({phaser_hit, Level, _Info}=Event, State) ->
    ok = notify(Event, State),
    {noreply, absorb_hit(Level, State)};
handle_info({update_condition}, State) ->
    {noreply, update_condition(State)};
handle_info(_Info, State) ->
    %% jj1bdx: how should these info messages be handled? Just ignored?
    %% kaos: Yes, I think so, but for now, lets print them so we spot
    %% early on if there are messages we miss, but ought to handle
    io:format("~p ~p: unhandled info: ~p~n", [self(), ?MODULE, _Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

handle_command({srscan}, State) ->
    %% TODO: we can check for damaged scanner device here.. ;)

    %% collect data (i.e. perform the scan) here, now, then send that
    %% off as a short range scan result for output.

    Stardate = erltrek_galaxy:stardate(),
    Data = erltrek_galaxy:srscan(),

    %% Using sync notify so the output is presented before returning.
    %% But for this to work, no event handler (directly or indirectly)
    %% may call into any of the processes in our call chain!
    {sync_notify({srscan, {Stardate, [State|Data]}}, State), State};
handle_command({lrscan}, State) ->
    {sync_notify({lrscan, erltrek_galaxy:lrscan()}, State), State};
handle_command({impulse, Course}, State) ->
    %% TODO: get speed from somewhere
    %% Free move until told otherwise..
    {erltrek_galaxy:impulse(Course, 1.5), State#ship_state{ tsect=undefined }};
handle_command({impulse, SX, SY}, State) ->
    SC = #sectxy{ x=SX, y=SY },
    {erltrek_move:impulse(SC), State#ship_state{ tquad=undefined, tsect=SC }};
handle_command({impulse, QX, QY, SX, SY}, State) ->
    QC = #quadxy{ x=QX, y=QY },
    SC = #sectxy{ x=SX, y=SY },
    {erltrek_move:impulse(QC, SC), State#ship_state{ tquad=QC, tsect=SC}};
handle_command(stop, State) ->
    {erltrek_galaxy:impulse(0,0), State};
handle_command({phaser, SX, SY, Energy}, #ship_state{ energy=E }=State) ->
    %% TODO: No firing when docked
    NKQ = erltrek_galaxy:count_nearby_enemies(),
    if NKQ == 0 ->
            {no_klingon_in_quadrant, State};
       E =< Energy ->
            {not_enough_energy, State};
       true ->
            {erltrek_phaser:phaser(SX, SY, Energy),
             State#ship_state{ energy = E - Energy }}
    end;
handle_command(Cmd, State) ->
    {{unknown_command, Cmd}, State}.


handle_commander_request(get_status, State) ->
    {State, State};
handle_commander_request(count_nearby_enemies, State) ->
    {erltrek_galaxy:count_nearby_enemies(), State}.


%% Notify commander of ship events
notify(_Event, #ship_state{ commander=undefined }) -> ok;
notify(Event, #ship_state{ commander=Commander }) -> Commander ! {event, Event}, ok.

sync_notify(_Event, #ship_state{ commander=undefined }) -> ok;
sync_notify(Event, #ship_state{ commander=Commander }) ->
    Ref = make_ref(),
    Commander ! {sync_event, {self(), Ref}, Event},
    receive {Ref, Rsp} -> Rsp end.

%% take damage from enemy attack
absorb_hit(Level, State) when Level =< 0 -> State;
absorb_hit(Level, #ship_state{ ship=#ship_def{ durability=D }, shield=S }=State)
  when S > 0 ->
    Shield0 = S - D(shield, Level),
    Shield = if Shield0 =< 0 ->
                     ok = notify(shields_gone, State),
                     0;
                true ->
                     ok = notify({shield_level, Shield0}, State),
                     Shield0
             end,
    absorb_hit(D(body, Level - (S - Shield)), State#ship_state{ shield=Shield });
absorb_hit(Level, #ship_state{ energy=E }=State) when Level < E ->
    ok = notify({damage_level, Level}, State),
    State#ship_state{ energy=E - Level };
absorb_hit(_, _) -> exit(normal).

%% update ship condition flag
update_condition(
    #ship_state{ship = #ship_def{max_energy = Maxenergy},
        condition = Old} = State) ->
    % update ship condition
    New = case {
        erltrek_galaxy:count_nearby_enemies() > 0,
        State#ship_state.energy < Maxenergy div 5,
        State#ship_state.docked} of
            {_, _, true} -> cond_docked;
            {false, true, false} -> cond_yellow;
            {true, _, false} -> cond_red;
            {false, false, false} -> cond_green
    end,
    if
        Old =/= New ->
            ok = notify({condition, New}, State),
            State#ship_state{condition = New};
        true ->
            State
    end.
