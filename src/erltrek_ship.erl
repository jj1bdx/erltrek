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


%%% --------------------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------------------

init([{ship, Ship}|Args]) ->
    case proplists:get_value(commander, Args) of
        undefined -> nop;
        Commander ->
            {ok, _} = Commander:start(self())
    end,
    #ship_def{ max_energy=E, max_shield=S } = Ship,
    {ok, #ship_state{ ship = Ship, energy = E, shield = S }}.

handle_call({command, Command}, _From, State0) ->
    {Reply, State} = handle_command(Command, State0),
    {reply, Reply, State};
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({Event, QC, SC}=Info, #ship_state{ tquad=TQC, tsect=TSC }=State)
  when Event == enter_sector; Event == enter_quadrant ->
    ok = erltrek_event:notify(Info),
    if SC == TSC andalso (QC == TQC orelse TQC == undefined) ->
            ok = erltrek_galaxy:impulse(0,0),
            ok = erltrek_event:notify(move_done);
       true -> nop
    end,
    {noreply, State};
handle_info({collision, _Object, _Info}=Event, State) ->
    %% todo: something ought to break when smashing into things..
    ok = erltrek_event:notify(Event),
    ok = erltrek_event:notify(move_done),
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
    notify_enterprise(Event, State),
    {noreply, absorb_hit(Level, State)};
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
    {erltrek_event:sync_notify({srscan, {Stardate, [State|Data]}}), State};
handle_command({lrscan}, State) ->
    {erltrek_event:sync_notify({lrscan, erltrek_galaxy:lrscan()}), State};
handle_command({impulse, SX, SY}, State) ->
    SC = #sectxy{ x=SX, y=SY },
    {erltrek_move:impulse(SC), State#ship_state{ tquad=undefined, tsect=SC }};
handle_command({impulse, QX, QY, SX, SY}, State) ->
    QC = #quadxy{ x=QX, y=QY },
    SC = #sectxy{ x=SX, y=SY },
    {erltrek_move:impulse(QC, SC), State#ship_state{ tquad=QC, tsect=SC}};
handle_command({phaser, SX, SY, Energy}, #ship_state{ energy=E }=State) ->
    %% TODO: Decide to fire or not here (no klingon, docked, etc.)
    %% klingons or no, I say fire any way.. (but not if docked, of course)
    if E > Energy ->
            {erltrek_phaser:phaser(SX, SY, Energy),
             State#ship_state{ energy = E - Energy }};
       true ->
            {not_enough_energy, State}
    end;
handle_command(Cmd, State) ->
    {{unknown_command, Cmd}, State}.

%% TODO: make this notify_commander instead, and add a enterprise commander that forwards the events..
notify_enterprise(Event, #ship_state{ ship=#ship_def{ class=s_enterprise }}) ->
    erltrek_event:notify(Event);
notify_enterprise(_, _) -> nop.

absorb_hit(Level, State) when Level =< 0 -> State;
absorb_hit(Level, #ship_state{ ship=#ship_def{ durability=D }, shield=S }=State)
  when S > 0 ->
    Shield0 = S - D(shield, Level),
    Shield = if Shield0 =< 0 ->
                     notify_enterprise(shields_gone, State),
                     0;
                true -> 
                     notify_enterprise({shield_level, Shield0}, State),
                     Shield0
             end,
    absorb_hit(D(body, Level - (S - Shield)), State#ship_state{ shield=Shield });
absorb_hit(Level, #ship_state{ energy=E }=State) when Level < E ->
    notify_enterprise({damage_level, Level}, State),
    State#ship_state{ energy=E - Level };
absorb_hit(_, _) -> exit(normal).
