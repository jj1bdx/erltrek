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

-spec command(pid(), tuple()) -> ok | tuple().
command(Ship, Command) ->
    gen_server:call(Ship, {command, Command}).


%% Commander API
status(Ship) ->
    gen_server:call(Ship, get_status).

count_nearby_enemies(Ship) ->
    gen_server:call(Ship, count_nearby_enemies).


%%% --------------------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------------------

init([{ship, Ship}|_Args]) ->
    erltrek_setup:seed(),
    Cmdr = case Ship#ship_def.commander of
               undefined ->
                   undefined;
               Commander ->
                   {ok, Pid} = Commander:start_link(self()),
                   Pid
           end,
    #ship_def{ max_energy=E, max_shield=S, initial_speed = IS } = Ship,
    {ok, #ship_state{ ship = Ship,
                      energy = E, shield = S, speed = IS,
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
handle_info(update_condition, State) ->
    {noreply, update_condition(State)};
handle_info(_Info, State) ->
    %% jj1bdx: how should these info messages be handled? Just ignored?
    %% kaos: Yes, I think so, but for now, lets print them so we spot
    %% early on if there are messages we miss, but ought to handle
    io:format("~p ~p: unhandled info: ~p~n", [self(), ?MODULE, _Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #ship_state{ commander=undefined }) ->
    ok;
terminate(Reason, #ship_state{ commander=Pid }) ->
    Pid ! {ship_destroyed, self(), Reason}.


%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%% Ship commands
handle_command(srscan, State) ->
    %% TODO: we can check for damaged scanner device here.. ;)
    Stardate = erltrek_galaxy:stardate(),
    Data = erltrek_galaxy:srscan(),
    {{ok, {Stardate, [State|Data]}}, State};
%%% --------------------------------------------------------------------
handle_command(lrscan, State) ->
    {{ok, erltrek_galaxy:lrscan()}, State};
%%% --------------------------------------------------------------------
handle_command({impulse, Course}, #ship_state{ speed=Speed }=State) ->
    %% Free move until told otherwise..
    {erltrek_galaxy:impulse(Course, Speed), State#ship_state{ tsect=undefined }};
handle_command({impulse, SX, SY}, #ship_state{docked = D} = State) ->
    if
        D ->
            {{move, no_move_while_docked}, State};
        true ->
            SC = #sectxy{ x=SX, y=SY },
            {impulse(SC, State), State#ship_state{ tquad=undefined, tsect=SC }}
    end;
handle_command({impulse, QX, QY, SX, SY}, #ship_state{docked = D} = State) ->
    if
        D ->
            {{move, no_move_while_docked}, State};
        true ->
            QC = #quadxy{ x=QX, y=QY },
            SC = #sectxy{ x=SX, y=SY },
            {impulse(QC, SC, State), State#ship_state{ tquad=QC, tsect=SC}}
    end;
%%% --------------------------------------------------------------------
handle_command(stop, State) ->
    {erltrek_galaxy:impulse(0,0), State};
%%% --------------------------------------------------------------------
handle_command({phaser, SX, SY, Energy},
        #ship_state{ energy=E, docked = D }=State) ->
    NKQ = erltrek_galaxy:count_nearby_enemies(),
    if
        D ->
           {{phaser, no_firing_when_docked}, State};
        NKQ == 0 ->
           {{phaser, no_klingon_in_quadrant}, State};
        E =< Energy ->
           {{phaser, not_enough_energy}, State};
        true ->
           {erltrek_phaser:phaser(SX, SY, Energy),
            State#ship_state{ energy = E - Energy }}
    end;
%%% --------------------------------------------------------------------
handle_command(dock, #ship_state{docked = Docked}=State) ->
    if Docked ->
           {{dock, already_docked}, State};
       true ->
           try_docking(State)
    end;
%%% --------------------------------------------------------------------
handle_command(undock, #ship_state{docked = Docked}=State) ->
    if not Docked ->
           {{undock, not_docked}, State};
       true ->
           {{undock, undock_complete}, State#ship_state{docked = false}}
    end;
%%% --------------------------------------------------------------------
handle_command(Cmd, State) ->
    {{unknown_command, Cmd}, State}.


%%% --------------------------------------------------------------------
%% Commander requests
handle_commander_request(get_status, State) ->
    {State, State};
%%% --------------------------------------------------------------------
handle_commander_request(count_nearby_enemies, State) ->
    {erltrek_galaxy:count_nearby_enemies(), State}.


%%% --------------------------------------------------------------------
%% Notify commander of ship events
notify(_Event, #ship_state{ commander=undefined }) -> ok;
notify(Event, #ship_state{ commander=Commander }) -> Commander ! {event, Event}, ok.


%%% --------------------------------------------------------------------
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

%%% --------------------------------------------------------------------
%% update ship condition flag
update_condition(#ship_state{ condition=Condition }=State) ->
    case get_condition(State) of
        Condition -> State;
        New ->
            ok = notify({condition, New}, State),
            State#ship_state{ condition = New }
    end.

get_condition(#ship_state{ docked=true }) -> cond_docked;
get_condition(#ship_state{ energy=E, ship=#ship_def{ max_energy=M }}) ->
    case erltrek_galaxy:count_nearby_enemies() of
        0 when E < M div 5 -> cond_yellow;
        0 -> cond_green;
        _ -> cond_red
    end.

%%% --------------------------------------------------------------------
%% try docking to the base
try_docking(
    #ship_state{ship = #ship_def{
            max_energy = Maxenergy, max_shield=Maxshield}} = State) ->
    {QC, SC} = erltrek_galaxy:get_position(),
    DB = erltrek_galaxy:bases(),
    case dict:is_key(QC, DB) of
        % starbase is in the quadrant
        true ->
            [TB] = dict:fetch(QC, DB),
            % if distance < sqrt(2) then dockable
            case erltrek_calc:sector_distance(SC, TB#base_info.xy) < 1.415 of
                true ->
                    {{dock, docking_complete},
                        State#ship_state{
                            docked = true,
                            % replenish energy and shield
                            energy = Maxenergy,
                            shield = Maxshield}};
                false ->
                    {{dock, base_not_adjacent}, State}
            end;
        false ->
            {{dock, base_not_in_quadrant}, State}
    end.

%%% --------------------------------------------------------------------
%% impulse move
impulse(DSC, State) ->
    %% erltrek_galaxy:get_position/0 must be called from a ship process
    {SQC, SSC} = erltrek_galaxy:get_position(),
    impulse(SQC, SSC, SQC, DSC, State).

impulse(DQC, DSC, State) ->
    %% erltrek_galaxy:get_position/0 must be called from a ship process
    {SQC, SSC} = erltrek_galaxy:get_position(),
    impulse(SQC, SSC, DQC, DSC, State).

impulse(SQC, SSC, DQC, DSC, State) ->
    case erltrek_calc:course_distance(SQC, SSC, DQC, DSC) of
        {ok, _Dx, _Dy, Course, Dist} ->
            if
                Dist > 0 ->
                    erltrek_galaxy:impulse(Course, State#ship_state.speed);
                true ->
                    %% zero distance = no move
                    {move, no_move_to_same_position}
            end;
        Error -> {move, Error}
    end.
