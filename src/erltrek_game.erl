-module(erltrek_game).
-behaviour(gen_server).

-export([
         handle_info/2,
         init/1,
         start_link/0,
         start_link/1
     ]).

-include("erltrek.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([]) ->
    % initialize the stardate clock,
    Tick = ?INITTICK,
    % {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} 
    InitState = erltrek_setup:setup_state(),
    Timer = erlang:send_after(1, self(), tick_event),
    GameTimeState = {Tick, Timer, InitState},
    {ok, GameTimeState}.

handle_info(tick_event, GameTimeState) ->
    {Tick, OldTimer, GameState} = GameTimeState,
    erlang:cancel_timer(OldTimer),
    % do interval timer task here,
    {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} = GameState,
    erltrek_scan:srscan(Tick, SHIP, SECT, DI),
    % increment tick counter and restart timer
    NewTick = Tick + 1,
    NewTimer = erlang:send_after(?TICK_INTERVAL, self(), tick_event),
    NewGameState = {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS},
    NewGameTimeState = {NewTick, NewTimer, NewGameState},
    {noreply, NewGameTimeState}.

