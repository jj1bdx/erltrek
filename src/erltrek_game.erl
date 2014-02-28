-module(erltrek_game).
-behaviour(gen_server).

-export([
         handle_cast/2,
         handle_info/2,
         init/1,
         start_link/0,
         start_link/1,
         stop/0,
         terminate/2
     ]).

-include("erltrek.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    % initialize the stardate clock,
    Tick = ?INITTICK,
    % {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} 
    InitState = erltrek_setup:setup_state(),
    Timer = erlang:send_after(1, self(), tick_event),
    GameTimeState = {Tick, Timer, InitState},
    {ok, GameTimeState}.

terminate(normal, State) ->
    ok.

handle_cast(stop, State) ->
    io:format("~s: stop call received~n", [?MODULE]),
    {stop, normal, State}.

handle_info(tick_event, GameTimeState) ->
    {Tick, OldTimer, GameState} = GameTimeState,
    erlang:cancel_timer(OldTimer),
    % do interval timer task here,
    {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} = GameState,
    % warping test
    QC = #quadxy{x = tinymt32:uniform(?NQUADS) - 1,
                 y = tinymt32:uniform(?NQUADS) - 1},
    {SECT2, DKS2} = erltrek_setup:setup_sector(QC, DS, DI, DB, DH, DKQ),
    % put enterprise in the current quadrant
    SC = erltrek_setup:rand_sect(SECT2),
    SECT3 = array:set(erltrek_setup:sectxy_index(SC), s_enterprise, SECT2), 
    SHIP2 = SHIP#enterprise_status{quadxy = QC, sectxy = SC},
    % displaying the status
    erltrek_scan:srscan(Tick, SHIP2, SECT3, DI, DKQ),
    % Set new game state
    % NOTE WELL ON THE VARIABLES!
    NewGameState = {SHIP2,NK,DS,DI,DB,DH,DKQ,SECT3,DKS},
    % increment tick counter and restart timer
    NewTick = Tick + 1,
    NewTimer = erlang:send_after(?TICK_INTERVAL, self(), tick_event),
    NewGameTimeState = {NewTick, NewTimer, NewGameState},
    {noreply, NewGameTimeState}.

