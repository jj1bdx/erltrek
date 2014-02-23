-module(timer_test).
-export([init/1, handle_info/2]).
-behaviour(gen_server).

init([]) ->
    Timer = erlang:send_after(1, self(), check),
    {ok, Timer}.

handle_info(check, OldTimer) ->
    erlang:cancel_timer(OldTimer),
    do_task(),
    Timer = erlang:send_after(1000, self(), check),
    {noreply, Timer}.

do_task() ->
    io:format("tick~n").
