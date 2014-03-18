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

-module(erltrek_shell).

-export([start/0]).

-record(command, {
          name :: atom() | list(atom()),
          expand = true :: boolean(), %% tab complete command? (default: yes)
          desc = "(no arguments)" :: string(), %% printed on expand command
          help = "No help available for this command." :: string(),
          dispatch :: fun((list()) -> ok)
         }).

start() ->
    spawn(fun server/0).

server() ->
    ok = io:put_chars("ErlTrek Shell (abort with ^G)\n"),
    ok = io:setopts([{expand_fun, get_expand_fun()}]),
    server_loop().

server_loop() ->
    case io:get_line("Command > ") of
        Command when is_list(Command) ->
            case dispatch_command(Command) of
                not_found ->
                    io:format("My sincere apologies, Captain, I do not understand your command: ~s", [Command]);
                {error, Message} ->
                    io:format("Hrrm.. you need to quit slurring, Captain~n  (~s)~n", [Message]);
                {ok, _} -> nop
            end,
            server_loop();
        eof ->
            io:format("End of commands~n"),
            init:stop();
        {error, Desc} ->
            io:format("Read command failed: ~s~n",
                      [file:format_error(Desc)]),
            server_loop()
    end.

dispatch_command(Command) ->
    case parse_command(erl_scan:string(Command)) of
        {#command{ dispatch=Dispatch }, Args} ->
            {ok, Dispatch(Args)};
        {undefined, _} ->
            not_found;
        {_Location, Mod, Desc} ->
            {error, Mod:format_error(Desc)};
        ok ->
            {ok, skipped}
    end.

parse_command({ok, [Name|Args], _}) ->
    {find_command(Name),
     [Arg || Arg <- Args, element(1, Arg) =/= ',']
    };
parse_command({ok, [], _}) -> ok;
parse_command({error, Info, _Location}) -> Info.

token_to_name({_,_,Name}) -> Name;
token_to_name({Name,_}) -> Name;
token_to_name(Name) -> Name.

find_command(Token) ->
    Name = token_to_name(Token),
    Cmds = commands(),
    case find_command(Name, Cmds) of
        undefined ->
            NameR = lists:reverse(atom_to_list(Name)),
            case (get_expand_fun())(NameR) of
                {yes, Suffix, _} ->
                    find_command(list_to_atom(lists:reverse(NameR, Suffix)), Cmds);
                {_, _, []} ->
                    undefined;
                {_, _, Matches} ->
                    io:format("Your command, Captain, is ambiguous for any of the following: ~s~n",
                              [string:join([atom_to_list(N) || #command{ name=N } <- Matches], ", ")])
                end;
        Cmd -> Cmd
    end.

find_command(_, []) -> undefined;
find_command(Name, [#command{ name=Name }=Cmd|_]) -> Cmd;
find_command(Name, [_|Cmds]) -> find_command(Name, Cmds).

get_expand_fun() ->
    Commands = [begin
                    NameL = atom_to_list(Name),
                    {lists:reverse(NameL), Cmd#command{ name=NameL }}
                end
                || #command{ name=Name, expand=Expand }=Cmd <- commands(),
                   Expand =:= true],
    fun ("") ->
            {yes, "", [Name || {_, #command{ name=Name }} <- Commands]};
        (Input) ->
            %% note: Input is the current command line, reversed!
            Tokens = string:tokens(Input, " "),
            Command = lists:last(Tokens),
            Matches = lists:filter(
                        fun ({Name, _}) -> lists:suffix(Command, Name) end,
                        Commands),
            case Matches of
                [{_, #command{ name=Name, desc=Desc }}] ->
                    {yes, if length(Tokens) == 1 ->
                                  lists:nthtail(length(Command), Name);
                             true -> ""
                          end, [Desc]};
                [] -> {no, "", []};
                _ -> {no, "", [Name || {_, #command{ name=Name }} <- Matches]}
            end
    end.


-define(CMD(C), erltrek_game:enterprise_command(C)).
-define(I(V), {integer,_, V}).

commands() ->
    [#command{
        name = srscan,
        help = "Perform a short range scan in current quadrant.",
        dispatch = fun ([]) -> ?CMD({srscan});
                       (_) -> io:format("A scan is a scan, Captain..~n"),
                              ?CMD({srscan})
                   end
       },
     #command{
        name = lrscan,
        help = "Perform a long range scan covering current and neighbouring quadrants.",
        dispatch = fun ([]) -> ?CMD({lrscan});
                       (_) -> io:format("A scan is a scan, Captain..~n"),
                              ?CMD({lrscan})
                   end
       },
     #command{
        name = impulse,
        desc = "[Quadrant X, Y] Sector X, Y",
        help = "Start impulse engine, heading for given sector (in current or specified quadrant).",
        dispatch = fun ([?I(SX), ?I(SY)]) -> ?CMD({impulse, SX, SY});
                       ([?I(QX), ?I(QY), ?I(SX), ?I(SY)]) -> ?CMD({impulse, QX, QY, SX, SY});
                       ([]) -> io:format("I need direction, Captain!~n");
                       (_) -> io:format("Bad impulse directions, Captain!~n")
                   end
       },
     #command{
        name = phaser,
        desc = "Sector X, Y, Energy",
        help = "Fire with ship pasers on sector.",
        dispatch = fun ([?I(SX), ?I(SY), ?I(E)]) -> ?CMD({phaser, SX, SY, E});
                       (_) -> io:format("I need data, Captain!~n")
                   end
       },
     #command{
        name = dock,
        help = "Dock a starbase in adjacent sectors.",
        dispatch = fun ([]) -> ?CMD({dock});
                       (_) -> io:format("Docking is docking, Captain..~n"),
                              ?CMD({dock})
                   end
       },
     #command{
        name = undock,
        help = "Undock from a starbase currently docked.",
        dispatch = fun ([]) -> ?CMD({undock});
                       (_) -> io:format("Undocking is undocking, Captain..~n"),
                              ?CMD({undock})
                   end
       },
     #command{
        name = help,
        desc = "[commands ...]",
        help = "List available commands, or show help for specific commands.",
        dispatch = fun ([]) ->
                           [io:format("~-12s ~s~n", [Name, Desc])
                            || #command{ name=Name, desc=Desc } <- commands()];
                       (Names) ->
                           Cmds = commands(),
                           [case find_command(Name, Cmds) of
                                #command{ name=N, desc=D, help=H } ->
                                    io:format("~-12s ~s~n~s~n~n", [N, D, H]);
                                _ ->
                                    io:format("Sorry, command ~s is not known to us.~n",
                                              [token_to_name(Name)])
                            end || Name <- Names]
                   end
       },
     #command{
        name = quit,
        expand = false, %% require user to type "quit", no shortcuts..
        help = "Quit ErlTrek.",
        dispatch = fun ([]) ->
                           io:format("Abandon ship!~n"),
                           init:stop(),
                           exit(normal);
                       (_) -> io:format("Eh.. are you sure?!~n")
                   end
       }
    ].
