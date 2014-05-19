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

-module(erltrek_shell_commands).

-export([commands/0]).
-import(erltrek_shell, [find_command/1, token_to_name/1]).

-include("erltrek.hrl").

-define(DEFAULT_PHASER_ENERGY, 100).

-define(CMD(C), erltrek_game:enterprise_command(C)).
-define(I(V), {integer, _, V}).

-spec get_coord(string(), Default) -> {integer(), integer()} | Default.
get_coord(Prompt, Default) ->
    case io:get_line(Prompt) of
        "\n" -> Default;
        Rsp when is_list(Rsp) ->
            case erl_scan:string(Rsp) of
                {ok, [?I(X), ?I(Y)], _} -> {X, Y};
                {ok, [?I(X), {',', _}, ?I(Y)], _} -> {X, Y};
                _ ->
                    io:format("Please provide X and Y coordinates, or no value for ~p.~n", [Default]),
                    get_coord(Prompt, Default)
            end;
        _ -> Default
    end.

-spec get_integer(string(), Default) -> integer() | Default.
get_integer(Prompt, Default) ->
    case io:get_line(Prompt) of
        "\n" -> Default;
        Rsp when is_list(Rsp) ->
            case erl_scan:string(Rsp) of
                {ok, [?I(V)], _} -> V;
                _ ->
                    io:format("Please provide integer value, or no value for ~p.~n", [Default]),
                    get_integer(Prompt, Default)
            end;
        _ -> Default
    end.

commands() ->
    [#command{
        name = srscan,
        help = "Perform a short range scan in current quadrant.",
        dispatch = fun ([]) -> ?CMD(srscan);
                       (_) -> io:format("A scan is a scan, Captain..~n"),
                              ?CMD(srscan)
                   end,
        result = fun ({ok, Scan}) ->
                         io:format("~s", [erltrek_scan:srscan_string(Scan)]);
                     (Other) -> Other
                 end
       },
     #command{
        name = lrscan,
        help = "Perform a long range scan covering current and neighbouring quadrants.",
        dispatch = fun ([]) -> ?CMD(lrscan);
                       (_) -> io:format("A scan is a scan, Captain..~n"),
                              ?CMD(lrscan)
                   end,
        result = fun ({ok, Scan}) ->
                         io:format("~s", [erltrek_scan:lrscan_string(Scan)]);
                     (Other) -> Other
                 end
       },
     #command{
        name = impulse,
        desc = "[Quadrant X, Y] Sector X, Y",
        help = "Start impulse engine, heading for given sector (in current or specified quadrant).",
        dispatch = fun ([?I(SX), ?I(SY)]) -> ?CMD({impulse, SX, SY});
                       ([?I(QX), ?I(QY), ?I(SX), ?I(SY)]) -> ?CMD({impulse, QX, QY, SX, SY});
                       ([]) ->
                           Q = get_coord("Quadrant: ", default),
                           S = get_coord("Sector: ", abort),
                           case {Q, S} of
                               {{QX, QY}, {SX, SY}} ->
                                   ?CMD({impulse, QX, QY, SX, SY});
                               {default, {SX, SY}} ->
                                   ?CMD({impulse, SX, SY});
                               _ -> nop
                           end;
                       (_) -> io:format("Bad impulse directions, Captain!~n")
                   end,
        result = fun ({move, no_move_to_same_position}) ->
                         io:format("No move to the same position!~n");
                     ({move, no_move_while_docked}) ->
                         io:format("No move allowed while docked!~n");
                     (Other) -> Other
                 end
       },
     #command{
        name = phaser,
        desc = "Sector X, Y, Energy",
        help = "Fire with ship phasers on sector.",
        dispatch = fun ([?I(SX), ?I(SY), ?I(E)]) -> ?CMD({phaser, SX, SY, E});
                       ([]) ->
                           S = get_coord("Sector: ", abort),
                           E = get_integer("Energy: ", ?DEFAULT_PHASER_ENERGY),
                           case {S, E} of
                               {{SX, SY}, E} ->
                                   ?CMD({phaser, SX, SY, E});
                               _ -> nop
                           end;
                       (_) -> io:format("Bad phaser command, Captain!~n")
                   end,
        result = fun ({phaser, not_enough_energy}) ->
                         io:format("Our ship energy reserves are running low!~n");
                     ({phaser, no_klingon_in_quadrant}) ->
                         io:format("No Klingon in the quadrant!~n");
                     ({phaser, no_firing_when_docked}) ->
                         io:format("No firing allowed when docked!~n");
                     ({phaser_hit, Hits}) ->
                         case lists:flatten(
                                [io_lib:format(
                                   "Phaser hit ~s at sector ~b,~b level ~b~n",
                                   [erltrek_terminal:describe_object(Class),
                                    SX, SY, Level])
                                 || {#sectxy{ x=SX, y=SY }, Class, Level} <- Hits,
                                    Level > 0])
                         of
                             [] ->
                                 io:format("Phaser did not hit any targets.~n");
                             Msg ->
                                 io:format(Msg)
                         end;
                     (Other) -> Other
                 end
       },
     #command{
        name = dock,
        help = "Dock a starbase in adjacent sectors.",
        dispatch = fun ([]) -> ?CMD(dock);
                       (_) -> io:format("Docking is docking, Captain..~n"),
                              ?CMD(dock)
                   end,
        result = fun ({dock, already_docked}) ->
                         io:format("The ship is already docked~n");
                     ({dock, docking_complete}) ->
                         io:format("Docking the ship complete~n");
                     ({dock, base_not_adjacent}) ->
                         io:format("No starbase in adjacent sectors~n");
                     ({dock, base_not_in_quadrant}) ->
                         io:format("No starbase in the quadrant~n");
                     (Other) -> Other
                 end
       },
     #command{
        name = undock,
        help = "Undock from a starbase currently docked.",
        dispatch = fun ([]) -> ?CMD(undock);
                       (_) -> io:format("Undocking is undocking, Captain..~n"),
                              ?CMD(undock)
                   end,
        result = fun ({undock, not_docked}) ->
                         io:format("The ship is not docked~n");
                     ({undock, undock_complete}) ->
                         io:format("Undocking complete~n");
                     (Other) -> Other
                 end
       },
     #command{
        name = help,
        desc = "[commands ...]",
        help = "List available commands, or show help for specific commands.",
        dispatch = fun ([]) ->
                           [io:format("~-12s ~s~n", [Name, Desc])
                            || #command{ name=Name, desc=Desc } <- commands()],
                           ok;
                       (Names) ->
                           [case find_command(Name) of
                                #command{ name=N, desc=D, help=H } ->
                                    io:format("~-12s ~s~n~s~n~n", [N, D, H]);
                                _ ->
                                    io:format("Sorry, command ~s is not known to us.~n",
                                              [token_to_name(Name)])
                            end || Name <- Names],
                           ok
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
