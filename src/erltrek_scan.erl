%%% --------------------------------------------------------------------
%%% Erltrek ("this software") is covered under the BSD 3-clause
%%% license.
%%%
%%% This product includes software developed by the University of
%%% California, Berkeley and its contributors.
%%%
%%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
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
%%% * Neither the name of Kenji Rikitake, k2r.org, nor the names of
%%%   its contributors may be used to endorse or promote products
%%%   derived from this software without specific prior written
%%%   permission.
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

-module(erltrek_scan).

-include("erltrek.hrl").

-export([
        srscan/4
        ]).


%% Display current sector info and ship status with
%% * tick time (integer)
%% * #enterprise_status
%% * current sector array
%% * dict of inhabited systems

-spec srscan(integer(), #enterprise_status{}, array(), dict()) -> ok.

srscan(T, SHIP, SECT, DI) ->
    DISP = orddict:from_list([
            {s_empty, $.}, {s_star, $*}, {s_enterprise, $E},
            {s_base, $#}, {s_inhabited, $@}, {s_klingon, $K},
            {s_hole, $H}]),
    STATUS = [
        io_lib:format("Game time:     ~b", [T]),
        io_lib:format("Position:      ~b,~b/~b,~b",
            [SHIP#enterprise_status.quadxy#quadxy.x, 
             SHIP#enterprise_status.quadxy#quadxy.y, 
             SHIP#enterprise_status.sectxy#sectxy.x, 
             SHIP#enterprise_status.sectxy#sectxy.y]),
        io_lib:format("Energy:        ~b", [SHIP#enterprise_status.energy]),
        io_lib:format("Shield:        ~b", [SHIP#enterprise_status.shield])
        ],
    io:format("Short range sensor scan~n"),
    io:format("  0 1 2 3 4 5 6 7 8 9~n"),
    srscan_xline(0, STATUS, SECT, DISP),
    io:format("  0 1 2 3 4 5 6 7 8 9~n"),
    case dict:is_key(SHIP#enterprise_status.quadxy, DI) of
        true ->
            LI = dict:fetch(SHIP#enterprise_status.quadxy, DI),
            [I] = LI,
            io:format("Starsystem ~s~n", [I#inhabited_info.systemname]);
        false ->
            ok % do nothing
    end,
    ok.

srscan_xline(?NSECTS, _SL, _SECT, _DISP) ->
    ok;
srscan_xline(X, SL, SECT, DISP) ->
    io:format("~c ", [X + $0]),
    srscan_ypos(0, X, SECT, DISP),
    io:format("~c  ", [X + $0]),
    case length(SL) > 0 of
        true ->
            [H|SL2] = SL,
            io:format("~s~n", [H]);
        false ->
            SL2 = SL,
            io:format("~n")
    end,
    srscan_xline(X + 1, SL2, SECT, DISP).
    
srscan_ypos(?NSECTS, _SL, _SECT, _DISP) ->
    ok;
srscan_ypos(Y, X, SECT, DISP) ->
    io:format("~c ", [orddict:fetch(
                array:get(erltrek_setup:sectxy_index(#sectxy{x = X, y = Y}), 
                    SECT), DISP)]),
    srscan_ypos(Y + 1, X, SECT, DISP).

