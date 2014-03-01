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

-module(erltrek_event).

-export([
        timer_tasks/2
     ]).

-include("erltrek.hrl").

%% Do timer tasks
%% Input and output:
%% Tick, and
%% {SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} = GameState
%% @todo isn't this spec definition clumsy?

-spec timer_tasks(integer(), {#enterprise_status{}, integer(),
            dict(), dict(), dict(), dict(), dict(), array(), dict()}) ->
    {integer(), {#enterprise_status{}, integer(),
            dict(), dict(), dict(), dict(), dict(), array(), dict()}}.

timer_tasks(Tick, GameState) ->
    GameState2 = enterprise_command(Tick, GameState),
    %QC = #quadxy{x = tinymt32:uniform(?NQUADS) - 1,
    %             y = tinymt32:uniform(?NQUADS) - 1},
    %{SECT2, DKS2} = erltrek_setup:setup_sector(QC, DS, DI, DB, DH, DKQ),
    % put enterprise in the current quadrant
    %SC = erltrek_setup:rand_sect(SECT2),
    %SECT3 = array:set(erltrek_setup:sectxy_index(SC), s_enterprise, SECT2), 
    %SHIP2 = SHIP#enterprise_status{quadxy = QC, sectxy = SC},
    % displaying the status
    %erltrek_scan:srscan(Tick, SHIP2, SECT3, DI, DKQ),
    %erltrek_scan:lrscan(SHIP2, DS, DI, DB, DKQ),

    % Set new game state

    NewGameState = GameState2,
    NewGameState.

%% Do commands for Enterprise

-spec enterprise_command(integer(), {#enterprise_status{}, integer(),
            dict(), dict(), dict(), dict(), dict(), array(), dict()}) ->
    {#enterprise_status{}, integer(),
        dict(), dict(), dict(), dict(), dict(), array(), dict()}.

enterprise_command(Tick, GameState) ->
    {SHIP, NK, DS, DI, DB, DH, DKQ, SECT, DKS} = GameState,
    Command = SHIP#enterprise_status.next_command,
    case Command of
        {lrscan} -> % long range scanner
            erltrek_scan:lrscan(SHIP, DS, DI, DB, DKQ),
            % clear command buffer
            SHIP2 = SHIP#enterprise_status{next_command = {}},
            {SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS};
        {srscan} -> % short range scanner
            erltrek_scan:srscan(Tick, SHIP, SECT, DI, DKQ),
            % clear command buffer
            SHIP2 = SHIP#enterprise_status{next_command = {}},
            {SHIP2, NK, DS, DI, DB, DH, DKQ, SECT, DKS};
        {} -> % do nothing
            GameState
    end.

