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

%% include tinymt32 config
-include("tinymt32.hrl").

%% dimensions of quadrant in sectors
-define(NSECTS, 10).
%% dimension of galaxy in quadrants
-define(NQUADS, 8).
%% maximum stars per quadrant
-define(MAXSTARQUAD, 9).
%% maximum klingons per quadrant
-define(MAXKLQUAD, 9).
%% maximum number of starbases in galaxy
-define(MAXBASES, 9).
%% max number of concurrently pending events
-define(MAXEVENTS, 25).
%% maximum concurrent distress calls
-define(MAXDISTR, 5).
%% Version number string
-define(ERLTREK_VERSION, "0.0").
%% Debug trace flag
-define(DEBUG_TRACE, true).
%% Initial Enterprise energy
-define(SHIPENERGY, 5000).
%% Initial Enterprise shield level
-define(SHIPSHIELD, 1000).
%% Initial klingon energy
-define(KLINGONENERGY, 400).
%% Interval in milliseconds per tick
-define(TICK_INTERVAL, 1000).
%% Initial Stardate * 100
-define(INITTICK, 200000).

%%% types and records

%% see OTP lib/stdlib/src/orddict.erl

-type orddict() :: [{Key :: term(), Value :: term()}].

%% quadrant and sector coordinates

-type quadcoord() :: 0..(?NQUADS - 1).
-type sectcoord() :: 0..(?NSECTS - 1).

-record(quadxy, { x :: quadcoord(), y :: quadcoord()}).
-record(sectxy, { x :: sectcoord(), y :: sectcoord()}).

%% entity atom in the sector array

-type sector_entity() ::
    's_empty' | 's_star' | 's_enterprise' | 's_base' | 's_inhabited' |
    's_klingon' | 's_hole'.

%% record for entities

-record(base_info, { xy :: #sectxy{} }).
-record(inhabited_info, { xy :: #sectxy{}, systemname :: string()}).

%% Enterprise status

-record(enterprise_status, {
        quadxy :: #quadxy{},
        sectxy :: #sectxy{},
        energy :: integer(),
        shield :: integer(),
        impulse_move :: boolean(),
        impulse_course :: [{#quadxy{}, #sectxy{}}],
        warp_move :: boolean(),
        warp_course :: [{#quadxy{}, #sectxy{}}],
        docked :: boolean(),
        condition:: 'cond_green' | 'cond_yellow' | 'cond_red' | 'cond_docked',
        % next command content
        next_command :: tuple()
    }).

%% Status for Klingons in the sector

-record(klingon_status, {
        energy :: integer()
    }).

%% Tuple type for saving Game State
%% {Tick,SHIP,NK,DS,DI,DB,DH,DKQ,SECT,DKS} = GameState

-type game_state() ::
    {non_neg_integer(), #enterprise_status{}, non_neg_integer(),
     dict(), dict(), dict(), dict(), dict(), array(), dict()}.

%% vim: set ts=4 sw=4 sts=4 et :
%% emacs: -*- mode:erlang; tab-width:4; indent-tabs-mode:nil;  -*-
