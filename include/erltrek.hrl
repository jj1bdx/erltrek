%%% ----------------------------------------------------------------------------------
%%% Erltrek ("this software") is covered under the BSD 3-clause license.
%%% 
%%% This product includes software developed by the University of California, Berkeley
%%% and its contributors.
%%% 
%%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%% 
%%% * Redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%% 
%%% * Redistributions in binary form must reproduce the above copyright notice, this
%%%   list of conditions and the following disclaimer in the documentation and/or
%%%   other materials provided with the distribution.
%%% 
%%% * Neither the name of Kenji Rikitake, k2r.org, nor the names of its
%%%   contributors may be used to endorse or promote products derived from
%%%   this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
%%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% 
%%% This software incorporates portions of the BSD Star Trek source code,
%%% distributed under the following license:
%%% 
%%% Copyright (c) 1980, 1993
%%%      The Regents of the University of California.  All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 3. All advertising materials mentioning features or use of this software
%%%    must display the following acknowledgement:
%%%      This product includes software developed by the University of
%%%      California, Berkeley and its contributors.
%%% 4. Neither the name of the University nor the names of its contributors
%%%    may be used to endorse or promote products derived from this software
%%%    without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%% 
%%% [End of LICENSE]
%%% ----------------------------------------------------------------------------------

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
-define(ERLTREK_VERSION, "0.0")
%% Debug trace flag
-define(DEBUG_TRACE, true)

%% check inside the quadrant
-define(INQUAD(X), ((X >= 0) andalso (X < ?NQUADS))).
-define(INQUAD2(X, Y), (?INQUAD(X) andalso ?INQUAD(Y))).
%% check inside the sectors
-define(INSECT(X), ((X >= 0) andalso (X < ?NSECTS))).
-define(INSECT2(X, Y), (?INSECT(X) andalso ?INSECT(Y))).
%% convert coordinates to Galaxy array position
-define(GALAXYCOORD(QX, QY, SX, SY), 
    ((((QX * ?NQUADS) + QY) * (?NSECTS * ?NSECTS)) + ((SX*?NSECTS) + SY))).
%% convert quadrant coordinates to Quad array position
-define(QUADCOORD(QX, QY), (QX * ?NQUADS) + QY).
%% convert sector coordinates to Sect array position
-define(SECTCOORD(SX, SY), (SX * ?NSECTS) + SY).

%% vim: set ts=4 sw=4 sts=4 et :
%% emacs: -*- mode:erlang; tab-width:4; indent-tabs-mode:nil;  -*-
