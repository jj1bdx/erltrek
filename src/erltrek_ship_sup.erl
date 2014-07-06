%%% --------------------------------------------------------------------
%%% BSD 3-clause license:
%%%
%%% Copyright (c) 2014, Andreas Stenius <kaos@astekk.se>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above
%%% copyright notice, this list of conditions and the following
%%% disclaimer in the documentation and/or other materials provided
%%% with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived
%%% from this software without specific prior written permission.
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
%%% --------------------------------------------------------------------

-module(erltrek_ship_sup).
-behaviour(supervisor).
-include("erltrek.hrl").

%% API
-export([start_link/0, start_ship/1]).

%% Callbacks
-export([init/1]).

-spec start_link() -> startlink_ret().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_ship(list()) -> supervisor:startchild_ret().

start_ship(Args) when is_list(Args) ->
    supervisor:start_child(?MODULE, Args).

-spec init(Args :: term()) ->
        {ok, {{RestartStrategy :: supervisor:strategy(),
               MaxR            :: non_neg_integer(),
               MaxT            :: non_neg_integer()},
               [ChildSpec :: supervisor:child_spec()]}} | ignore.

init(_Args) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxTime = 60,
    Child = [{ship,
              {erltrek_ship, start_link, []},
              temporary, brutal_kill, worker, [erltrek_ship]}
            ],
    {ok, {{RestartStrategy, MaxRestarts, MaxTime}, Child}}.
