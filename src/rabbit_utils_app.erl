%%%-------------------------------------------------------------------
%% @doc rabbit_utils public API
%% @end
%%%-------------------------------------------------------------------

-module(rabbit_utils_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rabbit_utils_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
