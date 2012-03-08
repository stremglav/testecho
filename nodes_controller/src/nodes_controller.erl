-module(nodes_controller).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    nodes_controller_sup:start_link().


stop(_State) ->
 ok.

