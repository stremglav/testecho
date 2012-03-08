-module(papers_server).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    papers_server_sup:start_link().


stop(_State) ->
 ok.

