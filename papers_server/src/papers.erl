-module(papers).
-include("../include/config.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2,
        connect/0,
        add_records/2,
        get_records/3,
        show_all/1,
        clean_all/1,
        selftest/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



%% API
connect() ->
    gen_server:call({?NODE_LIST_MODULE, ?NODE_LIST_NAME}, {connect, node()}).

start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

add_records(Name, Records) ->
    gen_server:call(Name, {add_records, Records}).

get_records(Name, StartTime, FinishTime) ->
    gen_server:call(Name, {get_records, StartTime, FinishTime}).

show_all(Name) ->
    gen_server:call(Name, show_all).

stop(Name) ->
    gen_server:cast(Name, stop).

clean_all(Name) ->
    gen_server:cast(Name, clean_all).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
  {ok, storage:new(Args)}.


handle_call({get_records, StartTime, FinishTime}, _From, State) ->
    List = storage:read(StartTime, FinishTime, State),
    {reply, List, State};

handle_call({add_records, Records}, _From, State) ->
    NewState = storage:save_list(Records, State),
    {reply, ok, NewState};

handle_call(show_all, _From, State) ->
    List = storage:show(State),
    {reply, List, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast(clean_all, _State) ->
    {noreply, storage:new()};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Tests
%%--------------------------------------------------------------------
selftest() ->
    start_link(test, [{0, {0, 0}}]),
    timer:sleep(100),
    ShowRes = show_all(test),
    ShowShould = storage:show(storage:new([{0, {0, 0}}])),
    if
        ShowRes == ShowShould ->
            io:format("show_all(): ok~n");
        true ->
            io:format("show_all(): false, res: ~p should be~p~n", [ShowRes, ShowShould])
    end,
    clean_all(test),
    timer:sleep(100),

    add_records(test, [{123, {123, 123}}]),
    AddRes = show_all(test),
    AddShould = [{123, {123, 123}}],
    if
        AddRes == AddShould ->
            io:format("add_records(): ok~n");
        true ->
            io:format("add_records(): false, res: ~p should be~p~n", [AddRes, AddShould])
    end,
    clean_all(test),
    timer:sleep(100),

    add_records(test, [{1,{1,1}}, {2,{2,2}}, {3,{3,3}}, {4,{4,4}}, {5,{5,5}}]),
    GetRes = get_records(test, 2, 4),
    GetShould = [{2, {2, 2}}, {3, {3, 3}}, {4, {4, 4}}],
    if
        GetRes == GetShould ->
            io:format("get_records(): ok~n");
        true ->
            io:format("get_records(): false, res: ~p should be~p~n", [GetRes, GetShould])
    end,
    clean_all(test),
    timer:sleep(100),

    stop(test).
