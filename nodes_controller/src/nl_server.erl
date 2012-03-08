-module(nl_server).

-include("../include/config.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
        show_all/0,
        get_free_node/0,
        add_records/2,
        selftest/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?NODE_LIST_MODULE}, ?NODE_LIST_MODULE, [], []).

show_all() ->
    gen_server:call(?NODE_LIST_MODULE, show_all).

get_free_node() ->
    gen_server:call(?NODE_LIST_MODULE, get_free_node).

add_records(Paper, Data) ->
    gen_server:call(?NODE_LIST_MODULE, {add_records, Paper, Data}).

start_paper_server(Node, Paper, Data) ->
    supervisor:start_child(
        {?PAPER_SUPERVISOR, Node},
        {Paper, {?PAPER_MODULE, start_link,[Paper, Data]}, permanent,1000,worker,[?PAPER_MODULE]}).




%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
  {ok, init_storage()}.

handle_call({connect, Node}, _From, {NodeList, PaperAddrs}) ->
    IsExist = is_exist_in_nl(Node, NodeList),

    NewNodeList = if
        IsExist ->
            NodeList;
        true ->
            add_nl(Node, NodeList)
    end,
    {reply, Node, {NewNodeList, PaperAddrs}};

handle_call(get_free_node, _From, {NodeList, PaperAddrs}) ->
    {Node, NewNodeList} = get_head_node(NodeList),
    {reply, Node, {NewNodeList, PaperAddrs}};

handle_call(show_all, _From, {NodeList, PaperAddrs}) ->
    Node_List = show_nl(NodeList),
    Addrs_List = show_paper_addrs(PaperAddrs),
    {reply, {Node_List, Addrs_List}, {NodeList, PaperAddrs}};

handle_call({add_records, Paper, Data}, _From, {NodeList, PaperAddrs}) ->
    NewState = case get_addr_by_paper(Paper, PaperAddrs) of
        {ok, Addr} -> 
            gen_server:cast(Addr, {add_records, Data}),
            {NodeList, PaperAddrs};
        error ->
            {Node, NewNodeList} = get_head_node(NodeList),
            start_paper_server(Node, Paper, Data),
            NewPaperAddrs = add_papper_addr(Paper, {Paper, Node}, PaperAddrs),
            {NewNodeList, NewPaperAddrs}
    end,
    {reply, ok, NewState};

handle_call(_Msg, _From, State) ->
    {noreply, State}.




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
%%% Internal functions
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?NODE_LIST_MODULE, stop).

init_storage() ->
    {init_nl(), init_papers_addr()}.

%% Paper storage function
init_papers_addr() ->
    dict:new().

get_addr_by_paper(Paper, Storage) ->
    dict:find(Paper, Storage).

add_papper_addr(Paper, Addr, Storage) ->
    dict:store(Paper, Addr, Storage).

show_paper_addrs(Storage) ->
    dict:to_list(Storage).

%% Node list storage functions
init_nl() ->
    ?NODES.

add_nl(Node, Storage) ->
    lists:append([Node], Storage).

is_exist_in_nl(Key, Storage) ->
    lists:foldl(
        fun(X, Acc) -> (Key == X) or Acc end,
        false,
        Storage
    ).

show_nl(Storage) ->
    Storage.

get_head_node([]) ->
    nil;
get_head_node([H|T]) ->
    {H, lists:append(T, [H])}.

%%--------------------------------------------------------------------
%%% Tests
%%--------------------------------------------------------------------
selftest() ->
    %start_link(),
    gen_server:call(?NODE_LIST_MODULE, {connect, test1@mac}),
    gen_server:call(?NODE_LIST_MODULE, {connect, test2@mac}),
    gen_server:call(?NODE_LIST_MODULE, {connect, test1@mac}),

    {NodeList, _} = gen_server:call(?NODE_LIST_MODULE, show_all),

    NlRes = show_nl(NodeList),
    NlShould = [test2@mac, test1@mac] ++ ?NODES,
    if
        NlRes == NlShould ->
            io:format("connect(): ok~n");
        true ->
            io:format("connect(): false, res: ~p should be~p~n", [NlRes, NlShould])
    end,

    GetFreeNodeRes = get_free_node(),
    GetFreeNodeShould = test2@mac,
    if
        GetFreeNodeRes == GetFreeNodeShould ->
            io:format("get_free_node() 1: ok~n");
        true ->
            io:format("get_free_node() 1: false, res: ~p should be~p~n", [GetFreeNodeRes, GetFreeNodeShould])
    end,

    GetFreeNodeRes2 = get_free_node(),
    GetFreeNodeShould2 = test1@mac,
    if
        GetFreeNodeRes2 == GetFreeNodeShould2 ->
            io:format("get_free_node() 2: ok~n");
        true ->
            io:format("get_free_node() 2: false, res: ~p should be~p~n", [GetFreeNodeRes2, GetFreeNodeShould2])
    end,


    stop().



