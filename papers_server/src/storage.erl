-module(storage).

-export([new/0,
        new/1,
        save/3,
        save_list/2,
        read/3,
        show/1,
        selftest/0]).

new() -> 
    dict:new().

new([]) ->
    dict:new();
new(Args) ->
    lists:foldl(
        fun({Time, {Cost, Value}}, Acc) ->
            save(Time, {Cost, Value}, Acc) end,
        dict:new(),
        Args).

save(Time, Data, Dict) ->
    dict:store(Time, Data, Dict).

save_list(Records, Dict) ->
    lists:foldl(
        fun({Time, {Cost, Value}}, Acc) ->
            save(Time, {Cost, Value}, Acc) end,
        Dict,
        Records).


read(StartTime, FinishTime, _Dict) when StartTime > FinishTime ->
    [];

read(StartTime, FinishTime, Dict) ->
    New = dict:filter(
        fun(K, _V) ->
            ((K >= StartTime) and (K =< FinishTime)) end,
        Dict),
    lists:sort(show(New)).

show(Dict) ->
    dict:to_list(Dict).

selftest() ->
    NewShould = [{123,{123,123}}],
    NewRes = show(new([{123, 123, 123}])),
    if
        NewShould == NewRes ->
            io:format("new: ok~n");
        true ->
            io:format("new: false, res: ~p should be~p~n", [NewRes, NewShould])
    end,

    S1 = new([]),
    SaveShould = [{123,{123,123}}],
    SaveRes = show(save(123, {123, 123}, S1)),
    if
        SaveShould == SaveRes ->
            io:format("save: ok~n");
        true ->
            io:format("save: false, res: ~p should be~p~n", [SaveRes, SaveShould])
    end,

    S2 = new([{0, 0, 0}, {1,1,1}, {3,3,3}, {5,5,5}]),
    ReadShould = [{1, {1, 1}}, {3,{3,3}}],
    ReadRes = read(1, 3, S2),
    if
        ReadShould == ReadRes ->
            io:format("read: ok~n");
        true ->
            io:format("read: false, res: ~p should be~p~n", [ReadRes, ReadShould])
    end,

    ok.
