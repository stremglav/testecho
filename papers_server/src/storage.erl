-module(storage).

-export([new/0,
        new/1,
        save/3,
        save_list/2,
        read/4,
        show/1,
        timestamp_to_datetime/1,
        timestamp_round/2,
        datetime_to_timestamp/1,
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


read(StartTime, FinishTime, _, _) when StartTime > FinishTime ->
    [];

read(StartTime, FinishTime, Scale, Dict) ->
    Frame = dict:filter(
        fun(K, _V) ->
            ((K >= StartTime) and (K =< FinishTime)) end,
        Dict),
    Compressed = dict:fold(
        fun(K, V, Acc) ->
            T = timestamp_round(K, Scale),
            dict:append(T, {K, V}, Acc)
        end,
        dict:new(),
        Frame
    ),
    Result = dict:fold(
        fun(K, V, Acc) ->
            {MinCost, MaxCost, AccValue} = gen_data_record(V),
            {_, {CostOpen,_}} = lists:min(V),
            {_, {CostClose,_}} = lists:max(V),
            dict:append(K, {CostOpen, CostClose, MinCost, MaxCost, AccValue}, Acc)
        end,
        dict:new(),
        Compressed
    ),
    Result.


gen_data_record([{_, {CostHead, ValueHead}}|T]) ->
    lists:foldl(
        fun({_, {Cost, Value}}, {MinCost, MaxCost, AccValue}) ->
            {erlang:min(MinCost, Cost), erlang:max(MaxCost, Cost), Value + AccValue}
        end,
        {CostHead, CostHead, ValueHead},
        T
    ).


show(Dict) ->
    lists:sort(dict:to_list(Dict)).

timestamp_to_datetime(T) ->
    calendar:now_to_universal_time({T div 1000000, T rem 1000000, 0}).

datetime_to_timestamp(D) ->
    calendar:datetime_to_gregorian_seconds(D)-62167219200. % 719528*24*3600

timestamp_round(D, Scale) ->
    {Dt = {Year, Month, Day}, {Hour, Minute, _}} = timestamp_to_datetime(D),
    case Scale of
        month ->
            datetime_to_timestamp({{Year, Month, 1}, {0, 0, 0}});
        week ->
            Days = calendar:date_to_gregorian_days(Dt),
            Offset = calendar:day_of_the_week(Dt) - 1,
            Date = calendar:gregorian_days_to_date(Days - Offset),
            datetime_to_timestamp({Date, {0, 0, 0}});
        day ->
            datetime_to_timestamp({{Year, Month, Day}, {0, 0, 0}});
        hour ->
            datetime_to_timestamp({{Year, Month, Day}, {Hour, 0, 0}});
        minute ->
            datetime_to_timestamp({{Year, Month, Day}, {Hour, Minute, 0}})
    end.

gen_n_records(_, _, 0, Acc) ->
    Acc;
gen_n_records(StartT, FinishT, N, Acc) ->
    NewAcc = 
        Acc ++ [{rand_between(StartT, FinishT),
                 {rand_between(1, 1000), rand_between(1, 10000)}}],
    gen_n_records(StartT, FinishT, N - 1 , NewAcc).


rand_between(Min, Max) ->
    Min + random:uniform(Max - Min + 1).



%%%%
%% Dict speed tests
%%
%% Read time(count: 5869.3125) for minute: 64395.1875, Normalization: 10.971504328658595
%% Read time(count: 433.0) for hour: 43141.09375, Normalization: 99.63301096997691
%% Read time(count: 19.0) for day: 42955.84375, Normalization: 2260.8338815789475
%% Read time(count: 3.0) for week: 94270.5625, Normalization: 31423.520833333332
%% Read time(count: 2.0) for month: 177330.1875, Normalization: 88665.09375
%%
%%%%


get_norm_speed(0, Gap, Scale, [H|Tail]) ->
    {Count, Time} = lists:foldl(
        fun({C, T}, {AccC, AccT}) ->
            {(AccC + C) / 2, (AccT + T) / 2}
        end,
        H,
        Tail
    ),
    io:format("Gap: ~p Read time(count: ~p) for ~p: ~p, Normalization: ~p~n", [Gap, Count, Scale, Time, Time / Count]);

get_norm_speed(N, Gap, Scale, Acc) ->
    StartTime = 1328833307, %{{2012,2,10},{0,21,47}}
    FinishTime = 1331197296, %{{2012,3,8},{9,1,36}}
    GenData = gen_n_records(StartTime, FinishTime, 10000, []),
    S0 = new(GenData),

    RightBound = 1330647707, %{{2012,3,2},{0,21,47}}
    LeftBound = RightBound - Gap,
    
    T1 = erlang:now(),
    ReadSpeedRes = read(LeftBound, RightBound, Scale, S0),
    T2 = erlang:now(),
    Count = erlang:length(show(ReadSpeedRes)),
    Time = timer:now_diff(T2, T1),
    get_norm_speed(N - 1, Gap, Scale, Acc ++ [{Count, Time}]).


selftest() ->

    io:format("round_ts 1: ~p~n", [datetime:round_ts(1328818907,1)]),
    io:format("round_ts 2: ~p~n", [datetime:round_ts(1328818907,2)]),
    io:format("round_ts 3: ~p~n", [datetime:round_ts(1328818907,3)]),
    io:format("round_ts 4: ~p~n", [datetime:round_ts(1328818907,4)]),
    io:format("round_ts 5: ~p~n", [datetime:round_ts(1328818907,5)]),

    %timer:tc(fun()-> lists:map(fun(_)-> datetime:round_ts(1328818907,1) end, lists:seq(1,10000)) end).
    %timer:tc(fun()-> lists:map(fun(_)-> storage:timestamp_round(1328818907, minute) end, lists:seq(1,10000)) end).

    get_norm_speed(6, 15*24*3600, minute, []),
    get_norm_speed(6, 1*24*3600, minute, []),
    get_norm_speed(6, 15*24*3600, hour, []),
    get_norm_speed(6, 1*24*3600, hour, []),
    get_norm_speed(6, 15*24*3600, day, []),
    get_norm_speed(6, 1*24*3600, day, []),
    get_norm_speed(6, 15*24*3600, week, []),
    get_norm_speed(6, 1*24*3600, week, []),
    get_norm_speed(6, 15*24*3600, month, []),
    get_norm_speed(6, 1*24*3600, month, []),


    NewShould = [{123,{123,123}}],
    NewRes = show(new([{123, {123, 123}}])),
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

    TestData = 
        [{1329410762,{427,788}},
        {1330243277,{795,459}},
        {1329242146,{495,702}},
        {1330321067,{725,612}},
        {1330533716,{945,375}},
        {1328913438,{282,35}},
        {1329107393,{431,505}},
        {1329277533,{323,154}},
        {1328845041,{382,300}},
        {1330338185,{266,281}},
        {1331197296,{878,84}}, %max time
        {1328907949,{310,271}},
        {1328911458,{653,574}},
        {1330952704,{640,795}},
        {1329189167,{546,722}},
        {1330372514,{179,679}}, 
        {1329693823,{785,169}},
        {1330405480,{501,368}},
        {1328898226,{500,58}},
        {1330444108,{800,756}},
        {1330307925,{262,174}},
        {1330020157,{402,934}},
        {1328833307,{566,880}}, %min time
        {1329537775,{561,902}},
        {1328949532,{935,919}}],

    S2 = new(TestData),

    MinuteReadRes = show(read(1328833307, 1331197296, minute, S2)),
    MinuteReadShould =
              [{1328833260,[{566,566,566,566,880}]},
              {1328845020,[{382,382,382,382,300}]},
              {1328898180,[{500,500,500,500,58}]},
              {1328907900,[{310,310,310,310,271}]},
              {1328911440,[{653,653,653,653,574}]},
              {1328913420,[{282,282,282,282,35}]},
              {1328949480,[{935,935,935,935,919}]},
              {1329107340,[{431,431,431,431,505}]},
              {1329189120,[{546,546,546,546,722}]},
              {1329242100,[{495,495,495,495,702}]},
              {1329277500,[{323,323,323,323,154}]},
              {1329410760,[{427,427,427,427,788}]},
              {1329537720,[{561,561,561,561,902}]},
              {1329693780,[{785,785,785,785,169}]},
              {1330020120,[{402,402,402,402,934}]},
              {1330243260,[{795,795,795,795,459}]},
              {1330307880,[{262,262,262,262,174}]},
              {1330321020,[{725,725,725,725,612}]},
              {1330338180,[{266,266,266,266,281}]},
              {1330372500,[{179,179,179,179,679}]},
              {1330405440,[{501,501,501,501,368}]},
              {1330444080,[{800,800,800,800,756}]},
              {1330533660,[{945,945,945,945,375}]},
              {1330952700,[{640,640,640,640,795}]},
              {1331197260,[{878,878,878,878,84}]}],
    
    if
        MinuteReadShould == MinuteReadRes ->
            io:format("read minute scale check: ok~n");
        true ->
            io:format("read minute scale check: false~n")
    end,

    HourReadRes = show(read(1328833307, 1331197296, hour, S2)),
    HourReadShould =
        [{1328832000,[{566,566,566,566,880}]},
        {1328842800,[{382,382,382,382,300}]},
        {1328896800,[{500,500,500,500,58}]},
        {1328907600,[{310,310,310,310,271}]},
        {1328911200,[{653,282,282,653,609}]},
        {1328947200,[{935,935,935,935,919}]},
        {1329105600,[{431,431,431,431,505}]},
        {1329188400,[{546,546,546,546,722}]},
        {1329238800,[{495,495,495,495,702}]},
        {1329274800,[{323,323,323,323,154}]},
        {1329408000,[{427,427,427,427,788}]},
        {1329537600,[{561,561,561,561,902}]},
        {1329692400,[{785,785,785,785,169}]},
        {1330020000,[{402,402,402,402,934}]},
        {1330243200,[{795,795,795,795,459}]},
        {1330304400,[{262,262,262,262,174}]},
        {1330318800,[{725,725,725,725,612}]},
        {1330336800,[{266,266,266,266,281}]},
        {1330369200,[{179,179,179,179,679}]},
        {1330405200,[{501,501,501,501,368}]},
        {1330441200,[{800,800,800,800,756}]},
        {1330531200,[{945,945,945,945,375}]},
        {1330952400,[{640,640,640,640,795}]},
        {1331197200,[{878,878,878,878,84}]}],

    if
        HourReadShould == HourReadRes ->
            io:format("read hour scale check: ok~n");
        true ->
            io:format("read hour scale check: false~n")
    end,
    

    DayReadRes = show(read(1328833307, 1331197296, day, S2)),
    DayReadShould = 
       [{1328832000,[{566,282,282,653,2118}]},
       {1328918400,[{935,935,935,935,919}]},
       {1329091200,[{431,431,431,431,505}]},
       {1329177600,[{546,495,495,546,1424}]},
       {1329264000,[{323,323,323,323,154}]},
       {1329350400,[{427,427,427,427,788}]},
       {1329523200,[{561,561,561,561,902}]},
       {1329609600,[{785,785,785,785,169}]},
       {1329955200,[{402,402,402,402,934}]},
       {1330214400,[{795,795,795,795,459}]},
       {1330300800,[{262,179,179,725,1746}]},
       {1330387200,[{501,800,501,800,1124}]},
       {1330473600,[{945,945,945,945,375}]},
       {1330905600,[{640,640,640,640,795}]},
       {1331164800,[{878,878,878,878,84}]}],

    if
        DayReadShould == DayReadRes ->
            io:format("read day scale check: ok~n");
        true ->
            io:format("read day scale check: false~n")
    end,

    WeekReadRes = show(read(1328833307, 1331197296, week, S2)),
    WeekReadShould =
            [{1328486400,[{566,935,282,935,3037}]},
            {1329091200,[{431,785,323,785,3942}]},
            {1329696000,[{402,795,402,795,1393}]},
            {1330300800,[{262,945,179,945,3245}]},
            {1330905600,[{640,878,640,878,879}]}],

    if
        WeekReadShould == WeekReadRes ->
            io:format("read week scale check: ok~n");
        true ->
            io:format("read week scale check: false~n")
    end,

    MonthReadRes = show(read(1328833307, 1331197296, month, S2)),
    MonthReadShould =
            [{1328054400,[{566,945,179,945,11617}]},
            {1330560000,[{640,878,640,878,879}]}],
        
    if
        MonthReadShould == MonthReadRes ->
            io:format("read month scale check: ok~n");
        true ->
            io:format("read month scale check: false~n")
    end,

    ok.
