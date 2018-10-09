-module(sort).
-export([
    ms/1
]).


ms([]) ->
    [];
ms([A | []]) ->
    [A];
ms(List) ->
    Len = length(List) div 2,
    L = ms(lists:sublist(List, 1, Len)),
    R = ms(lists:sublist(List, Len + 1, Len + 1)),
    merge(L, R, []).


merge([LH | LT], [ RH | RT] , C) ->
    if 
        LH < RH -> merge(LT, [RH | RT], [LH | C]);
        true -> merge([LH | LT] , RT, [RH | C])
    end;
merge([], R, C) ->
    lists:append(lists:reverse(C) ,R);
merge(L,[], C) ->
    lists:append(lists:reverse(C) ,L);
merge([], [], C) ->
    lists:reverse(C).