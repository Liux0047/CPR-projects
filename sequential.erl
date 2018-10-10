-module(sequential).
-export([
    sum/1,
    sum_interval/2,
    create/1,
    reverse_create/1,
    print_list/1,
    even_print/1
]).

sum(1) -> 1;
sum(N) when N > 1 -> 
    N + sum(N-1).

sum_interval(M, M) -> M;
sum_interval(N, M) when N < M ->
    M + sum_interval(N, M-1).

reverse_create(0) -> [];
reverse_create(V) when V > 0 ->
    [V | reverse_create(V-1)].

create(V) ->
    create(V, []).

create(0, Buff) -> Buff;
create(V, Buff) ->
    create(V-1, [V|Buff]).


print_list(1) ->
    io:format("Number: 1~n");
print_list(N) when N > 1 ->
    print_list(N -1),
    io:format("Number: ~p~n", [N]).

even_print(2) -> 
    io:format("Number: 2~n");
even_print(N) when N rem 2 == 0-> 
    even_print(N - 1),
    io:format("Number: ~p~n", [N]);
even_print(N) -> 
    even_print(N-1).
