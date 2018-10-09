-module(db).
-export([
    new/0,
    destroy/1,
    write/3,
    delete/2,
    read/2,
    match/2
]).

new() -> 
    [].

destroy(_) ->
    ok.

write(Key, Element, DbRef) ->
    [{Key, Element} | delete(Key, DbRef)].



delete(Key, [{Key, _} | T]) ->
    T;
delete(Key, [P | T]) ->
    [P|delete(Key,T)].


read(Key, [{Key, Element} | _ ]) ->
    {ok, Element};
read(_, []) ->
    {error, instance};
read(Key, [_ | T]) ->
    read(Key, T).

match(_, [], L) ->
    L;
match(V, [{K, V} | T], L) ->
    match(V, T, [K | L]);
match(V, [_ | T] , L) ->
    match(V, T, L).

match(Element, DbRef) ->
    match(Element, DbRef, []).

% db:write(a, 1, db:write(b, 2, (db:write(c, 3, db:new())))).
