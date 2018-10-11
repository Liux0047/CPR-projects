-module(db_ets).
-export([
    new/0,
    destroy/1,
    write/3,
    delete/2,
    read/2,
    match/2
]).

-record(person, {name, country}).


new() -> 
    ets:new(persons, [set, named_table, {keypos, #person.name}]).

destroy(_) ->
    ok.

write(Key, Element, _) ->
    ets:insert(persons, #person{name=Key, country=Element}).

delete(Key, _) ->
    ets:delete(persons, Key).


read(Key, _) ->
    ets:lookup(persons, Key).

match(Element, _) ->
    ets:match(persons, #person{name='$0', country=Element}).

% db_ets:write(a, 1, db_ets:write(b, 2, (db_ets:write(c, 3, db_ets:new())))).
