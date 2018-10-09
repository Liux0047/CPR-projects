-module(temp).
-export([f2c/1, c2f/1, convert/1]).

f2c(F) ->
    5 * (F - 32) / 9.

c2f(C) ->
    9 * C / 5 +32.

convert({c, Temp}) ->
    {f, c2f(Temp)};
convert({f, Temp}) ->
    {c ,f2c(Temp)}.