-module(echo).
-export([start/0, init/0]).

start() ->
    register(echoid, spawn(echo, init, [])),
    echoid ! {print, "Hello print"},
    ok. 

init() ->
    loop().

loop() -> 
    receive
        {print, Msg} ->
            io:format(Msg, []),
            loop();
        stop -> terminate()
    after 50000 -> terminate()
    end.

terminate() ->
    ok.