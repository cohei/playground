-module(temp).
-compile(export_all).
%-export([start/0, fact/1, loop/0, start/1, clock/2]).

start1() ->
    spawn(fun() -> loop() end).

start2() ->
    spawn(fun() -> reg() end).

reg() ->
    register(serv2, self()),
    loop().

call(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        Return -> Return
    end.

loop() ->
    receive
        {From, {add, X, Y}} ->
            From ! (X + Y),
            loop();
        {From, {mul, X, Y}} ->
            From ! (X * Y),
            loop();
        {From, stop} ->
            io:format("stop server~n"),
            From ! {ok, stop_server};
        {From, Other} ->
            io:format("~p is not supported. stop server~n", [Other]),
            From ! {get_unknown_type, server_killed}
    end.
