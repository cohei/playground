-module(translate).
-export([loop/0, translate/2]).

loop() ->
    receive
        {Pid, Word} ->
            Response =
                case Word of
                    "casa" ->
                        "house";
                    "blanca" ->
                        "white";
                    _ ->
                        "I don't understand."
                end,
            Pid ! Response,
            loop()
    end.

translate(To, Word) ->
    To ! {self(), Word},
    receive
        Translation ->
            Translation
    end.
