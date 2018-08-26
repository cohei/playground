-module(basic).
-export([mirror/1, plusOne/1]).

mirror(Anything) ->
    Anything.

plusOne(n) ->
    n + 1.
