%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by John Daily <jd@epep.us>

-module(fib).

-compile(export_all).

fib(1, _) ->
    1;
fib(2, _) ->
    1;
fib(N, K) ->
    K*fib(N-2, K) + fib(N-1, K).
