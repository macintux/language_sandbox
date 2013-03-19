%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by John Daily <jd@epep.us>

-module(fib).

-compile(export_all).

%% Traditional Fibonacci with one minor twist: add a multiplication
%% factor for the rabbit problem
fib(1, _) ->
    1;
fib(2, _) ->
    1;
fib(N, K) ->
    K*fib(N-2, K) + fib(N-1, K).

%% Let's build from the ground up with this one. Much, much faster.
fib2(N, K) ->
    fib2(3, N, K, 1, 1).

fib2(_Top, _Top, K, Accum, TwoLast) ->
    Accum + K*TwoLast;
fib2(Cur, Top, K, Accum, TwoLast) ->
    ThisGen = K*TwoLast,
    fib2(Cur + 1, Top, K, Accum + ThisGen, Accum).
