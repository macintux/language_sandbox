%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%   Rosalind 5 - Hamming distance
%%% @end
%%% Created : 23 Feb 2013 by John Daily <jd@epep.us>

-module(macintux_hamm5).
-export([hamming/2]).


hamming(S1, S2) when length(S1) =/= length(S2) ->
    string_lengths_must_match;
hamming(S1, S2) ->
    hamming(S1, S2, 0).

hamming([], [], Distance) ->
    Distance;
hamming([H|T1], [H|T2], Distance) ->
    hamming(T1, T2, Distance);
hamming([_H1|T1], [_H2|T2], Distance) ->
    hamming(T1, T2, Distance+1).
