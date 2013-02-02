%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Solve Rosalind, REVC 3: reverse complement a DNA string
%%% @end
%%% Created :  2 Feb 2013 by John Daily <jd@epep.us>

-module(macintux_revc3).

-export([revc/1]).

%% @doc Take the reverse complement of a DNA string. Reverse the
%% string, then replace each character with its complement ('A' <->
%% 'T', 'C' <-> 'G'.
%%
%% Outputs the new string to the console.
%%
%% Example:
%% revc("GATGGAACT").
%% AGTTCCATC
revc(String) ->
    console(complement(String, [])).

%% Tail-recursive complement will reverse the string while processing
complement([], Accum) ->
    Accum;
complement([$A|T], Accum) ->
    complement(T, [$T|Accum]);
complement([$T|T], Accum) ->
    complement(T, [$A|Accum]);
complement([$G|T], Accum) ->
    complement(T, [$C|Accum]);
complement([$C|T], Accum) ->
    complement(T, [$G|Accum]).

console(String) ->
    io:format("~s~n", [String]).
