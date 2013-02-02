%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Solve Rosalind, RNA 2: translate all T to U
%%% @end
%%% Created :  2 Feb 2013 by John Daily <jd@epep.us>

-module(macintux_rna2).

-export([dna2rna/1]).

%% @doc Translate a DNA string to RNA by converting each T to
%% U. Display results to the console.
%%
%% Example:
%% dna2rna("GATGGAACT").
%% GAUGGAACT
dna2rna([]) ->
    io:format("~n");
dna2rna([$T|T]) ->
    io:format("U"),
    dna2rna(T);
dna2rna([H|T]) ->
    io:format("~c", [H]),
    dna2rna(T).
