%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Solve Rosalind, DNA 1: count elements of a character string
%%% @end
%%% Created :  2 Feb 2013 by John Daily <jd@epep.us>

-module(macintux_dna1).

-export([count/1, count/2]).

%% @doc Count the number of DNA nucleotides in a string. Displays to
%% the console integers representing the count of each of the "ACGT"
%% elements in that order.
%%
%% Example:
%% count("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC").
%% 20 12 17 21
count(String) ->
    count(String, "ACGT").

%% @doc Count the elements of any arbitrary list of data. Displays to
%% the console integers representing the count of each element in the
%% order defined by the list passed as the second argument.
%%
%% It is not necessary to pass all elements of the first list in the
%% second list; any data items not in the second list will not be
%% reported.
%%
%% Any elements in the second list but not in the first will be
%% reported with a count of 0.
%%
%% Examples:
%% count([{3}, {4}, {3}, {1}, {3}, {4}], [{3}, {4}, {1}]).
%% 3 2 1
%%
%% count("AGCTTTTCA", "AZ").
%% 2 0
count(Data, Order) ->
    interpret(count_elements(Data, orddict:new()), Order).

%% Recursively loop through the list of data, counting each element
count_elements([], Dict) ->
    Dict;
count_elements([H|T], Dict) ->
    count_elements(T, orddict:update(H, fun(Count) -> Count + 1 end, 1, Dict)).

%% Recursively loop through the list of elements to report, and
%% display each count
interpret(_Dict, []) ->
    io:format("~n");
interpret(Dict, [H|T]) ->
    io:format("~B ", [ find_count(orddict:find(H, Dict)) ]),
    interpret(Dict, T).

%% orddict:find() will return an error if a value is not present in
%% the dictionary
find_count(error) ->
    0;
find_count({ok, Count}) ->
    Count.
