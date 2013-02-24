%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Solve Rosalind, GC 4: Computing GC content
%%% @end
%%% Created :  23 Feb 2013 by John Daily <jd@epep.us>

-module(macintux_gc4).

-export([gc_parse_file/1, calculate_gc/1]).

-record(state, {
          current_label=undefined,
          accum=[],
          top_label=undefined,
          top_gc=0.0,
          re_init,
          re_cont
         }).

%% Primary entry point, takes file handle as argument
gc_parse_file(Fh) ->
    gc_parse_line(Fh, io:get_line(Fh, ""), new_state()).

new_state() ->
    {ok, Init} = re:compile("^>\\s*(\\S+)\\s*($|(A|C|G|T)+)"),
    {ok, Cont} = re:compile("^\\s*((A|C|G|T)+)\\s*$"),
    #state{re_init=Init, re_cont=Cont}.

calculate_gc(String) ->
    calculate_gc(String, 0, 0).

calculate_gc([], GC, Total) ->
    GC / Total;
calculate_gc([$G|T], GC, Total) ->
    calculate_gc(T, GC+1, Total+1);
calculate_gc([$C|T], GC, Total) ->
    calculate_gc(T, GC+1, Total+1);
calculate_gc([_H|T], GC, Total) ->
    calculate_gc(T, GC, Total+1).


gc_parse_line(_Fh, eof, #state{accum=Accum}=State) ->
    LastState = pick_best_gc(calculate_gc(Accum), State),
    {LastState#state.top_label,
     lists:flatten(io_lib:format("~.5f", [100 * LastState#state.top_gc]))};
gc_parse_line(_Fh, {error, Error}, _State) ->
    {error, Error};
gc_parse_line(Fh, Line, #state{re_init=Init, re_cont=Cont}=State) ->
    gc_handle_line(Fh, Line, re:run(Line, Init), re:run(Line, Cont), State).


pick_best_gc(NewGC, #state{current_label=Label, top_gc=TopGC}=State) when NewGC > TopGC ->
    State#state{top_label=Label, top_gc=NewGC, accum=[]};
pick_best_gc(_NewGC, State) ->
    State#state{accum=[]}.

%% First clause means we've hit the first line of data
gc_handle_line(Fh, Line, {match, Matches}, nomatch, #state{current_label=undefined}=State) ->
    {Label, DNA} = interpret_init(Line, Matches),
    gc_parse_line(Fh, io:get_line(Fh, ""), State#state{current_label=Label,
                                                       accum=DNA});
%% This next clause means we've found a new data block and we have to
%% compare the accumulated DNA string against our current best GC
%% percentage
gc_handle_line(Fh, Line, {match, Matches}, nomatch, #state{accum=Accum}=State) ->
    {Label, DNA} = interpret_init(Line, Matches),
    Best = pick_best_gc(calculate_gc(Accum), State),
    gc_parse_line(Fh, io:get_line(Fh, ""), Best#state{current_label=Label, accum=DNA});
%% This clause is for a nested line of data
gc_handle_line(Fh, Line, nomatch, {match, Matches}, #state{accum=Accum}=State) ->
    gc_parse_line(Fh, io:get_line(Fh, ""), State#state{accum=Accum ++ interpret_nested(Line, Matches)});
%% Ignore lines that don't match anything
gc_handle_line(Fh, _Line, nomatch, nomatch, State) ->
    gc_parse_line(Fh, io:get_line(Fh, ""), State).

interpret_nested(Line, [_All, {Index, Length} | _T]) ->
    string:substr(Line, Index+1, Length).

interpret_init(Line, [_All, {LabelIndex, LabelLength}, {_Index, 0} | _T]) ->
    { string:substr(Line, LabelIndex+1, LabelLength), "" };
interpret_init(Line, [_All, {LabelIndex, LabelLength}, {DNAIndex, DNALength} | _T]) ->
    { string:substr(Line, LabelIndex+1, LabelLength),
      string:substr(Line, DNAIndex+1, DNALength) }.
