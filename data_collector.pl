/*
 * Name:        data_collector.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file collects and collates information on false positives from batch testing trials using the system implementation in 'qRRL.pl'.
 */

:- dynamic results_filter/3, results_tp_count/1, results_trials/1, results_time_g/1, results_time_b/1.

results_trials(0).
results_time_g(0).
results_time_b(0).

% This function, after a batch of Q-RRL has been run, analyses and collates the various output data, printing it to the file post_analysis.txt.
batch_collect_data :-
	batch_collect_data_true_pos,
	batch_collect_data_false_pos,
	batch_collect_data_time,
	printResult.

% This function prints a row to the file post_analysis.txt in the format
% True positives: X
% where X is the sum of true positives over batch trials.
batch_collect_data_true_pos :-
	output_file_counter(CounterFile),
	open(CounterFile, read, Str),
	read_all_TPs(Str, 0).

% Process the true positives stored in the output count file.
read_all_TPs(File, Num) :-
    read_line_to_codes(File, Codes),
	Codes \= end_of_file,
	results_trials(Current),
	(number_string(Number, Codes)
		->
		(NewNum is Num + Number, Current2 is Current +1)
		;
		(NewNum = Num, Current2 = Current)	),
	!,
	retractall(results_trials(Current)),
	asserta(results_trials(Current2)),
	read_all_TPs(File, NewNum).
read_all_TPs(_,Final) :- asserta(results_tp_count(Final)).

% This function for each level of filtering prints a row to the file post_analysis.txt in the format
% X: Y Z
% where X is how many filters have been used, Y is the number of false positives overspecifying true target axioms, and Z is the number of 'genuine' false positives.
batch_collect_data_false_pos :-
	output_file_error(ErrorFile),
	open(ErrorFile, read, Str),
	read_all_FPs(Str).

% Process the false positives stored in the output error file.
read_all_FPs(File) :-
    read_line_to_codes(File, Codes),
	Codes \= end_of_file,
    %split_string(Codes, " ", "", SubStringList),
	atom_string(ATOM, Codes),
	atomic_list_concat(SubStringList, ' ', ATOM),
	SubStringList = [_FilterString,NumberString,ActualString],
	attempt_to_refine_string(NumberString,ActualString),
	!,
	read_all_FPs(File).
read_all_FPs(_).

attempt_to_refine_string(NumberString,ActualString) :-
	split_string(NumberString, "]", "", [Head|_Tail]), atom_number(Head, ResultNum),
	refine_string(ResultNum,ActualString),
	!.
attempt_to_refine_string(_,_). % Always succeed, so never stop in the case where a line does not contain a square bracket

% Establish whether each 'false positive' axiom overfits a target axiom
refine_string(Index, Atom) :-
	atom_to_term(Atom, [Yes,No], _),
	(results_filter(Index,CY,CN)
	-> (CurrentYES = CY, CurrentNO = CN)
	; (CurrentYES = 0, CurrentNO = 0) ),
	sort(Yes,Y1),
	sort(No,N1),
	(
	(matchesWithOverfitting(Y1,N1))
	->
	(NewYes is CurrentYES +1, NewNo is CurrentNO)
	;
	(NewYes is CurrentYES, NewNo is CurrentNO +1)
	),
	retractall(results_filter(Index,CurrentYES,CurrentNO)),
	asserta(results_filter(Index,NewYes,NewNo)).

% Succeeds if the literals of a 'false positive' axiom overfits a target axiom
matchesWithOverfitting(Y1,N1) :-
	clause(domainAxiomClassifier([YesSubset, NoSubset], _ID),(domainGoalAction(Goal),_TailWithCut)), % Have to find the clause rather than call it because of the cut in the rule
	domainGoalAction(Goal),
	subset(YesSubset,Y1),
	subset(NoSubset,N1).
	
% This function, for the 'b' (BDT building) and 'g' (generalisation) times, prints to the file post_analysis.txt in the format
% Time b: W (X)
% Time g: Y (Z)
% where {W is the total 'b' time, X is the average 'b' time, Y is the total 'g' time, Z is the average 'g' time} for batch trials.
batch_collect_data_time :-
	output_file_time(TimeFile),
	open(TimeFile, read, Str),
	read_all_times(Str).

% Process the times stored in the output time file.
read_all_times(File) :-
    read_line_to_codes(File, Codes),
	Codes \= end_of_file,
	atom_string(ATOM, Codes),
	atomic_list_concat(Pair, ' ', ATOM),
	Pair = [SymbolAtom,NumberAtom],
	update_time(SymbolAtom,NumberAtom),
	!,
	read_all_times(File).
read_all_times(_).

update_time('b',NumberAtom) :-
	results_time_b(Current),
	atom_string(NumberAtom, NumberString),
	number_string(Number, NumberString),
	New is Current + Number,
	retractall(results_time_b(Current)),
	asserta(results_time_b(New)).
	
update_time('g',NumberAtom) :-
	results_time_g(Current),
	atom_string(NumberAtom, NumberString),
	number_string(Number, NumberString),
	New is Current + Number,
	retractall(results_time_g(Current)),
	asserta(results_time_g(New)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Send results to 'post_analysis.txt'
printResult :-
	open('post_analysis.txt', write, Stream),
	results_trials(TCount),
	results_tp_count(TPs),
	write(Stream, 'True positives over '),
	write(Stream, TCount),
	write(Stream, ' batch trials: '),
	write(Stream, TPs),
	write(Stream, '\n\n'),
	write(Stream, 'Filter #: Overspecifications, False positives\n'),
	number_of_filters(N),
	N2 is N+1,
	printEach(0,N2,Stream),
	write(Stream, '\n\n'),
	results_time_b(TimeB),
	write(Stream, 'Time b: '),
	write(Stream, TimeB),
	write(Stream, ' ('),
	MeanB is TimeB/TCount,
	write(Stream, MeanB),
	write(Stream, ')\n'),
	results_time_g(TimeG),
	write(Stream, 'Time g: '),
	write(Stream, TimeG),
	write(Stream, ' ('),
	MeanG is TimeG/TCount,
	write(Stream, MeanG),
	write(Stream, ')\n'),
	close(Stream).

printEach(Current,Current,_) :- !.
printEach(Current,Finish,Stream) :-
	results_filter(Current,A,B),
	printRes(Stream,Current,A,B),
	New is Current + 1,
	printEach(New,Finish,Stream).

printRes(Stream,N,A,B) :-
	write(Stream, N), write(Stream, ': '),
	write(Stream, A), write(Stream, ' '),
	write(Stream, B), write(Stream, ' '),
	write(Stream, '\n').

