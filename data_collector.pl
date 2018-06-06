/*
 * Name:        data_collector.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file collects and collates information on false positives from batch testing trials using the system implementation in 'qRRL.pl'.
 */

:- dynamic results_filter/5, results_tp_count/1, results_trials/1, results_time_g/1, results_time_b/1.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorting false positives -- identifying overspecificity

% Overspecificity type 1: Overfitting.
% e.g. the axiom 'x if y+z' to 'x if y+z+a'.
% These currently only appear during noise trials, and we count them as false positives.
% This is already tested for in the clause above.

% Overspecificity type 2: Subset negation.
% e.g. not(value_of(foo,bar)) in the target axiom to value_of(foo,dol) in the learned axiom.
% Our approach has been to ignore these as neither true positive nor genuine false positives.
% Need to add code for automatically finding these.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function for each level of filtering prints a row to the file post_analysis.txt in the format
% W: X / Y, Z
% where W is how many filters have been used, X and Y are the number of false positives overspecifying true target axioms, and Z is the number of 'genuine' false positives.
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
	(results_filter(Index,OST1,OST2,OST1and2,FP)
		-> (OverspecType1 = OST1, OverspecType2 = OST2, OverspecTypeBoth = OST1and2, FPs = FP)
		; (OverspecType1 = 0, OverspecType2 = 0, OverspecTypeBoth = 0, FPs = 0) ),
	sort(Yes,Y1),
	sort(No,N1),
	getOverspecificityInformation(Y1,N1,List),
	(List = [] ->    (NewOST1 is OverspecType1, NewOST2 is OverspecType2, NewOSTBoth is OverspecTypeBoth, NewFPs is FPs+1) ; true),
	(List = [1] ->   (NewOST1 is OverspecType1+1, NewOST2 is OverspecType2, NewOSTBoth is OverspecTypeBoth, NewFPs is FPs) ; true),
	(List = [2] ->   (NewOST1 is OverspecType1, NewOST2 is OverspecType2+1, NewOSTBoth is OverspecTypeBoth, NewFPs is FPs) ; true),
	(List = [1,2] -> (NewOST1 is OverspecType1+1, NewOST2 is OverspecType2+1, NewOSTBoth is OverspecTypeBoth, NewFPs is FPs) ; true),
	retractall(results_filter(Index,OverspecType1,OverspecType2,OverspecTypeBoth,FPs)),
	asserta(results_filter(Index,NewOST1,NewOST2,NewOSTBoth,NewFPs)).

getOverspecificityInformation(Y1,N1,[1]) :-
	matchesWithOverspecificityType1Only(Y1,N1),
	!.
getOverspecificityInformation(Y1,N1,[2]) :-
	matchesWithOverspecificityType2Only(Y1,N1),
	!.
getOverspecificityInformation(Y1,N1,[1,2]) :-
	matchesWithOverspecificityType1And2(Y1,N1),
	!.
getOverspecificityInformation(_Y1,_N1,[]).

% Overspecificity type 1: Overfitting.
% Succeeds if the literals of a 'false positive' axiom overfit a target axiom, e.g., 'x if y+z' overfits the target axiom 'x if y+z+a'.
matchesWithOverspecificityType1Only(Y1,N1) :-
	clause(domainAxiomClassifier([YesSubset, NoSubset], _ID),(domainGoalAction(Goal),_TailWithCut)), % Have to find the clause rather than call it because of the cut in the rule
	domainGoalAction(Goal),
	subset(YesSubset,Y1),
	subset(NoSubset,N1),
	!.

% Overspecificity type 2: Subset negation.
% Succeeds if a literal in a 'false positive' axiom specifies a negated value in a target axiom, e.g., 'value_of(foo,dol)' is a subset negation of the target 'not(value_of(foo,bar))'.
matchesWithOverspecificityType2Only(Yes,No) :-
% Note that an output axiom could both be incorrect and appear to have type 2 overspecificity, e.g., 'a if b + c(d)' for the target 'a if e + not(c(f))'.
% So must check that there is a 1-to-1 mapping between target and output axioms' literals such that they are either a perfect match or a subset negation.
% However, now may have to check differing orderings of the literals.
	clause(domainAxiomClassifier([TargetYes, TargetNo], _ID),(domainGoalAction(Goal),_TailWithCut)),
	domainGoalAction(Goal),
	permutation(TargetYes, TargetYesPermutation),
	match_type_2_all(Yes, TargetYesPermutation),
	permutation(TargetNo, TargetNoPermutation),
	match_type_2_all(No, TargetNoPermutation),
	!.

% TestList is a set of literals from the axiom being tested for overspecificity.
% TargetList is a set of literals from a target axiom which may match it.
match_type_2_all([], []).
match_type_2_all([A|TestList], [B|TargetList]) :-
	match_type_2_overspecificity(A,B),
	!,
	match_type_2_all(TestList, TargetList).

match_type_2_overspecificity(X,X) :- !.
% A says something specific of the form Predicate(Arg1...ArgN).
% B says something negated and general of the form not(Predicate(Arg1...ArgN')).
% N  !=  N'
match_type_2_overspecificity(TermA,TermB) :-
	functor(TermA, Predicate, Arity),
	functor(TermB, Predicate, Arity),
	TermA =.. ListA,
	TermB =.. ListB,
	append(StartA, [LastA], ListA),
	append(StartB, [LastB], ListB),
	StartA = StartB,
	LastA \= LastB,
	!.

matchesWithOverspecificityType1And2(Yes,No) :-
% With both types, there is no longer a 1-to-1 mapping.
	clause(domainAxiomClassifier([TargetYes, TargetNo], _ID),(domainGoalAction(Goal),_TailWithCut)),
	domainGoalAction(Goal),
	% Every element of TargetYes has a match_type_2_overspecificity analogue in Yes
	not(( member(YesTargetEl,TargetYes), not(( member(YesEl,Yes), match_type_2_overspecificity(YesEl,YesTargetEl) )) )),
	% Every element of TargetNo has a match_type_2_overspecificity analogue in No
	not(( member(NoTargetEl,TargetNo), not(( member(NoEl,No), match_type_2_overspecificity(NoEl,NoTargetEl) )) )),
	!.

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
	%%%%%
	results_trials(TCount),
	results_tp_count(TPs),
	write(Stream, 'True positives over '),
	write(Stream, TCount),
	write(Stream, ' batch trials: '),
	write(Stream, TPs),
	write(Stream, '\n\n'),
	%%%%%
	write(Stream, 'For each level of filter, overspecifications types 1 and 2, and "real" false positives:\n'),
	number_of_filters(N),
	N2 is N+1,
	printEach(0,N2,Stream),
	write(Stream, '\n\n'),
	%%%%%
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

printEach(CurrentIndex,CurrentIndex,_) :- !.
printEach(CurrentIndex,Finish,Stream) :-
	results_filter(CurrentIndex,Overspec1,Overspec2,OverspecBoth,FPs),
	printRes(Stream,CurrentIndex,Overspec1,Overspec2,OverspecBoth,FPs),
	New is CurrentIndex + 1,
	printEach(New,Finish,Stream).

printRes(Stream,N,Overspec1,Overspec2,OverspecBoth,FPs) :-
	write(Stream, 'Filter '),
	write(Stream, N), write(Stream, ': '),
	write(Stream, Overspec1), write(Stream, ' type 1; '),
	write(Stream, Overspec2), write(Stream, ' type 2; '),
	write(Stream, OverspecBoth), write(Stream, ' type 1&2; '),
	write(Stream, FPs), write(Stream, ' other false positives.\n').

