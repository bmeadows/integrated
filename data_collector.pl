/*
 * Name:        data_collector.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file collects and collates information on false positives from batch testing trials using the system implementation in 'qRRL.pl'.
 */

:- dynamic results_filter/5, results_tp_count/1, results_trials/1, results_time_g/1, results_time_b/1.

% Tracking fields for various data.
results_trials(0).
results_time_g(0).
results_time_b(0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contents
% 1. Main
% 2. Sort data on true positives
% 3. Sort data on false positives
% 4. False positives: 'Overfitting'
% 5. False positives: 'Subset negation'
% 6. False positives: Combined
% 7. Sort data on time costs
% 8. Printing results
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% 1. Main %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function, after a batch or series of Q-RRL trials have been run, analyses and collates the various output data.
% The collated information is printed to the file 'post_analysis.txt'.
batch_collect_data :-
	batch_collect_data_true_pos,
	batch_collect_data_false_pos,
	batch_collect_data_time,
	print_result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% 2. Sort data on true positives %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function prints a row to the file post_analysis.txt in the format
% True positives over N batch trials: X
% where N is the number of trials and X the sum of true positives.
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% 3. Sort data on false positives %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function, for each level of filtering, prints a row to the file post_analysis.txt in the format
% Filter V: W type 1; X type 2; Y type 1&2; Z other false positives
% where V is a level of filtering,
% W and X are the number of reported false positives that (only) 1- and 2-overspecify true target axioms,
% Y is the number exhibiting both kinds of overspecification, and Z is the number of false positives that are not overspecifications.
batch_collect_data_false_pos :-
	output_file_error(ErrorFile),
	open(ErrorFile, read, String),
	protocol('data_collector_error_reporting.txt'), % Send any debugging data to this file.
	read_all_FPs(String).

% Process the false positives stored in the output error file.
read_all_FPs(File) :-
    read_line_to_codes(File, Codes),
	Codes \= end_of_file,
	atom_string(ATOM, Codes),
	atomic_list_concat(SubStringList, ' ', ATOM),
	SubStringList = [_FilterString,NumberString,ActualString],
	begin_check_string_for_FPs(NumberString,ActualString),
	!,
	read_all_FPs(File).
read_all_FPs(_).

begin_check_string_for_FPs(NumberString,ActualString) :-
	split_string(NumberString, "]", "", [Head|_Tail]), atom_number(Head, ResultNum),
	check_string_for_FPs(ResultNum, ActualString),
	!.
begin_check_string_for_FPs(_,_). % Always succeed, so never stop in the case where a line does not contain a square bracket.

% Establish whether each 'false positive' axiom returned is an overspecification of a target axiom, or another kind of false positive.
% === Overspecificity type 1: Overfitting. ===
% Succeeds if the literals of a 'false positive' axiom overfit a target axiom, e.g., 'x if y+z' overfits the target axiom 'x if y+z+a'.
% (The architecture is by default configured sufficiently strictly that these will only appear during noise trials.)
% === Overspecificity type 2: Subset negation. ===
% Succeeds if a literal in a 'false positive' axiom specifies a negated value in a target axiom, e.g., 'value_of(a,b)' is a subset negation of the target 'not(value_of(a,c))'.
check_string_for_FPs(Index, Atom) :-
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
	(List = [1,2] -> (NewOST1 is OverspecType1, NewOST2 is OverspecType2, NewOSTBoth is OverspecTypeBoth+1, NewFPs is FPs) ; true),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% 4. False positives: 'Overfitting' %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Overspecificity type 1: Overfitting.
% This function succeeds if the literals of a 'false positive' axiom overfit a target axiom, e.g., 'x if y+z' overfits the target axiom 'x if y+z+a'.
matchesWithOverspecificityType1Only(Yes,No) :-
	clause(domainAxiomClassifier([YesSubset, NoSubset], AxiomID),(domainGoalAction(Goal),_TailWithCut)), % Have to find the clause rather than call it because of the cut in the rule
	AxiomID \= ignore_axiom, % Only consider target axioms. Do not count overspecifications of axioms that use this flag (e.g., they would be true, but duplicate already known rules).
	domainGoalAction(Goal),
	subset(YesSubset,Yes),
	subset(NoSubset,No),
	%%print([Yes,No]), write(' is a type 1 / overfitting of '), print(AxiomID), write('\n'),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% 5. False positives: 'Subset negation' %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Overspecificity type 2: Subset negation.
% This function succeeds if literals in a 'false positive' axiom specify a negated value in a target axiom, e.g., 'value_of(foo,dol)' is a subset negation of the target 'not(value_of(foo,bar))'.
matchesWithOverspecificityType2Only(Yes,No) :-
% Note that an output axiom could both be incorrect and appear to have type 2 overspecificity, e.g., 'a if b + c(d)' for the target 'a if e + not(c(f))'.
% This necessitates checking that there is a 1-to-1 mapping between target and output axioms' literals such that they are either a perfect match or a subset negation.
% However, now may have to check differing orderings of the literals.
	clause(domainAxiomClassifier([TargetYes, TargetNo], AxiomID),(domainGoalAction(Goal),_TailWithCut)),
	AxiomID \= ignore_axiom, % Only consider target axioms. Do not count overspecifications of axioms that use this flag (e.g., axioms that accurately duplicate already known rules of the domain).
	domainGoalAction(Goal),
	permutation(TargetYes, TargetYesPermutation),
	match_type_2_all(Yes, TargetYesPermutation),
	permutation(TargetNo, TargetNoPermutation),
	match_type_2_all(No, TargetNoPermutation),
	%%print([Yes,No]), write(' is a type 2 / subset-neg of '), print(AxiomID), write('\n'),
	!.

% TestList is a set of literals from the axiom being tested for overspecificity.
% TargetList is a set of literals from a target axiom which may match it.
match_type_2_all([], []).
match_type_2_all([A|TestList], [B|TargetList]) :-
	match_type_2_or_equal(A,B),
	!,
	match_type_2_all(TestList, TargetList).

match_type_2_or_equal(X,X) :- !.
% A says something specific of the form Predicate(Arg1...ArgN).
% B says something negated and general of the form not(Predicate(Arg1...ArgN')).
% N  !=  N'
% Therefore, A necessarily holds when B holds.
match_type_2_or_equal(TermA,not(TermB)) :-
	functor(TermA, Predicate, Arity),
	functor(TermB, Predicate, Arity),
	TermA =.. ListA,
	TermB =.. ListB,
	append(StartA, [LastA], ListA),
	append(StartB, [LastB], ListB),
	StartA = StartB,
	LastA \= LastB,
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% 6. False positives: combined %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function checks whether the provided sets of positive and negative literals match a target axiom only by 1-overspecifying and 2-overspecifying it.
% Note when both 1- and 2-overspecification occurs, there is no longer a 1-to-1 mapping between the literals of the output and target axioms.
matchesWithOverspecificityType1And2(YesLiterals,NoLiterals) :-
	% 1. Check specifically for overspecificity type 1, otherwise this function will also return true for cases with type 1 only, because of 'match_type_2_or_equal(X,X) :- !.'
	not(matchesWithOverspecificityType1Only(YesLiterals,NoLiterals)),
	% 2. Then check for the presence of a 2-overspecification of each literal in some target axiom.
	clause(domainAxiomClassifier([TargetYes, TargetNo], _ID),(domainGoalAction(Goal),_TailWithCut)),
	domainGoalAction(Goal),
	% Every element of TargetYes has a match_type_2_or_equal analogue in YesLiterals
	not(( member(YesTargetEl,TargetYes), not(( member(YesEl,YesLiterals), match_type_2_or_equal(YesEl,YesTargetEl) )) )),
	% Every element of TargetNo has a match_type_2_or_equal analogue in NoLiterals
	not(( member(NoTargetEl,TargetNo), not(( member(NoEl,NoLiterals), match_type_2_or_equal(NoEl,NoTargetEl) )) )),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% 7. Sort data on time costs %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function, for the 'b' (BDT building) and 'g' (generalisation) times, prints to the file post_analysis.txt in the format
% Time b: W (mean X)
% Time g: Y (mean Z)
% where W is the total 'b' time, X is the average 'b' time, Y is the total 'g' time, and Z is the average 'g' time. This is done for data from sets of trials using the provided batch functions.
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
	update_CPU_time(SymbolAtom,NumberAtom),
	!,
	read_all_times(File).
read_all_times(_).

update_CPU_time('b', NumberAtom) :-
	results_time_b(Current),
	atom_string(NumberAtom, NumberString),
	number_string(Number, NumberString),
	New is Current + Number,
	retractall(results_time_b(Current)),
	asserta(results_time_b(New)).

update_CPU_time('g', NumberAtom) :-
	results_time_g(Current),
	atom_string(NumberAtom, NumberString),
	number_string(Number, NumberString),
	New is Current + Number,
	retractall(results_time_g(Current)),
	asserta(results_time_g(New)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% 8. Printing results %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Write the collated data to 'post_analysis.txt'.
print_result :-
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
	write(Stream, ' (mean '),
	MeanB is TimeB/TCount,
	write(Stream, MeanB),
	write(Stream, ')\n'),
	results_time_g(TimeG),
	write(Stream, 'Time g: '),
	write(Stream, TimeG),
	write(Stream, ' (mean '),
	MeanG is TimeG/TCount,
	write(Stream, MeanG),
	write(Stream, ')\n'),
	close(Stream).

printEach(CurrentIndex,CurrentIndex,_) :- !.
printEach(CurrentIndex,Finish,Stream) :-
	results_filter(CurrentIndex,Overspec1,Overspec2,OverspecBoth,FPs),
	printResultLine(Stream,CurrentIndex,Overspec1,Overspec2,OverspecBoth,FPs),
	New is CurrentIndex + 1,
	printEach(New,Finish,Stream).

printResultLine(Stream,N,Overspec1,Overspec2,OverspecBoth,FPs) :-
	write(Stream, 'Filter '),
	write(Stream, N), write(Stream, ': '),
	write(Stream, Overspec1), write(Stream, ' type 1; '),
	write(Stream, Overspec2), write(Stream, ' type 2; '),
	write(Stream, OverspecBoth), write(Stream, ' type 1&2; '),
	write(Stream, FPs), write(Stream, ' other false positives.\n').
