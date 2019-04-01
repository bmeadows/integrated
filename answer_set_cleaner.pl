
answer_set_file('a_s.txt').

% Includes
:- [pretty_printer].

%test :- clean_answer_sets('answersetstest.txt', Return, false), print(Return), nl, nl.

% Replaces answer sets. Replaces ASP style curly bracket sets with Prolog style sublists of list.
clean_and_replace_answer_sets(File) :-
	clean_answer_sets(File, Return, false),
	open(File, write, O),
	writeln(O, Return),
	nl(O),
	close(O).

% Succeeds on empty answer set but not on null answer set
clean_answer_sets(File, Return, PrintToFile) :-
	% 1. Get text
	read_file_to_string(File, RawString, []),
	% 2. Translate from ASP form to Prolog form
	Separators = "\n",
	Pads = "\s\t\n",
	split_string(RawString, Separators, Pads, CurlyBracedSubStringList),
	curlyBracedToPrologListAll(CurlyBracedSubStringList, SubLists),
	pairWithValues(SubLists, SubListsPaired),
	reduceToMinimal(SubListsPaired, MinimalSubLists),
	orderCorrectly(MinimalSubLists, Return),
	printSets(PrintToFile, Return).

curlyBracedToPrologListAll([], []).
curlyBracedToPrologListAll([A|B], [HeadAtomicList|Tail]) :-
	split_string(A, "{}", "", [_Firstbracket,P0,_Lastbracket]),
	string_concat("[", P0, P1),
	string_concat(P1, "]", P2),
	atom_codes(P3, P2), % "string" to 'string'
	atom_to_term(P3, HeadAtomicList, []),
	curlyBracedToPrologListAll(B, Tail).
	
printSets(false, Content) :-
	prettyprint('Answer sets cleaned. '),
	length(Content,Num), prettyprint(Num),
	prettyprintln(' set(s) remaining.').
printSets(true, Content) :-
	answer_set_file(ASF),
	open(ASF, write, O),
	printEachSet(O,Content),
	close(O),
	prettyprint('Answer sets cleaned. '),
	length(Content,Num), prettyprint(Num),
	prettyprint(' set(s) remaining. Written out to '),
	prettyprint(ASF),
	prettyprintln('.').
printEachSet(_,[]).
printEachSet(O,[A|B]) :-
	writeln(O, A),
	nl(O),
	printEachSet(O,B).

%%%%%%%%%%%%%%%%%%%%%%

% Assign plans costs equal to number of actions
pairWithValues([], []).
pairWithValues([SubList1|RemainderSubLists], [SubListPair1|RemainderSubListsPaired]) :-
	countActions(SubList1, Count),
	SubListPair1 = [SubList1, Count],
	pairWithValues(RemainderSubLists, RemainderSubListsPaired).

countActions(SubList, Count) :-
	findall(Step, (member(occurs(_,Step), SubList)), Total),
	length(Total, Count).

% Reduce answer sets to minimal cost ones (plan steps and other literals)
% Do not include paired values with final sublists
reduceToMinimal(SubListsPaired, MinimalSubLists) :-
	findall(L, (member([_, L], SubListsPaired)), Lengths),
	member(N, Lengths),
	not(( member(M, Lengths), M < N )),
	!,
	findall(Set, (member([Set, N], SubListsPaired)), MinimalSubLists).


% Order remaining sets by time steps (starting with 'occurs')
orderCorrectly([], []).
orderCorrectly([List|Others], [Ordered|OthersOrdered]) :-
	findall(El1, (member(El1, List), El1 \= occurs(_,_)), Els1),
	findall(El2, (member(El2, List), El2  = occurs(_,_)), Els2),
	%trace,
	predsort(compareSteps, Els1, Els1Sorted),
	predsort(compareSteps, Els2, Els2Sorted),
	append(Els2Sorted, Els1Sorted, Ordered),
	orderCorrectly(Others, OthersOrdered).

%%%%%%%%%%%%%%%%%%%%%%
% These cases are incomparable - say they are larger and thus appear at the end of the list.
compareSteps(COMP, Arg1, Arg2) :- atomic(Arg1), atomic(Arg2), compare(COMP, Arg1, Arg2), !.
compareSteps(>, Arg1, _) :- atomic(Arg1), !.
compareSteps(<, _, Arg2) :- atomic(Arg2), !.
%%%%%%%%%%%%%%%%%%%%%%
compareSteps(>, Arg1, Arg2) :-	functor(Arg1, _Pred1, 2), arg(2, Arg1, Value1), number(Value1), 
								functor(Arg2, _Pred2, 2), arg(2, Arg2, Value2), number(Value2), 
								Value1 > Value2, !.
compareSteps(<, Arg1, Arg2) :-	functor(Arg1, _Pred1, 2), arg(2, Arg1, Value1), number(Value1), 
								functor(Arg2, _Pred2, 2), arg(2, Arg2, Value2), number(Value2), 
								Value1 < Value2, !.
compareSteps(COMP, Arg1, Arg2) :-	functor(Arg1, _Pred1, 2), arg(2, Arg1, Value1), number(Value1), 
								functor(Arg2, _Pred2, 2), arg(2, Arg2, Value2), number(Value2), 
								Value1 = Value2,
								arg(1, Arg1, A1), arg(1, Arg2, A2),
								compare(COMP, A1, A2), !.
%%%%%%%%%%%%%%%%%%%%%%
compareSteps(Return, -Arg1, -Arg2) :- compareSteps(Return, Arg1, Arg2), !.
compareSteps(Return, -Arg1, Arg2) :- compareSteps(Return, Arg1, Arg2), !.
compareSteps(Return, Arg1, -Arg2) :- compareSteps(Return, Arg1, Arg2), !.
%%%%%%%%%%%%%%%%%%%%%%
compareSteps(>, _, Arg2) :-	functor(Arg2, _Pred, 2), arg(2, Arg2, Value2), number(Value2), 
								!. % Implicitly, Arg1 does not meet the conditions - wrong arity or not a number. So say it is larger and thus appears at the end of the list.
compareSteps(<, Arg1, _) :-	functor(Arg1, _Pred, 2), arg(2, Arg1, Value1), number(Value1), 
								!. % Implicitly, Arg2 does not meet the conditions - wrong arity or not a number. So say it is larger and thus appears at the end of the list.
compareSteps(COMP, Arg1, Arg2) :- compare(COMP, Arg1, Arg2).
%%%%%%%%%%%%%%%%%%%%%%

