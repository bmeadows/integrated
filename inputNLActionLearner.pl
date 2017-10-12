:- dynamic obs_ex_action/3, obs_ex_action_tagged/3, exoActionDescription/4, args/1.

% Includes
:- include('wordnet/wn_s.pl').
:- include('wordnet/wn_sim.pl').
:- [verbs,rrl_domain,pretty_printer].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Examples of 'learnable' action descriptions already known, to demonstrate generalisation
%exoActionDescription(file(P, F, C), [P, F, C], [manager, book, furniture], [not(in_hand(P,F)),in(F,C),open(C,false)]).
%exoActionDescription(file_alternative(P, F, C), [P, F, C], [salesperson, item, cabinet], [not(in_hand(P,F)),in(F,C),open(C,false)]).
%exoActionDescription(file(P, F, C), [P, F, C], [salesperson, item, cabinet], [not(in_hand(P,C))]).

exoActionDescription(example_pickup(P, I), [P, I], [person, item], [in_hand(P,I)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

inputText("The_DT influential_JJ engineer_NN is_VBZ filing_VBG the_DT solid_JJ green_JJ folder_NN in_IN the_DT dense_JJ white_JJ locker_NN ._.").
inputLiterals([not(in_hand(p2,fol1)),in(fol1,cab1),open(cab1,false)]).

inputText("The_DT salesperson_NN is_VBZ tearing_VBG up_RP the_DT red_JJ folder_NN ._.").
inputLiterals([item_status(fol2,damaged)]).

inputText("The_DT authoritative_JJ salesperson_NN is_VBZ installing_VBG the_DT printer_NN on_IN the_DT light_JJ desk_NN ._.").
inputLiterals([loc(prin1,desk1),labelled(prin1,true)]).

obs_ex_action_tagged(	"The_DT unimportant_JJ engineer_NN is_VBZ polishing_VBG the_DT metallic_ADJ table_NN ._.",
					[reflectivity(tab1,bright)],
					1).
*/

/*
inputText("The_DT important_JJ engineer_NN is_VBZ balancing_VBG the_DT breakable_JJ cup_NN on_IN the_DT lightweight_JJ ledger_NN ._.").
inputLiterals([not(in_hand(p2,cup1)), loc(cup1,book1)]).

The influential engineer is filing the solid green folder in the dense white locker.
The salesperson is tearing up the red folder.
The authoritative salesperson is installing the printer on the light desk.
The unimportant engineer is polishing the metallic table.
The important engineer is balancing the breakable cup on the lightweight ledger.


exoActionDescription(file(_9974,_9976,_9978),[_9974,_9976,_9978],[person,item,furniture],[not(in_hand(_9974,_9976)),in(_9976,_9978),open(_9978,false)])
exoActionDescription(tear(_8590,_8592),[_8590,_8592],[salesperson,folder],[item_status(_8592,damaged)])
exoActionDescription(install(_9254,_9256,_9258),[_9254,_9256,_9258],[salesperson,printer,desk],[loc(_9256,_9258),labelled(_9256,true)])
exoActionDescription(polish(_8584,_8586),[_8584,_8586],[engineer,table],[reflectivity(_8586,bright)])
exoActionDescription(balance(_9442,_9444,_9446),[_9442,_9444,_9446],[engineer,cup,book],[not(in_hand(_9442,_9444)),loc(_9444,_9446)])

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%1. Check buffer of unprocessed observations from the previous cycle (or... on interrupt) for tuples <string, [0 or more logical literals]>
learnFromDescs :-
	findall([A,B,C], obs_ex_action_tagged(A,B,C), X),
	learnFromActionDescs(X),
	retractall(obs_ex_action_tagged(_,_,_)).

%2. Perform the following for each tuple in turn
%3. Parse string into list of POS-tagged words
learnFromActionDescs([]) :- translateLearnedActionDescsToPrologRules.
learnFromActionDescs([A|B]) :-
	A = [String,LiteralEffects,_Step],
	prettyprintstars,
	prettyprint('Tagged input string: '),
	prettyprintln(String),
	prettyprintstars,
	split_string(String, " ", "", WordsList),
	divideTags(WordsList, PairsListRev),
	reverse(PairsListRev, PairsListTotal),
	prettyprint('Learning from: '),
	prettyprintln(PairsListTotal),
	%4. Pick out verb as action predicate
	(once(select([Verb,"VBG"], PairsListTotal, PairsListElided)) -> true ; trace), % VBZ / VBG, depends
	standardizeVerb(Verb,Action),
	prettyprint('Verb: '),
	prettyprintln(Action),
	%5. Pick out sequences of 0 or more adjectives followed by a noun as static attribute values followed by an object sort
	extractObjectDescriptions(PairsListElided, ObjectDescriptions),
	prettyprint('Object descriptions: '),
	prettyprintln(ObjectDescriptions),
	%6. For each such sequence, determine static attributes from values (assume unambiguous)
	%7. For each such sequence, search knowledge of objects for one with correct sort and attribute-value pairs; take first match (assumes uniqueness)
	getOrderedListOfObjectSymbols(ObjectDescriptions, ObjectClassPredicates, OrderedList),
	prettyprint('Objects identified: '),
	prettyprintln(OrderedList),
	% Create instantiated head for new action
	length(OrderedList, Length),
	functor(Head, Action, Length),
	fillHead(Head, OrderedList, 1),
	% Assemble parts into one structure
	GroundedActionDescription = exoActionDescription(Head, OrderedList, ObjectClassPredicates, LiteralEffects),
	% Check if it matches, exactly or partially, against a known structure
	!,
	(exoActionDescription(Head, HeadVariables, OtherClasses, LiteralEffects)
	->
		(reconcile(Head, OrderedList, HeadVariables, ObjectClassPredicates, OtherClasses, LiteralEffects))
	;
		% Lift structure
		(lift(GroundedActionDescription, LiftedActionDescription), assert(LiftedActionDescription))
	),
	currentTime(T),
	assert(hpd(Head,T)),
	!,
	learnFromActionDescs(B).
learnFromActionDescs([A|B]) :-
	prettyprintstars,
	prettyprint('Failed to learn from '),
	prettyprint(A),
	prettyprintln('. Discarding and continuing.'),
	prettyprintstars,
	learnFromActionDescs(B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

fillHead(_Head, [], _Index).
fillHead(Head, [A|B], Index) :-
	arg(Index, Head, A),
	I2 is Index +1,
	fillHead(Head, B, I2).
	
lift(GroundedActionDescription, LiftedActionDescription) :-
	GroundedActionDescription = exoActionDescription(Head, HeadConstants, SortList, EffectsList),
	term_string(Head, HeadString),
	term_string(EffectsList, EffectsString),
	args_to_strings(HeadConstants, StringArgList),
	% A) For each element of StringArgList in turn, replace in Head and Effects by its capitalized form
	recursive_capitalise_arg_strings(HeadString, EffectsString, StringArgList, ResultsHead, ResultsEffects),
	% B) Convert back into terms, so that constants are now variables
	string_concat("[", ResultsHead, String1),
	string_concat(String1, ",", String2),
	string_concat(String2, ResultsEffects, String3),
	string_concat(String3, "]", String4),
	term_string([NewHead,NewEffects], String4),
	term_variables(NewHead, HeadVariables),
	LiftedActionDescription = exoActionDescription(NewHead, HeadVariables, SortList, NewEffects),
	prettyprintstars,
	prettyprint('Adding new action description, lifted: '),
	prettyprintln(LiftedActionDescription),
	prettyprintstars,
	!.

args_to_strings([], []).
args_to_strings([A|B], [NewElement|StringArgList]) :-
	term_string(A, NewElement),
	args_to_strings(B, StringArgList).

recursive_capitalise_arg_strings(ResultsHead, ResultsEffects, [], ResultsHead, ResultsEffects).
recursive_capitalise_arg_strings(HeadString, EffectsString, [StringArg|B], ResultsHead, ResultsEffects) :-
	% capitalised form of StringArg is StringArgCAPS... prefix with either '(' or ',' to try to prevent constant name overlap, e.g. 'b1' and 'rob1'
	string_concat("(", StringArg, StringArg1),
	string_upper(StringArg1,StringArgCAPS1),
	string_concat(",", StringArg, StringArg2),
	string_upper(StringArg2,StringArgCAPS2),
	% change HeadString to HeadString2 by replacing each occurrence of StringArg in it with StringArgCAPS
	split_string_custom(HeadString, StringArg1, HeadStringL),
	joinStrings(HeadStringL,StringArgCAPS1,HeadStringL0),
	split_string_custom(HeadStringL0, StringArg2, HeadStringL1),
	joinStrings(HeadStringL1,StringArgCAPS2,HeadString2),
	% change EffectsString to EffectsString2 by replacing each occurrence of StringArg in it with StringArgCAPS
	split_string_custom(EffectsString, StringArg1, EffectsStringL),
	joinStrings(EffectsStringL,StringArgCAPS1,EffectsStringL0),
	split_string_custom(EffectsStringL0, StringArg2, EffectsStringL1),
	joinStrings(EffectsStringL1,StringArgCAPS2,EffectsString2),
	recursive_capitalise_arg_strings(HeadString2, EffectsString2, B, ResultsHead, ResultsEffects).

joinStrings([A], _Joiner, A) :-
	!.
joinStrings([A|B], Joiner, Output) :-
	joinStrings(B, Joiner, Rest),
	string_concat(A, Joiner, A2),
	string_concat(A2, Rest, Output).
	
split_string_custom(InputString, Token, OutputListOfStrings) :-
	split_string_custom_recursive(InputString, Token, OutputListOfStrings).

split_string_custom_recursive(InputString, Token, [X|B]) :-
	sub_string(InputString, Before, Length, After, Token),
	Aft1 is Length + After,
	Bef1 is Before + Length,
	sub_string(InputString, 0, Before, Aft1, X),
	sub_string(InputString, Bef1, After, 0, Y),
	!,
	split_string_custom_recursive(Y, Token, B).
split_string_custom_recursive(In, _, [In]) :- !.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exact match exists
reconcile(_Head, HeadConstants, HeadVariables, ObjectClassPredicates, OtherClasses, _LiteralEffects) :-
	HeadConstants = HeadVariables,
	ObjectClassPredicates = OtherClasses, % If this unification succeeds...
	prettyprintln('Reconcile type 1: Exact match for learned action description already exists'),
	!.
% Have to generalise with partial match
reconcile(Head, HeadConstants, HeadVariables, ObjectClassPredicates, OtherClasses, LiteralEffects) :-
	% Find first common ancestor for each
	reconcileRecursive(ObjectClassPredicates, OtherClasses, New),
	% Replace known structure
	retractall(exoActionDescription(Head, HeadVariables, OtherClasses, LiteralEffects)),
	GroundedActionDescription = exoActionDescription(Head, HeadConstants, New, LiteralEffects),
	prettyprintln('Reconcile type 2: Generalising with existing action description that partially matches'),
	prettyprint('=> Argument sorts for existing description: '),
	prettyprintln(OtherClasses),
	prettyprint('=> Argument sorts for new discovered description: '),
	prettyprintln(ObjectClassPredicates),
	prettyprint('=> Reconciled argument sorts for generalised action description: '),
	prettyprintln(New),
	lift(GroundedActionDescription, LiftedActionDescription),
	assert(LiftedActionDescription),
	!.	

reconcileRecursive([], [], []).
reconcileRecursive([A1|B1], [A2|B2], [A1|Return]) :-
	A1 = A2,
	!,
	reconcileRecursive(B1, B2, Return).
reconcileRecursive([A1|B1], [A2|B2], [Ancestor|Return]) :-
	firstCommonAncestor(A1, A2, Ancestor),
	!,
	reconcileRecursive(B1, B2, Return).

firstCommonAncestor(Pred1, Pred2, Pred1) :-
	ancestor(Pred1,Pred2),
	!.
firstCommonAncestor(Pred1, Pred2, Pred2) :-
	ancestor(Pred2,Pred1),
	!.
firstCommonAncestor(Pred1, Pred2, A) :-
	ancestor(A,Pred1),
	ancestor(A,Pred2),
	not(( ancestor(A,B),ancestor(B,Pred1),ancestor(B,Pred2) )),
	!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%

divideTags(WordsList, PairsList) :-
	divideTagsRecursively(WordsList, [], PairsList).

divideTagsRecursively([], FinalList, FinalList).
divideTagsRecursively([Word|B], TempList, PairsList) :-
	split_string(Word, "_", "", WordTypePair),
	append([WordTypePair], TempList, NewTempList),
	divideTagsRecursively(B, NewTempList, PairsList).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

standardizeVerb(V,Pred) :-
	term_string(VTerm, V),
	vb(Pred, List),
	member(VTerm,List),
	!.
standardizeVerb(V,Pred) :-
	term_string(Pred, V).

/*
Need to use a corpus of English words to get the correct verb form. It would be easier for verb tense 'x foos y': can just use string manipulation:

% Case 1: sses -> ss
standardizeVerb(V,Pred) :-
	string_length(V,StrLength),
	X is StrLength -4,
	Y is StrLength -2,
	sub_string(V, X, 4, 0, "sses"), % Checks that the string ends in "sses"
	!,
	sub_string(V, _Before, Y, 2, V2), % Produces the substring without terminal "es"
	term_string(Pred, V2).
% Case 2: xes -> x
standardizeVerb(V,Pred) :-
	string_length(V,StrLength),
	X is StrLength -3,
	Y is StrLength -2,
	sub_string(V, X, 3, 0, "xes"), % Checks that the string ends in "xes"
	!,
	sub_string(V, _Before, Y, 2, V2), % Produces the substring without terminal "es"
	term_string(Pred, V2).
% Case 3: s ->
standardizeVerb(V,Pred) :-
	string_length(V,StrLength),
	X is StrLength -1,
	sub_string(V, X, 1, 0, "s"), % Checks that the string ends in "s"
	!,
	sub_string(V, _Before, X, 1, V2), % Produces the substring without terminal "s"
	term_string(Pred, V2).
standardizeVerb(V,Pred) :-
	term_string(Pred, V).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractObjectDescriptions(Pairs, ObjectDescriptions) :-
	extractObjectDescriptionsRecursively(Pairs, [], [], ObjectDescriptions).

extractObjectDescriptionsRecursively([], [], FinalDescs, FinalDescs).
extractObjectDescriptionsRecursively([Pair|B], WorkingAds, WorkingDescs, FinalDescs) :-
	% Pair is noun: append WorkingAds to it, add to WorkingDescs, and continues
	Pair = [Word, "NN"],
	!,
	append([Word], WorkingAds, Desc),
	append([Desc], WorkingDescs, NewWorkingDescs),
	extractObjectDescriptionsRecursively(B, [], NewWorkingDescs, FinalDescs).
extractObjectDescriptionsRecursively([Pair|B], WorkingAds, WorkingDescs, FinalDescs) :-
	% Pair is adjective: append to WorkingAds
	Pair = [Word, "JJ"],
	!,
	append([Word], WorkingAds, NewWorkingAds),
	extractObjectDescriptionsRecursively(B, NewWorkingAds, WorkingDescs, FinalDescs).
extractObjectDescriptionsRecursively([_|B], WorkingAds, WorkingDescs, FinalDescs) :-
	% Base case - Pair is irrelevant
	!,
	extractObjectDescriptionsRecursively(B, WorkingAds, WorkingDescs, FinalDescs).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%6. For each such sequence, determine static attributes from values (assume unambiguous)
%7. For each such sequence, search knowledge of objects for one with correct sort and attribute-value pairs; take first match (assumes uniqueness)
getOrderedListOfObjectSymbols(ObjectDescriptions, ObjectClassPredicates, OrderedList) :-
	getOrderedListOfObjectSymbolsRecursive(ObjectDescriptions, [], [], ObjectClassPredicates, OrderedList).

getOrderedListOfObjectSymbolsRecursive([], Final1, Final2, Final1, Final2).
getOrderedListOfObjectSymbolsRecursive([Description|B], WorkingList1, WorkingList2, ObjectClassPredicates, OrderedList) :-
	Description = [Noun|Adjs],
	term_string(InitialPred, Noun),
	translateNounThroughWordNet(InitialPred,Predicate),
	functor(Term, Predicate, 1),
	domain(sort(Term)),
	% !, % Can't cut here to get ancestor sorts because it needs to backtrack to try e.g. person(p1), person(p2), etc, as well as to try different wordnet senses of the noun class itself
	arg(1, Term, ObjectSymbol),
	changeAdjsToTermsAndCheckRecursive(ObjectSymbol, Adjs),
	append([Predicate], WorkingList1, NewWorkingList1),
	append([ObjectSymbol], WorkingList2, NewWorkingList2),
	getOrderedListOfObjectSymbolsRecursive(B, NewWorkingList1, NewWorkingList2, ObjectClassPredicates, OrderedList).

translateT(Word, Term, N) :-
	term_string(Pred, Word),
	functor(Term, Pred, N).
	
changeAdjsToTermsAndCheckRecursive(_, []).
changeAdjsToTermsAndCheckRecursive(ObjectSymbol, [Adj|B]) :-
	translateT(Adj, InitialValue, 0),
	translateAdjThroughWordNet(InitialValue,FinalValue),
	valid(attr(Term)),
	functor(Term, _SomePredicate, 2),
	arg(1, Term, ObjectSymbol),
	arg(2, Term, FinalValue),
	domain(attr(Term)),
	prettyprint(' => '), prettyprintln(Term),
	changeAdjsToTermsAndCheckRecursive(ObjectSymbol, B).

% Note n = noun
translateNounThroughWordNet(FinalValue,FinalValue).
translateNounThroughWordNet(InitialValue,FinalValue) :-
	s(Synset, _RankInSynsetI, InitialValue, n, _SenseNumberI, _TagCountI),
	s(Synset, _RankInSynsetF, FinalValue, n, _SenseNumberF, _TagCountF),
	InitialValue \= FinalValue.

% Note a = adjective, s = adjective satellite
translateAdjThroughWordNet(FinalValue,FinalValue).
translateAdjThroughWordNet(InitialValue,FinalValue) :-
	s(Synset, _RankInSynsetI, InitialValue, Type1, _SenseNumberI, _TagCountI),
	(Type1 = s ; Type1 = a),
	s(Synset, _RankInSynsetF, FinalValue, Type2, _SenseNumberF, _TagCountF),
	(Type2 = s ; Type2 = a),
	InitialValue \= FinalValue.
translateAdjThroughWordNet(InitialValue,FinalValue) :- % Similar adjectival sense sets
	s(Synset1, _RankInSynsetI, InitialValue, Type1, _SenseNumberI, _TagCountI),
	(Type1 = s ; Type1 = a),
	sim(Synset1, Synset2),
	s(Synset2, _RankInSynsetF, FinalValue, Type2, _SenseNumberF, _TagCountF),
	(Type2 = s ; Type2 = a),
	InitialValue \= FinalValue.

translateLearnedActionDescsToPrologRules :- !. % TODO
translateLearnedActionDescsToPrologRules :-
       tell('learned_action_descs.pl'),
       listing(exoActionDescription/4),
       told.
       %tell('exogenous_events.pl'),
       %listing(hpd/2),
       %told.
