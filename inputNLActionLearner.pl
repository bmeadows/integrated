:- dynamic obs_ex_action/3, obs_ex_action_tagged/3, exoActionDescription/4, args/1, recurseOnDescs/1, action_syntax/3.

% Includes
:- include('wordnet/wn_s.pl').
:- include('wordnet/wn_sim.pl').
:- include('wordnet/wn_hyp.pl').
:- [verbs,pretty_printer,domain].
% :- [rrl_domain] Does not match current domain version

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

learnFromActionDesc(ReturnedAction) :-
	obs_ex_action_tagged(A,B,C), % Asserted at the point in the control loop when the system is given a new exogenous action description
	learnFromActionDescs([A,B,C], ReturnedAction),
	!.
learnFromActionDesc(no_action_to_learn).

% Check an entire buffer of unprocessed observations from the previous cycle (or... on interrupt) for tuples <string, [0 or more logical literals]>
% (Deprecated under the assumption that new action learning opportunities are always dealt with immediately, first drawing the robot's attention, never stored up.)
/*
learnFromDescs :-
	findall([A,B,C], obs_ex_action_tagged(A,B,C), X),
	X=[Head|Tail],
	!,
	retractall(recurseOnDescs(_)),
	assert(recurseOnDescs(Tail)),
	learnFromActionDescs(Head),
	retractall(obs_ex_action_tagged(_,_,_)).
learnFromDescs.
*/

% Parse string into list of POS-tagged words
learnFromActionDescs([], learning_finished) :- !.
learnFromActionDescs(A, Head) :-
	A = [String,LiteralEffects,_Step],
	prettyprintstars,
	prettyprint('Tagged input string: '),
	prettyprintln(String),
	prettyprintstars,
	split_string(String, " ", "", WordsList),
	divideTags(WordsList, PairsListRev), % Splits each substring into a pair of the word and its tag by finding "_"
	reverse(PairsListRev, PairsListTotal),
	prettyprint('Learning from: '),
	prettyprintln(PairsListTotal),
	%4. Pick out verb as action predicate
	(select(["is","VBZ"], PairsListTotal, PairsListNext) -> true ; PairsListNext = PairsListTotal),
	
	((member([Verb,VTAG], PairsListNext), (VTAG = "VBG" ; VTAG = "VBZ" ; VTAG = "VBN" ; VTAG = "VBD")) % VBZ / VBG, depends
	->
	(append(PairsList1,PairsList2,PairsListNext), append(AgentDescriptionList,[[Verb,VTAG]],PairsList1))
	;
	(prettyprintln('Failure finding verb in input!'), trace, fail)),

	standardizeVerb(Verb,Action),
	prettyprint('Verb isolated: '),
	prettyprintln(Action),
	prettyprintln(''),
	% 5. Next, work back through the sentence template used for explanation:
	%    "[actor] [verb past tense] [object1] {to [object2]} {by [object3]} {with [object4]} {and [object5]}"
	% We assume that human-provided instructions take this form.
	% 5.(a) If the current pairs list contain the pair ["and", _Tag], separate out the remainder list as Object5, else no Object5 (empty list)
	(member(["and",AndVar], PairsList2)
	->
	(append(L2,Object5,PairsList2), append(PairsList3,[["and",AndVar]],L2))
	;
	(Object5 = [], PairsList3 = PairsList2)
	),
	!, % Assume at most one "and"
	% 5.(b) If the current pairs list contain the pair ["with", _Tag], separate out the remainder list as Object4, else no Object4 (empty list)
	(member(["with",WithVar], PairsList3)
	->
	(append(L3,Object4,PairsList3), append(PairsList4,[["with",WithVar]],L3))
	;
	(Object4 = [], PairsList4 = PairsList3)
	),
	!, % Assume at most one "with"
	% 5.(c) If the current pairs list contain the pair ["by", _Tag], separate out the remainder list as Object3, else no Object3 (empty list)
	(member(["by",ByVar], PairsList4)
	->
	(append(L4,Object3,PairsList4), append(PairsList5,[["by",ByVar]],L4))
	;
	(Object3 = [], PairsList5 = PairsList4)
	),
	!, % Assume at most one "by"
	% 5.(d) If the current pairs list contain the pair ["to", _Tag], separate out the remainder list as Object2, else no Object2 (empty list)
	(member(["to",ToVar], PairsList5)
	->
	(append(L5,Object2,PairsList5), append(PairsList6,[["to",ToVar]],L5))
	;
	(Object2 = [], PairsList6 = PairsList5)
	),
	!, % Assume at most one "to"
	
	% 6. If the current pairs list contains an object, extract it as Object1
	extractObjectDescription(PairsList6, RemainderObjectDesc),
	getObjectSymbol(RemainderObjectDesc, Object1Pred, Object1Symbol),
	!,
	
	% 7. AgentDescriptionList is required to contain an object
	extractObjectDescription(AgentDescriptionList, AgentDesc),
	getObjectSymbol(AgentDesc, AgentPred, ActorSymbol),
	!,
	prettyprint('Actor isolated: '),
	prettyprintln(ActorSymbol),
	prettyprintln(''),

	% 7. Now for each field Object2-6 that is nonempty, must be able to reduce it to a single object symbol.
	(Object1Symbol = [] -> (Syntax1 = []) ; Syntax1 = [object1]),
	(Object2 = [] -> (Predicate2 = [], Symbol2 = [], Syntax2 = Syntax1) ; (extractObjectDescription(Object2, ObjDescription2), getObjectSymbol(ObjDescription2, Predicate2, Symbol2), append(Syntax1, [object2], Syntax2))),
	(Object3 = [] -> (Predicate3 = [], Symbol3 = [], Syntax3 = Syntax2) ; (extractObjectDescription(Object3, ObjDescription3), getObjectSymbol(ObjDescription3, Predicate3, Symbol3), append(Syntax2, [object3], Syntax3))),
	(Object4 = [] -> (Predicate4 = [], Symbol4 = [], Syntax4 = Syntax3) ; (extractObjectDescription(Object4, ObjDescription4), getObjectSymbol(ObjDescription4, Predicate4, Symbol4), append(Syntax3, [object4], Syntax4))),
	(Object5 = [] -> (Predicate5 = [], Symbol5 = [], Syntax5 = Syntax4) ; (extractObjectDescription(Object5, ObjDescription5), getObjectSymbol(ObjDescription5, Predicate5, Symbol5), append(Syntax4, [object5], Syntax5))),
	
	% 8. Now assert a new action_syntax(Action, VerbPastTense, [List])
	ObjSyntaxList = Syntax5,
	standardizeVerbToPast(Verb,PastTense),
	(action_syntax(Action, PastTense, ObjSyntaxList) -> true ; asserta(action_syntax(Action, PastTense, ObjSyntaxList))), % Do not re-add the learned language template upon refinement of the concept

	% Bookkeeping
	OrderedListInit = [ActorSymbol, Object1Symbol, Symbol2, Symbol3, Symbol4, Symbol5],
	flatten(OrderedListInit, OrderedList),
	
	ClassPredListInit = [AgentPred, Object1Pred, Predicate2, Predicate3, Predicate4, Predicate5],
	flatten(ClassPredListInit,ObjectClassPredicates), % When the input did not include a particular syntactic field, removes the empty list associated with that field
	
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
	translateLearnedActionDescsToPrologRules,
	!,
	(recurseOnDescs(Remainder) -> learnFromActionDescs(Remainder,_) ; true).
learnFromActionDescs(N, learning_failed) :-
	prettyprintstars,
	prettyprint('Failed to learn from '),
	prettyprint(N),
	prettyprintln('. Discarding and continuing.'),
	prettyprintstars.

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
	% Capitalised form of StringArg is StringArgCAPS... prefix with either '(' or ',' to try to prevent constant name overlap, e.g. 'b1' and 'rob1'
	string_concat("(", StringArg, StringArg1),
	string_upper(StringArg1,StringArgCAPS1),
	string_concat(",", StringArg, StringArg2),
	string_upper(StringArg2,StringArgCAPS2),
	% Change HeadString to HeadString2 by replacing each occurrence of StringArg in it with StringArgCAPS
	split_string_custom(HeadString, StringArg1, HeadStringL),
	joinStrings(HeadStringL,StringArgCAPS1,HeadStringL0),
	split_string_custom(HeadStringL0, StringArg2, HeadStringL1),
	joinStrings(HeadStringL1,StringArgCAPS2,HeadString2),
	% Change EffectsString to EffectsString2 by replacing each occurrence of StringArg in it with StringArgCAPS
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
	split_string(Word, "_", "", [FirstPart,SecondPart]),
	string_lower(FirstPart, FirstPartLower), % Set all letters to lowercase
	append([[FirstPartLower,SecondPart]], TempList, NewTempList),
	divideTagsRecursively(B, NewTempList, PairsList).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

standardizeVerb(VString, ReturnSymbol) :-
	term_string(VTerm, VString),
	vb(ReturnSymbol, List),
	member(VTerm, List),
	!.
standardizeVerb(VString, ReturnSymbol) :-
	term_string(ReturnSymbol, VString).

% Imprecise: The verb list stores most but not all verbs in a form with past tense last
standardizeVerbToPast(VString, ReturnSymbol) :-
	term_string(VTerm, VString),
	vb(VTerm, List),
	last(List, ReturnSymbol),
	!.
standardizeVerbToPast(VString, ReturnSymbol) :-
	term_string(VTerm, VString),
	(vb(_SomeVerb, List), member(VTerm, List)),
	last(List, ReturnSymbol),
	!.
standardizeVerbToPast(VString, ReturnSymbol) :- % If verb cannot be found, default to adding suffix "-ed" to verb used
	string_concat(VString, "ed", NewString),	
	term_string(ReturnSymbol, NewString).

/*
Need to use a corpus of English words to get the correct verb form. It would be easier for input verb tense 'x foos y': can just use string manipulation:

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

% ReturnedObjectDescription is a list of nouns and adjectives describing a single object, beginning with the final descriptor (most likely to correspond to sort).
extractObjectDescription(Pairs, ReturnedObjectDescription) :-
	extractObjectDescriptionRecursively(Pairs, [], ReturnedObjectDescription).

extractObjectDescriptionRecursively([], FinalDesc, FinalDesc).
extractObjectDescriptionRecursively([Pair|B], Working, FinalDesc) :-
	% Pair is noun or adjective
	(Pair = [Word, "NN"] ; Pair = [Word, "JJ"]),
	!,
	append([Word], Working, Desc),
	extractObjectDescriptionRecursively(B, Desc, FinalDesc).
extractObjectDescriptionRecursively([_|B], Working, FinalDesc) :-
	% Base case - Pair is irrelevant; discard it
	!,
	extractObjectDescriptionRecursively(B, Working, FinalDesc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getObjectSymbol([], [], []) :- !.
getObjectSymbol(ObjectDescription, ObjectSortReturned, ObjectSymbolReturned) :-
	ObjectDescription = [PresumedNounString|RemainderDescription],
	term_string(PresumedNoun, PresumedNounString),
	translateNounThroughWordNet(PresumedNoun, WordNetNoun),
	functor(Term, WordNetNoun, 1),
	sort(Term),
	% !, % Can't cut here to get ancestor sorts because it needs to backtrack to try e.g. person(p1), person(p2), etc, as well as to try different WordNet senses of the noun class itself
	arg(1, Term, ObjectSymbol),
	changeAdjsOrNounsToTermsAndCheckRecursive(ObjectSymbol, RemainderDescription),
	ObjectSortReturned = WordNetNoun,
	ObjectSymbolReturned = ObjectSymbol,
	!.
% Alternative case: Final descriptor does not correspond to a sort, but to a static attribute like the other descriptors (assume that human-provided input will uniquely pick out a domain object).
getObjectSymbol(ObjectDescription, ObjectSortReturned, ObjectSymbolReturned) :-
	sort(Term),
	functor(Term, SomeSort, 1),
	arg(1, Term, ObjectSymbol),
	changeAdjsOrNounsToTermsAndCheckRecursive(ObjectSymbol, ObjectDescription),
	ObjectSortReturned = SomeSort,
	ObjectSymbolReturned = ObjectSymbol,
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

changeAdjsOrNounsToTermsAndCheckRecursive(_, []).
changeAdjsOrNounsToTermsAndCheckRecursive(ObjectSymbol, [Adj|B]) :-
	translateT(Adj, InitialValue, 0),
	translateAdjThroughWordNet(InitialValue,FinalValue),
	domain_attr(Term),
	functor(Term, _SomePredicate, 2),
	arg(1, Term, ObjectSymbol),
	arg(2, Term, FinalValue),
	prettyprint(' => '), prettyprintln(Term),
	changeAdjsOrNounsToTermsAndCheckRecursive(ObjectSymbol, B).

translateT(Word, Term, N) :-
	term_string(Pred, Word),
	functor(Term, Pred, N).

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
translateAdjThroughWordNet(InitialValue,FinalValue) :- % Hypernym or hyponym
	s(Synset1,_,InitialValue,Type1,_,_),
	(Type1 = s ; Type1 = a),
	(hyp(Synset1, Synset2) ; hyp(Synset2, Synset1)), 
	s(Synset2,_,FinalValue,Type2,_,_),
	(Type2 = s ; Type2 = a),
	InitialValue \= FinalValue.

translateLearnedActionDescsToPrologRules :-
       tell('learned_action_descs.pl'),
       listing(exoActionDescription/4),
       told.
       %tell('exogenous_events.pl'),
       %listing(hpd/2),
       %told.
translateLearnedActionDescsToPrologRules :- !. % TODO

%%%%%%%%%%%%%%%%%%%%%%%%%%%%











% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Note 2019-03-27
% The following (the remainder of this file) consists of older formulations of functions given above, not used in this version of the code.
% => extractObjectDescriptions_DEPRECATED is a conservative version of extractObjectDescription that operates over a string which may contain MULTIPLE object descriptions.
%    It also strictly finds sequences of one or more words tagged "JJ" (adjective) followed by a single word tagged "NN" (noun).
% => getOrderedListOfObjectSymbols_DEPRECATED is a conservative version of getObjectSymbol that takes as input MULTIPLE object description lists starting with exactly one noun.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractObjectDescriptions_DEPRECATED(Pairs, ObjectDescriptions) :-
	extractObjectDescriptionsRecursively_DEPRECATED(Pairs, [], [], ObjectDescriptions).

extractObjectDescriptionsRecursively_DEPRECATED([], [], FinalDescs, FinalDescs).
extractObjectDescriptionsRecursively_DEPRECATED([Pair|B], WorkingAds, WorkingDescs, FinalDescs) :-
	% Pair is noun: append WorkingAds to it, add to WorkingDescs, and continue
	Pair = [Word, "NN"],
	!,
	append([Word], WorkingAds, Desc),
	append([Desc], WorkingDescs, NewWorkingDescs),
	extractObjectDescriptionsRecursively_DEPRECATED(B, [], NewWorkingDescs, FinalDescs).
extractObjectDescriptionsRecursively_DEPRECATED([Pair|B], WorkingAds, WorkingDescs, FinalDescs) :-
	% Pair is adjective: append to WorkingAds
	Pair = [Word, "JJ"],
	!,
	append([Word], WorkingAds, NewWorkingAds),
	extractObjectDescriptionsRecursively_DEPRECATED(B, NewWorkingAds, WorkingDescs, FinalDescs).
extractObjectDescriptionsRecursively_DEPRECATED([_|B], WorkingAds, WorkingDescs, FinalDescs) :-
	% Base case - Pair is irrelevant; discard it
	!,
	extractObjectDescriptionsRecursively_DEPRECATED(B, WorkingAds, WorkingDescs, FinalDescs).

% For each sequence, determine static attributes from values (assume unambiguous)
% And for each sequence, search knowledge of objects for one with correct sort and attribute-value pairs; take first match (assumes uniqueness)
getOrderedListOfObjectSymbols_DEPRECATED(ObjectDescriptions, ObjectClassPredicates, OrderedList) :-
	getOrderedListOfObjectSymbolsRecursive_DEPRECATED(ObjectDescriptions, [], [], ObjectClassPredicates, OrderedList).

getOrderedListOfObjectSymbolsRecursive_DEPRECATED([], Final1, Final2, Final1, Final2).
getOrderedListOfObjectSymbolsRecursive_DEPRECATED([Description|B], WorkingList1, WorkingList2, ObjectClassPredicates, OrderedList) :-
	Description = [Noun|Adjs],
	term_string(InitialPred, Noun),
	translateNounThroughWordNet(InitialPred,Predicate),
	functor(Term, Predicate, 1),
	
	sort(Term),
	% !, % Can't cut here to get ancestor sorts because it needs to backtrack to try e.g. person(p1), person(p2), etc, as well as to try different WordNet senses of the noun class itself
	arg(1, Term, ObjectSymbol),
	changeAdjsToTermsAndCheckRecursive(ObjectSymbol, Adjs),
	
	append([Predicate], WorkingList1, NewWorkingList1),
	append([ObjectSymbol], WorkingList2, NewWorkingList2),
	getOrderedListOfObjectSymbolsRecursive_DEPRECATED(B, NewWorkingList1, NewWorkingList2, ObjectClassPredicates, OrderedList).

changeAdjsToTermsAndCheckRecursive(_, []).
changeAdjsToTermsAndCheckRecursive(ObjectSymbol, [Adj|B]) :-
	translateT(Adj, InitialValue, 0),
	translateAdjThroughWordNet(InitialValue,FinalValue),
	domain_attr(Term),
	functor(Term, _SomePredicate, 2),
	arg(1, Term, ObjectSymbol),
	arg(2, Term, FinalValue),
	prettyprint(' => '), prettyprintln(Term),
	changeAdjsToTermsAndCheckRecursive(ObjectSymbol, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
