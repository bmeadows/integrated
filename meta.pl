/*
 * Name:        meta.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file contains functions that reduce a search space to the relevant space for q-RRL and provides other supporting functionality.
 */

:- dynamic goalState/1, precalculate/0, step/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contents
% 1. Reduction to relevant signature
% 2. Noise simulation
% 3. Operations on the state
% 4. Action applicability
% 5. Initial directives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 1. Reduction to relevant signature %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function that reduces signature to relevant signature.
precalculate :-
	statistics(process_cputime, StartCPU),
	writef('(1/7). Precalculating.\n'),
	retractall(objrel(_)),
	domainGoalAction(Action),
	writef('(2/7). Target action.\n'),
	setRandomInitialStaticConfiguration, % Note - Source of an outstanding error with this component. (This randomly sets static attributes!)
	writef('(3/7). Randomised.\n'),
	findRelevantFluentSuperset(Action, Lits),
	writef('(4/7). Superset found.\n'),
	sort(Lits,L111), print(L111), nl, nl,
	adjustForRelevance(Lits, ReturnedTests, ReturnedObjectSet), % At this point the inconsistent behaviour is already observable.
	writef('(5/7). Adjusted for relevance.\n'),
	assert(allValidTests(ReturnedTests)),
	writef('(6/7). Target action.\n'),
	calculateConfigSpaceSize(ReturnedObjectSet),
	writef('(7/7). Calculated config size.\n'),
	retractall(objrel(_)),
	statistics(process_cputime, EndCPU),
	CPUDIFF is EndCPU - StartCPU,
	writef('CPU time for precalculation: '), print(CPUDIFF), nl.

% For each valid physical state from which the target ground action is permissible, find the relevant literals to that <physical state, action> pair.
findRelevantFluentSuperset(Action, Lits) :-
	findall(	Fluents, 
				(someDomainPhysicalStateWithApplicableAction(Action, Fluents)),
				Lits).

someDomainPhysicalStateWithApplicableAction(Action, Fluents) :-
	getTheoreticalStatePermutation(ListRepresentation), % Fluents only
	retract_facts_only(currentState(fluent(_))),
	assertFluents(ListRepresentation),
	not(stateConstraintsViolated),
	currentlyApplicableWithRelevantLits(Action, Fluents).
	
currentlyApplicableWithRelevantLits(Action, Fluents) :-
	% 1. Check it's applicable - part of the current error is that this also checks statics, so is dependent on the original randomisation of the static attributes
	validAction(Action),
	% 2. Return all fluent literals relevant to the physical config / action combo
	getRelevantFluentLits(Fluents).

adjustForRelevance(Lits, ReturnedTests, ReturnedObjectSet) :-
	sort(Lits,LitsA),
	flatten(LitsA,Lits2),
	%% Note the list_to_set/2 predicate would be more expensive than sort/2 because it involves two sorts and a linear scan.
	sort(Lits2,Lits3),
	addAllObjectAttributesAndActions(Lits3, ReturnedTests, ReturnedObjectSet).
	
% Returns fluents relevant to the action, based on current physical state
% Also asserts names of relevant objects in "objrel(X)".
getRelevantFluentLits(Set2) :-
	domainGoalAction(Action),
	Action =.. [_Predicate1|Set1], % 1. For the set1 of objects named in the action predicate;
	findall( 	F,
				(currentState(F), F=fluent(Content), Content =.. [_Predicate2|ArgList], length(ArgList, N), N>0, last(ArgList, Last), select(Last, ArgList, Remnant), allAreIn(Remnant, Set1) ),
				Set2
			), % 2. Find the set2 of all currently-true-in-state fluents such that the fluent's arguments are all in set1;
	findall( 	LastConstant,
				(member(fluent(Content),Set2), Content =.. [_Predicate3|ArgList], last(ArgList, LastConstant) ),
				Set3
			), % 3. Let set3 be the set of constants mapped onto by set2;
	assertEachAsObjRel(Set3).
	
assertEachAsObjRel([]) :- !.
assertEachAsObjRel([A|B]) :-
	objrel(A),
	!,
	assertEachAsObjRel(B).
assertEachAsObjRel([A|B]) :-
	assert(objrel(A)),
	!,
	assertEachAsObjRel(B).

allAreIn(SetA, SetB) :-
	not( (member(X, SetA), not(member(X, SetB))) ).

addAllObjectAttributesAndActions(Initial, Return, ReturnedObjectSet) :-	
	findall(X, objrel(X), ListObs),
	domainGoalAction(Action),
	Action =.. [_Predicate|ActionArgs],
	append(ListObs, ActionArgs, NewList), % Add the arguments of the target action
	sort(NewList, ReturnedObjectSet),
	addAllRelevantActions(ReturnedObjectSet, Initial, NewSet),
	% At this point, just need to find valid BDT tests, so find all relevant instantiated object properties
	% (While allValidTests(Tests) is also used to find meaningfully different obj configs, no changes are necessary for that)
	% Props1 should contain all valid instantiated multi-argument object properties whose n-1 arguments are all in ReturnedObjectSet
	findall(	attr(ObjectProperty),
				( valid(attr(ObjectProperty)), ObjectProperty =.. [_Pred|Args], length(Args, N), N>1, last(Args, Last), select(Last, Args, Remnant), allAreIn(Remnant, ReturnedObjectSet) ),
				Props1),
	sort(Props1,Props2),
	append(Props2, NewSet, Return).

addAllRelevantActions(ObjectSet, Initial, Final) :-
	findall( action(Act),
			(	
				valid(action(Act)),
				Act =.. [_Pred|Args],
				not( (member(Arg,Args), not(member(Arg,ObjectSet))) ) % No arg is outside the set of relevant domain objects
			),
			RelActions ),
	% This list kept separately, so it can be used not only to determine tests in the BDT, but also prevent the learner ever trying actions outside it
	assert(usableActionList(RelActions)),
	append(Initial,RelActions,Final).

% 1. Have determined which objects are relevant, 'ObjectSet'.
% 2. All properties of a relevant object are relevant.
% 3. For each property that CAN pertain to an object, it is guaranteed that the function will give SOME value.
% 4. Can call valid(attr(X)) and check number of arguments is >1.
% 5. So, for each predicate, count predicate([unique instantiated arguments all in ObjectSet], final argument) and then multiply out for all predicates

singleMapping(ObjectSet, Pred, Remnant, Number) :-
	valid(attr(ObjectProperty)),
	ObjectProperty =.. [Pred|Args],
	length(Args, N), N>1,
	last(Args, Last), select(Last, Args, Remnant),
	allAreIn(Remnant, ObjectSet),
	findall(	Last2,
				(valid(attr(ObjectProperty2)), ObjectProperty2 =.. [Pred|Args2], length(Args2, N2), N2>1, last(Args2, Last2), select(Last2, Args2, Remnant)),
				SetOfThingsMappedOnto),
	sort(SetOfThingsMappedOnto,SetOfThingsMappedOnto2),
	length(SetOfThingsMappedOnto2, Number).

% Speculation on alternative approach:
% Start with all the attr()s from the found set. Partition them into new lists where the whole literal except the last argument are the same in each.
% If necessary (check), remove the ones where some arg in [all but the last argument] isn't in the ObjectSet.
	
calculateConfigSpaceSize(ObjectSet) :-
	findall([Pred, Remn, Number], singleMapping(ObjectSet, Pred, Remn, Number), Collects),
	sort(Collects, Numbers),
	findall(Number, member([_,_,Number],Numbers), Final),
	list_multiply(Final, ResultTotal),
	assert(num_possible_attribute_configs(ResultTotal)).
	
list_multiply([], 0) :- !.
list_multiply(List, ResultTotal) :-
	multiply_out_list(List, 1, ResultTotal).
multiply_out_list([], Final, Final).
multiply_out_list([A|B], Working, Final) :-
	New is A * Working,
	multiply_out_list(B, New, Final).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% 2. Noise simulation %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

applyNoiseWhereAppropriate :-
	noiseChancePercent(0),
	!.
applyNoiseWhereAppropriate :-
	useNoiseRelevantToAction(true),
	noiseChancePercent(N),
	random_between(1,100,Rand1),
	% N% chance to change fluent relevant to action
	((Rand1 =< N) -> switchActionOutcome ; true),
	!.
applyNoiseWhereAppropriate :-
	useNoiseRelevantToAction(false),
	noiseChancePercent(N),
	random_between(1,100,Rand1),
	% N% chance to change some fluent in domain
	((Rand1 =< N) -> switchAFluent ; true),
	!.
applyNoiseWhereAppropriate :- trace.

switchAFluent :-
	findall(fluent(Fl), currentState(fluent(Fl)), FList),
	random_member(F, FList),
	get_all_alternative_domain_tests(F, AltList),
	retract(currentState(F)),
	random_member(NewFluent, AltList),
	assert(currentState(NewFluent)),
	(stateConstraintsViolated -> switchAFluent ; true). % Very important - just replacing with something from get_all_alternative_domain_tests can still result in causal violation.
	% That in turn causes things like learning a constraint that incorporates a physical constraint violation, which makes filter checking hang forever!
	
switchActionOutcome :-
	allValidTests(Tests),
	findall( X, (member(X, Tests), X = fluent(_), not(currentState(X))), List), % Get all relevant fluents not currently true
	(
	(List == [])
	->
	true
	; 
	(random_member(Fluent, List), % Take one at random
	assert(currentState(Fluent)),
	get_all_alternative_domain_tests(Fluent, AltList),
	retracteachfromstate(AltList), % First attempt to obey state constraints
	% If still inconsistent, fall back on changing a RANDOM fluent - this should not be a recursive call to switchActionOutcome
	(stateConstraintsViolated -> switchAFluent ; true) % Very important - just replacing with something from get_all_alternative_domain_tests can still result in causal violation.
	% That in turn causes things like learning a constraint that incorporates a physical constraint violation, which makes filter checking hang forever!
	)),
	!.
	
retracteachfromstate([]).
retracteachfromstate([A|B]) :- retract_facts_only(currentState(A)), retracteachfromstate(B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% 3. Operations on the state %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Accessing relevance information:
% Returns all static elements of current state that are valid BDT tests, i.e., relevant.
getCurrentRelevantConfigurationOfAttributes(RelevantConfigList) :-
	allValidTests(Tests), % Includes actions and fluents, too
	findall(attr(Attr),
		(currentState(attr(Attr)), member(attr(Attr), Tests)), % Note that Tests *already* has the attr() wrappers (amongst others)
		AttrList),
	sort(AttrList,RelevantConfigList).

resetStateInPrincipledWay :-
	setObservedUnexpectedState,
	permuteStateSemiRandomly,
	!,
	(stateConstraintsViolated -> resetStateInPrincipledWay ; true).
	
% Change an observed failure state to a semi-random permutation.
% Note this can cause bad states; must check for violations after calling.
permuteStateSemiRandomly :-
	fluent_change_iterations(N),
	fluent_change_chance(P),
	permuteStateSemiRandomlyIterate(N,P).
	
permuteStateSemiRandomlyIterate(0,_) :- !.
permuteStateSemiRandomlyIterate(N,P) :-
	random(R),
	(R < P -> makeSingleFluentChange ; true),
	N2 is N-1,
	permuteStateSemiRandomlyIterate(N2,P).

makeSingleFluentChange :- 
	% Select a random fluent currentState(fluent(f)), then call swapOut(fluent(f))
	findall(F, (currentState_overt_belief_only(F), F=fluent(_)), List), % Just currentState() will go wrong because of inferred beliefs not actively believed
	random_member(Fluent,List),
	swapOut(Fluent),
	!.

% Helper function for adding fluents to the state.
assertFluents([]).
assertFluents([A|B]) :-
	assert(currentState(fluent(A))),
	assertFluents(B).

% Helper function for adding static attributes to the state.
assertAtts([]).
assertAtts([A|B]) :-
	assert(currentState(attr(A))),
	assertAtts(B).

domainChangeObjectAtts(List) :-
	!,
	domainChangeObjectAttsRandomly(List).
	
% Important: This function is used by the qRRL system, and must be defined in each domain.
% The function is passed in a list of RELEVANT object properties as [..., attr(X), ...]
domainChangeObjectAttsRandomly(List) :-
	random_member(X,List), % Pick one literal representing a static attribute to change at random
	change_att_value(X, Y), % Make a valid change to something other than the original value
	retract_facts_only(currentState(X)),
	assert(currentState(Y)).

change_att_value(Input, Return) :-
	Input = attr(Term1),
	functor(Term1,AttrPredicate,2),
	arg(1,Term1,DomainObject),
	arg(2,Term1,CurrentValue),
	findall(Val, (functor(Term2,AttrPredicate,2), arg(2,Term1,Val), valid(attr(Term2))), List),
	select(CurrentValue, List, RevisedList),
	random_member(NewVal, RevisedList),
	functor(Term3,AttrPredicate,2),
	arg(1,Term3,DomainObject),
	arg(2,Term3,NewVal),
	Return = attr(Term3),
	!.

establishGoalState :-
	learning_type(positive_affordance_learning),
	!,
	executabilityConditionViolated(ID),
	domainGoalAction(Action),
	clause(impossible_if(Action, ID),_),
	Element = requiredToCheckBeforeTransition(Action, ID),
	establishGS([ Element ]).
establishGoalState :-
	% Not positive_affordance_learning
	establishGS([]).
	
establishGS(List) :-
	domainGoalAction(Act),
	% Unexpected observations
	unexpectedResult(X),
	append(X, [lastActionWas(Act)], List1),
	append(List1, List, List2),
	assert(goalState(List2)).

applyActionToState(Action) :-
	step(I),
	% 1. Update time
	retractall(step(I)),
	max_step(Max),
	(I == Max -> J = last ; J is I + 1), % Domain has maximum number of steps per episode
	assert(step(J)),
	% 2. Apply action
	applyActionToState_SingleCase(Action),
	applyNoiseWhereAppropriate.

% Removes only fluents that match the pattern and are not clauses.
% This is necessary because e.g. retractall(currentState(foo(1,2))) will retract RULES like 'currentState(foo(A,B)) :- currentState(bar(A,B)), valid(sort(A)).'
% These are needed if a domain has derived fluents, e.g., location dynamically inferred from being inside a container that is at a location.
retract_facts_only(Pattern) :-
	clause(Pattern, true), % Establishes we only match against facts
	retract(Pattern), % Only retract exactly that fact
	fail. % Failure-driven loop means 'Pattern' term doesn't get variables set, unlike in recursive loop
retract_facts_only(_) :- !.

currentState_overt_belief_only(X) :-
	clause(currentState(X), true), % There is a 'belief that X' in the fact store.
	not(( clause(currentState(X), Tail), Tail \= true, Tail )).  % 'belief that X' cannot be derived from other terms.
	% The reason BOTH checks are made is to help in cases where the system inappropriately findalls currentState beliefs and then later 'reinstates' them, turning a derived belief into a reified belief.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% 4. Action applicability %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validAction(A) :- actionDescription(A, Vars, Sorts), sortsHoldTrue(Sorts, Vars), not(impossible_if(A,_ID)).

validOrPotentialAction(A) :- validAction(A).
% May not be valid, but currently doing positive affordance learning and only the target executability condition is preventing the action being applicable.
validOrPotentialAction(A) :-
	learning_type(positive_affordance_learning),
	actionDescription(A, Vars, Sorts),
	sortsHoldTrue(Sorts, Vars),
	executabilityConditionViolated(TargetID),
	not(( impossible_if(A,OtherID), OtherID \= TargetID )).

sortsHoldTrue([], []).
sortsHoldTrue([Sort|Tail1], [Var|Tail2]) :-
	functor(Term, Sort, 1),
	arg(1, Term, Var),
	domain(sort(Term)),
	sortsHoldTrue(Tail1, Tail2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% 5. Initial directives %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- establishGoalState.
	
:- domain_relevance(X), call(X).
