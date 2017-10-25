:- dynamic step/1.
:- discontiguous actionDescription/3, impossible_if/2, causal_law/3.

% causal_law(Action, FluentsAndStaticsThatMustHold, Consequences).
% Can have multiple causal laws per action

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%subsort(thing, location).
subsort(thing, object).
subsort(thing, entity).
subsort(entity, robot).
subsort(entity, person).
subsort(object, item).
subsort(item, book).
%
ancestor(X,Y) :- subsort(X,Y).
ancestor(X,Y) :- subsort(Z,Y), ancestor(X,Z).
%
domain(sort(person(p0))).
domain(sort(robot(rob1))).
domain(sort(location(rmwor))).
domain(sort(location(rmoff))).
domain(sort(location(rmlib))).
domain(sort(book(book1))).
%
domain(sort(Term1)) :- subsort(General,Specific), functor(Term1,General,1), arg(1,Term1,Arg), functor(Term2,Specific,1), arg(1,Term2,Arg), domain(sort(Term2)).

% = currentState(attr()) ? TODO resolve

currentState(attr(type(rmwor,workshop))).
currentState(attr(type(rmoff,office))).
currentState(attr(type(rmlib,library))).

currentState(fluent(loc(O,L))) :- domain(sort(object(O))), currentState(fluent(in_hand(X,O))), domain(sort(entity(X))), currentState(fluent(loc(X,L))).

%

valid(attr(type(L,T))) :- domain(sort(location(L))), member(T, [office, library, workshop]).

% Permissible fluents
valid(fluent(loc(X,Y))) :- domain(sort(thing(X))), domain(sort(location(Y))).
valid(fluent(in_hand(E,O))) :- domain(sort(entity(E))), domain(sort(item(O))).

% Permissible actions
valid(action(pickup(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).
valid(action(putdown(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).
valid(action(move(R,L))) :- domain(sort(robot(R))), domain(sort(location(L))).
valid(action(serve(R,O,P))) :- domain(sort(robot(R))), domain(sort(item(O))), domain(sort(person(P))).
valid(action(affix_label(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).
valid(action(wait(R))) :- domain(sort(robot(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_specified_end :-	step(last), !.
domain_specified_end :-	domainGoalAction(serve(rob1,book1,_)), ( currentState(fluent(in_hand(P,book1))), currentState(attr(person(P))) ), !.
domain_specified_end :-	domainGoalAction(pickup(rob1,X)), currentState(fluent(in_hand(_,X))), !.
domain_specified_end :-	stateConstraintsViolated.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% STATE CONSTRAINTS
stateConstraintsViolated :- currentState(fluent(loc(O,L1))), currentState(fluent(loc(O,L2))), L1 \= L2. % One thing in two places
stateConstraintsViolated :- currentState(fluent(in_hand(H1,O))), currentState(fluent(in_hand(H2,O))), H1 \= H2. % Anything simultaneously in two different hands
stateConstraintsViolated :- currentState(fluent(in_hand(H,O1))), currentState(fluent(in_hand(H,O2))), O1 \= O2. % Same entity has two things in hand
stateConstraintsViolated :- domain(sort(thing(E))), not(currentState(fluent(loc(E,_)))). % An entity or object not at a place
stateConstraintsViolated :- currentState(X), not(valid(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_test_alternatives(attr(type(X, workshop)), [attr(type(X, library)),attr(type(X, office))]).
domain_test_alternatives(attr(type(X, library)), [attr(type(X, workshop)),attr(type(X, office))]).
domain_test_alternatives(attr(type(X, office)), [attr(type(X, library)),attr(type(X, workshop))]).

domain_test_alternatives(N, Return) :-
	N=fluent(_),
	select(N,
	[fluent(loc(X, rmwor)), fluent(loc(X, rmoff)), fluent(loc(X, rmlib)), fluent(in_hand(p0, X)), fluent(in_hand(rob1, X))],
	Return).
% Note that this can return some bad values, such as in_hand(p0, rob1) or in_hand(book1, p0), but these functions are only used to swap in and out literals,
% which is always followed by testing for 'stateConstraintsViolated' (and starting again if there is a problem).

defaultNullAction([wait(rob1)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% % % ORACLE % % %

applyActionToState(Action) :-
	step(I),
	% 1. Update time
	retractall(step(I)),
	(I == 6 -> J = last ; J is I + 1),
	assert(step(J)),
	% 2. Apply action
	applyActionToStateFinal(Action),
	applyNoiseWhereAppropriate.
	% 3. Move people who had been about to move

applyActionToStateFinal(move(Robot, Loc)) :-
	currentState(fluent(in_hand(Robot, O))),
	retract_facts_only(	currentState(fluent(loc(O,_)))), % May or may not be overt
	retract_facts_only(	currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.
applyActionToStateFinal(move(Robot, Loc)) :-
	not(currentState(fluent(in_hand(Robot, _)))),
	retract_facts_only(	currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.

% (To be learned: A person cannot be served in a library)
applyActionToStateFinal(serve(R, _Obj, _P)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(attr(type(Loc, library))),
	!.
	
% Succeeded serve: none of the above cases applied
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract_facts_only(currentState(fluent(in_hand(R, Obj)))),
	retract_facts_only(currentState(fluent(loc(Obj, Loc)))), % If overtly given, have to remove, because of e.g. exogenous events - it's served and then the person moves
	!.

% Succeeded pickup: none of the above cases applied
applyActionToStateFinal(pickup(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	not(currentState(fluent(in_hand(_, Obj)))),
	assert(currentState(fluent(in_hand(R, Obj)))),
	retract_facts_only(currentState(fluent(loc(Obj, Loc)))), % If overtly given, have to remove, because of e.g. exogenous events - it's served and then the person moves
	!.

% Succeeded putdown
applyActionToStateFinal(putdown(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	!.

applyActionToStateFinal(wait(_)) :- !.

applyActionToStateFinal(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	noiseChancePercent(Noise), % Noise is likely to blame, because it can set up impossible situations - ignore it
	((Noise > 0) -> true ; trace).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actionDescription(move(Robot, Destination), [Robot, Destination], [robot, location]).
causal_law(move(Robot, Destination), [], [fluent(loc(Robot, Destination))]).
causal_law(move(Robot, _Destination), [fluent(loc(Robot, X))], [not(fluent(loc(Robot, X)))]).
causal_law(move(Robot, _Destination), [fluent(in_hand(Robot, Object)), fluent(loc(Robot, X))], [not(fluent(loc(Object, X)))]).
impossible_if(move(Robot, Destination), 10) :-
	currentState(fluent(loc(Robot, Destination))).

actionDescription(putdown(Robot, Object), [Robot, Object], [robot, item]).
causal_law(putdown(Robot, Object), [fluent(loc(Robot,L))], [not(fluent(in_hand(Robot, Object))), fluent(loc(Object, L))]).
/*impossible_if(putdown(_Robot, Object), 20) :-
	currentState(attr(surface(Object, brittle))). %%%%%%%%%%%%%%%%%%%%%%% !!! 1*/
impossible_if(putdown(Robot, Object), 21) :-
	not(currentState(fluent(in_hand(Robot, Object)))).

%
actionDescription(wait(Robot), [Robot], [robot]).
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actionDescription(serve(Robot, Obj, Person), [Robot, Obj, Person], [robot, item, person]).
causal_law(serve(Robot, Obj, Person), [], [fluent(in_hand(Person, Obj)), not(fluent(in_hand(Robot, Obj)))]).
causal_law(serve(Robot, Obj, _Person), [fluent(loc(Robot, Loc))], [not(fluent(loc(Obj, Loc)))]).
impossible_if(serve(Robot, _Obj, Person), 30) :-
	not((	currentState(fluent(loc(Robot, Location))),
			currentState(fluent(loc(Person, Location))) )).
impossible_if(serve(Robot, Obj, _Person), 32) :-
	not(currentState(fluent(in_hand(Robot, Obj)))).
impossible_if(serve(_Robot, _Obj, Person), 33) :-
	currentState(fluent(in_hand(Person, _))).
	
actionDescription(pickup(Robot, Object), [Robot, Object], [robot, item]).
causal_law(pickup(Robot, Object), [fluent(loc(Robot,L))], [not(fluent(loc(Object, L))), fluent(in_hand(Robot, Object))]).
impossible_if(pickup(Robot, Object), 40) :-
	not((	currentState(fluent(loc(Robot, Loc))),
			currentState(fluent(loc(Object, Loc))) )).
impossible_if(pickup(_Robot, Object), 42) :-
	currentState(fluent(in_hand(_, Object))).
impossible_if(pickup(Robot, _Object), 43) :-
	currentState(fluent(in_hand(Robot, _))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Observed unexpected state: Fluents depend on target action
setObservedUnexpectedState :-
	assert(lastActionWas(none)),
	retract_facts_only(currentState(fluent(_))),
	retractall(step(_)),
	assert(step(1)),
	unexpectedStateFluents(S),
	assertFluents(S).

% The state is reset at random for each sequence of episodes.
resetStateAtRandom :-
	assert(lastActionWas(none)),
	retract_facts_only(currentState(fluent(_))),
	retractall(step(_)),
	assert(step(1)),
	setupFluentsRandomly,
	!.

setupFluentsRandomly :-
	% 1. Do entity locations independently
	setupEntityL(rob1), setupEntityL(p0),
	% 2. Do fixed object locations
	% 3. Any small object is labelled 1/2 of the time
	% 4. Objects are either intact or damaged
	% 5. Do non-fixed object locations dependently, because only one can be in a hand
	% THIS SHOULD BE DONE LAST BECAUSE IT TESTS STATE CONSISTENCY
	setObjLocationsRandomlyUntilValid([book1]),
	!.
	
setupEntityL(Entity) :-
	List1 = [loc(Entity, rmoff), loc(Entity, rmwor), loc(Entity, rmlib)],
	assertOneFluentAtRandom(List1).

setObjLocationsRandomlyUntilValid(List) :-
	retractAllLocations(List),
	randomiseAllLocations(List),
	!,
	(stateConstraintsViolated -> setObjLocationsRandomlyUntilValid(List) ; true).

retractAllLocations([]) :- !.
retractAllLocations([A|B]) :-
	retract_facts_only(currentState(fluent(loc(A,_)))),
	retract_facts_only(currentState(fluent(in_hand(_,A)))),
	retractAllLocations(B).

randomiseAllLocations([]) :- !.
randomiseAllLocations([A|B]) :-
	S = [in_hand(rob1,A),in_hand(p0,A),loc(A,rmlib),loc(A,rmwor),loc(A,rmoff)],
	random_member(F, S),
	assert(currentState(fluent(F))),
	randomiseAllLocations(B).

% Returns all physical states, even if they break constraints
getTheoreticalStatePermutation(List) :-
	tryalllocs([p0,rob1,book1],[p0,rob1],[rmwor,rmoff,rmlib],[],List).

tryalllocs([],_,_,Return,Return).
tryalllocs([A|B],Entities,Places,Working,Return) :-
	not(member(A,Entities)), % Precludes people being assigned in_hand other people
	member(X,Entities),
	append(Working,[in_hand(X,A)],New),
	tryalllocs(B,Entities,Places,New,Return).
tryalllocs([A|B],Entities,Places,Working,Return) :-
	member(X,Places),
	append(Working,[loc(A,X)],New),
	tryalllocs(B,Entities,Places,New,Return).
	
assertOneFluentAtRandom(List) :-
	random_member(F, List),
	assert(currentState(fluent(F))).

assertOneXAtRandom(List) :-
	random_member(X, List),
	assert(X).
	
setRandomInitialObjectConfig :-
	retract_facts_only(currentState(attr(_))),
	Alts = [office, library, workshop],
	random_member(RA, Alts),
	random_member(RB, Alts),
	random_member(RC, Alts),
	assertAtts([type(rmoff,RA), type(rmwor,RB), type(rmlib,RC)]),
	%
	true.


% This is passed in a list of RELEVANT object properties as [... attr(X), ...]
domainChangeObjectAtts(List) :-
	!,
	domainChangeObjectAttsRand(List).
	
domainChangeObjectAttsRand(List) :-
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




% Returns a number identifying the target axiom, or returns it back for "does not match any target axiom"

% 1. Learning that a person cannot be served in a library [EXECUTABILITY CONDITION]
% ( ) Location determined negatively
domainAxiomClassifier([ [attr(type(rmlib, library))], [fluent(loc(book1,rmoff)),fluent(loc(book1,rmwor))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(rmlib, library))], [fluent(loc(book1,rmoff)),fluent(loc(p0,rmwor))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(rmlib, library))], [fluent(loc(book1,rmwor)),fluent(loc(p0,rmoff))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(rmlib, library))], [fluent(loc(book1,rmoff)),fluent(loc(rob1,rmwor))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(rmlib, library))], [fluent(loc(book1,rmwor)),fluent(loc(rob1,rmoff))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(rmlib, library))], [fluent(loc(p0,rmoff)),fluent(loc(rob1,rmwor))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(rmlib, library))], [fluent(loc(p0,rmwor)),fluent(loc(rob1,rmoff))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
% (a) Location determined by p0
domainAxiomClassifier([ [attr(type(L, library)), fluent(loc(p0,L))], [] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [fluent(loc(p0,L))], [attr(type(L, office)), attr(type(L, workshop))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(p0,L1)), fluent(loc(p0,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(p0,L2)), fluent(loc(p0,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
% (b) Location determined by rob1
domainAxiomClassifier([ [attr(type(L, library)), fluent(loc(rob1,L))], [] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [fluent(loc(rob1,L))], [attr(type(L, office)), attr(type(L, workshop))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(rob1,L1)), fluent(loc(rob1,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(rob1,L2)), fluent(loc(rob1,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
% (c) Location determined by book1
domainAxiomClassifier([ [attr(type(L, library)), fluent(loc(book1,L))], [] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [fluent(loc(book1,L))], [attr(type(L, office)), attr(type(L, workshop))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L1)), fluent(loc(book1,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L2)), fluent(loc(book1,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
% (d) Location determined by mix
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(p0,L1)), fluent(loc(rob1,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(p0,L2)), fluent(loc(rob1,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L2)), fluent(loc(p0,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L1)), fluent(loc(p0,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(p0,L2), fluent(loc(rob1,L1)))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(p0,L1)), fluent(loc(rob1,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L2)), fluent(loc(rob1,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L1)), fluent(loc(rob1,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L1)), fluent(loc(rob1,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L2)), fluent(loc(rob1,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L1)), fluent(loc(p0,L2))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.
domainAxiomClassifier([ [attr(type(L1, office)), attr(type(L2, workshop))], [fluent(loc(book1,L2)), fluent(loc(p0,L1))] ], 1) :- domainGoalAction(serve(rob1,book1,p0)), !.


% 2. Learning that serving a person in a workshop causes them to leave [CAUSAL LAW]


%%%%%%%%%%%%%%%%%%%%

% Catch case: Everything else
domainAxiomClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- !.

%%%%%%%%%%%%%%%%%%%%









% Physical configurations:
% 2 entities in 3 locations = 3^2 = 9
% 1 item that can be held (at most 1 per agent) or at location (any number at a location) = 5
% 9 * 5 = 45 physical configurations

% Object property configurations:
% 3 object property configurations

/*  */
cached :-
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)), % TODO check if this is still used
	assert(allValidTests([
		attr(type(rmwor,workshop)),attr(type(rmwor,office)),attr(type(rmwor,library)),
		attr(type(rmoff,office)),attr(type(rmoff,workshop)),attr(type(rmoff,library)),
		attr(type(rmlib,library)),attr(type(rmlib,workshop)),attr(type(rmlib,office)),
		fluent(loc(book1,rmwor)),fluent(loc(book1,rmoff)),fluent(loc(book1,rmlib)),
		fluent(loc(rob1,rmwor)),fluent(loc(rob1,rmoff)),fluent(loc(rob1,rmlib)),
		fluent(loc(p0,rmwor)),fluent(loc(p0,rmoff)),fluent(loc(p0,rmlib)),
		fluent(in_hand(p0,book1)),fluent(in_hand(rob1,book1)),
		action(serve(rob1,book1,p0)),action(pickup(rob1,book1)),action(putdown(rob1,book1)),action(move(rob1,rmwor)),action(move(rob1,rmoff)),action(move(rob1,rmlib))
		])),
	assert(num_possible_attribute_configs(3)),
	assert(usableActionList(
		[action(serve(rob1,book1,p0)),action(pickup(rob1,book1)),action(putdown(rob1,book1)),action(move(rob1,rmwor)),action(move(rob1,rmoff)),action(move(rob1,rmlib))]
		)).
%
domainGoalAction(serve(rob1,book1,p0)).
unexpectedResult([fluent(in_hand(rob1,book1))]).
unexpectedStateFluents([loc(p0,rmlib),loc(rob1,rmlib),in_hand(rob1,book1)
	]). % An actual state from which unexpected outcomes occurred. Others are possible.
% executabilityConditionViolated(31). %(For affordances only)





