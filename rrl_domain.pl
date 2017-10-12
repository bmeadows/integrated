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

domain(attr(type(rmwor,workshop))).
domain(attr(type(rmoff,office))).
domain(attr(type(rmlib,library))).

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

currentState(fluent(loc(O,L))) :- currentState(fluent(in_hand(X,O))), currentState(fluent(loc(X,L))).

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
	retractall(	currentState(fluent(loc(O,_)))), % May or may not be overt
	retractall(	currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.
applyActionToStateFinal(move(Robot, Loc)) :-
	not(currentState(fluent(in_hand(Robot, _)))),
	retractall(	currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.

% Succeeded serve: none of the above cases applied
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	retractall(currentState(fluent(loc(Obj, Loc)))), % If overtly given, have to remove, because of e.g. exogenous events - it's served and then the person moves
	!.

% Succeeded pickup: none of the above cases applied
applyActionToStateFinal(pickup(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	not(currentState(fluent(in_hand(_, Obj)))),
	assert(currentState(fluent(in_hand(R, Obj)))),
	retractall(currentState(fluent(loc(Obj, Loc)))), % If overtly given, have to remove, because of e.g. exogenous events - it's served and then the person moves
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
causal_law(move(Robot, Destination), [fluent(loc(Robot, X))], [not(fluent(loc(Robot, X)))]).
causal_law(move(Robot, Destination), [fluent(in_hand(Robot, Object)), fluent(loc(Robot, X))], [not(fluent(loc(Object, X)))]).
impossible_if(move(Robot, Destination), 10) :-
	currentState(fluent(loc(Robot, Destination))).

actionDescription(putdown(Robot, Object), [Robot, Object], [robot, item]).
causal_law(putdown(Robot, Object), [fluent(loc(Robot,L))], [not(fluent(in_hand(Robot, Object))), fluent(loc(Object, L))]).
/*impossible_if(putdown(_Robot, Object), 20) :-
	currentState(attr(surface(Object, brittle))). %%%%%%%%%%%%%%%%%%%%%%% !!! 1*/
impossible_if(putdown(Robot, Object), 21) :-
	not(currentState(fluent(in_hand(Robot, Object)))).

actionDescription(wait(Robot), [Robot], [robot], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actionDescription(serve(Robot, Obj, Person), [Robot, Obj, Person], [robot, item, person]).
causal_law(serve(Robot, Obj, Person), [], [fluent(in_hand(Person, Obj)), not(fluent(in_hand(Robot, Obj)))]).
causal_law(serve(Robot, Obj, Person), [fluent(loc(Robot, Loc))], [not(fluent(loc(Obj, Loc)))]).
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
	retractall(currentState(fluent(_))),
	retractall(step(_)),
	assert(step(1)),
	unexpectedStateFluents(S),
	assertFluents(S).

% The state is reset at random for each sequence of episodes.
resetStateAtRandom :-
	assert(lastActionWas(none)),
	retractall(currentState(fluent(_))),
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
	retractall(currentState(fluent(loc(A,_)))),
	retractall(currentState(fluent(in_hand(_,A)))),
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
	retractall(domain(attr(_))),
	Alts = [office, library, workshop],
	random_member(RA, Alts),
	random_member(RB, Alts),
	random_member(RC, Alts),
	assertAtts([type(rmoff,RA), type(rmwor,RB), type(rmlib,RC)]),
	%
	true.


% This is passed in a list of RELEVANT object properties
domainChangeObjectAtts(List) :-
	!,
	domainChangeObjectAttsRand(List).
	
domainChangeObjectAttsRand(List) :-
	random_member(X,List), % Pick one literal representing a static attribute to change at random
	change_att_value(X, Y), % Make a valid change to something other than the original value
	retractall(domain(X)),
	assert(domain(Y)).

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

	
	

% Physical configurations:
% 2 entities in 3 locations = 3^2 = 9
% 1 item that can be held (at most 1 per agent) or at location (any number at a location) = 5
% 9 * 5 = 45 physical configurations

% Object property configurations:
% 3 object property configurations




% Returns a number identifying the target axiom, or returns it back for "does not match any target axiom"

% 1. Learning that a person cannot be served in a library [EXECUTABILITY CONDITION]
domainAxiomClassifier([ [attr(surface(prin1, brittle))], [] ], 1) :- domainGoalAction(putdown(rob1,prin1)), !.
domainAxiomClassifier([ [], [attr(surface(prin1, hard))] ], 1) :- domainGoalAction(putdown(rob1,prin1)), !.

% 2. Learning that serving a person in a workshop causes them to leave [CAUSAL LAW]


%%%%%%%%%%%%%%%%%%%%

% Catch case: Everything else
domainAxiomClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- !.

%%%%%%%%%%%%%%%%%%%%






	

/* "Even when book1 is damaged and served to a non-engineer, under conditions [book1 is labelled], 'serve(rob1,book1,p1)' is possible" */
cached :-
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)), % Is this even needed???
	assert(allValidTests([
		attr(arm_type(rob1,electromagnetic)),attr(arm_type(rob1,pneumatic)),
		attr(obj_weight(book1,heavy)),attr(obj_weight(book1,light)),
		attr(surface(book1,brittle)),attr(surface(book1,hard)),
		attr(role_type(p1,engineer)),attr(role_type(p1,sales)),attr(role_type(p1,manager)),
		fluent(item_status(book1,intact)),fluent(item_status(book1,damaged)),
		fluent(labelled(book1,false)),fluent(labelled(book1,true)),
		fluent(loc(book1,kitchen)),fluent(loc(book1,library)),fluent(loc(book1,office)),fluent(loc(book1,workshop)),
		fluent(loc(rob1,kitchen)),fluent(loc(rob1,library)),fluent(loc(rob1,office)),fluent(loc(rob1,workshop)),
		fluent(loc(p1,kitchen)),fluent(loc(p1,library)),fluent(loc(p1,office)),fluent(loc(p1,workshop)),
		fluent(in_hand(p1,book1)),fluent(in_hand(rob1,book1)),
		action(serve(rob1,book1,p1)),action(pickup(rob1,book1)),action(putdown(rob1,book1)),action(move(rob1,office)),action(move(rob1,workshop)),action(move(rob1,kitchen)),action(move(rob1,library)),action(affix_label(rob1,book1))
		])),
	assert(num_possible_attribute_configs(24)),
	assert(usableActionList(
		[action(serve(rob1,book1,p1)),action(pickup(rob1,book1)),action(putdown(rob1,book1)),action(move(rob1,office)),action(move(rob1,workshop)),action(move(rob1,kitchen)),action(move(rob1,library)),action(affix_label(rob1,book1))]
		)).
%
domainGoalAction(serve(rob1,book1,p1)).
unexpectedResult([fluent(in_hand(p1,book1))]).
unexpectedStateFluents([loc(p1,library),loc(p2,workshop),loc(p3,office),loc(rob1,library),in_hand(p3,prin1),
	in_hand(rob1,book1),loc(cup1,workshop),loc(shelf1,library),loc(shelf2,kitchen),loc(desk1,office),loc(tab1,workshop),
	labelled(cup1,false),labelled(book1,true),labelled(prin1,false),
	item_status(cup1,intact),item_status(book1,damaged),item_status(prin1,intact)
	]). % An actual state from which unexpected outcomes occurred. Others are possible.
% executabilityConditionViolated(31). %(For affordances only)





