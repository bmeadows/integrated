/*
 * Name:        rrl_domain.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file encodes a demonstrative domain intended for use with q-RRL learning.
 */

:- discontiguous actionDescription/3, impossible_if/2, causal_law/3, currentState/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contents
% 1. Abstract domain description
% 2. Domain details
% 3. State constraints
% 4. Oracle: Actual domain transitions
% 5. Agent knowledge
% 6. Core domain functions
% 7. Target axioms
% 8. Scenario set-up
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% 1. Abstract domain description %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Domain sort hierarchy
subsort(thing, object).
subsort(thing, entity).
subsort(entity, robot).
subsort(entity, person).
subsort(object, item).
subsort(item, book).
subsort(item, cup).
%
ancestor(X,Y) :- subsort(X,Y).
ancestor(X,Y) :- subsort(Z,Y), ancestor(X,Z).
%
domain(sort(person(p0))).
domain(sort(person(p1))).
domain(sort(person(p2))).
domain(sort(robot(rob1))).
domain(sort(location(rmwor))).
domain(sort(location(rmoff))).
domain(sort(location(rmlib))).
domain(sort(book(book1))).
domain(sort(cup(cup1))).
%
domain(sort(Term1)) :- subsort(General,Specific),
	functor(Term1,General,1), arg(1,Term1,Arg),
	functor(Term2,Specific,1), arg(1,Term2,Arg),
	domain(sort(Term2)).

% Relational rule for inferred fluent (for determining location of held item)
currentState(fluent(loc(O,L))) :- domain(sort(object(O))), currentState(fluent(in_hand(X,O))), domain(sort(entity(X))), currentState(fluent(loc(X,L))).

% Determining permissible static attributes
valid(attr(type(L,T))) :- domain(sort(location(L))), member(T, [office, library, workshop]).
valid(attr(role_type(P,R))) :- domain(sort(person(P))), member(R, [engineer, manager, sales]).
valid(attr(arm_type(R,T))) :- domain(sort(robot(R))), member(T, [pneumatic, electromagnetic]).
valid(attr(obj_weight(O,W))) :- domain(sort(object(O))), member(W, [light, heavy]).
valid(attr(surface(O,S))) :- domain(sort(object(O))), member(S, [hard, brittle]). 

% Determining permissible fluents
valid(fluent(loc(X,Y))) :- domain(sort(thing(X))), domain(sort(location(Y))).
valid(fluent(in_hand(E,O))) :- domain(sort(entity(E))), domain(sort(item(O))).
valid(fluent(labelled(I,Bool))) :- domain(sort(item(I))), member(Bool, [true,false]).
valid(fluent(item_status(O,S))) :- domain(sort(item(O))), member(S, [intact, damaged]).

% Determining permissible actions
valid(action(pickup(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).
valid(action(putdown(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).
valid(action(move(R,L))) :- domain(sort(robot(R))), domain(sort(location(L))).
valid(action(serve(R,O,P))) :- domain(sort(robot(R))), domain(sort(item(O))), domain(sort(person(P))).
valid(action(affix_label(R,O))) :- domain(sort(robot(R))), domain(sort(object(O))).
valid(action(wait(R))) :- domain(sort(robot(R))).

% Define additional permissible alternatives for domain tests.
% The system already checks obvious general forms for this, e.g., it will find the substitution fluent(loc(x,y)) for fluent(loc(x,z)), but tests with different functors must be specified in the domain.
% For example, in this domain the location fluents loc and in_hand are in a sense interchangeable.
% Note 1: Any number of permitted_domain_test_alternatives may be specified.
% Note 2: permitted_domain_test_alternatives may be specified for static attributes or fluents.
% Note 3: Duplications of alternatives the system itself finds will be ignored.
% Note 4: There is NO valid( ) requirement here, i.e., it can return bad values, such as in_hand(p0, rob1) or in_hand(book1, p0) for this domain.
%			This predicate should never itself be directly used, and the parent 'get_all_alternative_domain_tests(Term, ReturnList)' which calls it should
%			only be used to swap in and out literals, followed always by testing for 'stateConstraintsViolated' (and starting again if there is a problem).
% Note 5: In fact, given the ways in which this functionality is used (error checking and noise addition), it is not particularly important if they it hazy, unfair, or has gaps.
permitted_domain_test_alternatives(fluent(in_hand(_A, X)), [fluent(loc(X, rmwor)), fluent(loc(X, rmoff)), fluent(loc(X, rmlib))]).
permitted_domain_test_alternatives(fluent(loc(X, _L)), [fluent(in_hand(p0, X)), fluent(in_hand(p1, X)), fluent(in_hand(p2, X)), fluent(in_hand(rob1, X))]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% 2. Domain details %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starting attributes
currentState(attr(type(rmwor,workshop))).
currentState(attr(type(rmoff,office))).
currentState(attr(type(rmlib,library))).
currentState(attr(role_type(p0,engineer))).
currentState(attr(role_type(p1,manager))).
currentState(attr(role_type(p2,sales))).
currentState(attr(arm_type(rob1,pneumatic))).
currentState(attr(obj_weight(book1,heavy))).
currentState(attr(obj_weight(cup1,light))).
currentState(attr(surface(book1,hard))).
currentState(attr(surface(cup1,brittle))).

domain_specified_end :-	step(last), !.
domain_specified_end :-	domainGoalAction(serve(rob1,book1,_)), ( currentState(fluent(in_hand(P,book1))), currentState(attr(person(P))) ), !.
domain_specified_end :-	domainGoalAction(pickup(rob1,X)), currentState(fluent(in_hand(_,X))), !.
domain_specified_end :- domainGoalAction(affix_label(rob1,X)), currentState(fluent(labelled(X,true))), !.
domain_specified_end :-	stateConstraintsViolated.

% Nonaction for edge cases where no actions are possible, e.g., due to unusual consequences of simulated noise
defaultNullAction([wait(rob1)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% 3. State constraints %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% State constraints
stateConstraintsViolated :- currentState(fluent(loc(O,L1))), currentState(fluent(loc(O,L2))), L1 \= L2. % One thing in two places
stateConstraintsViolated :- currentState(fluent(in_hand(H1,O))), currentState(fluent(in_hand(H2,O))), H1 \= H2. % Anything simultaneously in two different hands
stateConstraintsViolated :- currentState(fluent(in_hand(H,O1))), currentState(fluent(in_hand(H,O2))), O1 \= O2. % Same entity has two things in hand
stateConstraintsViolated :- domain(sort(thing(E))), not(currentState(fluent(loc(E,_)))). % An entity or object not at a place
stateConstraintsViolated :- currentState(X), not(valid(X)).
stateConstraintsViolated :- currentState(fluent(labelled(O,B1))), currentState(fluent(labelled(O,B2))), B1 \= B2. % Labelled and not labelled
stateConstraintsViolated :- domain(sort(item(O))), not(currentState(fluent(labelled(O,_)))). % No boolean value for a small object's labelled status
stateConstraintsViolated :- currentState(fluent(item_status(O,B1))), currentState(fluent(item_status(O,B2))), B1 \= B2. % Damaged and intact
stateConstraintsViolated :- domain(sort(item(O))), not(currentState(fluent(item_status(O,_)))). % No value for a small object's item status


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% 4. Oracle: Actual domain transitions %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clauses of this function must be appropriately defined for the system to perform simulation.

applyActionToState_SingleCase(move(Robot, Loc)) :-
	currentState(fluent(in_hand(Robot, O))),
	retract_facts_only(currentState(fluent(loc(O,_)))), % May or may not be overt
	retract_facts_only(currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.
applyActionToState_SingleCase(move(Robot, Loc)) :-
	not(currentState(fluent(in_hand(Robot, _)))),
	retract_facts_only(currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.

% Uncomment to learn the positive affordance. The presence of this conflicts with ability to learn the executability condition it modulates.
/*
%%(8) "An item with a brittle surface cannot be labelled by a robot, unless item is heavy and robot has electromagnetic arm." [positive affordance]
applyActionToState_SingleCase(affix_label(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	currentState(attr(surface(Obj, brittle))),
	% Affordance: electromagnetic arm, heavy item
	currentState(attr(arm_type(R, electromagnetic))),
	currentState(attr(obj_weight(Obj, heavy))),
	%
	currentState(fluent(labelled(Obj, false))),
	retract_facts_only(currentState(fluent(labelled(Obj, false)))),
	assert(currentState(fluent(labelled(Obj, true)))),
	!.
*/

%%(5) "An item with a brittle surface cannot be labelled by a robot." [executability condition]
applyActionToState_SingleCase(affix_label(_R, Obj)) :-
	currentState(attr(surface(Obj, brittle))),
	!.

%%(6) "An item with item_status 'damaged' cannot be labelled by a robot with a pneumatic arm." [negative affordance]
applyActionToState_SingleCase(affix_label(R, Obj)) :-
	currentState(fluent(item_status(Obj, damaged))),
	currentState(attr(arm_type(R, pneumatic))),
	!.

%%(7) labelling a light object with a pneumatic arm causes it to be damaged [causal law]
applyActionToState_SingleCase(affix_label(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	currentState(attr(arm_type(R, pneumatic))),
	currentState(attr(obj_weight(Obj, light))),
	%
	retract_facts_only(currentState(fluent(labelled(Obj, false)))),
	assert(currentState(fluent(labelled(Obj, true)))),
	retract_facts_only(currentState(fluent(item_status(Obj, _)))),
	assert(currentState(fluent(item_status(Obj, damaged)))),
	!.

% Succeeded label affix
applyActionToState_SingleCase(affix_label(R, Obj)) :-
	currentState(attr(surface(Obj, hard))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	currentState(fluent(labelled(Obj, false))),
	retract_facts_only(currentState(fluent(labelled(Obj, false)))),
	assert(currentState(fluent(labelled(Obj, true)))),
	!.

%%(3) A robot with a pneumatic arm cannot serve a brittle object [negative affordance]
applyActionToState_SingleCase(serve(R, Obj, _P)) :-
	currentState(attr(surface(Obj, brittle))),
	currentState(attr(arm_type(R, pneumatic))),
	!.

% Uncomment to learn the positive affordance. The presence of this conflicts with ability to learn the executability condition it modulates.
/*
%%(4) "Item cannot be served if damaged, except to an engineer, unless item is labelled." [positive affordance]
applyActionToState_SingleCase(serve(R, Obj, P)) :-
	currentState(fluent(item_status(Obj, damaged))),
	not(currentState(attr(role_type(P, engineer)))),
	% Affordance: Labelled
	currentState(fluent(labelled(Obj, true))),
	%
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	retract_facts_only(currentState(fluent(in_hand(R, Obj)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	% Note that this exception doesn't result in the causal law 'serve unlabelled object to salesperson makes it labelled' applying, because it requires the object already be labelled.
	% However, in general be careful.
	% Suggestion: Should move to a more ASP-like distributed representation for causal laws, which would resolve this.
	!.
*/

%%(2) "Item cannot be served if damaged, except to an engineer." [executability condition]
applyActionToState_SingleCase(serve(_R, Obj, P)) :-
	currentState(fluent(item_status(Obj, damaged))),
	not(currentState(attr(role_type(P, engineer)))),
	!.
%%(1) "Serving an object to a salesperson causes it to be labelled." [causal law]
applyActionToState_SingleCase(serve(R, Obj, P)) :-
	currentState(attr(role_type(P, sales))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	retract_facts_only(currentState(fluent(in_hand(R, Obj)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract_facts_only(currentState(fluent(labelled(Obj,_)))),
	assert(currentState(fluent(labelled(Obj,true)))),
	!.
	
% Succeeded serve: none of the above cases applied
applyActionToState_SingleCase(serve(R, Obj, P)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract_facts_only(currentState(fluent(in_hand(R, Obj)))),
	retract_facts_only(currentState(fluent(loc(Obj, Loc)))), % If overtly given, must remove, because of things like exogenous events, e.g., the object is served and then the person moves
	!.

% "Can't pick up a heavy object with a weak arm."
applyActionToState_SingleCase(pickup(R, Obj)) :-
	currentState(attr(obj_weight(Obj, heavy))),
	currentState(attr(arm_type(R, electromagnetic))),
	!.

% Succeeded pickup: none of the above cases applied
applyActionToState_SingleCase(pickup(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	not(currentState(fluent(in_hand(_, Obj)))),
	assert(currentState(fluent(in_hand(R, Obj)))),
	retract_facts_only(currentState(fluent(loc(Obj, Loc)))), % If overtly given, must remove, because of things like exogenous events, e.g., the object is served and then the person moves
	!.

% Breaking a brittle object by putting it down
applyActionToState_SingleCase(putdown(R, Obj)) :-
	currentState(attr(surface(Obj, brittle))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract_facts_only(currentState(fluent(in_hand(R, Obj)))),
	retract_facts_only(currentState(fluent(item_status(Obj,_)))),
	assert(currentState(fluent(item_status(Obj,damaged)))),
	!.

% Succeeded putdown
applyActionToState_SingleCase(putdown(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract_facts_only(currentState(fluent(in_hand(R, Obj)))),
	!.

applyActionToState_SingleCase(wait(_)) :- !.

applyActionToState_SingleCase(X) :-
	writef('Note: Unexpected oracle failure.\n'),
	print(X), nl,
	noiseChancePercent(Noise),
	((Noise > 0) -> true ; trace). % Check to ignore if noise is being simulated, as it can lead to impossible states.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% 5. Agent knowledge %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define the axioms comprising the agent's domain knowledge. Consists of:
% actionDescription/3,	a fact, with arguments: action term; ordered list of action arguments; ordered list of valid types for those arguments;
% causal_law/3,			a fact, with arguments: action term; list of fluent and static conditions; list of fluent consequences;
% impossible_if/2,		a rule, with arguments in head: action term; unique numeric ID for condition. Rule body contains necessary conditions for rule to fire and thus action to fail.
% Each action should have one action description, any number of causal laws, and any number of executability conditions (impossible_if).

actionDescription(wait(Robot), [Robot], [robot]).

actionDescription(move(Robot, Destination), [Robot, Destination], [robot, location]).
causal_law(move(Robot, Destination), [], [fluent(loc(Robot, Destination))]).
causal_law(move(Robot, _Destination), [fluent(loc(Robot, X))], [not(fluent(loc(Robot, X)))]).
causal_law(move(Robot, _Destination), [fluent(in_hand(Robot, Object)), fluent(loc(Robot, X))], [not(fluent(loc(Object, X)))]).
impossible_if(move(Robot, Destination), 10) :-
	currentState(fluent(loc(Robot, Destination))).

actionDescription(putdown(Robot, Object), [Robot, Object], [robot, item]).
causal_law(putdown(Robot, Object), [], [not(fluent(in_hand(Robot, Object)))]).
causal_law(putdown(Robot, Object), [fluent(loc(Robot,L))], [fluent(loc(Object, L))]).
causal_law(putdown(_Robot, Object), [fluent(item_status(Object,intact)), attr(surface(Object, brittle))], [not(fluent(item_status(Object,intact))), fluent(item_status(Object,damaged))]). % {Tested elsewhere}
impossible_if(putdown(Robot, Object), 20) :-
	not(currentState(fluent(in_hand(Robot, Object)))).

actionDescription(serve(Robot, Object, Person), [Robot, Object, Person], [robot, item, person]).
causal_law(serve(Robot, Object, Person), [], [fluent(in_hand(Person, Object)), not(fluent(in_hand(Robot, Object)))]).
causal_law(serve(_Robot, Object, Person), [attr(role_type(Person, sales)), fluent(labelled(Object,false))], [not(fluent(labelled(Object, false))), fluent(labelled(Object,true))]). % Causal law to be elided for test #1
% Causal laws do not need to overtly retract derived fluents, e.g. Object being at its holder's Location.
impossible_if(serve(Robot, _Object, Person), 30) :-
	not((	currentState(fluent(loc(Robot, Location))),
			currentState(fluent(loc(Person, Location))) )).
impossible_if(serve(Robot, Object, _Person), 31) :-
	not(currentState(fluent(in_hand(Robot, Object)))).
impossible_if(serve(_Robot, _Object, Person), 32) :-
	currentState(fluent(in_hand(Person, _))).
impossible_if(serve(Robot, Object, _Person), 33) :-
	currentState(attr(surface(Object, brittle))),
	currentState(attr(arm_type(Robot, pneumatic))). % Disaffordance to be elided for test #3
impossible_if(serve(_Robot, Object, Person), 34) :- % This combines executability condition and its positive affordance...
	currentState(not(attr(role_type(Person, engineer)))),
	currentState(not(fluent(item_status(Object,intact)))),
	currentState(fluent(labelled(Object,false))). % Executability condition to be elided for test #2 ; both this and positive affordance to be elided for test #4

actionDescription(pickup(Robot, Object), [Robot, Object], [robot, item]).
causal_law(pickup(Robot, Object), [fluent(loc(Robot,L))], [not(fluent(loc(Object, L))), fluent(in_hand(Robot, Object))]).
impossible_if(pickup(Robot, Object), 40) :-
	not((	currentState(fluent(loc(Robot, Loc))),
			currentState(fluent(loc(Object, Loc))) )).
impossible_if(pickup(Robot, Object), 41) :-
	currentState(attr(obj_weight(Object, heavy))),
	currentState(attr(arm_type(Robot, electromagnetic))). % {Tested elsewhere}
impossible_if(pickup(_Robot, Object), 42) :-
	currentState(fluent(in_hand(_, Object))).
impossible_if(pickup(Robot, _Object), 43) :-
	currentState(fluent(in_hand(Robot, _))).

actionDescription(affix_label(Robot, Object), [Robot, Object], [robot, object]).
causal_law(affix_label(_Robot, Object), [fluent(labelled(Object, false))], [not(fluent(labelled(Object, false))), fluent(labelled(Object, true))]).
causal_law(affix_label(Robot, Object), [attr(arm_type(Robot, pneumatic)), attr(obj_weight(Object, light)), fluent(item_status(Object, intact))],
										[not(fluent(item_status(Object, intact))), fluent(item_status(Object, damaged))]). % Causal law to be elided for test #7
impossible_if(affix_label(Robot, Object), 50) :-
	not((	currentState(fluent(loc(Robot, Loc))), currentState(fluent(loc(Object, Loc))) )).
impossible_if(affix_label(_Robot, Object), 51) :-
	currentState(fluent(labelled(Object, true))).
impossible_if(affix_label(Robot, Object), 52) :- % This combines executability condition and its positive affordance...
	currentState(attr(surface(Object, brittle))),
	not((   currentState(attr(arm_type(Robot, electromagnetic))),
		currentState(attr(obj_weight(Object, heavy)))   )). % Executability condition to be elided in test #5 ; both this and positive affordance to be elided for test #8
impossible_if(affix_label(Robot, Object), 53) :-
	currentState(fluent(item_status(Object, damaged))),
	currentState(attr(arm_type(Robot, pneumatic))). % Disaffordance to be elided in test #6


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% 6. Core domain functions %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Usage: Call this function only when a fluent configuration does not exist, otherwise behaviour is undefined.
setupFluentsRandomly :-
	% 1. Establish locations of entities
	setupEntityL(rob1), setupEntityL(p0), setupEntityL(p1), setupEntityL(p2),
	% 2. Any object may be labelled
	Alts = [true, false],
	random_member(RA, Alts),
	random_member(RB, Alts),
	assert(currentState(fluent(labelled(cup1,RA)))),
	assert(currentState(fluent(labelled(book1,RB)))),
	% 3. Objects may be intact or damaged
	Alts2 = [intact, damaged],
	random_member(RA2, Alts2),
	random_member(RB2, Alts2),
	assert(currentState(fluent(item_status(cup1,RA2)))),
	assert(currentState(fluent(item_status(book1,RB2)))),
	% 4. Establish object locations last -- important, as it involves testing state consistency.
	setObjLocationsRandomlyUntilValid([book1,cup1]),
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
	S = [in_hand(rob1,A),in_hand(p0,A),in_hand(p1,A),in_hand(p2,A),loc(A,rmlib),loc(A,rmwor),loc(A,rmoff)],
	random_member(F, S),
	assert(currentState(fluent(F))),
	randomiseAllLocations(B).

% Returns all physical states, even if they break constraints
getTheoreticalStatePermutation(List) :-
	try_all_locations([p0,p1,p2,rob1,book1,cup1],[p0,p1,p2,rob1],[rmwor,rmoff,rmlib],[],List).

try_all_locations([],_,_,Return,Return).
try_all_locations([A|B],Entities,Places,Working,Return) :-
	not(member(A,Entities)), % Precludes people being assigned in_hand other people
	member(X,Entities),
	append(Working,[in_hand(X,A)],New),
	try_all_locations(B,Entities,Places,New,Return).
try_all_locations([A|B],Entities,Places,Working,Return) :-
	member(X,Places),
	append(Working,[loc(A,X)],New),
	try_all_locations(B,Entities,Places,New,Return).
	
assertOneFluentAtRandom(List) :-
	random_member(F, List),
	assert(currentState(fluent(F))).

assertOneXAtRandom(List) :-
	random_member(X, List),
	assert(X).

% Important: This function is used by the qRRL system, and must be defined in each domain.
setRandomInitialStaticConfiguration :-
	retract_facts_only(currentState(attr(_))),
	%
	Alts = [office, library, workshop],
	random_member(RA, Alts),
	random_member(RB, Alts),
	random_member(RC, Alts),
	assertAtts([type(rmoff,RA), type(rmwor,RB), type(rmlib,RC)]),
	%
	Alts2 = [engineer, manager, sales],
	random_member(RA2, Alts2),
	random_member(RB2, Alts2),
	random_member(RC2, Alts2),
	assertAtts([role_type(p0,RA2), role_type(p1,RB2), role_type(p2,RC2)]),
	%
	Alts3 = [pneumatic, electromagnetic],
	random_member(RA3, Alts3),
	assertAtts([arm_type(rob1,RA3)]),
	%
	Alts4 = [light, heavy],
	random_member(RA4, Alts4),
	random_member(RB4, Alts4),
	assertAtts([obj_weight(cup1,RA4), obj_weight(book1,RB4)]),
	%
	Alts5 = [hard, brittle],
	random_member(RA5, Alts5),
	random_member(RB5, Alts5),
	assertAtts([surface(cup1,RA5), surface(book1,RB5)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% 7. Target axioms %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define all target axioms to be learned by the qRRL system.
% The first argument is the [Positive, Negative] literals of the target axiom.
% The second argument gives a number identifying the target axiom, or ignore_axiom if it is not to be counted as a true positive. If no target is not matched it should give any other value.

% 1. Causal law: Unlabelled object served to a sales person becomes labelled.
domainAxiomClassifier([ [attr(role_type(P, sales))], [] ], 1) :- domainGoalAction(serve(rob1,_Ob,P)), !.
domainAxiomClassifier([ [], [attr(role_type(P, engineer)), attr(role_type(P, manager))] ], 1) :- domainGoalAction(serve(rob1,_Ob,P)), !.

% Technicality (ignore): Object served to a sales person is already labelled.
domainAxiomClassifier([ [fluent(labelled(O, true))], [] ], ignore_axiom) :- domainGoalAction(serve(rob1,O,_P)), !.
domainAxiomClassifier([ [], [fluent(labelled(O, false))] ], ignore_axiom) :- domainGoalAction(serve(rob1,O,_P)), !.

% 2. Executability condition: A damaged object, served to a non-engineer, causes action failure.
domainAxiomClassifier([ [], [attr(role_type(P, engineer)), fluent(item_status(O1, intact))] ], 2) :- domainGoalAction(serve(rob1,O1,P)), !.
domainAxiomClassifier([ [fluent(item_status(O1, damaged))], [attr(role_type(P,engineer))] ], 2) :- domainGoalAction(serve(rob1,O1,P)), !.

% 3. Negative affordance: A robot with a pneumatic arm cannot serve a brittle object.
domainAxiomClassifier([ [],[attr(arm_type(rob1,electromagnetic)),attr(surface(O1,hard))] ], 3) :- domainGoalAction(serve(rob1,O1,_P)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic)),attr(surface(O1,brittle))],[] ], 3) :- domainGoalAction(serve(rob1,O1,_P)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic))],[attr(surface(O1,hard))] ], 3) :- domainGoalAction(serve(rob1,O1,_P)), !.
domainAxiomClassifier([ [attr(surface(O1,brittle))],[attr(arm_type(rob1,electromagnetic))] ], 3) :- domainGoalAction(serve(rob1,O1,_P)), !.

% 4. Positive affordance: Even when a damaged object is served to a non-engineer, under conditions [object is labelled], serve action may succeed.
domainAxiomClassifier([ [fluent(labelled(O1, true))], [] ], 4) :- domainGoalAction(serve(rob1,O1,_P1)), !.
domainAxiomClassifier([ [], [fluent(labelled(O1, false))] ], 4) :- domainGoalAction(serve(rob1,O1,_P1)), !.
% Versions of positive affordances that include partial information from their associated executability condition are considered to be valid as target axioms:
domainAxiomClassifier([ [fluent(labelled(O1, true))], [attr(role_type(P1,engineer))] ], 4) :- domainGoalAction(serve(rob1,O1,P1)), !.
domainAxiomClassifier([ [], [attr(role_type(P1,engineer)), fluent(labelled(O1, false))] ], 4) :- domainGoalAction(serve(rob1,O1,P1)), !.
domainAxiomClassifier([ [fluent(labelled(O1, true))], [fluent(item_status(O1, intact))] ], 4) :- domainGoalAction(serve(rob1,O1,_P1)), !.
domainAxiomClassifier([ [], [fluent(item_status(O1, intact)), fluent(labelled(O1, false))] ], 4) :- domainGoalAction(serve(rob1,O1,_P1)), !.
domainAxiomClassifier([ [fluent(labelled(O1, true))], [attr(role_type(P1,engineer)), fluent(item_status(O1, intact))] ], 4) :- domainGoalAction(serve(rob1,O1,P1)), !.
domainAxiomClassifier([ [], [attr(role_type(P1,engineer)), fluent(item_status(O1, intact)), fluent(labelled(O1, false))] ], 4) :- domainGoalAction(serve(rob1,O1,P1)), !.
domainAxiomClassifier([ [fluent(item_status(O1, damaged)), fluent(labelled(O1, true))], [] ], 4) :- domainGoalAction(serve(rob1,O1,_P1)), !.
domainAxiomClassifier([ [fluent(item_status(O1, damaged))], [fluent(labelled(O1, false))] ], 4) :- domainGoalAction(serve(rob1,O1,_P1)), !.
domainAxiomClassifier([ [fluent(item_status(O1, damaged)), fluent(labelled(O1, true))], [attr(role_type(P1,engineer))] ], 4) :- domainGoalAction(serve(rob1,O1,P1)), !.
domainAxiomClassifier([ [fluent(item_status(O1, damaged))], [attr(role_type(P1,engineer)), fluent(labelled(O1, false))] ], 4) :- domainGoalAction(serve(rob1,O1,P1)), !.

% 5. Executability condition: An object without a hard surface, when labelling is attempted, causes action failure.
domainAxiomClassifier([ [attr(surface(O1, brittle))], [] ], 5) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [], [attr(surface(O1, hard))] ], 5) :- domainGoalAction(affix_label(rob1,O1)), !.

% 6. Negative affordance: A robot with a pneumatic arm cannot label a damaged object.
domainAxiomClassifier([ [],[attr(arm_type(rob1,electromagnetic)),fluent(item_status(O1,intact))] ], 6) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic)),fluent(item_status(O1,damaged))],[] ], 6) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic))],[fluent(item_status(O1,intact))] ], 6) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [fluent(item_status(O1,damaged))],[attr(arm_type(rob1,electromagnetic))] ], 6) :- domainGoalAction(affix_label(rob1,O1)), !.

% 7. Causal law: Light object labelled with a pneumatic arm becomes damaged.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic)), attr(obj_weight(O1,light))], [] ], 7) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(obj_weight(O1,light))], [attr(arm_type(rob1,electromagnetic))] ], 7) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic))], [attr(obj_weight(O1,heavy))] ], 7) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [], [attr(arm_type(rob1,electromagnetic)), attr(obj_weight(O1,heavy))] ], 7) :- domainGoalAction(affix_label(rob1,O1)), !.

% Technicality (ignore): Object being labelled is already damaged.
domainAxiomClassifier([ [fluent(item_status(O1, damaged))], [] ], ignore_axiom) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [], [fluent(item_status(O1, intact))] ], ignore_axiom) :- domainGoalAction(affix_label(rob1,O1)), !.

% 8. Positive affordance: Even when an object without a hard surface is labelled by a robot, under conditions [object is heavy and robot has electromagnetic arm], label action may succeed.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)),attr(obj_weight(O1,heavy))], [] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))], [attr(obj_weight(O1,light))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(obj_weight(O1,heavy))], [attr(arm_type(rob1,pneumatic))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [], [attr(arm_type(rob1,pneumatic)),attr(obj_weight(O1,light))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
% Versions of positive affordances that include partial information from their associated executability condition are considered to be valid as target axioms:
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(obj_weight(O1,heavy)), attr(surface(O1, brittle))], [] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(surface(O1, brittle))], [attr(obj_weight(O1,light))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(obj_weight(O1,heavy)), attr(surface(O1, brittle))], [attr(arm_type(rob1,pneumatic))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(surface(O1, brittle))], [attr(arm_type(rob1,pneumatic)), attr(obj_weight(O1,light))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(obj_weight(O1,heavy))], [attr(surface(O1, hard))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))], [attr(obj_weight(O1,light)), attr(surface(O1, hard))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [attr(obj_weight(O1,heavy))], [attr(arm_type(rob1,pneumatic)), attr(surface(O1, hard))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.
domainAxiomClassifier([ [], [attr(arm_type(rob1,pneumatic)), attr(obj_weight(O1,light)), attr(surface(O1, hard))] ], 8) :- domainGoalAction(affix_label(rob1,O1)), !.

% Catch case: Fail to match any target axiom.
domainAxiomClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% 8. Scenario set-up %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Provides a cached version of the calculated 'relevant subset' of parts of the domain.
cached :-
	assert(allValidTests([
		attr(arm_type(rob1,electromagnetic)),attr(arm_type(rob1,pneumatic)),
		attr(obj_weight(book1,heavy)),attr(obj_weight(book1,light)),
		attr(role_type(p0,engineer)),attr(role_type(p0,manager)),attr(role_type(p0,sales)),
		attr(surface(book1,brittle)),attr(surface(book1,hard)),
		% Action can happen in any room, so all rooms have relevant attributes
		attr(type(rmwor,workshop)),attr(type(rmwor,office)),attr(type(rmwor,library)),
		attr(type(rmoff,office)),attr(type(rmoff,workshop)),attr(type(rmoff,library)),
		attr(type(rmlib,library)),attr(type(rmlib,workshop)),attr(type(rmlib,office)),
		%
		fluent(loc(book1,rmwor)),fluent(loc(book1,rmoff)),fluent(loc(book1,rmlib)),
		fluent(loc(rob1,rmwor)),fluent(loc(rob1,rmoff)),fluent(loc(rob1,rmlib)),
		fluent(loc(p0,rmwor)),fluent(loc(p0,rmoff)),fluent(loc(p0,rmlib)),
		fluent(in_hand(p0,book1)),fluent(in_hand(rob1,book1)),
		fluent(labelled(book1,true)),fluent(labelled(book1,false)),
		fluent(item_status(book1,intact)),fluent(item_status(book1,damaged)),
		action(serve(rob1,book1,p0)),action(pickup(rob1,book1)),action(putdown(rob1,book1)),action(move(rob1,rmwor)),action(move(rob1,rmoff)),action(move(rob1,rmlib)),action(affix_label(rob1,book1))
		])),
	% 3 room types * 3 room types * 3 room types * 3 person roles * 2 arm types * 2 book weights * 2 book surfaces; cup and two people are not relevant
	assert(num_possible_attribute_configs(648)),
	assert(usableActionList(
		[action(serve(rob1,book1,p0)),action(pickup(rob1,book1)),action(putdown(rob1,book1)),action(move(rob1,rmwor)),action(move(rob1,rmoff)),action(move(rob1,rmlib)),action(affix_label(rob1,book1))]
		)).

% The observed action with unexpected consequences that initiated learning.
domainGoalAction(serve(rob1,book1,p0)).

% The unexpected consequences that were observed.
unexpectedResult([not(fluent(in_hand(p0,book1))), fluent(in_hand(rob1,book1))]).

% An actual state from which unexpected outcomes were observed to occur. Others are possible.
unexpectedStateFluents([loc(p0,rmlib),loc(rob1,rmlib),in_hand(rob1,book1),
				loc(p1,rmoff),loc(p2,rmoff), loc(cup1,rmoff),
				labelled(book1,false),labelled(cup1,false),
				item_status(book1,damaged),item_status(cup1,intact)
	]).

% When learning positive affordances, a statement in this form should be included to indicate which executability condition appeared to be violated:
% executabilityConditionViolated(31).
