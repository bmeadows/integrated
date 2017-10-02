:- dynamic step/1.
:- discontiguous actionDescription/3, impossible_if/2, causal_law/3.

% causal_law(Action, FluentsAndStaticsThatMustHold, Consequences).
% Can have multiple causal laws per action

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subsort(thing, location).
subsort(thing, object).
subsort(thing, entity).
subsort(entity, robot).
subsort(entity, person).
subsort(person, salesperson).
subsort(person, engineer).
subsort(person, manager).
subsort(object, item).
subsort(object, furniture).
subsort(item, cup).
subsort(item, folder).
subsort(item, book).
subsort(item, printer).
subsort(furniture, shelf).
subsort(furniture, desk).
subsort(furniture, cabinet).
subsort(furniture, table).
%
ancestor(X,Y) :- subsort(X,Y).
ancestor(X,Y) :- subsort(Z,Y), ancestor(X,Z).
%
domain(sort(salesperson(p1))).
domain(sort(engineer(p2))).
domain(sort(engineer(p3))).
domain(sort(robot(rob1))).
domain(sort(location(office))).
domain(sort(location(workshop))).
domain(sort(location(kitchen))).
domain(sort(location(library))).
domain(sort(cup(cup1))).
domain(sort(cup(cup2))).
domain(sort(folder(fol1))).
domain(sort(folder(fol2))).
domain(sort(book(book1))).
domain(sort(printer(prin1))).
domain(sort(shelf(shelf1))).
domain(sort(shelf(shelf2))).
domain(sort(desk(desk1))).
domain(sort(table(tab1))).
domain(sort(cabinet(cab1))).
domain(sort(cabinet(cab2))).
%
domain(sort(Term1)) :- subsort(General,Specific), functor(Term1,General,1), arg(1,Term1,Arg), functor(Term2,Specific,1), arg(1,Term2,Arg), domain(sort(Term2)).

% = currentState(attr()) ? TODO resolve

domain(attr(importance(p1,important))).
domain(attr(importance(p2,important))).
domain(attr(importance(p3,unimportant))).
domain(attr(color(cup1,blue))).
domain(attr(color(cup2,white))).
domain(attr(color(fol1,green))).
domain(attr(color(fol2,red))).
domain(attr(color(book1,green))).
domain(attr(color(prin1,white))).
domain(attr(color(shelf1,metallic))).
domain(attr(color(shelf2,metallic))).
domain(attr(color(desk1,white))).
domain(attr(color(tab1,metallic))).
domain(attr(color(cab1,white))).
domain(attr(color(cab2,metallic))).
domain(attr(obj_weight(cup1,light))).
domain(attr(obj_weight(cup2,light))).
domain(attr(obj_weight(fol1,light))).
domain(attr(obj_weight(fol2,light))).
domain(attr(obj_weight(book1,light))).
domain(attr(obj_weight(prin1,heavy))).
domain(attr(obj_weight(shelf1,light))).
domain(attr(obj_weight(shelf2,light))).
domain(attr(obj_weight(desk1,heavy))).
domain(attr(obj_weight(tab1,heavy))).
domain(attr(obj_weight(cab1,heavy))).
domain(attr(obj_weight(cab2,heavy))).
domain(attr(surface(cup1,brittle))).
domain(attr(surface(cup2,hard))).
domain(attr(surface(fol1,hard))).
domain(attr(surface(fol2,hard))).
domain(attr(surface(book1,hard))).
domain(attr(surface(prin1,brittle))).
domain(attr(surface(shelf1,hard))).
domain(attr(surface(shelf2,hard))).
domain(attr(surface(desk1,hard))).
domain(attr(surface(tab1,hard))).
domain(attr(surface(cab1,hard))).
domain(attr(surface(cab2,hard))).
domain(attr(arm_type(rob1,pneumatic))).

%

valid(attr(importance(P,R))) :- domain(sort(person(P))), member(R, [important, unimportant]).
valid(attr(color(O,C))) :- domain(sort(object(O))), member(C, [red, blue, green, white, metallic]).
valid(attr(obj_weight(O,S))) :- domain(sort(object(O))), member(S, [light, heavy]).
valid(attr(surface(O,S))) :- domain(sort(item(O))), member(S, [hard, brittle]). 
valid(attr(arm_type(R,S))) :- domain(sort(robot(R))), member(S, [pneumatic, electromagnetic]).

% Permissible fluents
valid(fluent(loc(X,Y))) :- domain(sort(entity(X))), domain(sort(location(Y))).
valid(fluent(loc(X,Y))) :- domain(sort(object(X))), domain(sort(location(Y))).
valid(fluent(in_hand(E,O))) :- domain(sort(entity(E))), domain(sort(item(O))).
valid(fluent(item_status(O,S))) :- domain(sort(item(O))), member(S, [intact, damaged]).
valid(fluent(reflectivity(O,S))) :- domain(sort(object(O))), member(S, [bright, dull]).
valid(fluent(labelled(O,Bool))) :- domain(sort(item(O))), member(Bool, [true,false]).
valid(fluent(open(C,Bool))) :- domain(sort(cabinet(C))), member(Bool, [true,false]).

% Permissible actions
valid(action(pickup(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).
valid(action(putdown(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).
valid(action(move(R,L))) :- domain(sort(robot(R))), domain(sort(location(L))).
valid(action(serve(R,O,P))) :- domain(sort(robot(R))), domain(sort(person(P))), domain(sort(item(O))).
valid(action(affix_label(R,O))) :- domain(sort(robot(R))), domain(sort(item(O))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_specified_end :-	step(last), !.
domain_specified_end :-	domainGoalAction(serve(rob1,cup1,p1)), ( currentState(fluent(in_hand(P,cup1))), currentState(attr(person(P))) ), !.
domain_specified_end :-	domainGoalAction(pickup(rob1,X)), currentState(fluent(in_hand(_,X))), !.
domain_specified_end :-	domainGoalAction(affix_label(rob1,X)), currentState(fluent(labelled(X,true))), !.

domain_specified_end :-	( not(currentState(fluent(loc(cup1,_)))), not(currentState(fluent(in_hand(_,cup1)))) ).
domain_specified_end :-	( not(currentState(fluent(loc(book1,_)))), not(currentState(fluent(in_hand(_,book1)))) ).
domain_specified_end :-	( not(currentState(fluent(loc(prin1,_)))), not(currentState(fluent(in_hand(_,prin1)))) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% STATE CONSTRAINTS
stateConstraintsViolated :- currentState(fluent(loc(O,L1))), currentState(fluent(loc(O,L2))), L1 \= L2. % One thing in two places
stateConstraintsViolated :- currentState(fluent(loc(O,_))), currentState(fluent(in_hand(_,O))). % Anything simultaneously in hand and at a place
stateConstraintsViolated :- currentState(fluent(in_hand(H1,O))), currentState(fluent(in_hand(H2,O))), H1 \= H2. % Anything simultaneously in two different hands
stateConstraintsViolated :- currentState(fluent(in_hand(H,O1))), currentState(fluent(in_hand(H,O2))), O1 \= O2. % Same entity has two things in hand
stateConstraintsViolated :- domain(sort(entity(E))), not(currentState(fluent(loc(E,_)))). % An entity not at a place
stateConstraintsViolated :- domain(sort(object(O))), not(currentState(fluent(loc(O,_)))), not(currentState(fluent(in_hand(_,O)))). % An object neither in_hand nor at a place
stateConstraintsViolated :- currentState(fluent(labelled(O,B1))), currentState(fluent(labelled(O,B2))), B1 \= B2. % Labelled and not labelled
stateConstraintsViolated :- currentState(fluent(item_status(O,B1))), currentState(fluent(item_status(O,B2))), B1 \= B2. % Damaged and intact
stateConstraintsViolated :- domain(sort(item(O))), not(currentState(fluent(labelled(O,_)))). % No boolean value for a small object being labelled

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_test_alternatives(attr(arm_type(X, electromagnetic)), [attr(arm_type(X, pneumatic))]).
domain_test_alternatives(attr(arm_type(X, pneumatic)), [attr(arm_type(X, electromagnetic))]).

domain_test_alternatives(attr(role_type(P, sales)), [attr(role_type(P, engineer)), attr(role_type(P, manager))]).
domain_test_alternatives(attr(role_type(P, engineer)), [attr(role_type(P, sales)), attr(role_type(P, manager))]).
domain_test_alternatives(attr(role_type(P, manager)), [attr(role_type(P, sales)), attr(role_type(P, engineer))]).

domain_test_alternatives(attr(obj_weight(X, heavy)), [attr(obj_weight(X, light))]).
domain_test_alternatives(attr(obj_weight(X, light)), [attr(obj_weight(X, heavy))]).

domain_test_alternatives(attr(surface(X, brittle)), [attr(surface(X, hard))]).
domain_test_alternatives(attr(surface(X, hard)), [attr(surface(X, brittle))]).

domain_test_alternatives(fluent(labelled(X,true)), [fluent(labelled(X,false))]) :- !.
domain_test_alternatives(fluent(labelled(X,false)), [fluent(labelled(X,true))]) :- !.
domain_test_alternatives(fluent(item_status(X,damaged)), [fluent(item_status(X,intact))]) :- !.
domain_test_alternatives(fluent(item_status(X,intact)), [fluent(item_status(X,damaged))]) :- !.

domain_test_alternatives(N, Return) :-
	N=fluent(_),
	select(N,
	[fluent(loc(X, office)), fluent(loc(X, library)), fluent(loc(X, workshop)), fluent(loc(X, kitchen)), fluent(in_hand(p1, X)), fluent(in_hand(p2, X)), fluent(in_hand(p3, X)), fluent(in_hand(rob1, X))],
	Return).
% Note that this can return some bad values, such as in_hand(p1, p2) or even in_hand(p1, p2), but these functions are only used to swap in and out literals,
% which is always followed by testing for 'stateConstraintsViolated' (and starting again if there is a problem).


domain_test_alternatives(attr(Literal), Alts) :-
	domain(attr(Literal)), attr_alts(List), select(Literal, List, Alts), !. % Assume it's only in one list.
domain_test_alternatives(fluent(Literal), Alts) :-
	domain(fluent(Literal)), fluent_alts(List), select(Literal, List, Alts), !. % Assume it's only in one list.
	


/*
************************************************************************************************************************************
************************************************************************************************************************************
Positive affordance sought:
- Action 'label(rob1, cup1)'
- Exception to executability condition "an object with a brittle surface cannot be labelled"
- Even when cup1 is brittle, under conditions [object is heavy, robot arm is electromagnetic], 'label(rob1, cup1)' is possible
************************************************************************************************************************************
************************************************************************************************************************************
*/

defaultNullAction([wait(rob1)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% % % ORACLE % % %

applyActionToState(Action) :-
	step(I),
	% 1. Update time
	retractall(step(I)),
	(I == 9 -> J = last ; J is I + 1),
	assert(step(J)),
	% 2. Apply action
	applyActionToStateFinal(Action),
	applyNoiseWhereAppropriate.
	% 3. Move people who had been about to move

applyActionToStateFinal(move(Robot, Loc)) :-
	retractall(	currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.

% 'serve':

% Positive affordance - action succeeds despite (damaged + non-engineer)
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(fluent(item_status(Obj, damaged))),
	not(currentState(attr(role_type(P, engineer)))),
	% Affordance: Labelled
	currentState(fluent(labelled(Obj, true))),
	%
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	% Note that this exception doesn't result in the causal law 'serve unlabelled object to salesperson makes it labelled' applying,
	% because it requires the object already be labelled.
	% However, in other more general cases I'll have to be careful.
	% Should really move to an ASP-like distributed representation for causal laws, because it would fix this.
	!.

% "An object should not be served if damaged, except to an engineer" [negative affordance]
applyActionToStateFinal(serve(_R, Obj, P)) :-
	currentState(fluent(item_status(Obj, damaged))),
	not(currentState(attr(role_type(P, engineer)))),
	!.
% Serving an object to a salesperson
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(attr(role_type(P, sales))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract(currentState(fluent(labelled(Obj,_)))),
	assert(currentState(fluent(labelled(Obj,true)))),
	!.
	
% 'pickup':

% "Can't pick up a heavy object with a weak arm."
applyActionToStateFinal(pickup(R, Obj)) :-
	currentState(attr(obj_weight(Obj, heavy))),
	currentState(attr(arm_type(R, electromagnetic))),
	!.
	
% Breaking a brittle object
applyActionToStateFinal(putdown(R, Obj)) :-
	currentState(attr(surface(Obj, brittle))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	retract(currentState(fluent(item_status(Obj,_)))),
	assert(currentState(fluent(item_status(Obj,damaged)))),
	!.

% 'affix_label':

% Positive affordance - action succeeds despite brittle
applyActionToStateFinal(affix_label(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	currentState(attr(surface(Obj, brittle))),
	% Affordance: Electromagnetic & Heavy
	currentState(attr(arm_type(R, electromagnetic))),
	currentState(attr(obj_weight(Obj, heavy))),
	%
	currentState(fluent(labelled(Obj, false))),
	retract(currentState(fluent(labelled(Obj, false)))),
	assert(currentState(fluent(labelled(Obj, true)))),
	!.

% "An object can only be labelled if it has a hard surface"
applyActionToStateFinal(affix_label(_R, Obj)) :-
	currentState(attr(surface(Obj, brittle))),
	!.

% "A damaged object should not be labelled by a pneumatic arm"
applyActionToStateFinal(affix_label(R, Obj)) :-
	currentState(fluent(item_status(Obj, damaged))),
	currentState(attr(arm_type(R, pneumatic))),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Succeeded serve: none of the above cases applied
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	!.

% Succeeded pickup: none of the above cases applied
applyActionToStateFinal(pickup(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	not(currentState(fluent(in_hand(_, Obj)))),
	assert(currentState(fluent(in_hand(R, Obj)))),
	retract(currentState(fluent(loc(Obj, Loc)))),
	!.

% Succeeded putdown
applyActionToStateFinal(putdown(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	!.

% Succeeded label affix
applyActionToStateFinal(affix_label(R, Obj)) :-
	currentState(attr(surface(Obj, hard))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	currentState(fluent(labelled(Obj, false))),
	retract(currentState(fluent(labelled(Obj, false)))),
	assert(currentState(fluent(labelled(Obj, true)))),
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

% Check commenting for whether particular different axioms are targeted or known

actionDescription(serve(Robot, Obj, Person), [Robot, Obj, Person], [robot, item, person]).
causal_law(serve(Robot, Obj, Person), [], [fluent(in_hand(Person, Obj)), not(fluent(in_hand(Robot, Obj)))]).
impossible_if(serve(Robot, _Obj, Person), 30) :-
	not((	currentState(fluent(loc(Robot, Location))),
			currentState(fluent(loc(Person, Location))) )).
impossible_if(serve(_Robot, Obj, Person), 31) :-
	not(currentState(attr(role_type(Person, engineer)))),
	not(currentState(fluent(item_status(Obj, intact)))). %%%%%%%%%%%%%%%%%%%%%%% !!! 2
impossible_if(serve(Robot, Obj, _Person), 32) :-
	not(currentState(fluent(in_hand(Robot, Obj)))).
impossible_if(serve(_Robot, _Obj, Person), 33) :-
	currentState(fluent(in_hand(Person, _))).
	
	
actionDescription(pickup(Robot, Object), [Robot, Object], [robot, item]).
causal_law(pickup(Robot, Object), [fluent(loc(Robot,L))], [not(fluent(loc(Object, L))), fluent(in_hand(Robot, Object))]).
impossible_if(pickup(Robot, Object), 40) :-
	not((	currentState(fluent(loc(Robot, Loc))),
			currentState(fluent(loc(Object, Loc))) )).
impossible_if(pickup(Robot, Object), 41) :-
	currentState(attr(obj_weight(Object, heavy))),
	currentState(attr(arm_type(Robot, electromagnetic))). %%%%%%%%%%%%%%%%%%%%%%% !!! 4
impossible_if(pickup(_Robot, Object), 42) :-
	currentState(fluent(in_hand(_, Object))).
impossible_if(pickup(Robot, _Object), 43) :-
	currentState(fluent(in_hand(Robot, _))).

actionDescription(affix_label(Robot, Object), [Robot, Object], [robot, item]).
causal_law(affix_label(Robot, Object), [], [fluent(labelled(Object, true))]).
impossible_if(affix_label(Robot, Object), 50) :-
	not((	currentState(fluent(loc(Robot, Loc))),
			currentState(fluent(loc(Object, Loc))) )).
impossible_if(affix_label(_Robot, Object), 51) :-
	currentState(fluent(labelled(Object, true))).
impossible_if(affix_label(_Robot, Object), 52) :-
	currentState(attr(surface(Object, brittle))). %%%%%%%%%%%%%%%%%%%%%%% !!! 5
impossible_if(affix_label(Robot, Object), 53) :-
	currentState(fluent(item_status(Object, damaged))),
	currentState(attr(arm_type(Robot, pneumatic))). %%%%%%%%%%%%%%%%%%%%%%% !!! 6
	
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
	setupEntityL(rob1), setupEntityL(p1), setupEntityL(p2), setupEntityL(p3),
	% 2. Do fixed object locations
	assert(currentState(fluent(loc(tab1,workshop)))),
	assert(currentState(fluent(loc(shelf1,library)))),
	assert(currentState(fluent(loc(shelf2,kitchen)))),
	assert(currentState(fluent(loc(desk1,office)))),
	% 3. Any small object is labelled 1/2 of the time
	randomlyLabel,
	% 4. Objects are either intact or damaged
	setObjStatusesRandomly,
	% 5. Do non-fixed object locations dependently, because only one can be in a hand
	% THIS SHOULD BE DONE LAST BECAUSE IT TESTS STATE CONSISTENCY
	setObjLocationsRandomlyUntilValid([cup1,book1,prin1]),
	!.
	
randomlyLabel :-
	retractall(currentState(fluent(labelled(_,_)))),
	randomlyLabelObject([cup1, prin1, book1]).
randomlyLabelObject([]).
randomlyLabelObject([A|B]) :-
	random(R),
	(R < 0.5 -> Bool = true ; Bool = false),
	assert(currentState(fluent(labelled(A,Bool)))),
	randomlyLabelObject(B).


setupEntityL(Entity) :-
	List1 = [loc(Entity, office), loc(Entity, workshop), loc(Entity, kitchen), loc(Entity, library)],
	assertOneFluentAtRandom(List1).

	
setObjStatusesRandomly :-
	retractall(currentState(fluent(item_status(_,_)))),
	StatusAlts = [damaged, intact],
	random_member(S1, StatusAlts),
	random_member(S2, StatusAlts),
	random_member(S3, StatusAlts),
	assertFluents([item_status(cup1,S1), item_status(book1,S2), item_status(prin1,S3)]).
	
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
	S = [in_hand(rob1,A),in_hand(p1,A),in_hand(p2,A),in_hand(p3,A),loc(A,library),loc(A,workshop),loc(A,kitchen),loc(A,office)],
	random_member(F, S),
	assert(currentState(fluent(F))),
	randomiseAllLocations(B).

% Returns all physical states, even if they break constraints
getTheoreticalStatePermutation(List) :-
	tryalllocs([p1,p2,p3,rob1,cup1,book1,prin1],[p1,p2,p3,rob1],[workshop,library,kitchen,office],[],List1),
	append(List1, [loc(tab1,workshop), loc(shelf1,library), loc(shelf2,kitchen), loc(desk1,office)], List2),
	tryallstatuspermutations([cup1,book1,prin1], [], PermutationOfAllStatuses),
	append(List2, PermutationOfAllStatuses, List3),
	tryalllabelpermutations([cup1,book1,prin1], [], PermutationOfAllLabels),
	append(List3, PermutationOfAllLabels, List).

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

tryallstatuspermutations([], Return, Return).
tryallstatuspermutations([A|B], Current, PermutationOfAllLabels) :-
	member(Choice, [item_status(A,damaged), item_status(A,intact)]),
	append([Choice], Current, NewCurrent),
	tryallstatuspermutations(B, NewCurrent, PermutationOfAllLabels).
	
tryalllabelpermutations([], Return, Return).
tryalllabelpermutations([A|B], Current, PermutationOfAllLabels) :-
	member(Choice, [labelled(A,true), labelled(A,false)]),
	append([Choice], Current, NewCurrent),
	tryalllabelpermutations(B, NewCurrent, PermutationOfAllLabels).
	
assertOneFluentAtRandom(List) :-
	random_member(F, List),
	assert(currentState(fluent(F))).

assertOneXAtRandom(List) :-
	random_member(X, List),
	assert(X).
	
setRandomInitialObjectConfig :-
	retractall(domain(attr(_))),
	ArmAlts = [pneumatic,electromagnetic],
	random_member(Arm, ArmAlts),
	assertAtts([arm_type(rob1,Arm)]),
	%
	RTAlts = [engineer, manager, sales],
	random_member(RA, RTAlts),
	random_member(RB, RTAlts),
	random_member(RC, RTAlts),
	assertAtts([role_type(p1,RA), role_type(p2,RB), role_type(p3,RC)]),
	%
	WAlts = [heavy, light],
	random_member(W1, WAlts),
	%random_member(W2, WAlts),
	random_member(W3, WAlts),
	random_member(W4, WAlts),
	random_member(W5, WAlts),
	random_member(W6, WAlts),
	random_member(W7, WAlts),
	random_member(W8, WAlts),
	assertAtts([obj_weight(cup1,W1), obj_weight(tab1,W3), obj_weight(shelf1,W4), obj_weight(shelf2,W5), obj_weight(desk1,W6), obj_weight(book1,W7), obj_weight(prin1,W8)]),
	SurAlts = [hard, brittle],
	random_member(Sur1, SurAlts),
	%random_member(Sur2, SurAlts),
	random_member(Sur3, SurAlts),
	random_member(Sur4, SurAlts),
	assertAtts([surface(cup1,Sur1), surface(book1,Sur3), surface(prin1,Sur4)]).


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
% Assuming furniture location is fixed...
% 3+1 agents in 4 locations = 4^4 = 256
% 3 items that can be held (at most 1 per agent) or at locations (any number at a location) = 424
% 2^3 item_status for 3 items = 8
% 2^3 = 8 combinations of labelled/unlabelled for 3 items
% 256 * 424 * 8 * 8 = 6,946,816 physical configurations (!)

% Object property configurations:
% For 3 items and 4 furniture = 7 objects
% 3^3 * 2^7 * 2^3 (role_type, obj_weight, surface hard or brittle for small objects)
% 27 * 128 * 8
% = 27,648 object property configurations




% Returns a number identifying the target axiom, or returns it back for "does not match any target axiom"

% 1. brittle object damaged when put down [CAUSAL LAW]
domainAxiomClassifier([ [attr(surface(prin1, brittle))], [] ], 1) :- domainGoalAction(putdown(rob1,prin1)), !.
domainAxiomClassifier([ [], [attr(surface(prin1, hard))] ], 1) :- domainGoalAction(putdown(rob1,prin1)), !.

% 1(B) SPECIAL - already damaged
domainAxiomClassifier([ [fluent(item_status(prin1, damaged))], [] ], ignore_axiom) :- domainGoalAction(putdown(rob1,prin1)), !.
domainAxiomClassifier([ [], [fluent(item_status(prin1, intact))] ], ignore_axiom) :- domainGoalAction(putdown(rob1,prin1)), !.

% 2. Damaged object, not served to engineer [EXECUTABILITY CONDITION]
domainAxiomClassifier([ [], [attr(role_type(p1,engineer)), fluent(item_status(cup1, intact))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(cup1, damaged))], [attr(role_type(p1,engineer))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.

% 3. Unlabelled object served to a sales person becomes labelled [CAUSAL LAW]
domainAxiomClassifier([ [attr(role_type(p2, sales))], [] ], 3) :- domainGoalAction(serve(rob1,book1,p2)), !.
domainAxiomClassifier([ [], [attr(role_type(p2, engineer)),attr(role_type(p2, manager))] ], 3) :- domainGoalAction(serve(rob1,book1,p2)), !.

% 3(B) SPECIAL - already labelled
domainAxiomClassifier([ [fluent(labelled(book1, true))], [] ], ignore_axiom) :- domainGoalAction(serve(rob1,book1,p2)), !.
domainAxiomClassifier([ [], [fluent(labelled(book1, false))] ], ignore_axiom) :- domainGoalAction(serve(rob1,book1,p2)), !.

% 4. Heavy object cannot be picked up by an electromagnetic arm [NEGATIVE AFFORDANCE]
domainAxiomClassifier([ [],[attr(arm_type(rob1,pneumatic)),attr(obj_weight(prin1,light))] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)),attr(obj_weight(prin1,heavy))],[] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))],[attr(obj_weight(prin1,light))] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.
domainAxiomClassifier([ [attr(obj_weight(prin1,heavy))],[attr(arm_type(rob1,pneumatic))] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.

% 5. No hard surface - label [EXECUTABILITY CONDITION]
domainAxiomClassifier([ [attr(surface(prin1, brittle))], [] ], 5) :- domainGoalAction(affix_label(rob1,prin1)), !.
domainAxiomClassifier([ [], [attr(surface(prin1, hard))] ], 5) :- domainGoalAction(affix_label(rob1,prin1)), !.

% 6. Damaged - label with pneumatic arm [NEGATIVE AFFORDANCE]
domainAxiomClassifier([ [],[attr(arm_type(rob1,electromagnetic)),fluent(item_status(book1,intact))] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic)),fluent(item_status(book1,damaged))],[] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic))],[fluent(item_status(book1,intact))] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.
domainAxiomClassifier([ [fluent(item_status(book1,damaged))],[attr(arm_type(rob1,electromagnetic))] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.

%%%%%%%%%%%%%%%%%%%%

% 7. Positive affordance - even when cup1 is brittle [5], under conditions [object is heavy, robot arm is electromagnetic], 'affix_label(rob1, cup1)' is possible
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)),attr(obj_weight(cup1,heavy))], [] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))], [attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(obj_weight(cup1,heavy))], [attr(arm_type(rob1,pneumatic))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [], [attr(arm_type(rob1,pneumatic)),attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
% POSITIVE AFFORDANCES INCLUDING PARTIAL INFORMATION FROM THEIR ASSOCIATED EXECUTABILITY CONDITION ARE OKAY
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(obj_weight(cup1,heavy)), attr(surface(cup1, brittle))], [] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(surface(cup1, brittle))], [attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(obj_weight(cup1,heavy)), attr(surface(cup1, brittle))], [attr(arm_type(rob1,pneumatic))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(surface(cup1, brittle))], [attr(arm_type(rob1,pneumatic)), attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(obj_weight(cup1,heavy))], [attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))], [attr(obj_weight(cup1,light)), attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(obj_weight(cup1,heavy))], [attr(arm_type(rob1,pneumatic)), attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [], [attr(arm_type(rob1,pneumatic)), attr(obj_weight(cup1,light)), attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.

% 8. Positive affordance - even when a damaged object is served to a non-engineer [2], under conditions [object is labelled], 'serve(rob1,book1,p1)' is possible
domainAxiomClassifier([ [fluent(labelled(book1, true))], [] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
% POSITIVE AFFORDANCES INCLUDING PARTIAL INFORMATION FROM THEIR ASSOCIATED EXECUTABILITY CONDITION ARE OKAY
domainAxiomClassifier([ [fluent(labelled(book1, true))], [attr(role_type(p1,engineer))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [attr(role_type(p1,engineer)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
%
domainAxiomClassifier([ [fluent(labelled(book1, true))], [fluent(item_status(book1, intact))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [fluent(item_status(book1, intact)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(labelled(book1, true))], [attr(role_type(p1,engineer)), fluent(item_status(book1, intact))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [attr(role_type(p1,engineer)), fluent(item_status(book1, intact)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
%
domainAxiomClassifier([ [fluent(item_status(book1, damaged)), fluent(labelled(book1, true))], [] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(book1, damaged))], [fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(book1, damaged)), fluent(labelled(book1, true))], [attr(role_type(p1,engineer))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(book1, damaged))], [attr(role_type(p1,engineer)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.



%%%%%%%%%%%%%%%%%%%%

% Catch case: Everything else
domainAxiomClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- !.

%%%%%%%%%%%%%%%%%%%%






	

% (8). 
% Positive Affordance
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
executabilityConditionViolated(31).

