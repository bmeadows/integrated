
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Domain (control loop) %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Deprecated? goal(I, [holds(in_hand(per0, text0), I)]).

currentGoal("goal(I) :- holds(in_hand(P,book1),I), #person(P).").

agent(rob1).

% Attributes
domain_attr(loc_type(rmlib, library)).
domain_attr(loc_type(rmwor, workshop)).
domain_attr(loc_type(rmoff, office)).

% Initial beliefs
obs(loc(rob1, rmwor),true,0).
obs(loc(p0, rmoff),true,0).
obs(loc(book1,rmlib),false,1).

% Sorts
sort(location(rmwor)).
sort(location(rmoff)).
sort(location(rmlib)).
sort(robot(rob1)).
sort(person(p0)).
sort(entity(X)) :- sort(robot(X)) ; sort(person(X)).
sort(type(office)).
sort(type(library)).
sort(type(workshop)).
sort(book(book1)).
sort(item(X)) :- sort(book(X)).
sort(object(X)) :- sort(item(X)).
sort(thing(X)) :- sort(entity(X)) ; sort(object(X)).
sort(boolean(true)).
sort(boolean(false)).

% Causal laws, duplicating those in the generated .sp file and the relevant component, pr_predicates.txt
% (Note rrl_domain.pl is not compatible with the current integrated system version)
causal_law(move(Robot, Destination), [], [fluent(loc(Robot, Destination))]). %% Moving changes location to target room...
causal_law(move(Robot, _Destination), [fluent(loc(Robot, X))], [not(fluent(loc(Robot, X)))]).
causal_law(pickup(Robot, Object), [], [fluent(in_hand(Robot, Object))]). %% Grasping an object causes object to be in hand...
causal_law(putdown(Robot, Object), [], [not(fluent(in_hand(Robot, Object)))]). %% Putting an object down causes it to no longer be in the hand...
causal_law(serve(_Robot, Object, Person), [], [fluent(in_hand(Person, Object))]). %% Serving an object to a human causes the object to be in human's hand...
causal_law(serve(Robot, Object, _Person), [], [not(fluent(in_hand(Robot, Object)))]). %% Serving an object causes the object to not be in robot's hand...

% Causal law for learned human exoaction, an example that is currently provided in inputNLActionLearner.pl
causal_law(example_pickup(Person, Object), [], [fluent(in_hand(Person, Object))]). %% Serving an object causes the object to not be in robot's hand...
% TODO: Assert causal laws for interactively learned axioms (genuine ones, not this example) into the running Prolog program as necessary.
