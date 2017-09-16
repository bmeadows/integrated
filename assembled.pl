#const numSteps = 5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#location = [rm][0..2].

#robot = {rob0}.
#person = {per0}.
#entity = #robot + #person.	

#type = {office, library, workshop}.
#role = {engineer, sales, manager}.
#status = {damaged, good}.
#weight = {light, heavy}.

#textbook = {text0}.
#object = #textbook.

#thing = #object + #entity.

#step = 0..numSteps.


%% Fluents
#inertial_fluent = loc(#thing, #location) + in_hand(#entity, #object).
#fluent = #inertial_fluent.% + #defined_fluent.
#action = move(#robot, #location) + pickup(#robot, #object) 
	+ putdown(#robot, #object) + serve(#robot, #object, #person).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loc_type(#location, #type).
role_type(#person, #role).
obj_status(#object, #status).
obj_weight(#object, #weight).

holds(#fluent,#step).
occurs(#action,#step).

%obs(#fluent, #step).
%hpd(#action, #step).

success().
goal(#step). 
something_happened(#step).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 rules			        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Causal Laws
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Moving changes location to target room...
holds(loc(R, L), I+1) :- occurs(move(R, L), I).

%% Grasping an object causes object to be in hand...
holds(in_hand(R, O), I+1) :- occurs(pickup(R, O), I). 

%% Putting an object down causes it to no longer be in hand...
-holds(in_hand(R, O), I+1) :- occurs(putdown(R, O), I). 

%% Serving an object to a human causes the object to be in human's hand...
holds(in_hand(P, O), I+1) :- occurs(serve(R, O, P), I).

%% Serving an object causes the object to not be in robot's hand...
-holds(in_hand(R, O), I+1) :- occurs(serve(R, O, P), I).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Executability Conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Cannot move to a location if you are already there...
-occurs(move(R, L), I) :- holds(loc(R, L), I).

%% Cannot pick up an object if you are not in the same room...
-occurs(pickup(R, O), I) :- holds(loc(R, L1), I), holds(loc(O, L2), I), L1 != L2.

%% Cannot pick up an object already in hand... 
-occurs(pickup(R, O), I) :- holds(in_hand(R, O), I).

%% Rules to prevent incorrect grasping...
-occurs(pickup(R, O), I) :- holds(loc(R, L), I), -holds(loc(O, L), I).
-occurs(pickup(R, O), I) :- holds(loc(O, L), I), -holds(loc(R, L), I).

%% Cannot put down an object unless it is in hand...
-occurs(putdown(R, O), I) :-  not holds(in_hand(R, O), I).

%% Cannot serve an object that is not in hand...
-occurs(serve(R, O, P), I) :- not holds(in_hand(R, O), I).

%% Cannot serve an object unless robot and human are in same location...
-occurs(serve(R, O, P), I) :- holds(loc(R, L1), I), holds(loc(P, L2), I), L1 != L2.

%% Rules to prevent incorrect serving...
-occurs(serve(R, O, P), I) :- holds(loc(R, L), I), -holds(loc(P, L), I).
-occurs(serve(R, O, P), I) :- holds(loc(P, L), I), -holds(loc(R, L), I).

%% Rules to be discovered...
-occurs(serve(R, O, P), I) :- role_type(P, engineer), 
			      loc_type(L, workshop), 
			      holds(loc(P, L), I).
-occurs(serve(R, O, P), I) :- obj_status(O, damaged).
-occurs(pickup(R, O), I) :- obj_weight(O, heavy).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% State Constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Any object exists in only one location...
-holds(loc(O, L2), I) :- holds(loc(O, L1), I), L1!=L2.

%% If a robot is holding an object, they have the same location...
holds(loc(O, L), I) :- holds(loc(R, L), I), holds(in_hand(R, O), I).

%% Only one entity can have an object in hand...
-holds(in_hand(E2, O), I) :- holds(in_hand(E1, O), I), E1 != E2.

%% Only one object can be held at any time...
-holds(in_hand(E, O2), I) :- holds(in_hand(E, O1), I), O1 != O2.

%% If thing is not at a location initially, assume not there ...
-holds(loc(Th, L), 0) :- not holds(loc(Th, L),0), #thing(Th). 

%% Location type, room type, status and weight have unique values...
-loc_type(L, T2) :- loc_type(L, T1), T1 != T2. 
-role_type(P, RT2) :- role_type(P, RT1), RT1 != RT2.
-obj_status(O, S2) :- obj_status(O, S1), S1 != S2.
-obj_weight(O, W2) :- obj_weight(O, W1), W1 != W2.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inertial axiom + CWA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% General inertia axioms...
holds(F,I+1) :- #inertial_fluent(F),
                holds(F,I),
                not -holds(F,I+1).

-holds(F,I+1) :- #inertial_fluent(F),
                 -holds(F,I),
                 not holds(F,I+1).
                 
%% CWA for Actions...
-occurs(A,I) :- not occurs(A,I).

%% CWA for defined fluents...
%-holds(F, I) :- #defined_fluent(F), not holds(F, I). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Planning Module...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Failure is not an option...
success :- goal(I).
:- not success. 

%% Cannot be idle while goal remains unachieved...
occurs(A, I) | -occurs(A, I) :- not goal(I). 

%% Cannot execute two actions at the same time...
:- occurs(A1,I), occurs(A2,I), A1 != A2.

something_happened(I) :- occurs(A, I).

:- not goal(I),
   not something_happened(I).

%:- goal(I), goal(I-1),
%   J < I,
%   not something_happened(J).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holds(loc(rob0, rm1), 0).
holds(loc(text0, rm0), 0). 
holds(loc(per0, rm2), 0).

%obj_weight(text0, heavy).
obj_status(text0, good).
%loc_type(rm2, workshop).
role_type(per0, engineer).


%% Goal...
%goal(I) :- holds(loc(text0, rm1), I), -holds(in_hand(rob0, text0), I).
goal(I) :- holds(in_hand(per0, text0), I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

occurs.
%-holds.
%holds.
