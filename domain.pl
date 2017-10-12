
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


