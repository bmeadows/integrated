
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Domain (control loop) %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Deprecated? goal(I, [holds(in_hand(per0, text0), I)]).

currentGoal("goal(I) :- holds(in_hand(P,book1),I), #person(P).").

agent(rob1).

% Attributes
domain_attr(role_type(p0,sales)).
domain_attr(role_type(p1,sales)).
domain_attr(role_type(p2,engineer)).
domain_attr(role_type(p3,manager)).
domain_attr(has_arm_type(rob1, electromagnetic)).
domain_attr(obj_weight(cup1, light)).
domain_attr(obj_weight(book1, light)).
domain_attr(obj_weight(prin1, heavy)).
domain_attr(obj_weight(tab1, heavy)).
domain_attr(obj_weight(shelf1, light)).
domain_attr(obj_weight(shelf2, light)).
domain_attr(obj_weight(desk1, heavy)).
domain_attr(has_surface(cup1, brittle)).
domain_attr(has_surface(book1, brittle)).
domain_attr(has_surface(prin1, brittle)).
domain_attr(loc_type(rm0, library)).
domain_attr(loc_type(rm1, workshop)).
domain_attr(loc_type(rm2, office)).
domain_attr(loc_type(rm3, office)).

% Initial beliefs
obs(item_status(cup1, intact),true,0).
obs(item_status(book1, intact),true,0).
obs(item_status(prin1, damaged),true,0).
obs(is_labelled(cup1, false),true,0).
obs(is_labelled(book1, true),true,0).
obs(is_labelled(prin1, false),true,0).
obs(loc(cup1, rm1),true,0).
% obs(loc(book1, rm0),true,0). % Should be established by default?
obs(loc(prin1, rm0),true,0).
obs(loc(tab1, rm1),true,0).
obs(loc(shelf1, rm0),true,0).
obs(loc(shelf2, rm3),true,0).
obs(loc(desk1, rm2),true,0).
obs(loc(rob1, rm1),true,0).
obs(loc(p0, rm2),true,0).
obs(loc(p1, rm0),true,0).
obs(loc(p2, rm2),true,0).
obs(loc(p3, rm3),true,0).



