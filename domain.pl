
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Domain (control loop) %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal(I, [holds(in_hand(per0, text0), I)]).

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
holds_at_zero(item_status(cup1, intact)).
holds_at_zero(item_status(book1, intact)).
holds_at_zero(item_status(prin1, damaged)).
holds_at_zero(is_labelled(cup1, false)).
holds_at_zero(is_labelled(book1, true)).
holds_at_zero(is_labelled(prin1, false)).
holds_at_zero(loc(cup1, rm1)).
% holds_at_zero(loc(book1, rm0)). % Should be established by default?
holds_at_zero(loc(prin1, rm0)).
holds_at_zero(loc(tab1, rm1)).
holds_at_zero(loc(shelf1, rm0)).
holds_at_zero(loc(shelf2, rm3)).
holds_at_zero(loc(desk1, rm2)).
holds_at_zero(loc(rob1, rm1)).
holds_at_zero(loc(p0, rm2)).
holds_at_zero(loc(p1, rm0)).
holds_at_zero(loc(p2, rm2)).
holds_at_zero(loc(p3, rm3)).



