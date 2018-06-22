%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Section 1: Parameters %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Includes
:- [answer_set_cleaner2018,pretty_printer].
:- [inputNLActionLearner2018]. % Exclude this to remove startup lag associated with WordNet

:- dynamic inPlanMode/1, learningMode/1, last_transitions_failed/1, 
currently_believed_to_hold/1, currentTime/1, currentTime_unaltered/1, currentGoal/1, 
obs/3, hpd/2, answer_set_goal/1, expected_effects/3, user_alerted_interruption/0,
granularity/1, object_reference_specificity/1, plan_detail/1, outcome_detail/1.

:- discontiguous describe_outcomes/2, asp/1.

%inPlanMode(true).
os(windows).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Section 2: Control %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

control_loop :-
	read_ASP_program_and_output_to_predicates,
	get_user_explanation_request -> continue_to_explanation ; (prettyprintln('No explanation requested.'), true).
	
read_ASP_program_and_output_to_predicates :-
	read_ASP_program_and_translate_to_predicates,
	read_ASP_output_and_translate_to_predicates.

continue_to_explanation :-
	generate_explanation,
	!,
	solicit_user_response.

solicit_user_response :- 
% Start with just the same overall set of cues, but given time we can try having them give feedback on particular PARTS of the explanation, e.g. a coarse grained action name).
	get_text_feedback(Input),
	process_user_input_text(Input),
	!,
	continue_to_explanation.
solicit_user_response.

get_user_explanation_request :-
	get_text(Input),
	process_user_input_text(Input).

get_text(Input) :-
	prettyprintln('Please provide instruction (within single quotes followed by period): '),
	read(Input),
	prettyprintln(' '),
	Input \= [],
	Input \= '',
	Input \= '\n'.

get_text_feedback(Input) :-
	prettyprintln('Explanation finished. Please provide feedback (within single quotes followed by period): '),
	read(Input),
	prettyprintln(' '),
	Input \= [],
	Input \= '',
	Input \= '\n'.

process_user_input_text(InputString) :-
	% As long as a synonym for 'explain', 'explanation', 'analyse', 'analyze', 'describe', or 'tell' appear, take the part before that and extract any specificity cue
	partition_input(InputString, Preamble, Tell, _Remainder),
	prettyprint('=> "'),
	prettyprint(Tell),
	prettyprintln('"'),
	change_specificity_from_cues(Preamble).
	
partition_input(InputStringX, Preamble, Tell, Remainder) :-
	string_lower(InputStringX, InputString), % Set all letters to lowercase
	split_string_into_three_at_first_instance_of_some_word(InputString,
			["explain", "analyse", "analyze", "explanation", "describe", "tell"], Preamble, Tell, Remainder),
	!.
split_string_into_three_at_first_instance_of_some_word(Input, WordList, Output1, Output2, Output3) :-
	member(Token, WordList),
	sub_string(Input, CharactersBefore, Length, CharactersAfter, Token), % Fixes place of a target word in the string
	sub_string(Input, 0, CharactersBefore, _, Output1),
	FirstParts is CharactersBefore + Length,
	sub_string(Input, FirstParts, CharactersAfter, 0, Output3),
	Output2 = Token,
	!.
	
read_ASP_program_and_translate_to_predicates :-
	true. % TODO

read_ASP_output_and_translate_to_predicates :-
	true. % TODO



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Section 3: Cue parsing %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_specificity_from_cues(Text) :-
	establish_cues(Text, AxisOrGeneral, Direction), % decrease_specificity/increase_specificity/standard_specificity
	prettyprint('Axis of specificity indicated by user input: '),
	prettyprintln(AxisOrGeneral),
	prettyprint('Direction of specificity indicated by user input: '),
	prettyprintln(Direction),
	change_axes_based_on_cues(AxisOrGeneral, Direction).

change_axes_based_on_cues(_AxisOrGeneral, standard_specificity) :-
	!.
change_axes_based_on_cues(general, decrease_specificity) :-
	decrease_axis(granularity),
	decrease_axis(object_reference_specificity),
	decrease_axis(plan_detail),
	decrease_axis(outcome_detail),
	!.
change_axes_based_on_cues(general, increase_specificity) :-
	increase_axis(granularity),
	increase_axis(object_reference_specificity),
	increase_axis(plan_detail),
	increase_axis(outcome_detail),
	!.
change_axes_based_on_cues(AxisOrGeneral, decrease_specificity) :-
	decrease_axis(AxisOrGeneral), !.
change_axes_based_on_cues(AxisOrGeneral, increase_specificity) :-
	increase_axis(AxisOrGeneral), !.

% Parse to find words indicating specificity + direction + negation.
establish_cues(Preamble, AxisOrGeneral, Direction) :-
	set_preamble_specificity_word(Preamble, AxisOrGeneral, Spec),
	!,
	preamble_direction_word(Preamble, Direc),
	preamble_negation_words(Preamble, Negs),
	length(Negs,L), % An odd number of negations means overall negation.
	MOD is mod(L,2),
	( (MOD = 1) -> (DirIndicator is (Spec * Direc * -1)) ; (DirIndicator is (Spec * Direc)) ), % 1 = increase specificity, -1 = decrease specificity
	( (DirIndicator == 1) -> Direction = increase_specificity ; true ),
	( (DirIndicator == -1) -> Direction = decrease_specificity ; true ),
	( (DirIndicator == 0) -> Direction = standard_specificity ; true ).
establish_cues(_, general, standard_specificity).

% % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % %

% 1 is specific, -1 is generic

% First, try to find a match using only exact matches, no WordNet ties
set_preamble_specificity_word(Preamble, AxisOrGeneral, Spec) :-
	preamble_specificity_word(Preamble, false, AxisOrGeneral, Spec),
	!.
% Only if that fails, try to find a match using WordNet ties
set_preamble_specificity_word(Preamble, AxisOrGeneral, Spec) :-
	preamble_specificity_word(Preamble, true, AxisOrGeneral, Spec),
	!.
% Finally if that fails, fall back on default (no change)
set_preamble_specificity_word(_Preamble, general, 0).

% SPECIFICITY words
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["specify", "specific"]), Ret = object_reference_specificity, Spec = 1, !.
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["generic", "general"]), Ret = object_reference_specificity, Spec = -1, !.

% ABSTRACTNESS of detail words
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["concrete", "grounded"]), Ret = plan_detail, Spec = 1, !.
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["abstract", "vague"]), Ret = plan_detail, Spec = -1, !.

% GRANULARITY (coarse/fine distinction) words
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["fine", "fineness", "refined", "finegrained", "fine-grained, gritty"]), Ret = granularity, Spec = 1, !. % "granular, granularity"
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["coarse", "coarseness", "coarsegrained", "coarse-grained"]), Ret = granularity, Spec = -1, !.

% QUANTITY of detail words
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["thorough", "elaborate", "extended", "extensively", "slowly"]), Ret = outcome_detail, Spec = 1, !.
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["concise", "quick", "quicker", "fast", "faster", "speedily", "rapidly"]), Ret = outcome_detail, Spec = -1, !.

% (Assume change on all axes)
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["narrow", "narrowly", "bottom-up", "detail", "details", "detailed"]), Ret = general, Spec = 1, !.
preamble_specificity_word(Preamble, FollowWordNetLinks, Ret, Spec) :-
	contains_a_word(Preamble, FollowWordNetLinks, ["broad", "broadly", "top-down", "summary", "summarise", "summarize"]), Ret = general, Spec = -1, !.

% % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % %

% Ambiguity: Signal magnitude increase
preamble_direction_word(Preamble, 1) :-
	contains_a_word(Preamble, true, ["much", "more", "increase", "increased", "considerable", "high", "maximum", "maximal"]),
	contains_a_word(Preamble, true, ["little", "reduce", "reduced", " less ", " low ", "minimum", "minimal"]),
	prettyprintln('(Increase or decrease in specificity ambiguous; defaulting)'),
	!.
% Negative words found: Signal magnitude decrease
preamble_direction_word(Preamble, -1) :-
	contains_a_word(Preamble, true, ["little", "reduce", "reduced", " less ", " low ", "minimum", "minimal"]),
	!.
% No words found or positive words found: Signal magnitude increase by default
preamble_direction_word(_Preamble, _Bool, 1).
	
% List of terms that indicate negation
preamble_negation_words(Preamble, Negs) :-
	find_words(Preamble, ["absent", "without", "sans ", "not "], Negs).

find_words(Text, List, Negs) :- % Returns each instance of a target word found as a substring
	findall([A,B,C,L], (member(L, List), sub_string(Text, A, B, C, L)), Negs).

% True in the presence of a target word or a synonym for one
contains_a_word(Text, true, List) :-
	member(Word, List),
	related_to(Word, Word2), % Word2 returns as a const but this is fine for sub_string
	sub_string(Text, _, Length, _, Word2),
	!,
	Length >= 4. % Word2 has to be at least 4 characters to qualify (to lower false matches when a short synonym appears as part of an unrelated word).
contains_a_word(Text, false, List) :-
	member(Word, List),
	sub_string(Text, _, _Length, _, Word),
	!.
	
related_to(WordStr, Word2) :-
	term_string(Word,WordStr), % String to constant
	s(SID,_,Word,_,_,_),
	s(SID,_,Word2,_,_,_). % Any word in same group including itself
related_to(WordStr, Word2) :-
	term_string(Word,WordStr), % String to constant
	s(SID,_,Word,_,_,_),
	sim(SID,ID2),
	s(ID2,_,Word2,_,_,_),
	Word \= Word2.
related_to(WordStr, WordStr). % Each word related to itself - Covers cases where the word is not found in the wordnet data.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Section 4: Axes %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
granularity(fine). % coarse, moderate, fine
object_reference_specificity(3). % 1, 2, 3, 4
plan_detail(high). % low, medium, high
outcome_detail(medium). % low, medium, high

next_higher(coarse, moderate).
next_higher(moderate, fine).
next_higher(low, medium).
next_higher(medium, high).
next_higher(1,2).
next_higher(2,3).
next_higher(3,4).

set_axis(Axis, Value) :-
	functor(Term, Axis, 1),
	Term,
	retractall(Term),
	functor(Term2, Axis, 1),
	arg(1, Term2, Value),
	assert(Term2).
	
increase_axis(Axis) :- 
	functor(Term, Axis, 1),
	Term,
	arg(1, Term, CurrentValue),
	next_higher(CurrentValue, NewValue),
	retractall(Term),
	functor(Term2, Axis, 1),
	arg(1, Term2, NewValue),
	assert(Term2),
	!.
increase_axis(Axis) :-
	prettyprint('Unchanging (increase): '),
	prettyprintln(Axis).
decrease_axis(Axis) :- 
	functor(Term, Axis, 1),
	Term,
	arg(1, Term, CurrentValue),
	next_higher(NewValue, CurrentValue),
	retractall(Term),
	functor(Term2, Axis, 1),
	arg(1, Term2, NewValue),
	assert(Term2),
	!.
decrease_axis(Axis) :-
	prettyprint('Unchanging (decrease): '),
	prettyprintln(Axis).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Section 5: Explanation %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_explanation :-
	goal_was_achieved,
	!,
	print_goal_time_prefix,
	prettyprint(' and accomplished in '),
	print_plan_length,
	prettyprintln(' time steps.'),
	print_action_sequence,
	prettyprintln('').

generate_explanation :-
	!,
	print_goal_time_prefix,
	prettyprint('. '),
	print_action_sequence,
	prettyprintln(' At this point, the plan failed.').

goal_was_achieved :-
	asp(goal(_)),
	!. % The goal was achieved at some timestep

print_goal_time_prefix :-
	prettyprint('The goal ("'),
	asp_goal_string(String),
	prettyprint(String),
	prettyprint('") was received before time '),
	earliest_time(T),
	prettyprint(T).

% % %

earliest_time(T) :-
	asp(holds(_,T)),
	not(( asp(holds(_,T2)), T2 < T )),
	!.
latest_operation_time(T) :-
	asp(fine(hpd(_,T))),
	not(( asp(fine(hpd(_,T2))), T2 > T )),
	!.

print_plan_length :-
	earliest_time(TStart),
	latest_operation_time(TEnd),
	T is TEnd - TStart +1,
	prettyprint(T).

% % %

print_action_sequence :-
	plan_detail(high),
	!,
	report_first_action,
	report_all_actions.
	
print_action_sequence :-
	plan_detail(medium),
	!,
	report_first_action,
	reportActionNumber,
	report_last_action.

print_action_sequence :-
	plan_detail(low),
	!,
	prettyprint('The plan had several steps. '),
	report_last_action.

reportActionNumber :-
	prettyprint('This was followed by a sequence of '),
	findall(CA, asp(coarse(CA)), CList),
	findall(FA, asp(fine(FA)), FList),
	length(CList, C),
	length(FList, F),
	printActionNumberSpecific(C, F).

printActionNumberSpecific(C, _) :-
	granularity(coarse),
	prettyprint(C),
	prettyprint(' coarse actions. ').
printActionNumberSpecific(C, F) :-
	(granularity(moderate) ; granularity(fine)),
	prettyprint(C),
	prettyprint(' coarse actions comprising '),
	prettyprint(F),
	prettyprint(' fine actions. ').
	
%

report_all_actions :-
	granularity(fine),
	asp(fine(Action)),
	not(reported(Action)),
	!,
	Action = hpd(ActualAction,T),
	not(( asp(fine(hpd(A2,T2))), T2 < T, not(reported(hpd(A2,T2))) )),
	describe_action(fine(ActualAction), T),
	assert(reported(Action)),
	!,
	report_all_actions.
report_all_actions :-
	(granularity(coarse) ; granularity(moderate)),
	asp(coarse(Action)),
	not(reported(Action)),
	!,
	Action = hpd(ActualAction,T),
	not(( asp(coarse(hpd(A2,T2))), T2 < T, not(reported(hpd(A2,T2))) )),
	describe_action(fine(ActualAction), T),
	assert(reported(Action)),
	!,
	report_all_actions.
report_all_actions.

report_first_action :-
	granularity(fine),
	!,
	asp(fine(Action)),
	Action = hpd(ActualAction,T),
	not(( asp(fine(hpd(_,T2))), T2 < T )),
	describe_action_with(fine(ActualAction), T, 'Initially, '),
	assert(reported(Action)),
	!.
report_first_action :-
	% (granularity(coarse) ; granularity(moderate)),
	asp(coarse(Action)),
	Action = hpd(ActualAction,T),
	not(( asp(coarse(hpd(_,T2))), T2 < T )),
	describe_action_with(coarse(ActualAction), T, 'Initially, '),
	assert(reported(Action)),
	!.
	
report_last_action :-
	granularity(fine),
	!,
	asp(fine(Action)),
	Action = hpd(ActualAction,T),
	not(( asp(fine(hpd(_,T2))), T2 > T )),
	describe_action_with(fine(ActualAction), T, 'In the final action, '),
	assert(reported(Action)),
	!.
report_last_action :-
	% (granularity(coarse) ; granularity(moderate)),
	asp(coarse(Action)),
	Action = hpd(ActualAction,T),
	not(( asp(coarse(hpd(_,T2))), T2 > T )),
	describe_action_with(coarse(ActualAction), T, 'In the final action, '),
	assert(reported(Action)),
	!.

describe_action(A, T) :-
	describe_action_with(A, T, 'Next, ').



%%% Functions for giving a description of one action

describe_action_with(Action, T, Prefix) :-
	prettyprint(Prefix),
	(Action = coarse(ActionTerm) ; Action = fine(ActionTerm)),
	employ_grammar_rule(ActionTerm),
	describe_outcomes(Action, T),
	prettyprintln('').

employ_grammar_rule(ActionTerm) :-
	% "[actor] [verb past tense] [object1] {to [object2]} {with [object3]} {(note: [object4])}"
	functor(ActionTerm, ActionName, _Arity),
	action_syntax(ActionName, VerbPastTense, TypeList),
	% nth1(Index, TypeList, TargetElement)
	getCorrectArg(actor, ActionTerm, TypeList, ActorValue),
	print_obj(ActorValue),
	% '[actor]'
	prettyprint(' '),
	prettyprint(VerbPastTense),
	% '[verb past tense]'
	(getCorrectArg(object1, ActionTerm, TypeList, O1Value) -> print_obj(O1Value) ; true), % '[object1]'
	(getCorrectArg(object2, ActionTerm, TypeList, O2Value) -> (prettyprint(' to '), print_obj(O2Value)) ; true), % 'to [object2]'
	(getCorrectArg(object3, ActionTerm, TypeList, O3Value) -> (prettyprint(' with '), print_obj(O3Value)) ; true), % 'with [object3]'
	(getCorrectArg(object4, ActionTerm, TypeList, O4Value) -> (prettyprint(' (relevant: '), print_obj(O4Value), prettyprint(')')) ; true), % '(relevant: [object4])'
	prettyprint('. ').

getCorrectArg(Symbol, Term, TypeList, Value) :-
	nth1(Index, TypeList, Symbol),
	arg(Index, Term, Value),
	!.

% % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % %

% FIRST VARIATION: Coarse granularity + medium outcome detail.
% 1. If there is another coarse action after this, say the coarse action enabled it.
% 2. Else if the overall goal is achieved, say the coarse action achieved it.
% 3. Else say the coarse action produced a failure state.
describe_outcomes(coarse(Action), Time) :-
	outcome_detail(medium),
	!,
	get_nonrefined_end_time(Action, Time, EndTime),
	describe_outcomes_coarse_continue('This action ', Time, EndTime).

% SECOND VARIATION: Coarse granularity + high outcome detail.	
% 1. State nonrefined fluent outcomes of coarse action.
% 2. If there is another coarse action after this, say the outcomes enabled it.
% 3. Else if the overall goal is achieved, say the outcomes achieved it.
% 4. Else say the outcomes produced a failure state.
describe_outcomes(coarse(Action), Time) :-
	outcome_detail(high),
	!,
	get_nonrefined_end_time(Action, Time, EndTime),
	prettyprint('This resulted in effects: '),
	print_nonrefined_fluent_outcomes(Time, EndTime),
	describe_outcomes_coarse_continue('These effects ', Time, EndTime).

describe_outcomes_coarse_continue(Prefix, _Time, EndTime) :-
	T2 is EndTime +1,
	asp(coarse(hpd(_,T2))),
	prettyprint(Prefix),
	prettyprint('enabled the next action. '),
	!.
describe_outcomes_coarse_continue(Prefix, _Time, _EndTime) :-
	goal_was_achieved,
	prettyprint(Prefix),
	prettyprint('achieved the overall goal. '),
	!.
describe_outcomes_coarse_continue(Prefix, _Time, _EndTime) :-
	prettyprint(Prefix),
	prettyprint('resulted in a failure state. '),
	!.

%%%%%%%%%%%%%%%%%%%%%%

% Coarse action occurred "at" T, i.e., that was the start time.
% Find the end time T2 from the final related fine action (+1).
get_nonrefined_end_time(Action, T, T2) :-
	link(FineActionList, Action, T),
	last(FineActionList, T2),
	!.
	
print_nonrefined_fluent_outcomes(T, TFinal) :-
	T2 is TFinal +1,
	% Find all coarse fluents whose truth value reversed between T and T2.
	% TODO for now, find all fluents, regardless of granularity
	% Recursively print this list with '; ' appended, finishing in '. '
	findall(not(X), (asp(holds(X, T)), not_holds_either_way(X, T2)), ChangeList1),
	findall(X, (asp(holds(X, T2)), not_holds_either_way(X, T)), ChangeList2),
	append(ChangeList1, ChangeList2, ChangeList),
	print_recursive_changelist(ChangeList).
	
print_recursive_changelist([A]) :-
	prettyprint(A),
	prettyprint('. ').
print_recursive_changelist([A|B]) :-
	prettyprint(A),
	prettyprint('; '),
	print_recursive_changelist(B).

not_holds_either_way(X, T) :-
	not(asp(holds(X, T))),
	!.
not_holds_either_way(X, T) :-
	asp(not_holds(X, T)),
	!.

%%%%%%%%%%%%%%%%%%%%%%

% THIRD VARIATION: Fine granularity + medium outcome detail.
% 1. Only if the fine action is the final in a coarse action x, call functions to describe x.
% 2. Then say the fine action achieved it.
% 3. If there is another coarse action after x, say x enabled it.
% 4. Else if the overall goal is achieved, say x achieved it.
% 5. Else say x produced a failure state.
describe_outcomes(fine(_Action), FineTime) :-
	outcome_detail(medium),
	link(FineActionList, CoarseAction, CoarseActionTime),
	last(FineActionList, FineTime),
	!,
	prettyprint('The result was that overall: '),
	employ_grammar_rule(CoarseAction),
	describe_outcomes_coarse_continue('This higher level action ', CoarseActionTime, FineTime).

% FOURTH VARIATION: Fine granularity + high outcome detail.
% 1. State refined fluent outcomes of fine action. i.e. for now, anything that changed on the immediate time step.
% 2. Case 1: It is the end of a coarse action X.
%    a) Call functions to describe X.
%    b) State nonrefined fluent outcomes of X.
%    c) If there is another coarse action after X, say the nonrefined outcomes enabled it.
%    d) Else if the overall goal is achieved, say the nonrefined outcomes achieved it.
%    e) Else say the nonrefined outcomes produced a failure state.
% 3. Case 2: It is not the end of any coarse action.
%    a) If there is a fine action following, say the refined outcomes enabled that next fine action.
%    b) Else if the goal was not achieved, say the refined outcomes produced a failure state immediately.
%    c) Else error.
describe_outcomes(fine(_Action), Time) :-
	outcome_detail(high),
	!,
	prettyprint('This resulted in effects: '),
	print_refined_fluent_outcomes(Time),
	describe_fine_extensive(Time).
	
% FIFTH VARIATION: low outcome detail...
describe_outcomes(_, _T).

% % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % %

% 2. Case 1: It is the end of a coarse action.
describe_fine_extensive(FineTime) :-
	link(FineActionList, CoarseAction, CoarseActionTime),
	last(FineActionList, FineTime),
	!,
	prettyprint('The result was that overall: '),
	employ_grammar_rule(CoarseAction),
	prettyprint('This higher level action resulted in total effects: '),
	print_nonrefined_fluent_outcomes(CoarseActionTime, FineTime), % Start, End
	describe_outcomes_coarse_continue('Then this higher level action ', CoarseActionTime, FineTime).

% 3. Case 2: It is not the end of any coarse action.
describe_fine_extensive(Time) :-
	describe_outcomes_fine_continue('These effects ', Time).

describe_outcomes_fine_continue(Prefix, Time) :-
	T2 is Time +1,
	asp(fine(hpd(_,T2))),
	prettyprint(Prefix),
	prettyprint('enabled the next fine action. '),
	!.
describe_outcomes_fine_continue(_Prefix, _Time) :-
	goal_was_achieved,
	trace, fail,
	!.
describe_outcomes_fine_continue(Prefix, _Time) :-
	prettyprint(Prefix),
	prettyprint('resulted in an immediate failure state. '),
	!.

print_refined_fluent_outcomes(T) :-
	T2 is T +1,
	% Find all fine fluents whose truth value reversed between T and T2.
	% TODO for now, find all fluents, regardless of granularity. So other than inputs, this is almost a copy of the 'nonrefined' function!
	% Recursively print this list with '; ' appended, finishing in '. '
	findall(not(X), (asp(holds(X, T)), not_holds_either_way(X, T2)), ChangeList1),
	findall(X, (asp(holds(X, T2)), not_holds_either_way(X, T)), ChangeList2),
	append(ChangeList1, ChangeList2, ChangeList),
	print_recursive_changelist(ChangeList).
	
% % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % %

/*
NOTE 1: Describing coarse vs fine actions necessarily includes reference differences because e.g. it makes no sense to mix levels, say 'moved from room1 to gridcell25'.
But in all this I haven't really dealt with referring to component actions / component att values / component fluents?
And it hasn't slotted into the list of extracted ASP domain terms around line 600 below!

Looks like at least for things like grid cells, can just have a straightforward component(x,y) relation we can use for specificity.
But refined.sp 
a) already defines all actions in FINE terms,
b) has component relations like nextto_fine, nextto_coarse, which have SOME inferrable parts, e.g. nextto_coarse can be inferred from nextto_fine and component relations.
But nextto_coarse is never actually used anywhere!
Others (loc_c can be inferred from loc_f and component relations) seem like they might be used elsewhere, e.g. the coarse is indirectly observed if the fine is directly observed.
But nothing seems integral.
[start without even having the coarse/fine distinction, if possible??]
*/


	
%

print_obj(DomainSymbol) :-
	object_reference_specificity(1),
	!,
	prettyprint('a '),
	less_specific_sort(DomainSymbol, Sort),
	prettyprint(Sort).
	
print_obj(DomainSymbol) :-
	object_reference_specificity(2),
	!,
	prettyprint('a '),
	specific_sort(DomainSymbol, Sort),
	prettyprint(Sort).
	
print_obj(DomainSymbol) :-
	object_reference_specificity(3),
	!,
	prettyprint('the'),
	specific_sort(DomainSymbol, Sort),
	print_determining_attributes(Sort, DomainSymbol),
	prettyprint(Sort).
	
print_obj(DomainSymbol) :-
	object_reference_specificity(4),
	!,
	prettyprint('the'),
	print_all_attributes(DomainSymbol),
	specific_sort(DomainSymbol, Sort),
	prettyprint(Sort),
	prettyprint(' '),
	prettyprint(DomainSymbol).

%	
	
specific_sort(DomainSymbol, Sort) :-
	asp(sorts(Sort, List)),
	member(DomainSymbol, List).
	
less_specific_sort(DomainSymbol, GeneralSort) :-
	asp(sorts(ChildSort, List)),
	member(DomainSymbol, List),
	asp(sort_group(GeneralSort, ChildList)),
	member(ChildSort, ChildList),
	!.
% Default...
less_specific_sort(DomainSymbol, GeneralSort) :-
	specific_sort(DomainSymbol, GeneralSort).

%

domain_symbol_att_values(DomainSymbol, ReturnList) :-
	findall(	Val,
				(   asp(predicate(Term)), functor(Term, _Pred, 2), arg(1, Term, DomainSymbol), arg(2, Term, Val)   ),
				ReturnList).
				
% Print all the entity's/object's attributes.
print_all_attributes(DomainSymbol) :-
	domain_symbol_att_values(DomainSymbol, ValList),
	print_each_element(ValList, " ").

print_each_element([], _) :-
	prettyprint(" ").
print_each_element([Val|Tail], PrefixSpace) :-
	prettyprint(PrefixSpace),
	prettyprint(Val),
	print_each_element(Tail, ", ").

% Print a set of attributes that are sufficient for uniqueness!
% Check for uniqueness currently, and if not, add another attribute.
% Catch case where it's impossible to genuinely get uniqueness.
print_determining_attributes(Sort, DomainSymbol) :-
	domain_symbol_att_values(DomainSymbol, ValList),
	CurrentReportableAttList = [],
	continue_until_uniqueness(Sort, DomainSymbol, CurrentReportableAttList, ValList).
	
continue_until_uniqueness(_Sort, _DomainSymbol, CurrentReportableAttList, []) :-
	!,
	print_each_element(CurrentReportableAttList, " ").
continue_until_uniqueness(Sort, DomainSymbol, CurrentReportableAttList, _ValList) :-
	uniquely_identified(Sort, DomainSymbol, CurrentReportableAttList),
	!,
	print_each_element(CurrentReportableAttList, " ").
continue_until_uniqueness(Sort, DomainSymbol, CurrentReportableAttList, [Val|ValTail]) :-
	append(CurrentReportableAttList, [Val], NewAttList),
	!,
	continue_until_uniqueness(Sort, DomainSymbol, NewAttList, ValTail).

% Does the list of currently picked domain symbols plus sort uniquely identify the object/entity?
% i.e. Is it not the case that there is some X with all of those, such that X does not equal DomainSymbol?
uniquely_identified(Sort, DomainSymbol, CurrentReportableAttList) :-
	not((
		asp(sorts(Sort, List)),
		member(OtherSymbol, List),
		has_all_att_vals(OtherSymbol, CurrentReportableAttList),
		OtherSymbol \= DomainSymbol
	)).

has_all_att_vals(_Symbol, []).
has_all_att_vals(Symbol, [A|B]) :- 
	asp(predicate(Term)),
	functor(Term, _Pred, 2),
	arg(1, Term, Symbol),
	arg(2, Term, A),
	has_all_att_vals(Symbol, B).

%




% action_syntax(ActionName, PastTense, [List]) gives syntactic information for each action, to fit into the grammar rule
% => "[actor] [verb past tense] [object1] {to [object2]} {with [object3]} {(relevant: [object4])}"
% e.g. action_syntax(serve, served, [actor, object1, object2])
% e.g. action_syntax(move, moved, [actor, object2])
% e.g. action_syntax(sweep, swept, [actor, object1, object3])

% asp(coarse(hpd(_,T))) states that a coarse action occurred at time T
% e.g. asp(coarse(hpd(serve(rob1,cup1,p1),10)))

% asp(fine(hpd(_,T))) states that a fine action occurred at time T
% e.g. asp(fine(hpd(serve*(rob1,cup1-handle,p1-hand),12)))

% link([List], A, T) associates sequences of fine actions with a single coarse action occurring at T
% e.g. link([9, 10, 11, 12], serve(rob1,cup1,p1), 9)

% plan([List]) lists the (unique) times for coarse actions
% e.g. plan([1, 10, 17])

% asp(goal(T)) is the ASP term stating the time step at which the goal was achieved
% e.g. asp(goal(20))

% asp_goal_string(S) is a string containing the original goal
% e.g. asp_goal_string("goal(I) :- holds(in_hand(P,book1),I), #person(P).").

%%%%%%%%%%%%% -- % asp(occurs(A, I)) -- ?
% asp(holds(A, I))
% asp(not_holds(A, I)) -holds

% Sort hierarchy:
% asp(sorts(Sort, List))
% e.g. asp(sorts(location, [rmwor, rmoff, rmlib])).
% asp(sort_group(ParentSort, ChildList))
% e.g. asp(sort_group(entity, [robot, person])).
% e.g. asp(sort_group(thing, [object, entity])).

% Static attributes:
% asp(predicate(Term))
% e.g. asp(predicate(has_role(p1, engineer))).



% FOR TESTING ONLY:
action_syntax(serve, served, [actor, object1, object2]).
action_syntax(move, moved, [actor, object2]).
action_syntax(serve_fine, served_fine, [actor, object1, object2]).
action_syntax(move_fine, moved_fine, [actor, object2]).
asp(coarse(hpd(move(rob1,loc2),1))).
asp(coarse(hpd(move(rob1,loc3),3))).
asp(fine(hpd(move_fine(rob1,cell1_1,cell1_2),1))).
asp(fine(hpd(move_fine(rob1,cell1_2,cell2_1),2))). % Moved to loc2
asp(fine(hpd(move_fine(rob1,cell2_1,cell2_2),3))).
asp(fine(hpd(move_fine(rob1,cell2_2,cell3_1),4))). % Moved to loc3
link([1, 2], move(rob1,loc2), 1).
link([3, 4], move(rob1,loc3), 3).
plan([1, 3]).
asp(goal(5)).
asp_goal_string("goal(I) :- holds(loc(rob1,loc3),I).").
%
asp(sort_group(thing, [place, entity, object])).
asp(sort_group(entity, [robot])).
asp(sort_group(object, [block])).
asp(sorts(place, [loc1, loc2, loc3])).
asp(sorts(robot, [rob1])).
asp(sorts(block, [block1, block2])).
%
asp(sort_group(thing_fine, [place_fine, entity_fine, object_fine])).
asp(sort_group(entity_fine, [robot_fine])).
asp(sort_group(object_fine, [block_fine])).
asp(sorts(place_fine, [cell1_1, cell1_2, cell2_1, cell2_2, cell3_1, cell3_2])).
asp(sorts(robot_fine, [rob1_fine])).
asp(sorts(block_fine, [block1_fine, block2_fine])).
%
asp(predicate(size(block1, big))).
asp(predicate(size(block2, small))).
asp(predicate(speed(rob1, fast))).
asp(predicate(purpose(loc1, reading))).
asp(predicate(purpose(loc2, medical))).
asp(predicate(purpose(loc3, engineering))).
%
asp(predicate(size(block1_fine, big))).
asp(predicate(size(block2_fine, small))).
asp(predicate(speed(rob1_fine, fast))).
asp(predicate(purpose(cell1_1, reading))).
asp(predicate(purpose(cell1_2, reading))).
asp(predicate(purpose(cell2_1, medical))).
asp(predicate(purpose(cell2_2, medical))).
asp(predicate(purpose(cell3_1, engineering))).
asp(predicate(purpose(cell3_2, engineering))).
asp(predicate(position(cell1_1, west))).
asp(predicate(position(cell1_2, east))).
asp(predicate(position(cell2_1, west))).
asp(predicate(position(cell2_2, east))).
asp(predicate(position(cell3_1, west))).
asp(predicate(position(cell3_2, east))).
%
asp(holds(loc(rob1, loc1), 1)).
asp(holds(loc(rob1, loc1), 2)).
asp(holds(loc(rob1, loc2), 3)).
asp(holds(loc(rob1, loc2), 4)).
asp(holds(loc(rob1, loc3), 5)).
asp(holds(loc_fine(rob1, cell1_1), 1)).
asp(holds(loc_fine(rob1, cell1_2), 2)).
asp(holds(loc_fine(rob1, cell2_1), 3)).
asp(holds(loc_fine(rob1, cell2_2), 4)).
asp(holds(loc_fine(rob1, cell3_1), 5)).
%
asp(holds(loc(block1, loc1), 1)).
asp(holds(loc(block1, loc1), 2)).
asp(holds(loc(block1, loc1), 3)).
asp(holds(loc(block1, loc1), 4)).
asp(holds(loc(block1, loc1), 5)).
asp(holds(loc_fine(block1, cell1_1), 1)).
asp(holds(loc_fine(block1, cell1_1), 2)).
asp(holds(loc_fine(block1, cell1_1), 3)).
asp(holds(loc_fine(block1, cell1_1), 4)).
asp(holds(loc_fine(block1, cell1_1), 5)).
%
asp(holds(loc(block2, loc1), 1)).
asp(holds(loc(block2, loc1), 2)).
asp(holds(loc(block2, loc1), 3)).
asp(holds(loc(block2, loc1), 4)).
asp(holds(loc(block2, loc1), 5)).
asp(holds(loc_fine(block2, cell1_2), 1)).
asp(holds(loc_fine(block2, cell1_2), 2)).
asp(holds(loc_fine(block2, cell1_2), 3)).
asp(holds(loc_fine(block2, cell1_2), 4)).
asp(holds(loc_fine(block2, cell1_2), 5)).

% asp(not_holds(A, I)) ??



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
perform_active_action_learning :-
	obs_ex_action(TextString,Cons,ObsTime),
	tell('vocalisation2018.txt'),
	write(TextString),
	nl,
	told,
	% Stanford parts of speech tagger:
	%java -mx300m -classpath "stanford-postagger.jar;" edu.stanford.nlp.tagger.maxent.MaxentTagger -outputFormat slashTags -model tagger-models\english-left3words-distsim.tagger -textFile vocalisation2018.txt 1>tagged2018.txt
	shell('tagging2018.bat', _ExitStatus),
	read_file_to_string('tagged2018.txt', TS, []),
	atom_codes(TextString2,TS),
	assert(obs_ex_action_tagged(TextString2,Cons,ObsTime)),
	retract(obs_ex_action(TextString,Cons,ObsTime)),
	learnFromActionDesc(ExtractedAction), % Finally, perform learning over tagged text descriptions
	prettyprint('Interactively learned:    '),
	prettyprintln(ExtractedAction),
	collate_demonstrated_exoaction_consequences(Cons,ListOfExtraObsToSetAtNewZero).
*/




	
	
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Translate belief store to ASP file domain.sp
% 2. Call SPARC (ASP) with     java -jar sparc.jar domain2018.sp -solver dlv -A > answersets2018.txt
% 3. Take first(?) answer set from answersets.txt, translate to plan steps, and store them
% 4. Get updated beliefs 
% The batch command is necessary, as this fails under windows:     shell('cmd.exe java -jar sparc.jar robotassist.sp -solver dlv -A > net.txt', _ExitStatus).
compute_answer_sets :-
	% Must exist 'domain2018.sp'
	os(windows), shell('sparc_2018.bat', _ExitStatus),
	translate_answer_sets('answersets2018.txt').

translate_answer_sets(File) :-
	read_file_to_string(File, "", []),
	prettyprintln('ASP failed!'),
	trace,
	!.
translate_answer_sets(File) :-
	clean_answer_sets(File, SubLists, true), % answer_set_cleaner
	handle_answer_sets(SubLists).

handle_answer_sets(List) :-	
	% Get what is in common for holds(H,T) at current time step T... because it may return a plan, there may be holds(H,T') for T' > T; ignore these
	currentTime(T),
	findall( Literal,
			not(( member(SubList,List), not(member(holds(Literal,T),SubList)) )),
			Current),
	% 4. Update current beliefs
	retractall(currently_believed_to_hold(_)),
	assertallcurrent(Current),
		
	findall( I,
			(member(I, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]), not(( member(SubList,List), not(member(goal(I),SubList)) )) ),
			GoalsMet),
	retractall(answer_set_goal(_)),
	assertallgoals(GoalsMet),
	
	% 7. Get first found plan step and store it as next_plan_step(NPS); note handle_answer_sets is only called when 1 or more answer sets exist
	% However, there might be no plan step because there is no goal
	retractall(next_plan_step(_)),
	List = [First|_],
	(
		member(occurs(NPS,T), First)
		->
		assert(next_plan_step(NPS))
		;
		true
	).
*/


test :-
	protocol('explanation_test_output.txt'),
	begin_test.

begin_test :-
	member(Val1, [coarse, moderate, fine]),
	member(Val2, [1, 2, 3, 4]),
	member(Val3, [low, medium, high]),
	member(Val4, [low, medium, high]),
	set_axis(granularity, Val1), set_axis(object_reference_specificity, Val2), set_axis(plan_detail, Val3), set_axis(outcome_detail, Val4), generate_explanation, prettyprintln('\n***************\n'),
	fail.
begin_test :-
	noprotocol.

