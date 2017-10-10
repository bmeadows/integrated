%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Section 1: Parameters %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(domain).
:- include(construct_asp_file).
:- include(inputNLActionLearner). % exclude this to remove startup lag associated with wordnet

:- dynamic inPlanMode/1, learningMode/1, last_transitions_failed/1, 
currently_believed_to_hold/1, currentTime/1, currentTime_unaltered/1, currentGoal/1, 
obs/3, hpd/2, 
answer_set_goal/1, expected_effects/3, user_alerted_interruption/0.

%holds_at_zero/1

inPlanMode(true).
learningMode(off). % rrlForSpecificUnexpectedTransition, activeExplorationRRLOrActionLearning
os(windows).
last_transitions_failed(false).


test :-
	compute_answer_sets.

currentTime(0).
currentTime_unaltered(5). % Not considering history resets
%number_of_ASP_steps_to_lookahead(5).

currentGoal("goal(I) :- holds(in_hand(P,book1),I), #person(P).").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Section 2: Main %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


control_loop :-
	add_new_observations_to_history,
	interior_loop,
	control_loop.

add_new_observations_to_history :-
	increment_time,
	get_observations.
	
increment_time :-
	currentTime(T),
	T2 is T+1,
	retract(currentTime(T)),
	assert(currentTime(T2)).

get_observations :-
	currentTime(T),
	prettyprint('Time '),
	prettyprint(T),
	% Note that the last step's action and its effects will only be observed if default or next is chosen
	prettyprintln('. Please give Prolog list of observations (or string encoding goal, or "d." for default, or "i." for default+interruption, or "n." for next from list): '),
	read(Input),
	prettyprintln(' '),
	process_observations(Input).
	
process_observations(d) :- !, confirm_expected_effects.
process_observations(n) :- !, confirm_expected_effects.
process_observations(i) :- !, assert(user_alerted_interruption), confirm_expected_effects.
process_observations([_A|_B]) :- !, prettyprintln('TODO!'), true.
process_observations(X) :- string(X), assert(user_alerted_interruption), currentGoal(Current), prettyprint('Removing goal:  '), prettyprintln(Current), retractall(currentGoal(_)), assert(currentGoal(X)), prettyprint('Adding goal:    '), prettyprintln(X), !.
process_observations(X) :- atom(X),   assert(user_alerted_interruption), currentGoal(Current), prettyprint('Removing goal:  '), prettyprintln(Current), retractall(currentGoal(_)), assert(currentGoal(X)), prettyprint('Adding goal:    '), prettyprintln(X), !.
process_observations(_) :- get_observations.

% Anticipated effects of last cycle's actions are put into form obs(X,true,newtime) for ASP if 'default' or 'next' is chosen
confirm_expected_effects :-
	currentTime(TNew),
	TOld is TNew-1,
	expected_effects(Action,Effects,TOld),
	prettyprint('New observations: '),
	prettyprintln(Effects),
	expected_to_obs(Effects,TNew),
	assert(hpd(Action, TOld)),
	!.
confirm_expected_effects.	
	
expected_to_obs([],_).
expected_to_obs([fluent(A)|B],T) :-
	!,
	assert(obs(A,true,T)),
	expected_to_obs(B,T).
expected_to_obs([not(fluent(A))|B],T) :-
	!,
	assert(obs(A,false,T)),
	expected_to_obs(B,T).

	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Section 3: Main %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
interior_loop :-
	inPlanMode(false),
	interrupted, % New goal just assigned, or forced to plan. Assumption: The robot never has to abandon a goal in favour of another one, so this can only happen when not planning.
	!,
	retractall(inPlanMode(_)),
	assert(inPlanMode(true)).
interior_loop :-
	inPlanMode(false),
	learningMode(rrlForSpecificUnexpectedTransition),
	!,
	continue_RRL_for_specific_unexpected_transition.
interior_loop :-
	inPlanMode(false),
	learningMode(activeExplorationRRLOrActionLearning),
	request_verbal_cue,
	!, % If it succeeds, a human has agreed to provide a verbal description of action
	perform_active_action_learning.
interior_loop :-
	inPlanMode(false),
	learningMode(activeExplorationRRLOrActionLearning),
	!,
	continue_exploratory_RRL.
% Failed
interior_loop :-
	inPlanMode(false),
	trace.

interior_loop :-
	inPlanMode(true),
	!,
	compute_answer_sets,
	(exists_unachieved_goal
	->
		execute_plan
	;
		(
		retractall(inPlanMode(_)),
		assert(inPlanMode(false)),
		retractall(learningMode(_)),
		assert(learningMode(activeExplorationRRLOrActionLearning))
		)
	).

execute_plan :-
	explained_all_transitions_last_step,
	!,
	nowExecutePlanStep.
execute_plan :-
	retractall(inPlanMode(_)),
	assert(inPlanMode(false)),
	retractall(learningMode(_)),
	assert(learningMode(rrlForSpecificUnexpectedTransition)),
	retractall(last_transitions_failed(_)),
	assert(last_transitions_failed(false)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Section 4: Belief store %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exists_unachieved_goal :-
	not(( answer_set_goal(I),
	currentTime(I) )).

/*
exists_unachieved_goal :-
	goal(_Timestep, LiteralList),
	not(all_achieved_goals(LiteralList)).

all_achieved_goals([]).
all_achieved_goals([A|B]) :-
	A = holds(X,_Time),
	!,
	currently_believed_to_hold(X),
	all_achieved_goals(B).
all_achieved_goals([A|B]) :-
	A = not(holds(X,_Time)),
	not(currently_believed_to_hold(X)),
	all_achieved_goals(B).
*/

% TODO extend properly
interrupted :-
	user_alerted_interruption,
	retractall(user_alerted_interruption).

explained_all_transitions_last_step :-
	last_transitions_failed(false).

% 1. Translate belief store to ASP file domain.sp
% 2. Call SPARC (ASP) with     java -jar sparc.jar domain.sp -solver dlv -A > answersets.txt
% 3. Take first(?) answer set from answersets.txt, translate to plan steps, and store them
% 4. Get updated beliefs
% The batch command is necessary, as this fails under windows:     shell('cmd.exe java -jar sparc.jar robotassist.sp -solver dlv -A > net.txt', _ExitStatus).
compute_answer_sets :-
	construct_sp_file('domain.sp'), % 1
	os(windows), shell('sparc.bat', _ExitStatus), % 2
	translate_answer_sets('answersets.txt'). % 3

translate_answer_sets(File) :-
	read_file_to_string(File, "", []),
	% If answer set is empty, i.e. unable to explain, leave previous currently_believed_to_hold(X), but assert something to note that transitions were unexplained
	retractall(last_transitions_failed(_)),
	assert(last_transitions_failed(true)),
	!.
translate_answer_sets(File) :-


	% 1. Get text
	read_file_to_string(File, RawString, []),
	% 2. Translate from ASP form to Prolog form
	Separators = "\n",
	Pads = "\s\t\n",
	split_string(RawString, Separators, Pads, CurlyBracedSubStringList),
	curlyBracedToPrologListAll(CurlyBracedSubStringList, SubLists),
	
	
	
	handle_answer_sets(SubLists).

curlyBracedToPrologListAll([], []).
curlyBracedToPrologListAll([A|B], [HeadAtomicList|Tail]) :-
	split_string(A, "{}", "", [_Firstbracket,P0,_Lastbracket]),
	string_concat("[", P0, P1),
	string_concat(P1, "]", P2),
	atom_codes(P3, P2), % "string" to 'string'
	atom_to_term(P3, HeadAtomicList, []),
	curlyBracedToPrologListAll(B, Tail).

handle_answer_sets(List) :-	
	% Get what is in common for holds(H,T) at current time step T... because it may return a plan, there may be holds(H,T') for T' > T; ignore these
	currentTime(T),
	findall( Literal,
			not(( member(SubList,List), not(member(holds(Literal,T),SubList)) )),
			Current),
	% 4. Update current beliefs
	retractall(currently_believed_to_hold(_)),
	assertallcurrent(Current),
	% 5. Get things that held at time zero
	/*findall( Literal,
			not(( member(SubList,List), not(member(holds(Literal,0),SubList)) )),
			TimeZero),
	% 6. Record what was believed at time zero
	retractall(holds_at_zero(_)),
	assertallzero(TimeZero),*/
	
	findall( I,
			not(( member(SubList,List), not(member(goal(I),SubList)) )),
			GoalsMet),
	retractall(answer_set_goal(_)),
	assertallgoals(GoalsMet),
	
	% 7. Get first found plan step and store it as next_plan_step(NPS); note handle_answer_sets is only called when 1 or more exists
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

reset_times :-
	% currentTime(T),
	% currentTime_unaltered(U),
	retractall(currentTime(_)),
	assert(currentTime(1)).
	

assertallgoals([]).
assertallgoals([A|B]) :-
	assert(answer_set_goal(A)),
	assertallgoals(B).
	

%assertallzero([]).
%assertallzero([A|B]) :-
	%assert(holds_at_zero(A)),
	%assertallzero(B).

assertallcurrent([]).
assertallcurrent([A|B]) :-
	assert(currently_believed_to_hold(A)),
	assertallcurrent(B).

nowExecutePlanStep :-
	prettyprint('Executing next plan step:  '),
	next_plan_step(NPS),
	prettyprintln(NPS),	
	% Give this planned action to agent knowledge and record (and print) expected effects
	find_expected_effects_given_current_beliefs(NPS,Effects),
	currentTime(T),
	assert(expected_effects(NPS,Effects,T)).
nowExecutePlanStep :-
	trace.

find_expected_effects_given_current_beliefs(Action, Effects) :-
	find_expected_effects_each(Action, Effects),
	!.
find_expected_effects_given_current_beliefs(Action, _Effects) :-
	prettyprint('No agent model (causal law) found for '),
	prettyprintln(Action),
	trace.
find_expected_effects_each(Action, Effects) :-
	findall(E, 
			(causal_law(Action, Conditions, E),
			% Conditions is a list of fluent(), attr(), not(fluent()), not(attr())
			conditions_believed_to_hold(Conditions)),
		Effects).

conditions_believed_to_hold([]).
conditions_believed_to_hold([A|B]) :-
	condition_believed(A),
	conditions_believed_to_hold(B).

% TODO clarify place of negated beliefs etc. If the -holds appears in the display section, negations will also be returned; need to take all factors into account.
condition_believed(not(fluent(F))) :-
	not(currently_believed_to_hold(F)),
	!.
condition_believed(fluent(F)) :-
	currently_believed_to_hold(F),
	!.
% TODO ASP will not report static attributes, there is a separate store which is turned into the ASP program's attributes.
% These are obviously fixed, as outside the RRL part.
condition_believed(not(attr(A))) :-
	not(domain_attr(A)),
	!.
condition_believed(attr(A)) :-
	domain_attr(A),
	!.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Section 5: Learning functions %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_verbal_cue :- 
	prettyprint('Requesting verbal cue (or "n"	for no):   '),
	read(Input),
	prettyprintln(' '),
	check_verbal_cue(Input).

check_verbal_cue('n') :-
	!,
	fail.
check_verbal_cue([A,B,C]) :-
	!,
	assert(obs_ex_action(A,B,C)).
check_verbal_cue(_) :-
	prettyprintln('Cue format not recognised.'),
	request_verbal_cue.

perform_active_action_learning :-
	obs_ex_action(TextString,B,C),
	tell('vocalisation.txt'),
	write(TextString),
	nl,
	told,
	% Stanford parts of speech tagger:
	%java -mx300m -classpath "stanford-postagger.jar;" edu.stanford.nlp.tagger.maxent.MaxentTagger -outputFormat slashTags -model tagger-models\english-left3words-distsim.tagger -textFile vocalisation.txt 1>tagged.txt
	shell('tagging.bat', _ExitStatus),
	read_file_to_string('tagged.txt', TS, []),
	atom_codes(TextString2,TS),
	assert(obs_ex_action_tagged(TextString2,B,C)),
	retract(obs_ex_action(TextString,B,C)).
%	perform_active_action_learning. % Repeat for any more verbal cues stored
perform_active_action_learning :-
	% Finally, perform learning over tagged text descriptions
	!,
	learnFromDescs.
	
/*
continue_RRL_for_specific_unexpected_transition :- ?

continue_exploratory_RRL :- ?
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% assert(obs_ex_action("The unimportant engineer is polishing the metallic table.",[reflectivity(tab1,bright)],1)).
% assert(obs_ex_action("The robot is polishing the desk.",[reflectivity(desk1,bright)],1)).
