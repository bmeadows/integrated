%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Section 1: Parameters %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- include(construct_asp_file).
:- include(inputNLActionLearner). % exclude this to remove startup lag associated with wordnet

:- dynamic inPlanMode/1, learningMode/1, currently_holds/1.

inPlanMode(true).
learningMode(off). % rrlForSpecificUnexpectedTransition, activeExplorationRRLOrActionLearning
os(windows).


test :-
	compute_answer_sets.

currentTime(1).
goal(I, [holds(in_hand(per0, text0), I)]).
agent(rob1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Section 2: Main %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


control_loop :-
	add_new_observations_to_history,
	interior_loop,
	control_loop.

add_new_observations_to_history :-
	currentTime(T),
	T2 is T+1,
	retract(currentTime(T)),
	assert(currentTime(T2)),
	prettyprint('Time '),
	prettyprint(T2),
	prettyprint('. Please give list of observations (or "." for default or "," for next from list):   '),
	read(Input),
	prettyprintln(' '),
	process_observations(Input).
	
process_observations('.') :- !, true.
process_observations(',') :- !, true.
process_observations([_A|_B]) :- !, true.
process_observations(_) :- add_new_observations_to_history.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Section 3: Main %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
interior_loop :-
	inPlanMode(false),
	interrupted, % New goal just assigned, or forced to plan.
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
	assert(learningMode(rrlForSpecificUnexpectedTransition)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Section 4: Belief store %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


exists_unachieved_goal :-
	goal(_Timestep, LiteralList),
	not(all_achieved_goals(LiteralList)).

all_achieved_goals([]).
all_achieved_goals([A|B]) :-
	A = holds(X,_Time),
	currently_holds(X),
	all_achieved_goals(B).

/*
interrupted :- ?

explained_all_transitions_last_step :- ?
*/


% 1. Translate belief store to ASP file domain.sp
% 2. Call SPARC (ASP) with     java -jar sparc.jar domain.sp -solver dlv -A > answersets.txt
% 3. Take first(?) answer set from answersets.txt, translate to plan steps, and store them
% The batch command is necessary, as this fails under windows:     shell('cmd.exe java -jar sparc.jar robotassist.sp -solver dlv -A > net.txt', _ExitStatus).
compute_answer_sets :-
	construct_sp_file('domain.sp'), % 1
	os(windows), shell('sparc.bat', _ExitStatus), % 2
	translate_answer_sets('answersets.txt'). % 3

translate_answer_sets(File) :-
	% 1. Translate from ASP form to Prolog form

	
	handle_answer_sets(List).
handle_answer_sets([]) :-	
	% 2. If answer set is empty(?), i.e. unable to explain, leave previous currently_holds(X)
	!.
handle_answer_sets(List) :-	
	%Get what is in common for holds(H,T) at current(?) time step T
	currentTime(T),
	
	% 2. retractall currently_holds(X), set new currently_holds(X) for each
	
	% 3. Get first found plan step and store it as next_plan_step(NPS)
	
	example:
	
{occurs(move(rob0,rm2),0), occurs(move(rob0,rm2),3), occurs(move(rob0,rm0),1), occurs(serve(rob0,text0,per0),4), occurs(pickup(rob0,text0),2)}

{occurs(move(rob0,rm2),2), occurs(move(rob0,rm0),0), occurs(serve(rob0,text0,per0),3), occurs(pickup(rob0,text0),1)}

{occurs(move(rob0,rm1),2), occurs(move(rob0,rm2),3), occurs(move(rob0,rm0),0), occurs(serve(rob0,text0,per0),4), occurs(pickup(rob0,text0),1)}


nowExecutePlanStep :-
	next_plan_step(NPS),
	???.
nowExecutePlanStep :-
	trace.



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
