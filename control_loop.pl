%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Section 1: Parameters %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(construct_asp_file).
%:- include(inputNLActionLearner). % exclude this to remove startup lag associated with wordnet

:- dynamic inPlanMode/1, learningMode/1.

inPlanMode(true).
learningMode(off). % rrlForSpecificUnexpectedTransition, activeRRLOrFromVisualVerbal
os(windows).


test :-
	compute_answer_sets.

	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Section 2: Main %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

control_loop :-
	add_new_observations_to_history,
	interior_loop,
	control_loop.

interior_loop :-
	inPlanMode(false),
	!,
	interrupted, % New goal just assigned, or forced to plan.
	retractall(inPlanMode(_)),
	assert(inPlanMode(true)).
interior_loop :-
	inPlanMode(false),
	!,
	learningMode(rrlForSpecificUnexpectedTransition),
	continue_RRL_for_specific_unexpected_transition.
interior_loop :-
	inPlanMode(false),
	!,
	learningMode(activeRRLOrFromVisualVerbal),
	(exists_verbal_cue  ->  continue_active_action_learning  ;  continue_exploratory_RRL).
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
		assert(learningMode(activeRRLOrFromVisualVerbal))
		)
	).

execute_plan :-
	explained_transitions_last_step,
	!,
	nowExecutePlanStep.
execute_plan :-
	retractall(inPlanMode(_)),
	assert(inPlanMode(false)),
	retractall(learningMode(_)),
	assert(learningMode(rrlForSpecificUnexpectedTransition)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Section 3: Belief store %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


exists_unachieved_goal :- ?

interrupted :- ?

exists_verbal_cue :- ?

explained_transitions_last_step :- ?

add_new_observations_to_history :- ?


% 1. Translate belief store to ASP file domain.sp
% 2. Call SPARC (ASP) with     java -jar sparc.jar domain.sp -solver dlv -A > answersets.txt
% 3. Take first(?) answer set from answersets.txt, translate to plan steps, and store them
% The batch command is necessary, as this fails under windows:     shell('cmd.exe java -jar sparc.jar robotassist.sp -solver dlv -A > net.txt', _ExitStatus).
compute_answer_sets :-
	construct_sp_file('domain.sp'), % 1
	os(windows), shell('sparc.bat', _ExitStatus), % 2
	translate_answer_sets_to_plan('answersets.txt'). % 3

	
generate_full_ASP_description :- ?
% As well as IO to translate between forms, may have to append some dynamic content to an existing ASP file.

translate_answer_sets_to_plan :- ?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Section 4: Learning functions %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

continue_active_action_learning :-
	know there exists at least one verbal cue
	grab that and put it in vocalisation.txt
	% Stanford parts of speech tagger:
	%java -mx300m -classpath "stanford-postagger.jar;" edu.stanford.nlp.tagger.maxent.MaxentTagger -outputFormat slashTags -model tagger-models\english-left3words-distsim.tagger -textFile vocalisation.txt 1>tagged.txt
	shell('tagging.bat', _ExitStatus),
	get tagged info from tagged.txt
	note that add_new_observations_to_history must store a TIMESTEP record for corresponding logical symbols
	repeat for any more verbal cues stored

continue_RRL_for_specific_unexpected_transition :- ?

continue_exploratory_RRL :- ?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

