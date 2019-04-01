
% Includes
:- [pretty_printer].

:- dynamic cl_rec/2.

/*
Dynamic content:
	holds_at_zero(L) -> holds(L,0)
	domain_attr(A) -> A
	obs(X,Y,Z) -> obs(X,Y,Z)
	hpd(Action, T) -> hpd(Action, T)
*/

% % % % % % % % % % % % % % % % % % % % % % % % % % %
 % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % %
	
%update_asp :- construct_sp_file('assembled.pl').

construct_sp_file(File) :-
	open(File, write, F),
	close(F),
	readwrite_preamble(File),
	readwrite_actions(File),
	readwrite_predicates(File),
	readwrite_axioms(File), % And meta
	readwrite_current_state_and_goal(File).
	%readwrite_display(File).

% % % % % % % % % % % % % % % % % % % % % % % % % % %
 % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % %

% The following parts can be considered static; read them directly from a file

readwrite_predicates(File) :-
	direct_readwrite('pr_predicates.txt', File).

% % % % % % % % % % % % % % % % % % % % % % % % % % %
 % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % %

% The following parts are dynamic and will change

readwrite_preamble(File) :-
	/*
	currentTime(CurrentTime),
	number_of_ASP_steps_to_lookahead(N),
	Steps is CurrentTime + N,
	open(File, append, O),
	write(O, "#const numSteps = "),
	write(O, Steps),
	write(O, "."),
	close(O),
	*/
	direct_readwrite('pr_preamble.txt', File).
/* Deprecated
	open(File, append, O2),
	write(O2, "#step = "),
	write(O2, CurrentTime),
	write(O2, "..numSteps."),
	close(O2).
*/


/*
Use internal list of valid actions, which can be extended through learning.
Also includes exoactions, which must be given dynamically.
*/
readwrite_actions(File) :-
	%direct_readwrite('pr_actions.txt', File),
	writeExoActions(File),
	open(File, append, O),
	writeln(O, "#action = #agentaction + #learned_exoaction."),
	nl(O),
	close(O).
writeExoActions(File) :-
	not(exoActionDescription(_,_,_,_)),
	!,
	open(File, append, O),
	writeln(O, "#learned_exoaction = unknown(#thing)."),
	close(O).
writeExoActions(File) :-
	open(File, append, O),
	write(O, "#learned_exoaction = "),
	findall([A,B,C,D], exoActionDescription(A,B,C,D), List),
	write_each_exo(O, List),
	writeln(O, "."),
	nl(O),
	close(O).
write_each_exo(O, [A,B|Tail]) :-
	!,
	write_exogenous_and_create_causal_laws(O, A),
	write(O, " + "),
	write_each_exo(O, [B|Tail]).
write_each_exo(O, [A]) :-
	write_exogenous_and_create_causal_laws(O, A).
write_exogenous_and_create_causal_laws(O, Action) :-
	Action = [Head, _Args, Sorts, Results],
	functor(Head, Predicate, _NumArgs),
	write(O, Predicate),
	write(O, "("),
	writeExoSorts(O, Sorts),
	write(O, ")"),
	record_causal_laws_for_exoactions(Head, Results).
	
writeExoSorts(O, [A,B|Tail]) :-
	!,
	write(O, "#"),
	write(O, A),
	write(O, ", "),
	writeExoSorts(O, [B|Tail]).
writeExoSorts(O, [A]) :-
	write(O, "#"),
	write(O, A).


% Record exoactions' causal laws for later addition to ASP program
record_causal_laws_for_exoactions(Head, Results) :-
	retractall(cl_rec(_, _)),
	record_causal_laws_exos_each(Head, Results),
	cleanuptemp.
	
cleanuptemp :-
	delete_file('pr_temp.txt'), !.
cleanuptemp.

record_causal_laws_exos_each(_Head, []).
record_causal_laws_exos_each(Head, [A|B]) :-
	establish_causal_law_for_learned_action(Head, A, String),
	assert(cl_rec(String)),
	record_causal_laws_exos_each(Head, B).

establish_causal_law_for_learned_action(Head, not(Outcome), ReturnString) :-
	!,
	Clause = (holds(Outcome,Time +1) :- occurs(Head,Time)),
	numbervars(Clause), % Doing this means writing it out should retain alphabetical variable names
	open('pr_temp.txt', write, O),
	write(O, Clause),
	writeln(O, "."),
	close(O),
	read_file_to_string('pr_temp.txt', String1, []),
	split_string(String1, ":", "-", [StringHead, StringTail]),
	string_concat(StringHead, " :- ", ST),
	string_concat("-", ST, ST2), % Negated head. Add now to prevent "-" being removed in splitting
	string_concat(ST2, StringTail, ReturnString).
establish_causal_law_for_learned_action(Head, Outcome, ReturnString) :-
	!,
	Clause = (holds(Outcome,Time +1) :- occurs(Head,Time)),
	numbervars(Clause), % Doing this means writing it out should retain alphabetical variable names
	open('pr_temp.txt', write, O),
	write(O, Clause),
	writeln(O, "."),
	close(O),
	read_file_to_string('pr_temp.txt', String1, []),
	split_string(String1, ":", "-", [StringHead, StringTail]),
	string_concat(StringHead, " :- ", ST),
	string_concat(ST, StringTail, ReturnString).

	

/*
Axioms: Use existing ASP file, and then add learned causal laws and executability conditions (stored in a form convenient for ASP).
*/
/*
Attributes: While static, these should be generated from a single central data store.
RRL does purport to change statics, but consider that to be relabelling or interacting with a simulation; any actual changes it makes should be reverted.
Or better yet, the RRL module should have access to a list of literals that may be originally derived from the true list, but not actually the robot's model of the world.
*/
readwrite_axioms(File) :-
	write_exogenous_causal_laws(File),
	direct_readwrite('pr_axioms.txt', File),
	add_domain_attributes(File).

write_exogenous_causal_laws(File) :-
	open(File, append, O),
	nl(O),
	writeln(O, "% Learned human actions : causal laws"),
	write_cl_recs(O),
	close(O).

write_cl_recs(O) :-
	cl_rec(String),
	!,
	retractall(cl_rec(String)),
	write(O, String),
	write_cl_recs(O).
write_cl_recs(_) :- !.

/*
Create the list of what holds.
- Terms passed back and forth between ASP and other modules?
- And then added to with observations here.
- Also the goal, if it exists.
- Note ASP never CHANGES the goal, so can just store it locally and pass it to ASP whenever needed.
- But how to evaluate when goal is met, therefore remove it?
*/
readwrite_current_state_and_goal(File) :-
	%add_holds_at_zero(File),
	add_obs(File),
	add_hpd(File),
	add_goal(File),
	direct_readwrite('pr_state_goal_display.txt', File).

%holds_at_zero(L) -> holds(L,0)
/*add_holds_at_zero(File) :-
	findall(holds(L,0), holds_at_zero(L), List),
	open(File, append, O),
	nl(O),
	writelneach(List, O),
	close(O).*/
%obs(X,Y,Z) -> obs(X,Y,Z)
add_obs(File) :-
	findall(obs(X,Y,Z), obs(X,Y,Z), List),
	open(File, append, O),
	nl(O),
	writelneach(List, O),
	close(O).
%hpd(Action, T) -> hpd(Action, T)
add_hpd(File) :- % Includes exoactions
	findall(hpd(A,T), hpd(A,T), List),
	open(File, append, O),
	nl(O),
	writelneach(List, O),
	nl(O),
	close(O).

add_goal(File) :-
	open(File, append, O),
	trace,
	currentGoal(Goal),
	(atom(Goal)
	->
	atom_string(Goal,GoalString)
	;
	GoalString = Goal
	),
	writeln(O, GoalString),
	nl(O),
	close(O).

add_domain_attributes(File) :-
	findall(Att, domain_attr(Att), AttList),
	open(File, append, O),
	nl(O),
	writelneach(AttList, O),
	close(O).

writelneach([A|B], O) :-
	write(O, A),
	writeln(O, "."),
	writelneach(B, O).
writelneach([], _).
	
/*
Change this to include in the printed portion of the answer set
- what occurs (for planning)
- what holds (including e.g. inertia)
- what doesn't hold (including e.g. inertia)

readwrite_display(File) :-
	direct_readwrite('pr_display.txt', File).
*/



% % % % % % % % % % % % % % % % % % % % % % % % % % %
 % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % %

direct_readwrite(InputFile, OutputFile) :-
	open(InputFile, read, I),
	open(OutputFile, append, O),
	read_and_write_all(I, O),
	close(I),
	close(O).
read_and_write_all(I, O) :-
	get_char(I, Char),
	check_for_end_of_file(I, O, Char).
check_for_end_of_file(_I, _O, end_of_file) :- !.
check_for_end_of_file(I, O, Char) :-
	put_char(O, Char),
	read_and_write_all(I, O).
	
% % % % % % % % % % % % % % % % % % % % % % % % % % %
 % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % %


