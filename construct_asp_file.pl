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
	readwrite_axioms(File),
	readwrite_state_constraints_meta_and_statics(File),
	readwrite_current_state_and_goal(File),
	readwrite_display(File).

% % % % % % % % % % % % % % % % % % % % % % % % % % %
 % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % %

% The following parts can be considered static; read them directly from a file

readwrite_predicates(File) :-
	direct_readwrite('predicates.txt', File).

% % % % % % % % % % % % % % % % % % % % % % % % % % %
 % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % %

% The following parts are dynamic and will change

readwrite_preamble(File) :-
	currentTime(CurrentTime),
	number_of_ASP_steps_to_lookahead(N),
	Steps is CurrentTime + N,
	open(File, append, O),
	write(O, "#const numSteps = "),
	write(O, Steps),
	write(O, "."),
	close(O),
	direct_readwrite('preamble.txt', File).
/* Deprecated
	open(File, append, O2),
	write(O2, "#step = "),
	write(O2, CurrentTime),
	write(O2, "..numSteps."),
	close(O2).
*/
	

/*
Use internal list of valid actions, which can be extended through learning.
Also includes EXOACTIONS, which must be given dynamically.
*/
readwrite_actions(File) :-
	direct_readwrite('actions.txt', File).

/*
Use existing ASP file, and then add learned causal laws and executability conditions (stored in a form convenient for ASP).
*/
readwrite_axioms(File) :-
	direct_readwrite('axioms.txt', File).

/*
List of what holds.
I suppose the terms are passed back and forth between ASP and other modules?
And then added to with observations here.
Plus goal, if it exists.
ASP never CHANGES the goal, so can just store it locally and pass it to ASP whenever needed. But how to evaluate when goal is met, so remove?
*/
readwrite_current_state_and_goal(File) :-
	add_holds_at_zero(File),
	add_obs(File),
	add_hpd(File),
	direct_readwrite('current_state.txt', File).

%holds_at_zero(L) -> holds(L,0)
add_holds_at_zero(File) :-
	findall(holds(L,0), holds_at_zero(L), List),
	open(File, append, O),
	nl(O),
	writelneach(List, O),
	close(O).
%obs(X,Y,Z) -> obs(X,Y,Z)
add_obs(File) :-
	findall(obs(X,Y,Z), obs(X,Y,Z), List),
	open(File, append, O),
	nl(O),
	writelneach(List, O),
	close(O).
%hpd(Action, T) -> hpd(Action, T)
add_hpd(File) :-
	findall(hpd(A,T), hpd(A,T), List),
	open(File, append, O),
	nl(O),
	writelneach(List, O),
	nl(O),
	close(O).

/*
While static, these should be generated from a single central data store.
RRL purports to change statics, but consider that to be relabelling or interacting with a simulation; any actual changes it makes should be reverted.
Or better yet, the RRL module should have access to a list of literals that may be originally derived from the true list, but not actually the robot's model of the world.
*/
readwrite_state_constraints_meta_and_statics(File) :-
	direct_readwrite('state_constraints_meta_and_statics.txt', File),
	add_domain_attributes(File).
	
add_domain_attributes(File) :-
	findall(Att, domain_attr(Att), AttList),
	open(File, append, O),
	nl(O),
	writelneach(AttList, O),
	close(O).

writelneach([A|B], O) :-
	writeln(O, A),
	writelneach(B, O).
writelneach([], _).
	
/*
Change this to include in the printed portion of the answer set
- what occurs (for planning)
- what holds (including e.g. inertia)
- what doesn't hold (including e.g. inertia)
*/
readwrite_display(File) :-
	direct_readwrite('display.txt', File).



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


