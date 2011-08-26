% Cogbot testing support
%
% Cogbot testing takes place in a virtual environment
% where there are objects that perform the role of asserts
% in most unit testing. There are currently 3 of these elements,
% forbidden, needed, and obstacle. These chat (on channel zero)
% prolog, which a listener on the bot asserts into prolog.
%
% So, hearing

%  obstacle (collision(v3(147.50407409668, 134.732803344727, 1001.00823974609)))
%  the test framework asserts
%  obstacle (collision(v3(147.50407409668, 134.732803344727, 1001.00823974609))).
% This allows the test to reason about what happened at the end
% of the test
%
% The objects formats are
% obstacle (collision(v3(147.50, 134.73, 1001.00)))
% needed (collision(v3(152.83, 137.445, 1001.00085)), 1,1)
% forbidden (collision(v3(153.57, 139.33, 1001.00)), 1,1)
%
% More generally object_name(verb(location_or_someday_other_args) ,
% test_number , object_number)
%
%  obstacles don't have a test or object number
%  So, for example, testing that no forbidden objects have been
%  encountered during test 3 is as simple as
%  \+ forbidden(_,3,_)
%

:- module(testsupport, [start_test/1, end_test/0,
			needed/2, needed/3,
                        forbidden/1, forbidden/2,
			obstacle/0, obstacle/1,
			failure/1, failure/2,
			current_test/1,
			apiBotClientCmd/1,
			onChat/3, std_end/2, std_end/3,
			doTest/3 , ppTest/1]).
:-use_module(library(clipl)).

:- dynamic(current_test/1).
:- dynamic(chat_hook_installed/0).
:- dynamic(needed/3).
:- dynamic(forbidden/2).
:- dynamic(obstacle/1).
:- dynamic(failure/2).
:- dynamic(current_test_started_at_time/1).
:- dynamic(time_limit_exceeded/2).

% debug output
testDebug(Term):-format(user_error,'  ~q~n',[Term]),flush_output(user_error).

% unify if this is the bot's name
botName(Name) :-
	Name = 'testbot Ogborn'.  %TODO Douglas, how do I read this from
% Configuration in ClientManager.cs

needed(TestName , Number) :-
	botName(Name),
	needed(Name , TestName  , Number).

forbidden(TestName) :-
	botName(Name),
	forbidden(Name , TestName).

obstacle :-
	botName(Name),
	obstacle(Name).

failure(TestName) :-
	botName(Name),
	failure(Name , TestName).

all_needed(_ , 0).

all_needed(N , Num_Needed) :-
	Num_Needed > 0,
	needed(N , Num_Needed),
	NN is Num_Needed - 1,
	all_needed(N , NN).

%
% Convenience method that performs common end
% condition tests and calls end_test.
% unifies only if the test succeeded
%
% std_end(+Name , +NumNeeded)
% Name is the name of the test
% NumNeeded is the number of consecutive, 1 based needed/2 tests that
% must be found
std_end(N , NumNeeded) :-
	all_needed(N , NumNeeded),
	\+ forbidden(N),
	\+ obstacle,
	\+ failure(N),
	end_test.

% same as std_end/2 but has a time limit
% std_end(+N , +TimeLimit , +NumNeeded)
%
std_end(N , TimeLimit , NumNeeded) :-
	time_limit(TimeLimit),
	std_end(N , NumNeeded).

% Call prior to starting a test.
start_test(Name) :-
	\+ var(Name),
	retractall(current_test(_)),
	asserta(current_test(Name)),
	retractall(current_test_started_at_time(_)),
	retractall(time_limit_exceeded(_,_)),
	get_time(Time),
	assert(current_test_started_at_time(Time)),
	require_chat_hook,
	retractall(needed(_,_,_)),
	retractall(forbidden(_,_)),
	retractall(obstacle(_)),
	retractall(failure(_,_)),
	apiBotClientCmd(stop),!.

% call at conclusion of test
% this only gets there if the test succeeds, so it's pretty suspicious.
end_test :-
	current_test(_Name),
%	write('Test '),write(Name),write(' succeeded'),nl,flush_output,
	retractall(current_test(_)).

% enforce a time limit.
% unifies only if called within Limit seconds of
% the start of the test
% time_limit(+Limit)
%
time_limit(Limit) :-
	get_time(Time),
	current_test_started_at_time(Start),
	Taken is Time - Start,
	(
	    Taken > Limit
	->
	    assert(time_limit_exceeded(Limit , Taken)),
	    !,fail
	;
	    true
	).

% this is installed as a callback, called when the bot hears chat
% the various objects in the VW environment chat facts that it
% asserts.
%
% Then the test can check these facts.
onChat(_Originator, _Sender, Event) :-
	Event = event(
		      'ChatEventArgs',
		      _Orig,
		      Content,   % string
		      _,_,_,
		      Name,      % string
		      _,_,_Loc),
        string_subst(Content , "/me " , Name , Term_As_String),
	string_to_atom(Term_As_String , Term_As_Atom),
	catch(atom_to_term(Term_As_Atom , Term , _), SyntaxError,(testDebug(SyntaxError),fail)),
	catch(asserta(Term) , _Assert_error , true).

onChat(_Originator, _Sender, _Event) :-!.

% make sure the chat hook is installed
%
% we assert chat_hook_installed when we've installed the
% onChat callback so we only do it once
require_chat_hook :-
	chat_hook_installed.

require_chat_hook :-
	asserta(chat_hook_installed),
	user:gridClient(Obj),
	cliGet(Obj , 'Self' , S),
	cliAddEventHandler(S , 'ChatFromSimulator' , onChat(_,_,_)).

% string_subst(?S , ?T , ?R , ?NS)
% Substitute all occurances of T with R in S, producing NS
%
string_subst(S , T , R , NS) :-
    string_to_list(S , LS),
    string_to_list(T , LT),
    string_to_list(R , LR),
    once(list_string_subst(LS , LT , LR , LNS)),
    string_to_list(NS , LNS).

list_string_subst(S , T , R , NS) :-
	append(PreAndTag , Post , S),
	append(Spre , T , PreAndTag),
	append(Spre , R , PreAndR),
	append(PreAndR , Post , OneLessT),
	list_string_subst(OneLessT , T , R , NS).

list_string_subst(S , _ , _ , S).

apiBotClientCmd(A) :- user:botClientCmd(A,_).

user:onChat(X,Y,Z):-testsupport:onChat(X,Y,Z).

%
% results(+Name , -R)
% returns a list of conditions tripped and not tripped
% for a given test name
results(Name , R) :-
	findall(N , needed(Name , N) , Needed),
	(
	   forbidden(Name)
	->
	   L = [forbidden, needed(Needed)]
	;
	   L = [needed(Needed)]
	),
	(
	   obstacle
	->
	   LL = [obstacle|L]
	;
	   LL = L
	),
	(
	   failure(Name)
	->
	   R = [failure|LL]
	;
	   R = LL
	),
	!.

%
%  doTest(+Name , :Goal , -Results)
% run the goal, generating the results.
% The results is success or fail atom
%
doTest(Name , Goal , Results) :-
	(   catch(once(Goal) , E , (print_message(error , E),fail)) ->
	    results(Name , R),
	    Results = [results(success)|R]
	    ;
	    results(Name , R),
	    Results = [results(fail)|R]
	),!.


% famulus to ppTest
concat_options([] , X , X) :- !.

concat_options([option(Name , Value)|T] , "" , Out) :- !,
	swritef(Str , '%p=%p' , [Name , Value]),
	concat_options(T , Str , Out).

concat_options([option(Name , Value)|T] , In , Out) :-
	In \= "", !,
	swritef(Str , '%s, %p=%p' , [In , Name , Value]),
	concat_options(T , Str , Out).

concat_options([H|T] , In , Out) :-
	H \= option(_,_),!,
	concat_options(T , In , Out).

%
%  flexible pretty print method for tests
%
%  failure clause
ppTest(List) :-
	member(results(R) , List),
	member(results(fail) , R), !,
	(   member(name(N) , List) ;  N = 'no name'),
	(   member(desc(D) , List) ;  D = ''),
	concat_options(List , "" , Options),
	writef('****************\nFAIL: test %p %p %p FAILED\n%p\n' ,
	       [N,D,Options,R]),
	write_time_limit_exceeded , flush_output, !.

ppTest(List) :-
	member(results(R), List),
	member(results(success) , R), !,
	(   member(name(N) , List) ;  N = 'no name'),
	(   member(desc(D) , List) ;  D = ''),
	concat_options(List , "" , Options),
	writef('OK: test %p %p %p\n%p\n' , [N,D,Options,R]) ,
	write_time_limit_exceeded, flush_output, !.


%
% print warning if time limit exceeded
%
write_time_limit_exceeded :-
	time_limit_exceeded(Limit , Taken),!,
	writef('Time limit exceeded, allowed %d, took %d\n' , [Limit,Taken]).

write_time_limit_exceeded :- !.





