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
                        forbidden/1, forbidden/3,
			obstacle/0, obstacle/1,
			failure/1, failure/2,
			current_test/1,
			apiBotClientCmd/1,
			onChat/3, std_end/2,
			doTest/3 , ppTest/1]).
:-use_module(library(clipl)).

:- dynamic(current_test/1).
:- dynamic(chat_hook_installed/0).
:- dynamic(needed/3).
:- dynamic(forbidden/3).
:- dynamic(obstacle/1).
:- dynamic(failure/2).

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
	forbidden(Name , TestName , _).

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

std_end(N , Num_Needed) :-
	all_needed(N , Num_Needed),
	needed(N,1),
	needed(N,2),
	\+ forbidden(N),
	\+ obstacle,
	\+ failure(N),
	end_test.

% Call prior to starting a test.
start_test(Name) :-
	\+ var(Name),
	retractall(current_test(_)),
	asserta(current_test(Name)),
	require_chat_hook,
	retractall(needed(_,_,_)),
	retractall(forbidden(_,_,_)),
	retractall(obstacle(_)),
	retractall(failure(_,_)),
	apiBotClientCmd(stop).

% call at conclusion of test
end_test :-
	current_test(Name),
	write('Test '),write(Name),write(' succeeded'),nl,flush_output,
	retractall(current_test(_)).

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
%  doTest(+Name , :Goal , -Results)
% run the goal, generating the results.
% The results is success or fail atom
%
doTest(_Name , Goal , Results) :-
	(   catch(once(Goal) , E , (print_message(error , E),fail)) ->
	    Results = success
	    ;
	    Results = fail
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
	member(results(fail) , List), !,
	(   member(name(N) , List) ;  N = 'no name'),
	(   member(desc(D) , List) ;  D = ''),
	concat_options(List , "" , Options),
	writef('****************\nFAIL: test %p %p %p FAILED\n' , [N,D,Options]), !.

ppTest(List) :-
	member(results(success) , List), !,
	(   member(name(N) , List) ;  N = 'no name'),
	(   member(desc(D) , List) ;  D = ''),
	concat_options(List , "" , Options),
	writef('OK: test %p %p %p\n' , [N,D,Options]) , !.



