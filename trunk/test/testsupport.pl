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

:- module(testsupport, [start_test/1, test_test_support/0, time_limit/2, test_assert/1, end_test/0, needed/3,
                       forbidden/3, obstacle/1, failure/1, current_test/1, apiBotClientCmd/1, onChat/3,
                       stdGoto/1, stdGoto/2, stdStart/1, doTest/3, callTest/1, failureCond/1]).
:-use_module(library(clipl)).

:- dynamic(current_test/1).
:- dynamic(chat_hook_installed/0).
:- dynamic(needed/3).
:- dynamic(forbidden/3).
:- dynamic(obstacle/1).
:- dynamic(failure/1).
:- dynamic(failureCond/1).

testDebug(Term):-format(user_error,'  ~q~n',[Term]),flush_output(user_error).

% Call prior to starting a test.
start_test(Name) :-
	\+ var(Name),
	retractall(current_test(_)),
	asserta(current_test(Name)),
	require_chat_hook,
	retractall(needed(_,_,_)),
	retractall(forbidden(_,_,_)),
	retractall(obstacle(_)),
	retractall(failure(_)),
        retractall(failureCond(_)),
	apiBotClientCmd(stop).

end_test :-
	current_test(Name),
	write('Test '),write(Name),write(' succeeded'),nl,flush_output,
	retractall(current_test(_)).

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

%
%  evaluate Goal, as in once(), but fail if goal fails or
%  takes more than Secs seconds to complete
time_limit(Secs , Goal) :-
	get_time(Start),
	once(Goal),
	get_time(Finish),
	Finish =< Secs + Start.

%
% evaluate a goal. If the goal fails, the test fails.
test_assert(Goal) :- once(Goal).

% entry point to test this module
:- dynamic(skeemumkin/1).
test_test_support :- string_subst("test /me" , "/me" , "Annie" , "test Annie"),
	string_subst("/me foo" , "/me", "onions" , "onions foo"),
	string_subst("taco" , "/me" , "foo" , "taco"),
	string_subst("/me /me" , "/me" , "foo" , "foo foo"),
	time_limit(5 , sleep(2)),
	\+ time_limit(3 , sleep(5)),
	time_limit(5 , assert(skeemumkin(47))),
	skeemumkin(47),
	retractall(skeemumkin(_)).

apiBotClientCmd(A) :- user:botClientCmd(A,_).

stdStart(Place):-apiBotClientCmd('teleport'(Place)).


user:onChat(X,Y,Z):-testsupport:onChat(X,Y,Z).

%% astargoto does not block so we only give it enough time
stdGoto(Place):-apiBotClientCmd('astargoto'(Place)).
stdGoto(Time,Place):-callTest((apiBotClientCmd('astargoto'(Place)),sleep(Time))),!.
stdGoto(Time,Place):-callTest((apiBotClientCmd('follow*'(Place)),sleep(Time))),!.

%pass
doTest(N,_S,Cs):-
        nl,
        testDebug([starting,test,N]),
        callTest((apiBotClientCmd(stop),Cs)),!,
        apiBotClientCmd(stop).

%fail
doTest(_N,_S,_Cs):- current_test(Name),apiBotClientCmd(stop),testDebug([failed,test,Name]). %%,listing(failureCond),!.

callTest((C,Cs)):-!,callTest(C),!,callTest(Cs),!.
callTest(time_limit(Time , apiBotClientCmd('follow*'(Place)))):-!,stdGoto(Time,Place).
callTest(time_limit(Time , stdGoto(Place))):-!,stdGoto(Time,Place).
callTest(call(Cs)):-testDebug(call(Cs)),Cs,!.
callTest(end_test):-failureCond(_),!,fail. % squeltch
callTest(Cs):-testDebug(call(Cs)),Cs,!.
callTest(Cs):-testDebug(failed(Cs)),assert(failureCond(Cs)),!.


