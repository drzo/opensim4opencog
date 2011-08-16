% Cogbot testing path finder _Debugish
%
% Cogbot testing takes place in a virtual environment
% where there are objects that perform the role of asserts
% in most unit testing. There are currently 3 of these elements,
% forbidden, needed, and obstacle. These chat (on channel zero)
% prolog, which a listener on the bot asserts into prolog.
%

:-module(testpathfind_d, [tpf/0]).

:-use_module(library(testsupport)).
:-use_module(library(testpathfind)).
:-use_module(library(clipl)).


tpf :-
        cliSet('SimAvatarImpl','UseTeleportFallback','@'(false)),
	clause(testpathfind:test(N,S),Cs),
        doTest(N,S,Cs),
	fail.
tpf:-!.

%% astargoto does not block so we only give it enough time
testGoto(Time,Place):-callTest((apiBotClientCmd('astargoto'(Place)),sleep(Time))).


%pass
doTest(N,_S,Cs):-
        nl,writeq([starting,test,N]),nl,
        callTest((apiBotClientCmd(stop),Cs)),!.

%fail
doTest(_N,_S,_Cs):- current_test(Name),writeq([failed,test,Name]),nl,!.

callTest((C,Cs)):-!,callTest(C),callTest(Cs).
callTest(time_limit(Time , apiBotClientCmd('follow*'(Place)))):-!,testGoto(Time,Place).
callTest(Cs):-writeq(call(Cs)),nl,Cs,!.
callTest(Cs):-writeq(failed(Cs)),nl,!,fail.


