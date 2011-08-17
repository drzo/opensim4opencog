:-module(testpathfind, [testpathfind/0, tpf/0]).

:- use_module(library(testsupport)).
:-use_module(library(clipl)).

test(1, N) :-
	N= 'clear path 10 meters',
	start_test(N),
        test_assert(apiBotClientCmd(teleport('annies haven/129.044327/128.206070/81.519630/'))),
	time_limit(15 , stdGoto('annies haven/133.630234/132.717392/81.546028/')),
	needed(_,1,1),
	needed(_,1,2),
	\+ forbidden(_,1,_),
	\+ obstacle(_),
	\+ failure(1),
	end_test.

test(2, N) :-
	N= 'Zero Distance',
	start_test(N),
	test_assert(apiBotClientCmd(teleport(start_test_2))),
	time_limit(1 , stdGoto(stop_test_2)),
        \+ forbidden(_,_,_),
	\+ obstacle(_),
	end_test.


test(3, N) :-
         N= 'Go around obstacle',
         start_test(N),
         test_assert(apiBotClientCmd(teleport(start_test_3))),
         time_limit(25 , stdGoto(stop_test_3)),
         needed(_,3,1),         
         \+ obstacle(_),
         \+ forbidden(_,3,1),
         end_test.

/*
test(3, N) :-
	N= 'Rotating Obstacle',
	start_test(N),
	apiBotClientCmd(teleport('annies haven/137.404724/187.234711/1000.985291/')),
	time_limit(15 , apiBotClientCmd('follow*'('annies haven/139.016434/206.675934/1000.985229/'))),
	needed(_,3,1),
	\+ obstacle(_),
	end_test.


test(4, N) :-
	N= 'Goal Other Side Of Wall',
	start_test(N),
	apiBotClientCmd(teleport('annies haven/138.298828/225.949951/1000.984985/')),
	time_limit(15 , apiBotClientCmd('follow*'('annies haven/138.507294/227.840652/1000.988586/'))),
	needed(_,4,1),
	\+ obstacle(_),
	end_test.



test(6, N) :-
	N= 'narrowest gap we can go through',
	start_test(N),
	apiBotClientCmd(teleport('annies haven/150.241486/131.945526/1000.985291/')),
	time_limit(15 , apiBotClientCmd('follow*'('annies haven/148.898590/146.752121/1000.988281/'))),
	\+ obstacle(_),
	\+ forbidden(_,_,_),
	end_test.

test(6, N) :-
	N= 'tortured prim tube',
	start_test(N),
	apiBotClientCmd(teleport('annies haven/236.392776/245.958130/1000.986572/')),
	time_limit(20 , apiBotClientCmd('follow*'('annies haven/239.544891/232.117767/1000.987122/'))),
	end_test.

test(7, N) :-
	N= 'jagged maze',
	start_test(N),
	apiBotClientCmd(teleport('annies haven/233.436218/221.673218/1000.988770/')),
	time_limit(60 , apiBotClientCmd('follow*'('annies haven/248.193939/190.898941/1000.985291/'))),
	\+ obstacle(_),
	end_test.

*/
testpathfind :-
	cliSet('SimAvatarImpl','UseTeleportFallback','@'(false)),
	test(_,_),
	fail.

tpf :-
        cliSet('SimAvatarImpl','UseTeleportFallback','@'(false)),
	clause(testpathfind:test(N,S),Cs),
        doTest(N,S,Cs),
	fail.
tpf:-!.

