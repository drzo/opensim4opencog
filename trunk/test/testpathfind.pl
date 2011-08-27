:-module(testpathfind, [tpf_method/1, tpf/0]).

:- use_module(library(testsupport)).
:-use_module(library(clipl)).

:- dynamic(goMethod/1).

% set the method of movement we're testing
goMethod('follow*').



% using the test method of movement, go to Location
goByMethod(Location) :-
	goMethod(GM),!, % slight ugliness, just want first one
	Goal =.. [GM, Location],
	apiBotClientCmd(Goal).

% convenience method that does the 'normal' thing -
% tp to Start, move using the standard method to
move_test(Time , Start , End) :-
	apiBotClientCmd(teleport(Start)),
	goByMethod(End),
	sleep(Time).


% this is just to debug the test framework with
test_desc(easy , 'insanely easy test').
test(N) :-
	N = easy,
	writeq('in easy'),nl.

test_desc(clear , 'clear path 10 meters').
test(N) :-
	N = clear,
	start_test(N),
        move_test(15,
		'annies haven/129.044327/128.206070/81.519630/',
	        'annies haven/133.630234/132.717392/81.546028/'),
	std_end(N , 17 ,2).


test_desc(zero , 'Zero Distance').
test(N) :-
	N = zero,
	start_test(N),
	move_test(1 , start_test_2 , stop_test_2),
        std_end(N , 2 , 0).


test_desc(obstacle , 'Go around obstacle').
test(N) :-
	 N = obstacle,
         start_test(N),
	 move_test(25 , start_test_3 , stop_test_3),
         std_end(N , 27 , 2).

test_desc(other_side_wall , 'Goal Other Side Of Wall').
test(N) :-
	N = other_side_wall,
	start_test(N),
        move_test(25 , start_test_4 , stop_test_4),
        std_end(N , 27 , 1).

test_desc(elev_path , 'On Elevated Path').
test(N) :-
	N = elev_path,
	start_test(N),
	move_test(15 ,
		  'annies haven/149.389313/129.028732/85.411255/',
		  'annies haven/156.894470/137.385620/85.394775/'),
	std_end(N , 17 , 0).

test_desc(spiral , 'Spiral Tube').
test(N) :-
	N = spiral,
	start_test(N),
	move_test(60,
		  'annies haven/188.477066/142.809982/81.559509/',
		  'annies haven/181.878403/140.768723/101.555061/'),
	std_end(N , 65 , 1).

test_desc(grnd_maze , 'Ground maze simple').
test(N) :-
	N = grnd_maze,
	start_test(N),
	move_test(30 ,
		  'annies haven/4.813091/6.331439/27.287579/',
		  'annies haven/26.930264/12.801470/27.149252/'),
	std_end(N , 34 , 2).

test_desc(island_hop , 'Island hop').
test(N) :-
	N = island_hop,
	start_test(N),
	move_test(10 , start_island_hop , stop_island_hop),
	std_end(N , 12 , 2).

test_desc(hill_walk , 'Hill Walk').
test(N) :-
	N = hill_walk,
	start_test(N),
	move_test(60 , start_hill_walk , stop_hill_walk),
	std_end(N , 65 , 1).





/*

	keep this stuff, it's from the old opensim build, but it's a record
	of what tests we were doing

test(3, N) :-
	N= 'Rotating Obstacle',
	start_test(N),
	apiBotClientCmd(teleport('annies haven/137.404724/187.234711/1000.985291/')),
	time_limit(15 , apiBotClientCmd('follow*'('annies haven/139.016434/206.675934/1000.985229/'))),
	needed(_,3,1),
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

tpf_method(GoMethod) :-
	retractall(goMethod(_)),
	asserta(goMethod(GoMethod)),
	cliSet('SimAvatarImpl' , 'UseTeleportFallback' , '@'(false)),
%	clause(testpathfind:test(Name) , _),
	test_desc(Name , Desc),
	doTest(Name , testpathfind:test(Name) , Results),
	ppTest([name(Name),
		desc(Desc) ,
		results(Results) ,
		option('goMethod ' , GoMethod)]),
	fail.

tpf_method(_) :- !.

tpf :-
	member(Method , ['follow*' , astargoto]),
	tpf_method(Method),
	fail.

tpf :- !.



