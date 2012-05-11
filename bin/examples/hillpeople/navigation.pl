:- module(navigation, [
		       waypoint/1,
		       move_via_wp/2
		      ]).


%
% unifies with waypoints - points mentioned in a c
%
waypoint(X) :- c(X, _).
waypoint(X) :- c(_, X).

%
%  this object isn't part of the pathfinding system,
%  we walk straight to/from the nearest waypoint from it
free_point(X) :-
	\+ waypoint(X).

conn(A,B) :- c(A,B),!.
conn(A,B) :- c(B,A).

c(hut1, wp1).
c(hut2, wp2).
c(hut3, wp3).
c(wp1, wp4).
c(wp1, wp3).
c(wp3, wp5).
c(wp5, wp2).
c(wp2, wp3).
c(wp6, wp1).
c(wp6, wp4).
c(wp6, wp5).
c(wp6, wp2).
c(wp6, wp_stream1).
c(wp_stream1, wp_stream2).
c(wp_stream2, wp_stream3).
c(wp_stream1,  fishball1).
c(wp_stream1,  fishball2).
c(wp_stream4, fishball1).
c(wp_stream4, fishball2).
c(wp_stream1, wp_stream4).
c(wp_stream4, wp_stream5).
c(wp_stream5, wp_stream6).
c(wp_stream6, wp_berry1).
c(wp_berry1, wp_berry2).
c(wp_berry2, wp_hunt1).
c(wp_hunt1, wp_hunt2).
c(wp_hunt2, wp_hunt3).
c(wp_hunt3, wp_hunt4).
c(wp_hunt4, wp_hunt5).
c(wp_hunt5, wp_hunt6).
c(wp_hunt5, wp_hunt7).
c(wp_hunt6, wp_hunt7).
c(wp_hunt6, wp_hunt8).
c(wp_hunt7, wp_hunt8).
c(wp_hunt8, wp_hunt10).
c(wp_hunt10, wp_hunt9).
c(X, Y) :-
	memberchk(X, [wp_hunt8, wp_hunt10, wp_hunt11]),
	memberchk(Y, [wp_hunt8, wp_hunt10, wp_hunt11]).
c(wp_hunt11, wp_hunt12).
c(wp_hunt12, wp_hunt13).
c(wp_hunt14, wp_hunt15).
c(wp_hunt15, wp_hunt16).
c(X, Y) :-
	memberchk(X, [wp_hunt14, wp_hunt16, wp_hunt17]),
	memberchk(Y, [wp_hunt14, wp_hunt16, wp_hunt17]).
c(X, Y) :-
	A = [
	     wp_hunt17,
	     wp_hunt18,
	     wp_hunt19,
	     wp_hunt20,
	     wp_hunt21],
	memberchk(X, A),
	memberchk(Y, A).











