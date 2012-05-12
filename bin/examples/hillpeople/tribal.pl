:- module(tribal, [
	 be_tribal/1
		  ]).

:- use_module(hillpeople(weather)).
:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(navigation)).
:- use_module(hillpeople(actions)).
:- use_module(cogbot(cogrobot)).

be_tribal(Name) :-
	botID(Name, ID),
	set_current_bot(ID),
	sex(Name, Sex),
	age(Name, Age),
	be_tribal(
	    _,
	    Name,
	    status(
		Sex,
		Age,
		10.0, % cal
		10.0)).

%
%  In test_wander_mode they just wander from point to point
%
test_wander_mode.

be_tribal(
    Loc,
    Name,
    _) :-
	test_wander_mode,
	nearest_waypoint(WP, Dist),
	Dist >= 3.0,
	botClientCmd(


%
% Die if yer starved
%
be_tribal(
    _,
    Name,
    status(
	_,
	_,
	Cal,
	_)) :-
    Cal < -4.0,
    play_animation(Name, die),
    sleep(30),
    logout(Name).

%
% die if yer outta protein
%
be_tribal(
    _,
    Name,
    status(
	_,
	_,
	_,
	Pro)) :-
    Pro < -4.0,
    play_animation(Name, die),
    sleep(30),
    logout(Name).

%
% Go home at night
%
be_tribal(
    Location,
    Name,
    Status) :-
	is_night,
	\+ memberchk(Location, [hut1, hut2, hut3]),
	home(Name, Home),
	nearest_waypoint(Name, WP),
	waypoint_path(WP, Home, Path),
	navigate(
	    Location,
	    Name,
	    Status,
	    Path).
	% this is evil - what if you die, or are attacked,
	% etc.?

%
% sleep on mat when at home at night
%
be_tribal(
    Location,
    Name,
    Status) :-
	is_night,
	home(Name, Location),
	\+ sitting_on(Name, sleeping_mat),
	sit_on(Name, sleeping_mat),
	be_tribal(
	    Location,
	    Name,
	    Status).

%
%  when on mat at home at night, sleep
%
be_tribal(
    Location,
    Name,
    Status) :-
	is_night,
	home(Name, Location),
	sitting_on(Name, sleeping_mat),
	play_sound(Name, snore),
	basal_metabolism(Status, NewStatus, 30, 0.20),
	      % 20% because we're sleeping
	sleep(30),
	be_tribal(
	    Location,
	    Name,
	    NewStatus).

%
% At this point it's obvious, I need a planner.
% just because there's a combinatorial explosion here.
% I need to get up, decide what to do, get out of the hut,
% now imagine I start taking off clothes at night,
% it just gets complicated...
%






