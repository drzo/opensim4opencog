:- module(tribal, [
	 be_tribal/1
		  ]).

:-set_prolog_flag(double_quotes,string).

:- use_module(hillpeople(weather)).
:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(navigation)).
:- use_module(hillpeople(actions)).
:- use_module(cogbot(cogrobot)).

:- discontiguous be_tribal/3.

be_tribal(Name) :-
	botID(Name, ID),
	set_current_bot(ID),
	sex(Name, Sex),
	age(Name, Age),
	be_tribal(
	    _,
	    Name,
	    [
		sex(Sex),
		age(Age),
		cal(10.0),
		pro(10.0)
	    ]).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Initialization
%       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%       get near home
%
be_tribal(_,
	  Name,
	  Status) :-
	\+ memberchk(on_sim, Status),
	% get the current simulator name
%	botget([self,network,currentsim,name],Sim),
	tribal_land(Loc),
	botcmd(teleport(Loc)),
	be_tribal(home, Name, [on_sim|Status]).


%
%       if we don't have our inventory, get it
%
be_tribal(_,
	  Name,
	  Status) :-
	\+ memberchk(requested_inventory, Status),
	memberchk(on_sim, Status),
	\+ has_inventory,
	botcmd(touch('inventory_giver')),
	sleep(3),
	be_tribal(home, Name, [requested_inventory|Status]).

%
%       continue to wait until we get our inventory
%
be_tribal(_,
	  Name,
	  Status) :-
	memberchk(requested_inventory, Status),
	\+ has_inventory,
	sleep(5),
	be_tribal(home, Name, Status).


%
%       dress in starter outfit
%
be_tribal(_,
	  Name,
	  Status) :-
	\+ memberchk(inited, Status),
	memberchk(on_sim, Status),
	has_inventory,
	%%remove_all,
	start_wearing(Name, Items),
	wear_list(Items),
	be_tribal(home, Name, [inited|Status]).



%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Test Wander Mode
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  In test_wander_mode they just wander from point to point
%
test_wander_mode.

%
% if we have a route, walk to next waypoint
%
be_tribal(
    _,
    Name,
    Status) :-
	test_wander_mode,
	memberchk(en_route([H|T]), Status),
	botcmd(moveto(H, 1), MoveStat),
	botcmd(waitpos(20, H , 1), WaitStat),
	say_format('en_route went to ~w Remaining: ~w', [H,T]),
	say_ref('Move', MoveStat),
	say_ref('Wait', WaitStat),
	select(en_route(_), Status, en_route(T) , NewStatus),
	be_tribal(H, Name, NewStatus).

%
% cope with being done
%
be_tribal(
    Loc,
    Name,
    Status) :-
	test_wander_mode,
	memberchk(en_route([]), Status),
	select(en_route([]), Status, NewStatus),
	say_format('en_route empty, removing it', []),
	be_tribal(Loc, Name, NewStatus).

%
% go to the nearest waypoint
%
be_tribal(
    _Loc,
    Name,
    Status) :-
	test_wander_mode,
	\+ memberchk(en_route(_), Status),
	nearest_waypoint(WP, Dist),
	Dist >= 3.0,
	botcmd(moveto(WP, 1), MoveStat),
	botcmd(waitpos(10, WP, 1), WaitStat),
	say_format('too far from nearest waypoint, moving to~w',[WP]),
	say_ref('Move', MoveStat),
	say_ref('Wait', WaitStat),
	be_tribal(WP, Name, Status).

%
%  Set up a path
%
be_tribal(
    _Loc,
    Name,
    Status) :-
	test_wander_mode,
	\+ memberchk(en_route(_), Status),
	nearest_waypoint(Start, Dist),
	Dist < 3.0,
	waypoints(AllWP),
	random_member(End, AllWP),
	End \= Start,
	waypoint_path(Start, End, Path),
	say_format('No Path, new ~w to ~w is ~w',
	       [Start, End, Path]),
	be_tribal(Start, Name, [en_route(Path) | Status]).

%
% If we haven't gotten our inventory, get it and wear the items
%
/*
be_tribal(
    Loc,
    Name,
    Status) :-
	\+ memberchk(inited, Status),

	be_tribal(Loc, Name, [inited | Status]).
*/

%
% Die if yer starved
%
be_tribal(
    _,
    _Name,
    status(
	_,
	_,
	Cal,
	_)) :-
    Cal < -4.0,
    botcmd(anim(die)),
    sleep(30),
    botcmd(logout).

%
% die if yer outta protein
%
be_tribal(
    _,
    _Name,
    status(
	_,
	_,
	_,
	Pro)) :-
    Pro < -4.0,
    botcmd(anim(die)),
    sleep(30),
    botcmd(logout).

%
% Go home at night
%
/*
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
*/
%
% At this point it's obvious, I need a planner.
% just because there's a combinatorial explosion here.
% I need to get up, decide what to do, get out of the hut,
% now imagine I start taking off clothes at night,
% it just gets complicated...
%






