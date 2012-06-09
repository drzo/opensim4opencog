:- module(tribal, [
	 be_tribal/1,
		   get_current_action/2
		  ]).

:-set_prolog_flag(double_quotes,string).

:- use_module(hillpeople(weather)).
:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(navigation)).
:- use_module(hillpeople(actions)).
:- use_module(cogbot(cogrobot)).
:- use_module(hillpeople(events)).
:- use_module(hillpeople(actionselection)).

:- discontiguous be_tribal/3.

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Botvar support
%       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic tribal_dyn:current_action/2.

%
% Set the botvar for the current bot action
%
set_current_action(Name, Action) :-
	with_output_to(string(S), writeq(Action)),
	retractall(tribal_dyn:current_action(Name, _)),
	assert(tribal_dyn:current_action(Name, S)).

% as for format/2
set_current_action(Name, ActionFormat, Args) :-
	format(string(S), ActionFormat, Args),!,
	retractall(tribal_dyn:current_action(Name, _)),
	assert(tribal_dyn:current_action(Name, S)).
set_current_action(_, ActionFormat, Args) :-
	format('Illegal: set_current_action/3 requires 2nd and 3rd arg as for format/2, you supplied ~w  ~w~n', [ActionFormat, Args]).

bv:hook_botvar_get(BotID, bot, current_action, X) :-
	@(get_current_action(BotID, X), tribal).

get_current_action(BotID, X) :-
	botID(Name, BotID),
	tribal_dyn:current_action(Name, X),!.
get_current_action(_, "nothing").

bv:hook_botvar_set(_, bot, current_action, _) :-
	format('the botvar current_action is readonly~n', []).

bv:hook_botvar_key(_, bot, current_action).

bv:hook_botvar_desc(_, bot, current_action,
     "ReadOnly - The current action performed by the bot").

be_tribal(Name) :-
	botID(Name, ID),
	register_listeners,
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
        set_current_action(Name, "teleporting to start location"),
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
	set_current_action(Name, "getting starting inventory"),
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
	set_current_action(Name, "waiting for inventory to arrive"),
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
	set_current_action(Name, "getting dressed in starter outfit"),
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
% in test_wander_mode, if we don't have a plan,
% and are far from a waypoint,
% go to the nearest
% waypoint
%
be_tribal(
    _Loc,
    Name,
    Status) :-
	test_wander_mode,
	\+ memberchk(cur_plan(_), Status),
	nearest_waypoint(WP, Dist),
	Dist >= 3.0,
	botcmd(moveto(WP, 1), MoveStat),
	botcmd(waitpos(10, WP, 1), WaitStat),
	set_current_action(Name,
		   'too far from nearest waypoint, moving to~w',[WP]),
	say_format('too far from nearest waypoint, moving to~w',[WP]),
	say_ref('Move', MoveStat),
	say_ref('Wait', WaitStat),
	be_tribal(WP, Name, Status).

%
%  Set up a path in test_wander_mode
%
be_tribal(
    _Loc,
    Name,
    Status) :-
	test_wander_mode,
	\+ memberchk(cur_plan(_), Status),
	set_current_action(Name, "Creating a new test wander path"),
	nearest_waypoint(Start, Dist),
	Dist < 3.0,
	waypoints(AllWP),
	random_member(End, AllWP),
	End \= Start,
	waypoint_path(Start, End, Path),
	say_format('No Path, new ~w to ~w is ~w',
	       [Start, End, Path]),
	be_tribal(Start, Name, [cur_plan(Path) | Status]).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Movement
%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% if we have a route, walk to next waypoint
%
be_tribal(
    _,
    Name,
    Status) :-
	memberchk(cur_plan([goto(H)|T]), Status),
	set_current_action(Name, 'Walking to ~w', [H]),
	botcmd(moveto(H, 1), MoveStat),
	set_current_action(Name, 'Waiting to arrive at ~w', [H]),
	botcmd(waitpos(20, H , 1), WaitStat),
	set_current_action(Name, 'Processing after arriving at waypoint ~w', [H]),
	say_format('cur_plan went to ~w Remaining: ~w', [H,T]),
	say_ref('Move', MoveStat),
	say_ref('Wait', WaitStat),
	select(cur_plan(_), Status, cur_plan(T) , NewStatus),
	be_tribal(H, Name, NewStatus).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Replan
%
%        clauses in this section involve creating a new plan
%
%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Remove the plan if we're done with it
%
be_tribal(
    Loc,
    Name,
    Status) :-
	memberchk(cur_plan([]), Status),
	select(cur_plan([]), Status, NewStatus),
	say_format('cur_plan empty, removing it', []),
	be_tribal(Loc, Name, NewStatus).

/*
%
%  No plan, do something suggested by a nearby affordance
%
be_tribal(
    Loc,
    Name,
    Status) :-
	\+ memberchk(cur_plan(_), Status),
	nearest_in_list(
	    [
	    wumpus,
	    wumpus_meat,
	    unplanted_corn,
	    ripe_corn,
	    fishball1,
	    fishball2,
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






