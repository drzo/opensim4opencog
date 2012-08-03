:- module(tribal, [
	 be_tribal/1,
		   get_current_action/2
		  ]).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tribal.pl
%
%	A bot has a current_action, what it's currently doing
%	and a cur_plan, which is a list of actions it intends
%	to take.
%
%	On a larger level, the bot has a super_plan, which is
%	a single botvar that is it's overall goal, like 'go fishing'
%
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-set_prolog_flag(double_quotes,string).

:- use_module(hillpeople(weather)).
:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(navigation)).
:- use_module(hillpeople(actions)).
:- use_module(cogbot(cogrobot)).
:- use_module(hillpeople(events)).
:- use_module(hillpeople(actionselection)).
:- use_module(library(swicli)).

:- discontiguous be_tribal/3.

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Botvar support
%       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic tribal_dyn:current_action/2.

dump_action(Action) :-
	botvar_get(bot, dumpaction, "true"),
	write('A:'),
	write(Action),nl.
dump_action(_).


time_goal(Goal):-call(Goal).
%time_goal(Goal):-'format'('TIME GOAL: ~q~n',[Goal]),time(Goal).

%
% Set the botvar for the current bot action
%
set_current_action(Name, Action) :-
	dump_action(Action),
	with_output_to(string(S), writeq(Action)),
	retractall(tribal_dyn:current_action(Name, _)),
	assert(tribal_dyn:current_action(Name, S)).

% as for 'format'/2
set_current_action(Name, ActionFormat, Args) :-
	'format'(string(S), ActionFormat, Args),!,
	dump_action(S),
	retractall(tribal_dyn:current_action(Name, _)),
	assert(tribal_dyn:current_action(Name, S)).
set_current_action(_, ActionFormat, Args) :-
	dump_action("Illegal call to set_current_action"),
	'format'('Illegal: set_current_action/3 requires 2nd and 3rd arg as for format/2, you supplied ~w  ~w~n', [ActionFormat, Args]).

bv:hook_botvar_get(BotID, bot, 'currentAction', X) :-
	@(get_current_action(BotID, X), tribal).

get_current_action(BotID, X) :-
	botID(Name, BotID),
	tribal_dyn:current_action(Name, X),!.
get_current_action(_, "nothing").

bv:hook_botvar_set(_, bot, 'currentAction', _) :-
	'format'('the botvar \'currentAction\' is readonly~n', []).

bv:hook_botvar_key(_, bot, 'currentAction').

bv:hook_botvar_desc(_, bot, 'currentAction',
     "ReadOnly - The currentAction performed by the bot").

%%	%%%%%%%%

bv:hook_botvar_get(BotID, bot, amonbed, X) :-
	@(am_i_on_bed(BotID, X), tribal).

am_i_on_bed(BotID, yup) :-
	botID(Name, BotID),
	on_bed(Name),!.
am_i_on_bed(BotID, nope) :-
	botID(Name, BotID),
	\+ on_bed(Name),!.
am_i_on_bed(_, idunno).

bv:hook_botvar_set(_, bot, amonbed, _) :-
	'format'('the botvar \'amonbed\' is readonly~n', []).

bv:hook_botvar_key(_, bot, amonbed).

bv:hook_botvar_desc(_, bot, amonbed,
     "ReadOnly - yup if the bot is on the bed, nope otherwise").

%%	%%%%%%%%%%

be_tribal(Name) :-
	botID(Name, ID),
	register_listeners,
	set_current_bot(ID),
	sex(Name, Sex),
	age(Name, Age),
	botvar_set(bot, superplan, "wander"),
	be_tribal_loop(
	    _,
	    Name,
	    [
		sex(Sex),
		age(Age),
		cal(10.0),
		pro(10.0)
	    ]),
	'format'('~w be_tribal thread exiting~n', [Name]).

:-dynamic(be_tribal_status/3).

be_tribal_loop(Loc0, Name, Status0):-
      be_tribal_next(Loc0, Name, Status0),
       repeat,
         retract(be_tribal_status(Loc, Name, Status)),
          once(time_goal(be_tribal(Loc, Name, Status))),fail.

be_tribal_next(Loc, Name, Status):-
       retractall(be_tribal_status(_Loc, Name, _Status)),
       assert(be_tribal_status(Loc, Name, Status)).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	bail when the user asks us to
%
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
be_tribal(
    _Loc,
    Name,
    _Status) :-
	botvar_get(bot, byebye, "true"),
	botvar_set(bot, byebye, "false"),
	set_current_action(Name, "I am going byebye"),
	say_ref('I am going byebye', []),
	'format'('#################################~n~w is going byebye~n', [Name]).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  Termination actions
%                  actions that are needed to 'get out of' a superplan
%      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% get up when not tired
%
be_tribal(
    Loc,
    Name,
    Status) :-
	\+ (botvar_get(bot, superplan, X), X = "rest"),
	set_current_action(Name, "not resting, will test to see if on bed"),
	on_bed(Name),
	botcmd(stand),
	set_current_action(Name, "getting up from sleep"),
	be_tribal_next(Loc, Name, Status).


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
        set_current_action(Name, "teleporting to start location"),
	tribal_land(Loc),
	botcmd(teleport(Loc)),
	be_tribal_next(home, Name, [on_sim|Status]).

be_tribal(_,
	  Name,
	  Status) :-
	botvar_get(bot, knockitoff, "true"),
	botvar_set(bot, knockitoff, "false"),
	set_current_action(Name, "flushing plan at user request"),
	write('flushing plan per knockitoff directive'),nl,
	replace_plan(Status, [], NewStatus),
	be_tribal_next(home, Name, NewStatus).


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
	get_time(Time),
	sleep(3),
	be_tribal_next(home, Name, [requested_inventory(Time)|Status]).

%
%       continue to wait until we get our inventory
%
be_tribal(_,
	  Name,
	  Status) :-
	memberchk(requested_inventory(Time), Status),
	\+ has_inventory,
	get_time(CurTime),
	CurTime < Time + 30,
	set_current_action(Name, "waiting for inventory to arrive"),
	sleep(5),
	be_tribal_next(home, Name, Status).

%
%       Ask for it again every 30 sec
%
be_tribal(_,
	  Name,
	  Status) :-
	memberchk(requested_inventory(Time), Status),
	\+ has_inventory,
	get_time(CurTime),
	CurTime >= Time + 30,
	set_current_action(Name, "retrying getting starting inventory"),
	botcmd(touch('inventory_giver')),
	sleep(5),
	select(requested_inventory(_), Status,
	       requested_inventory(CurTime), NewStatus),
	be_tribal_next(home, Name, NewStatus).


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
	be_tribal_next(home, Name, [inited|Status]).



%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Test Wander Mode
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  In test_wander_mode they just wander from point to point
%
test_wander_mode :-
	botvar_get(bot, superplan, "wander").

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
	set_current_action(Name,
		   'too far from nearest waypoint, moving to~w',[WP]),
	say_format('too far from nearest waypoint, moving to~w',[WP]),
	botcmd(moveto(WP, 1), MoveStat),
	botcmd(waitpos(10, WP, 1), WaitStat),
	say_ref('Move', MoveStat),
	say_ref('Wait', WaitStat),
	be_tribal_next(WP, Name, Status).

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
	be_tribal_next(Start, Name, [cur_plan(Path) | Status]).


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
	replace_plan(Status, T, NewStatus),
	be_tribal_next(H, Name, NewStatus).

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
	set_current_action(Name, "cur_plan empty, removing it"),
	say_format('cur_plan empty, removing it', []),
	be_tribal_next(Loc, Name, NewStatus).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Rest
%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% if we're far from home, and not planning to go hom,
% plan to go home. This is so we don't keep adding more
% go home
%
be_tribal(
    _Loc,
    Name,
    Status) :-
	botvar_get(bot, superplan, "rest"),
	sleeps_at(Name, Home),
	name_to_location_ref(Home, Obj),
	distance_to(Obj, D),
	D > 8.0,
	nearest_waypoint(WPName, _),
	waypoint_path(WPName, Home, Path),
	\+ botvar_get(bot, goinghome, "true"),
	botvar_set(bot, goinghome, "true"),
	replace_plan(Status, Path, NewStatus),
	set_current_action(Name, 'Planning to go rest at ~w', [Home]),
	botcmd(say("Aborting my currentAction, heading home to rest")),
	be_tribal_next(WPName, Name, NewStatus).

%
% lay down
%
be_tribal(
    Loc,
    Name,
    Status) :-
	time_goal(botvar_get(bot, superplan, "rest")),
	sleeps_at(Name, Home),
	name_to_location_ref(Home, Obj),
	distance_to(Obj, D),
	D =< 8.0,
	\+ on_bed(Name),
	set_current_action(Name, "laying down to sleep"),
	botvar_set(bot, goinghome, "false"),
	time_goal(bed_for_name(Name, BedName)),
	'format'(string(S), 'sit ~w 1', [BedName]),
	time_goal(botcmd(S)),
	be_tribal_next(Loc, Name, Status).

on_bed(Name) :-
	gtrace_by_botvar(breakonbed),
	bed_for_name(Name, BedName),
	botID(Name, ID),
	wbot_sitting_on(ID, BedRef),
	cli_get(BedRef, [properties, name], BedNameO),!,
	cli_unify(BedName, BedNameO).

% TODO backup bed_for_name to provide a bed if they don't have one
% TODO what if somebody's already sleeping there?

%
% sleep
%
be_tribal(
    Loc,
    Name,
    Status) :-
	ignore(gtrace_by_botvar(gtsleeping)),
	botvar_get(bot, superplan , "rest"),
	on_bed(Name),
	set_current_action(Name, "Sleeping"),
	sleep(10),
	be_tribal_next(Loc, Name, Status).

%
% this makes home/2 det
%
sleeps_at(Name, Location) :-
	home(Name, Location),!.
sleeps_at(_, hut3) :- !.

%
% add something to status
%
be_tribal(
    Loc,
    Name,
    Status) :-
	memberchk(cur_plan([add(Atom)|_]), Status),
	be_tribal_next(Loc, Name, [Atom | Status]).

%
%  remove statuses with the remove action
%
be_tribal(
    Loc,
    Name,
    Status) :-
	memberchk(cur_plan([remove(Atom)|T]), Status),
	select(Atom, Status, NewStatus),
	replace_plan(NewStatus, T, NNStatus),
	be_tribal_next(Loc, Name, NNStatus).

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
    Name,
    status(
	_,
	_,
	_,
	Pro)) :-
    Pro < -4.0,
    botcmd(anim(die)),
    set_current_action(Name, "dieing of protein deficiency"),
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
	sleeps_at(Name, Home),
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
	sleeps_at(Name, Location),
	\+ sitting_on(Name, sleeping_mat),
	sit_on(Name, sleeping_mat),
	be_tribal_next(
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
	sleeps_at(Name, Location),
	sitting_on(Name, sleeping_mat),
	play_sound(Name, snore),
	basal_metabolism(Status, NewStatus, 30, 0.20),
	      % 20% because we're sleeping
	sleep(30),
	be_tribal_next(
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


%
%  fallback if you can't do anything else
%
be_tribal(
    Location,
    Name,
    Status) :-
    say_format('~w in trouble, no valid action at ~w~n~w~n',
	   [Name, Location, Status]),
    sleep(10),
    gtrace_by_botvar(breakontrouble),
    be_tribal_next(Location, Name, Status).


gtrace_by_botvar(Var) :-
    \+ botvar_get(bot, Var, "true"),
    !.
gtrace_by_botvar(_) :-
       gtrace.

%
% given a status, replace the cur_plan(_) with
% cur_plan(Plan), binding NewStatus. If cur_plan(_)
% doesn't exist, just add the new plan
%
replace_plan(Status, Plan, NewStatus) :-
	memberchk(cur_plan(_), Status),
	select(cur_plan(_), Status, cur_plan(Plan) , NewStatus).
replace_plan(Status, Plan, [cur_plan(Plan) | Status]) :-
	\+ memberchk(cur_plan(_), Status).

