:- module(actionselection, [
			    action/1,
			    action_possible/2
			   ]).

:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(weather)).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Base action names
%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(sleep).
action(rest).
action(play).
action(hunt).
action(corn).   % tend corn
action(berry).   % pick berries
action(fish).
action(cook).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Closed form possibility
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%  action_possible(+Name, +Action)
%
%  unifies if this action is possible for this person today

% if this is forbidden it's not possible (convenience)
action_possible(Name, Action) :-
	action_forbidden(Name, Action),
	!,fail.

%
% if some other action is demanded
% this is impossible
action_possible(Name, Action) :-
	action_demanded(Name, OtherAction),
	OtherAction \= Action,
	!,fail.

% anybody can rest
action_possible(_, rest).

%
% everyone must sleep at night
action_demanded(_, sleep) :-
	think(is_night).

%  cultural restriction, women don't hunt
action_forbidden(Name, hunt) :-
	sex(Name, f).

% cultural restriction, adult men don't pick berries
action_forbidden(Name, berry) :-
	sex(Name, m),
	age(Name, Age),
	Age > 12.

%
%  children don't do some forms of work
%
action_forbidden(Name, Action) :-
	age(Name, Age),
	Age < 13,
	memberchk(Action, [hunt, corn]).

%
%  aged people don't do many forms of work
%
action_forbidden(Name, Action) :-
	age(Name, Age),
	Age > 55,
	memberchk(Action, [hunt, corn, fish, berry]).

%
% we have to think berries are in season
%
action_forbidden(_, berry) :-
	\+ think(berries_in_season).

%
% can't hunt in snow
action_forbidden(_, hunt)  :- think(is_snowing).
action_forbidden(_, corn)  :- think(is_snowing).
action_forbidden(_, berry) :- think(is_snowing).
action_forbidden(_, fish)  :- think(is_snowing).

action_forbidden(_, hunt)  :- think(is_raining).
action_forbidden(_, corn)  :- think(is_raining).
action_forbidden(_, play)  :- think(is_raining).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Mental models
%
%           for now we're omniscient
%
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

think(berries_in_season) :- berries_in_season.
think(is_snowing) :- is_snowing.
think(is_raining) :- is_raining.
think(is_night) :- is_night.

