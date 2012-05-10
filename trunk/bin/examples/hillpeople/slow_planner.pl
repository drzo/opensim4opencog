:-module(slow_planner, [
			think_slowly/1
		       ]).

%
%  This is a 'planner' that works in a manner like a store
%  that keeps a certain level of inventory on hand.
%
%  The bot wants to avoid certain discomforts. For example,
%  the bot wants to keep a fire going. If the fire goes out
%  relighting it is difficult.
%  This means the bot
%  keeps a certain amount of firewood F on hand.
%
%

think_slowly(Name) :-
	format('think_slowly ~w~n' , [Name]),nl.
