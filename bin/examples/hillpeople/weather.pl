:- module(weather, [
		    test_weather/0,
		    is_night/0,
		    berries_in_season/0,
		    is_snowing/0,
		    is_raining/0,
		    hill_date/1
		    ]).

% The year is 12 months of 28 days, The year starts
% April 1
%
%  by week, percentages (0 -100)
%  if negative it's probability of
%  snow, if positive probability of rain
%  if Rain-Snow then probabilty of each
climate([
    25-7, 45-0, 50-0, 35-0,
    20-0, 20-0, 20-0, 10-0,
    15-0, 10-0, 3-0, 5-0,   % June
    10-0, 15-0, 3-0, 3-0,
    3-0, 3-0, 2-0, 5-0,
    25-0, 25-0, 15-0, 35-0,  %september
    20-0, 15-0, 2-0, 8-20,
    2-25, 7-35, 3-55, 1-70,
    0-55, 0-80, 0-80, 0-90,   % December
    0-50, 0-40, 0-40, 0-65,
    0-74, 7-40, 20-25, 25-45,
    25-20, 15-40, 25-20, 25-20   % march

	]).

%
%  one_days_weather
%
one_days_weather(Rain, _, rain) :-
	RainProb is Rain / 100.0,
	random(X),
	X < RainProb,!.
one_days_weather(_, Snow, snow) :-
	SnowProb is Snow / 100.0,
	random(X),
	X < SnowProb,!.
one_days_weather(_, _, clear) :- !.


%
%  weather
%
weather(Climate, Weather) :-
	weather0(0, Climate, Weather).

weather0(_, [], []).
weather0(N, [Rain-Snow|T], [TodaysWeather|WeatherTail]) :-
	N < 7,
	one_days_weather(Rain, Snow, TodaysWeather),
	ND is N + 1,
	weather0(ND, [Rain-Snow|T], WeatherTail).
weather0(7, [_|T], Weather) :-
	weather0(0, T, Weather).

month_names([
    april,
    may,
    june,
    july,
    august,
    september,
    october,
    november,
    december,
    january,
    february,
    march
	    ]).

print_weather(28, [], []) :- !.
print_weather(28, [HMonth|TMonth], W) :- !,
	format('====== ~w =========~n', [HMonth]),
	print_weather(0, TMonth, W).
print_weather(N, Months, [Today|W]) :-
	6 =:= N mod 7,!,
	format('| ~w |~n', [Today]),
	NN is N + 1,
	print_weather(NN, Months, W).
print_weather(N, Months, [Today|W]) :-
	format('| ~w ', [Today]),
	NN is N + 1,
	print_weather(NN, Months, W).

test_weather :-
	climate(Climate),
	weather(Climate, W),
	month_names(Months),
	print_weather(28,
		      Months,
		      W).

% TODO
is_night :- fail.

%  OOoops !  Bot has to figure out date.
%
% TODO
% note that this is just 'in season', not actually ripe
berries_in_season :-
	hill_date(Date),
	berries_in_season(Date).

berries_in_season(Date) :-
	Month =:= Date / 28,
	memberchk(Month, [5,6,7]).  % berries in season in june, july, aug

% TODO
is_snowing :- fail.

% TODO
is_raining :- fail.


% TODO
%
% hill_date(?Date) unifies if the world time is
% the int value (0-335) of the day in world
%
hill_date(180).


