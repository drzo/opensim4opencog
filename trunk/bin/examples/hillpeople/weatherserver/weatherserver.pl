:- module(weatherserver, [
			  start/1
			 ]).
% Given that this file is called weatherserver.pl,
% start the server by:
%
% swipl -g "[weatherserver], start(19888)."
%
% Usage:
%
% http://localhost:19888/weather?utime=
%
% utime is unix timestamp.
%
% day is the day of the 336 day year, starting from 0 being April 1
% of year 0, and fractional day. The day starts at sunrise.
% seed is an arbitrary integer that controls which weather track
% to return. It is modded into 0-255
%
% returns a single line CSV list of values. boolean values
% are 0 for false and 1 for true. Values may be added to end,
% and there should be a default if the list is short
%
% valid - if false, couldn't return a valid answer
% sun  - current sun angle, in 24 hours
% temp  - current centigrade temperature
% fallen_snow  - amount of fallen snow
% is_snowing  - it's currently snowing
% is_raining  - it's currently raining
%
%
%
% http://localhost:8000/sum?summands=blah
% returns <error>ERROR</error>

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(climate).
:- use_module(windlight).

:- http_handler('/windlight', windlight_handler, []).

start(Port) :-
    http_server(http_dispatch, [port(Port), workers(1)]),
    thread_get_message(_),
    halt.


windlight_handler(Request) :-
    http_parameters(Request, [utime(UTime, [integer, default(0)])]),
    format('Content-type: text/plain~n~n'),
    at_utime_windlight_str_is(UTime, Windlight),
    format('~s', [Windlight]).


weather_handler(Request) :-
    http_parameters(Request, [day(Day, [default(0)])]),
    catch(
        (
            weather_for_day(
		Day,
		weather(
		    Sun,
		    Temperature,
		    SnowAccum,
		    Snowing,
		    Raining)),
            with_output_to(
		atom(Output),
		format("~f,~f,~f,~d,~d",
		       [Sun, Temperature, SnowAccum, Snowing, Raining]))
        ),
        _,
        Output = '0,1.0,1.0,0.0,0,0'
    ),
    format('Content-type: text/css\r\n\r\n~w', [Output]).

temperature(_, 25).

weather_for_day(Day,
	weather(Sun, Temperature, SnowAccum, Snowing, Raining)) :-
	Date =:= floor(Day),
	Sun =:= 24.0 * (Day rem 1.0),
	temperature(Date, Temperature),
	snow_accumulation(Date, SnowAccum),
	is_snowing(Date, Snowing),
	is_raining(Date, Raining).

