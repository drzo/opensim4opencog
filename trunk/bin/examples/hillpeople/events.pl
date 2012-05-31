:- module(events, [
		   register_listeners/0,
		   handle_im/3
		  ]).

:- use_module(cogbot(cogrobot)).

:- dynamic listeners_registered/0.

%
%  Register events we're interested in
%  for the current bot
%
register_listeners :- listeners_registered, !.
register_listeners :-
	botget(['Self'], AM),
	cli_add_event_handler(AM, 'IM',
			      events:handle_im(_Origin,
					_Object,
					_InstantMessageEventArgs)),
	assert(listeners_registered).

handle_im(Origin, Object, IMEventArgs) :-
	cli_to_str(Origin, OriginStr),
	cli_to_str(Object, ObjectStr),
	cli_to_str(IMEventArgs, IMEventArgsStr),
	writeq('Origin:'),
	writeq(OriginStr),
	writeq(' Object:'),
	writeq(ObjectStr),
	writeq(' IMEventArgs:'),
	writeq(IMEventArgsStr).


