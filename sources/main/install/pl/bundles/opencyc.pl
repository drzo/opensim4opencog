%
%   bundle file that causes config page to get the
%   params for the cyc world server
%

bundle_for(opencyc, opencyc_group).

bundle(opencyc_group, group, 'Cyc World Group', []).

deps(opencyc_group, configure_cyc_world).
deps(opencyc_group, find_cyc_server).

bundle(configure_cyc_world,
       config,
       'Configure The Cyc Client',
       Args) :-
	configure_cyc_world_question(X),
	Args = [inc(X)].

configure_cyc_world_question(X) :-
	    X = [
	 p([
	    input([name=usecycworld, type=hidden, value=true], []),
	    input([
		name=clearcycbetweensessions,
		id=clearcycbetweensessionstrue,
		type=radio,
		checked,
		value=true], 'Clear the Cyc KB between sessions')
	   ]),
	 p([
	    input([
		name=clearcycbetweensessions,
		id=clearcycbetweensessionsfalse,
		type=checkbox,
		value=false
		  ],'Retain Cyc KB between sessions')
	   ])].


