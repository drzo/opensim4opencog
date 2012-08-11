%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file for cogbot support for Cyc Query
%   bundle file that causes config page to get the
%   uri of the cyc server
%

bundle_for(cyc_query, cyc_query_group).

bundle(cyc_query_group, group, 'Cyc Query support Group', []).

deps(cyc_query_group, find_cyc_server).

bundle(find_cyc_server,
       config,
       'Configure The Cyc Client',
       Args) :-
	find_cyc_server_question(X),
	Args = [inc(X)].

find_cyc_server_question(X) :-
	    X = [
	    label(for=cycuri, 'URI of your Cyc server:'),
	    input([
		name=cycuri,
		id=cycuri,
		type=text,
		value='http://127.0.0.1:3602/cgi-bin/cyccgi/cg?'
		  ],[])
	     ].


