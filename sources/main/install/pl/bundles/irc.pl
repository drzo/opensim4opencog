%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file for cogbot support for IRC
%   This component is just to grab the irc server config bundle
%

bundle_for(irc, irc_query_group).

bundle_for(irc_query, irc_query_group).

bundle(irc_query_group, group, 'IRC Group', []).

deps(irc_query_group, find_irc_server).

bundle(find_irc_server,
       config,
       'Configure The IRC Client',
       Args) :-
	find_irc_server_question(X),
	Args = [inc(X)].

find_irc_server_question(X) :-
	    X = [
	    p([
	       label(for=ircuri, 'irc server (eg. irc.freenode.net:6667):'),
	       input([
		   name=ircuri,
		   id=ircuri,
		   type=text,
		   value=''],[])]),
	    p([
	       label(for=ircchannel, 'irc channel to join (eg. #cogbot):'),
	       input([
		   name=ircchannel,
		   id=ircchannel,
		   type=text,
		    value=''], [])]),
	    p([
	       label(for=botnick, 'bot nickname on IRC:'),
	       input([
		   name=botnick,
		   id=botnick,
		   type=text,
		   value=''], [])]),
	    p([
		input([
		    name=relaysimtoirc,
		    type=radio,
		    value=true,
		    checked],'Relay sim chat to IRC')]),
	    p([
		input([
		    name=relaysimtoirc,
		    type=radio,
		    value=false],'Relay sim chat to IRC')])
	     ].


