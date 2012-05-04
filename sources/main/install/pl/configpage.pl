:-module(configpage, [
		  ]).
%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
% This is the configuration page.
%   bundles of type config are accumulated, and all the
%   questions needed from the user are asked.
%
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(logger).
:- use_module(bundle).
:- use_module(execute, [plan_config_entries/2]).
:- use_module(layout).

:- http_handler(root(config) ,
		config_page_handler,
		[id(config)]).

config_page_handler(_Request) :-
	reply_html_page(
	    cogbot_web_style,
	    html([title('Cogbot Configuration Information')]),
	    configpage:config_page_content).

config_page_content -->
	{
	    bundle_install_plan(Plan),
	    plan_config_entries(Plan, ConfigHTML)
	},
	html([
	    h2('Configuration'),
	    form([
		action='do',
		name=config
		 ],[
		div(class=config_container, [
					     \ConfigHTML
					    ]),
		\action_bar(configpage:config_buttons)
		   ])
	     ]).

config_buttons -->
	html([
	    \wizard_button(reset, 'Cancel The Installation'),
	    \wizard_button_default_submit('Install Cogbot')
	     ]).

config_entry(Title, Stuff) -->
	html([div([class=configentrybox],[
		  h4(Title),
		  div(class=configentry, \Stuff)
		  ])
	     ]).

