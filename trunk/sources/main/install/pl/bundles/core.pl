%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file for cogbot core
%
%   also defines a lot of common bundles
%
%

bundle_for(core, coregroup).

bundle(coregroup, group, 'Core Component', []).
deps(coregroup, cogbot_license).
deps(coregroup, dotnet32, 32).
deps(coregroup, dotnet64, 64).
deps(coregroup, core).
deps(coregroup, desktop).
deps(coregroup, conclude).
before(dotnet32, core).
before(dotnet64, core).
before(core, fixbotconfig).
deps(core, fixbotconfig).
deps(fixbotconfig, getgridcreds).
before(fixbotconfig, desktop).

bundle(cogbot_license,
       license,
       'Cogbot New BSD License',
       [url('/f/cogbotlicense.html')]).

bundle(core, files,
       'Cogbot Core',
       [from(temp('cogbot-core.zip')),
	url(logicmoo('cogbot-core.zip')),
	to(program(.))]).

bundle(fixbotconfig,
       goal,
       'Edit botconfig.xml',
       [goal(bundle:edit_botconfig)]).

deps(fixbotconfig, ask_filelocs).

bundle(getgridcreds,
       config,
       'Credentials For Cogbot Login',
       [inc([
	    p([
		label([for=botname], 'Name of the bot account:'),
		input([type=text, name=botname, id=botname], [])
	      ]),
	    p([
		label([for=botpassword], 'Password for the bot account:'),
		input([type=text, name=botpassword, id=botpassword], [])
	      ]),
	    p([
		label([for=loginuri], 'Login URI:'),
		input([type=text, name=loginuri, id=loginuri,
		    value='https://login.agni.lindenlab.com/cgi-bin/login.cgi'],
		  [])
	      ])
	    ])]).


%
%  writes the include file for botconfig
%
edit_botconfig(Config) :-
	execute:id_abs_path(program('bin\\startupLisp.lisp'), Config, Loc),
	setup_call_cleanup(
	    open(Loc, write, Out),
	    (	write(Out, '(setj installer_data \'('),
	        write_config_stream(Out, Config),
		write(Out, '))'),
		nl(Out),
		write(Out, '(setj installed_components \'('),
		write_components(Out),
		write(Out, '))')
	    ),
	    close(Out)
	).

write_config_stream(_, []).
write_config_stream(Out, [HK=HV|T]) :-
	uri_encoded(path, HK, UHK),
	uri_encoded(path, HV, UHV),
	format(Out, ' ("~w" "~w") ', [UHK, UHV]),
	write_config_stream(Out, T).

:- use_module(component, [component_will_install/1]).

write_components(Out) :-
	setof(Name, component_will_install(Name), ComponentList),
	format(user_error, 'ComponentList ~w~n', [ComponentList]),
	write_component_list(Out, ComponentList).

write_component_list(_Out, []).
write_component_list(Out, [H|T]) :-
	format(Out, ' "~w" ', [H]),
	write_component_list(Out, T).


