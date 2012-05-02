:- module(execute,
	  [
	   do_plan_in_thread/2,
	   plan_license_entries/2,
	   plan_config_entries/2,
	   done_so_far/1,
	   id_abs_path/3
	  ]).

%
%   Execute the plan
%

:- use_module(library(http/http_open)).
:- use_module(bundle).
:- use_module(coginstall, [server_port/1]).
:- use_module(logger).

% this is the html
:- dynamic
	current_progress_data/1.

:- initialization
	mutex_create(progress).

% TODO make this handle errors gracefully
do_plan_in_thread(Plan, Config) :-
	debugout('Executing plan ~w with Config ~w~n', [Plan, Config]),
	make_directories_exist(Config),
	retractall(current_progress_data(_)),
	assert(current_progress_data([])),
	do_plan_or_dont(Plan, Config),
	thread_create(do_plan_or_dont(Plan, Config),
		      _,
		      [
		       alias(plan_execution_thread),
		       at_exit(execute:execution_done),
		       detached(true)]).

execution_done :-
	append_progress(finished).

done_so_far(NamesDone) :-
	with_mutex(progress, current_progress_data(NamesDone)).

append_progress(Item) :-
	debugout('Appending ~w to done list~n', [Item]),
	with_mutex(progress, append_progress_core(Item)).

% famulus for append_progress - nobody else call
append_progress_core(Item) :-
	current_progress_data(SoFar),
	append(SoFar, [Item], New),
	retractall(current_progress_data(_)),
	assert(current_progress_data(New)).

do_plan_or_dont(Plan, Config) :-
	attach_console,
	do_plan(Plan, Config),!.

do_plan_or_dont(Plan, _) :-
	debugout('Plan failed to execute~w~n', [Plan]),!.

do_plan([], _).

do_plan([Name|T], Config) :-
	bundle(Name, Type, _, _),
	memberchk(Type, [group, license, licensepage, config, configpage]),!,
	do_plan(T,  Config).

do_plan([Name|T],  Config) :-
	bundle(Name, files, _, Args),
	memberchk(from(From), Args),
	memberchk(to(To), Args),
	id_abs_path(From, Config, FromAbs),
	id_abs_path(To, Config, ToAbs),
	exists_file(FromAbs),!,
	file_unzip(FromAbs, ToAbs),   % blocks
	append_progress(Name),
	do_plan(T,  Config).

do_plan([Name|T],  Config) :-
	bundle(Name, files, _, Args),
	memberchk(from(From), Args),
	id_abs_path(From, Config, FromAbs),
	\+ exists_file(FromAbs),!,
	memberchk(url(URL), Args),
	id_abs_path(URL, Config, RealURL),
	file_download(RealURL, FromAbs),   % blocks
	append_progress(Name),
	do_plan([Name|T],  Config).

do_plan([Name|T],  Config) :-
	bundle(Name, page, _, Args),!,
	memberchk(url(Page), Args),
	www_open_url(Page),
	append_progress(Name),
	do_plan(T,  Config).

do_plan([Name|T], Config) :-
	bundle(Name, goal, _, Args),!,
	memberchk(goal(G), Args),
	(   call(G, Config)
	;   debugout('goal ~w in bundle ~w has failed~n', [G, Name])
	),
	append_progress(Name),
	do_plan(T, Config).

do_plan([Name|T], Config) :-
	bundle(Name, desktop, _, _Args),!,
	memberchk(path(ExePath), Args),
	memberchk(icon(IconPath), Args),
	id_abs_path(ExePath, Config, PLExe),
	id_abs_path(IconPath, Config, PLIcon),!,
	working_directory(CWD, CWD),
	prolog_to_os_filename(CWD, WindowsCWD),
	prolog_to_os_filename(PLExe, WinExe),
	prolog_to_os_filename(PLIcon, WinIcon),
	id_abs_path(program('\\bin'), Config, PLBin),
	prolog_to_os_filename(PLBin, WinBin),
	format(string(S), '~w\\cogbotshortcut.vbs "~w" "~w" "~w"~n',
	       [WindowsCWD, WinExe, WinIcon, WinBin]),
	string_to_atom(S, A),
	(
	   shell(A, Status),
	   debugout('Shell completes with status ~w~n', [Status])
	;
	   thread_signal(main, format(user_error,
				      'Desktop Shell failed  ~w~n', [A]))
	),
	append_progress(Name),
	do_plan(T, Config).


file_download(URL, File) :-
	setup_call_cleanup(
	    open(File, write, Out, [type(binary)]),
	    (
                http_open(URL, In, []),
		copy_stream_data(In, Out)
	    ),
	    close(In)).

%
%  unzip the
file_unzip(From, To) :-
	prolog_to_os_filename(From, FromWindows),
	prolog_to_os_filename(To, ToWindows),
	working_directory(CWD, CWD),
	prolog_to_os_filename(CWD, WindowsCWD),

	format(string(S), '~w\\cogzip.bat "~w" "~w"~n',
	       [WindowsCWD, FromWindows, ToWindows]),
	thread_signal(main, format(user_error, '~s~n', [S])),
	string_to_atom(S, A),
	(
	   shell(A, Status),
	   debugout('Shell completes with status ~w~n', [Status])
	;
	   thread_signal(main, format(user_error, 'Shell failed  ~w~n', [A]))
	).


%%	%%%%%%%%%%%%%%%%% Convert symbolic to absolute paths
%
%	converts a symbolic to an absolute path in the install
%	These are path names in the os native
%	typical use
%
%bundle(core, files,
%       'Cogbot Core',
%       [from(temp('cogbot-core.zip')),
%	url(logicmoo('cogbot-core.zip')),
%	to(program(unzip))]).
%
%	known symbols
%	temp(X)  X is a relative path not starting with \ from the
%	         Cogbot Install Files directory
%
%	program(X) X is a relative path from the program install
%	           directory  (eg. C:\Program Files\Cogbot\
%	           X should not start with \
%
%	logicmoo(X) X is a relative path from the uri where
%	            the file can be found.
%
%	If you add to this, note that it needs to handle being
%	backtracked into
%

id_abs_path(program(.), Config, NoTrail) :-
	memberchk(install=Loc, Config),
	prolog_to_os_filename(PLLoc, Loc),
	trailing_slash(NoTrail, PLLoc).

id_abs_path(temp(.), Config, NoTrail) :-
	memberchk(temp=Loc, Config),
	prolog_to_os_filename(PLLoc, Loc),
	trailing_slash(NoTrail, PLLoc).

id_abs_path(prolog(.), Config, NoTrail) :-
	memberchk(prolog_location=Loc, Config),
	prolog_to_os_filename(PLLoc, Loc),
	trailing_slash(NoTrail, PLLoc).

id_abs_path(Symbol, _Config, PLSymbol) :-
	atomic(Symbol),
	prolog_to_os_filename(PLSymbol, Symbol).

id_abs_path(temp(Symbol), Config, PLOut) :-
	memberchk(temp=Loc, Config),
	trailing_backslash(Loc, NLoc),
	atom_concat(NLoc, Symbol, Out),
	prolog_to_os_filename(PLOut, Out).

id_abs_path(program(Symbol), Config, PLOut) :-
	memberchk(install=Loc, Config),
	trailing_backslash(Loc, NLoc),
	atom_concat(NLoc, Symbol, Out),
	prolog_to_os_filename(PLOut, Out).

id_abs_path(prolog(Symbol), Config, PLOut) :-
	memberchk(prolog_location=Loc, Config),
	trailing_backslash(Loc, NLoc),
	atom_concat(NLoc, Symbol, Out),
	prolog_to_os_filename(PLOut, Out).

id_abs_path(logicmoo(URI), _Config, URLOut) :-
	atom_concat('http://cogbot.logicmoo.com/install/cogbot/',
		    URI, URLOut).

id_abs_path(Symbol , _, _) :-
	thread_signal(main,
             debugout('Cannot create path from ~w~n', [Symbol])),
	!,fail.

trailing_slash(Path, Path) :-
	atom_concat(_, '/', Path).

trailing_slash(Path, PathEndingInSlash) :-
	atom_concat(Path, '/', PathEndingInSlash).

trailing_backslash(Path, Path) :-
	atom_concat(_, '\\', Path).

trailing_backslash(Path, PathEndingInSlash) :-
	atom_concat(Path, '\\', PathEndingInSlash).

make_directories_exist(Config) :-
	make_directories_exist(Config, [temp, program]).

make_directories_exist(_, []).
make_directories_exist(Config, [H|T]) :-
	Symbol =.. [H, ''],
	id_abs_path(Symbol, Config, Path),
	make_directory_path(Path),
	make_directories_exist(Config, T).

%%	%%%%%%%%%%%%%%%%%%%%%%%% Extract License Info  %%%%%%%%%%
%

%   Given a plan, make the corresponding licenseHTML
%  plan_license_entries(+Plan, -LicenseHTML)
%
plan_license_entries([], []).

plan_license_entries([Name|_] , []) :-
	bundle(Name, licensepage, _, _).

plan_license_entries([Name|T] ,
		     [\license_entry(Title, URL)| LicenseHTML]) :-
	bundle(Name, license, Title, Args),
	memberchk(url(URL), Args),
	plan_license_entries(T, LicenseHTML).

plan_license_entries([Name|T] , LicenseHTML) :-
	bundle(Name, Type, _, _),
	Type \= license,
	Type \= licensepage,
	plan_config_entries(T, LicenseHTML).

%%	%%%%%%%%%%%%%%%%%%%%%%%% Extract Config Info  %%%%%%%%%%
%

%   Given a plan, make the corresponding ConfigHTML
%  plan_config_entries(+Plan, -ConfigHTML)
%
plan_config_entries([], []).

plan_config_entries([Name|_] , []) :-
	bundle(Name, configpage, _, _).

plan_config_entries([Name|T] ,
		     [\config_entry(Title, html(Stuff))| ConfigHTML]) :-
	bundle(Name, config, Title, Args),
	(
	    memberchk(inc(Stuff), Args)
	;
	    Stuff = [p(blink('Missing inc in bundle'))]
	),
	plan_config_entries(T, ConfigHTML).

plan_config_entries([Name|T] , ConfigHTML) :-
	bundle(Name, Type, _, _),
	Type \= config,
	Type \= configpage,
	plan_config_entries(T, ConfigHTML).

debugout(Format, Args) :-
	thread_self(main),
	format(user_error, Format, Args).

debugout(Format, Args) :-
	thread_signal(main, format(user_error, Format, Args)).

% TODO make this handle errors gracefully
debug_exec(Plan, Config) :-
	debugout('Executing plan ~w with Config ~w~n', [Plan, Config]),
	make_directories_exist(Config),
	retractall(current_progress_data(_)),
	assert(current_progress_data([])),
	do_plan_or_dont(Plan, Config).

dbx :-
	debug_exec(
	    [cogbot_license,licensepage,ask_filelocs,getgridcreds,
	     configpage,core,fixbotconfig,desktop,conclude,coregroup],
       [temp='C:\\Users\\Annie\\AppData\\Roaming\\Cogbot\\InstallerFiles\\',
install='C:\\Program Files\\Cogbot\\',botname='',botpassword='',
loginuri='https://login.agni.lindenlab.com/cgi-bin/login.cgi']).

db_id(Symbol, PLOut) :-
    id_abs_path(Symbol,
    [temp='C:\\Users\\Annie\\AppData\\Roaming\\Cogbot\\InstallerFiles\\',
install='C:\\Program Files\\Cogbot\\',botname='',botpassword='',
loginuri='https://login.agni.lindenlab.com/cgi-bin/login.cgi'],
		PLOut).
