%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%%%%%%%%%%%%%%%%%%%%%%%%% General bundle entries
% not connected to any one component
%

%%%%%%%%%% general bundles %%%%%%%%%%

bundle(dotnet32,
       group,
       'Make sure .Net 3.5 framework for Intel 32 bit architecture is installed',
       [url('http://www.microsoft.com/download/en/details.aspx?id=21')]).

bundle(dotnet64,
       group,
       'Make sure .Net 3.5 framework for Intel 64 bit architecture is installed',
       [url('http://www.microsoft.com/download/en/details.aspx?id=21')]).

bundle(desktop,
       desktop,
       'Add Desktop Icon and Start Menu',
       [path(program('bin\\Cogbot.exe')), icon(program('bin\\cogbot.ico'))]).

bundle(conclude,
       group,
       'marker at end of install',
       [url('/thanks')]).

bundle(licensepage,
       licensepage,
       'Display licenses for user to accept',
       []).

bundle(configpage,
       configpage,
       'Collect configuration information',
       []).

bundle(ask_filelocs,
       config,
       'Locations To Install Components',
       [inc([
	    p([
		label(for=temp,
 'Temporary directory to download files into for installation. Tiles already in this directory will be used instead of downloading if possible. Cogbot uses many large auxiliary files, and is actively being developed. If you retain the contents of this directory you can reduce the pain of updating Cogbot:'),
		input(
		    [type=text, name=temp, id=temp, size=80, value=TempDir], [])
	      ]),
	    p([
		label(for=install, 'Directory to install Cogbot:'),
		input([type=text, name=install, id=install, size=80, value=Install], [])
	      ])
	    ])]) :-
        win_folder(appdata, AppDir),
	prolog_to_os_filename(AppDir, OSAppDir),
	format(atom(TempDir), '~w\\Cogbot\\InstallerFiles\\', [OSAppDir]),
	(   architecture(64) ->
	    getenv('programW6432', InstallBase) ;
	    getenv('programFiles(x86)', InstallBase)),
	atomic_concat(InstallBase, '\\Cogbot\\', Install).

% TODO make this get the data from the OS some day

% TODO implement inc() in bundle(_, config, _, _)
% TODO convert the directory paths to prolog form and make sure the directories
% exist (make them if needed)

before(licensepage, configpage).
before(licensepage, ask_filelocs).
before(ask_filelocs, configpage).
before(configpage, desktop).
before(desktop, conclude).
before(X, desktop) :-
	bundle(X, Type, _, _),
	memberchk(Type, [install, files, url, overlay]).

before(ask_filelocs, X) :-
	bundle(X, Type, _, _),
	memberchk(Type, [install, files, url, overlay]).

before(licensepage, X) :-
	bundle(X, Type, _, _),
	memberchk(Type, [install, files, url, overlay]).

before(configpage, X) :-
	bundle(X, Type, _, _),
	memberchk(Type, [install, files, url, overlay]).

deps(X, ask_filelocs) :-
	bundle(X, Type, _, _),
	memberchk(Type, [install, files, url, overlay]).

deps(X, licensepage) :-
	bundle(X, license, _, _).

deps(X, configpage) :-
	bundle(X, config, _, _).

before(X, licensepage) :-
	bundle(X, license, _, _).

before(X, configpage) :-
	bundle(X, config, _, _).

deps(A, B) :-
	architecture(Arch),
	deps(A, B, Arch).



