:- module(coginstall, [
	start/0,
        autostart/0,
	server_port/1
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).

:- use_module(logger).

% force load of page modules
:- use_module(startpage).
:- use_module(componentspage).
:- use_module(showplan).
:- use_module(coglicense).
:- use_module(configpage).
:- use_module(do).
:- use_module(pages, [reset_installer/0]).

:- dynamic started/0.

server_port(8070).   % change this number to use a different port

% http://www.swi-prolog.org/howto/http/HTTPFile.html

% an image is /f/fluffybunny.png, not /f/img/fluffybunny.png
%
http:location(files_uri, '/static', []).

user:file_search_path(document_root, './files').
% static file handlers. js, images, etc. served from ./f
:- http_handler(root(f), serve_files_in_directory(document_root), [prefix]).

%
% redirect root to start page
:- http_handler(root(reset) , coginstall:redir(
					 start,
					 pages:reset_install_request),
		[id(reset)]).

:- http_handler(root(.) , redir_to_start,
		[id(startroot)]).

redir_to_start(Request) :-
	http_redirect(moved_temporary, location_by_id(start), Request).

%%	%%%%%%%%%%%%%%%%%%%% STYLE CONTROL %%%%%%%%%%%%%%%%%%%%%%

:- multifile
	user:head//2,
	user:body//2.

user:head(cogbot_web_style , Head) -->
	html([\Head,
	      \html_head
	     ]).

user:body(cogbot_web_style , Body) -->
	html([
	    div(id=surround, [
		        div(id=content, [
			    \nav,
			    \Body
			])
		    ])
	     ]).

user:head(cogbot_web_style_refresh , Head) -->
	html([\Head,
	      \html_head,
	      meta([http-equiv=refresh, content=15], [])
	     ]).

user:body(cogbot_web_style_refresh , Body) -->
	html([
	    div(id=surround, [
		        div(id=content, [
			    \nav,
			    \Body
			])
		    ])
	     ]).

nav --> html([
	    div(id=nav, [
	        ul([], [
		    li([], ['One']),
		    li([], ['Two']),
		    li([], ['Three'])
	        ])
	    ])
       ]).

html_head -->
	html([
		meta(charset = 'UTF-8'),
		meta([
		     name='Keywords',
		     content='cogbot, virtual robot, opensim, Second Life, virtual world, artificial intelligence, bot'], []),
		meta([
		    name='Description',
		    content='Installer for Cogbot virtual robot'], []),
		link([
			rel = stylesheet,
			type = 'text/css',
			href = 'f/style.css'
		]),
		script(src = 'f/jquery-1.7.1.min.js', []),
		script(src = 'f/components.js', [])
	]).

%%	%%%%%%%%%%%%%%%%%%%%  SERVER CONTROL  %%%%%%%%%%%%%%%%%%%

start:-
	started,!,
	server_port(Port),
	format(user_error, 'Already running - browse http://127.0.0.1:~w/\n', [Port]).

start:-
	format(user_error, 'Starting Cogbot Installer\n', []),
	server_port(Port),
	http_server(http_dispatch, [port(Port)]),
	assert(started).

autostart :-
       debug(message),
       start,
       server_port(Port),
       format(string(S), 'http://127.0.0.1:~w/' , [Port]),
       www_open_url(S).

% TODO when it's no longer annoying, call autostart as directive

stop_server :-
	catch(thread_exit(plan_runner_thread), _,
	      format(user_error, 'Cannot halt plan_runner_thread~n', [])),
	server_port(Port),
	http_stop_server(Port, []),
	format(user_error, 'Server halted on port ~n', [Port]),
	reset_installer.
