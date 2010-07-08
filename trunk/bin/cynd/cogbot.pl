

:-dynamic(cogbot_pl_dir/1).
:-dynamic(cogbot_temp_dir/1).
:-dynamic(cogbot_aiml_dir/1).
:-dynamic(filepath_hints/1).

extern_pathname(FullPah,Hint):-current_stream(FullPah,read,_),join_pathnames(_,Hint,FullPah),!.

expand_file_name_each(A,B):-expand_file_name(A,BB),!,member(B,BB).

cb_file_search_path(X,Y):-nonvar(Y),!,throw(cb_file_search_path(X,Y)).
cb_file_search_path(X,Z):-nonvar(X),expand_file_name_each(X,Y),absolute_file_name(Y,Z).
cb_file_search_path(X,Z):-file_search_path(X,Y),expand_file_name_each(Y,Z).

expand_aliases(X,Z):-nonvar(X),expand_file_name_each(X,Y),absolute_file_name(Y,Z).
expand_aliases(X,Z):-file_search_path(X,Y),expand_file_name_each(Y,Z).


expand_aliases_locally(X,Z):-nonvar(X),expand_file_name_each(X,Z).
expand_aliases_locally(X,Z):-file_search_path(X,Y),expand_file_name_each(Y,Z)


expand_to_absolutes(A,AAA):-expand_aliases(A,AA),absolute_file_name(AA,AAA).

same_paths(A,B):-expand_to_absolutes(A,BB),
                 expand_to_absolutes(B,BB),
                 same_refs(AA,BB),!.


same_refs(A,A):-!.
same_refs(A,B):-nonvar(A),nonvar(B),same_file(A,B),!.
same_refs(A,B):-expand_to_absolutes(A,AA),expand_to_absolutes(B,BB),same_file(AA,BB),!.

join_pathnames(First,Second,All):-var(First),nonvar(Second),nonvar(All),!,
   expand_to_absolutes(All,Joined),atomic(Joined),trace,   
   concat_atom(List,'/',Joined),
   expand_aliases_locally(Second,BE),
   append(AE0,BE0,List),concat_atom(BE0,'/',BE),   
   expand_aliases(First,AE),!.
   
join_pathnames(First,Second,All):-nonvar(First),nonvar(Second),var(All),!,
   cb_file_search_path(First,A),expand_to_absolutes(A,AE),
   cb_file_search_path(Second,B),concat_atom([AE,B],'/',Joined),expand_to_absolutes(Joind,Place),!,same_refs(Place,All),!.


join_pathnames(First,Second,All):-nonvar(First),var(Second),nonvar(All),!,
   cb_file_search_path(First,A),expand_to_absolutes(A,AE),
   cb_file_search_path(Second,B),concat_atom([AE,B],'/',Joined),expand_to_absolutes(Joind,Place),!,same_refs(Place,All),!.

join_pathnames(First,Second,All):-var(First),nonvar(Full),
   
   cb_file_search_path(First,A),expand_to_absolutes(A,AE),
   cb_file_search_path(Second,B),concat_atom([AE,B],'/',Joined),expand_to_absolutes(Joind,Place),is_absolute_file_name(Place),!.

/*
:- extern_pathname(FullPath,'cogbot.pl'),file_directory_name(FullPath),asserta(cogbot_pl_dir(FullPath)).

directory_of_file(File,Start,Hints,Dir):- member(Dir,Hints),join_pathnames(Dir,File,Path), exists_file(Path),!.

:- cogbot_pl_dir(D)->Try=D;D='cy ,join_pathnames('cogbot.pl',),file_exists(NewFile)))) -> retract(cogbot_pl_dir(D)).
:- ignore(( not(cogbot_pl_dir(D)),file_exists('cogbot.pl'),asserta(cogbot_pl_dir('./')) )).


      (file_exists('cogbot.pl'),asserta(cogbot_pl_dir('./'))).
*/

%:-file_exists('cogbot.pl'),asserta(cogbot_pl_dir('./').
:-['cyc'].
%:-['bootstrap.aiml.pl'].
%%:-assertz(librar
%file_search_path(X,Y).

