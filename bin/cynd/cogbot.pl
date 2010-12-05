% ===================================================================
% File 'cogbot.pl'
% Purpose: Toplevle loader file for proog support
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'cogbot.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================


:-dynamic(cogbot_pl_dir/1).
:-dynamic(cogbot_temp_dir/1).
:-dynamic(cogbot_aiml_dir/1).
:-dynamic(filepath_hints/1).

%% ======================================================
%% Add a library directory here
%% ======================================================
asserta_if_new_hlper2(C):-catch(C,_,fail),!.
asserta_if_new_hlper2(C):-asserta(C),!.
:-source_location(File,_Line),file_directory_name(File, ParentDir),
   writeq(logicmoo_module_aiml:asserta(library_directory(ParentDir))),
   asserta_if_new_hlper2(library_directory(ParentDir)).


extern_pathname(FullPah,Hint):-current_stream(FullPah,read,_),join_pathnames(_,Hint,FullPah),!.

expand_file_name_each(A,B):-expand_file_name(A,BB),!,member(B,BB).

cb_file_search_path(X,Y):-nonvar(Y),!,throw(cb_file_search_path(X,Y)).
cb_file_search_path(X,Z):-nonvar(X),expand_file_name_each(X,Y),absolute_file_name(Y,Z).
cb_file_search_path(X,Z):-file_search_path(X,Y),expand_file_name_each(Y,Z).

expand_aliases(X,Z):-nonvar(X),expand_file_name_each(X,Y),absolute_file_name(Y,Z).
expand_aliases(X,Z):-file_search_path(X,Y),expand_file_name_each(Y,Z).


expand_aliases_locally(X,Z):-nonvar(X),expand_file_name_each(X,Z).
expand_aliases_locally(X,Z):-file_search_path(X,Y),expand_file_name_each(Y,Z).


expand_to_absolutes(A,AAA):-expand_aliases(A,AA),absolute_file_name(AA,AAA).

same_paths(A,B):-expand_to_absolutes(A,AA),
                 expand_to_absolutes(B,BB),
                 same_refs(AA,BB),!.


same_refs(A,A):-!.
same_refs(A,B):-nonvar(A),nonvar(B),same_file(A,B),!.
same_refs(A,B):-expand_to_absolutes(A,AA),expand_to_absolutes(B,BB),same_file(AA,BB),!.

join_pathnames(First,Second,All):-var(First),nonvar(Second),nonvar(All),!,
   expand_to_absolutes(All,Joined),atomic(Joined),trace,   
   concat_atom(List,'/',Joined),
   expand_aliases_locally(Second,BE),
   append(AE,BE0,List),concat_atom(BE0,'/',BE),   
   expand_aliases(First,AE),!.
   
join_pathnames(First,Second,All):-nonvar(First),nonvar(Second),var(All),!,
   cb_file_search_path(First,A),expand_to_absolutes(A,AE),
   cb_file_search_path(Second,B),concat_atom([AE,B],'/',Joined),expand_to_absolutes(Joined,Place),!,same_refs(Place,All),!.


join_pathnames(First,Second,All):-nonvar(First),var(Second),nonvar(All),!,
   cb_file_search_path(First,A),expand_to_absolutes(A,AE),
   cb_file_search_path(Second,B),concat_atom([AE,B],'/',Joined),expand_to_absolutes(Joined,Place),!,same_refs(Place,All),!.

join_pathnames(First,Second,Full):-var(First),nonvar(Full),   
   cb_file_search_path(First,A),expand_to_absolutes(A,AE),
   cb_file_search_path(Second,B),concat_atom([AE,B],'/',Joined),expand_to_absolutes(Joined,Place),is_absolute_file_name(Place),!.

/*
:- extern_pathname(FullPath,'cogbot.pl'),file_directory_name(FullPath),asserta(cogbot_pl_dir(FullPath)).

directory_of_file(File,Start,Hints,Dir):- member(Dir,Hints),join_pathnames(Dir,File,Path), exists_file(Path),!.

:- cogbot_pl_dir(D)->Try=D;D='cynd',join_pathnames('cogbot.pl',),file_exists(NewFile)))) -> retract(cogbot_pl_dir(D)).
:- ignore(( not(cogbot_pl_dir(D)),file_exists('cogbot.pl'),asserta(cogbot_pl_dir('./')) )).


      (file_exists('cogbot.pl'),asserta(cogbot_pl_dir('./'))).
*/

%:-file_exists('cogbot.pl'),asserta(cogbot_pl_dir('./').

:- exists_file('cogbot.pl') -> cd('..') ; true.


:-ensure_loaded('cynd/cyc_pl/cyc').

/*
:-abolish(cyc:debugFmt/1).
cyc:debugFmt(Stuff):- notrace((debugFmtS(Stuff))),!.

debugFmtS([]):-!.
debugFmtS([A|L]):-!,debugFmt('% ~q~n',[[A|L]]).
debugFmtS(Comp):-hideIfNeeded(Comp,Comp2),!,debugFmt('% ~q~n',[Comp2]).
debugFmtS(Stuff):-!,debugFmt('% ~q~n',[Stuff]).


hideIfNeeded(I,I):- (var(I);atomic(I)),!.
hideIfNeeded([I|_],ctx):-nonvar(I),I=frame(_,_,_),!.
hideIfNeeded([I|N],[I0|N0]):-!,hideIfNeeded(I,I0),hideIfNeeded(N,N0),!.
hideIfNeeded(Comp,Comp2):-compound(Comp),Comp=..[L,I|ST],hideIfNeeded(I,II),Comp2=..[L,II|ST].
hideIfNeeded(I,I):-!.
*/

noaimltrace(X):- X. %% notrace(X).

guitracer:-debug.

:-ensure_loaded('cynd/programk/logicmoo_module_aiml.pl').
%%:-assertz(librar
%file_search_path(X,Y).

:-ensure_loaded(library('programk/logicmoo_module_aiml_main.pl')).

