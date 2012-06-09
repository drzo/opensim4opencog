%--------------------------------------------------------
%
%  dtoplevel.pl
%
%    Testing of multi bot logon and movement
%
%    This requires a set of the 'hillpeople' bots
%
%---------------------------------------------------------------------

:-set_prolog_flag(double_quotes,string).
:-at_initialization(set_prolog_flag(double_quotes,string)).
 
dbgfmt(F,A):-'format'(F,A).

:-use_module(library(swicli)).

/*
:-use_module('../test/movesupport').
:-use_module('../test/acctsupport').
:-use_module('../test/testpathfind').
:-use_module('../test/testsupport').
*/

:-use_module('examples/hillpeople/hillpeople.pl').

:-use_module(cogbot(cogrobot)).

pbd:-cli_test_pbd(wbotvar_impl,O),writeq(O),nl.
pbd1:-cli_test_pbct(wbotvar_impl,O),writeq(O),nl.
pbd2:-cli_new_prolog_collection(wbotvar_impl,object,Obj0),cli_call(Obj0,'Copy',Obj),!,forall(cli_col(Obj,E),writeq(E)).

swt0:-cli_new('System.Collections.Generic.List'(string),[],[],O),cli_call(O,add("sdfsdf"),_),cli_lib_call('ToString'(O),WS),writeq(WS),nl.
%%swt1:-start_wearing(_,Items),!,bot_replaceoutfit(["tribal"],Items).

l1:-logon_bot('ExampleBot','Resident','pass123', "https://login.agni.lindenlab.com/cgi-bin/login.cgi","last",_).
l2:-logon_bot('Nephrael','Rae','abc123', "https://login.agni.lindenlab.com/cgi-bin/login.cgi","last",_).
:-set_num_bots(1).
:-set_tribe('Dougstribe').
:-logon_bots.
:-botdo(showgui).
:-botdo('setmaster Douglas Miles').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% USING BOTVAR EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% register a small helper for ourselves
same_botkey(NameSpace,Key,MyKey):-botname(BotName),global_samekey(NameSpace,BotName),global_samekey(Key,MyKey).

% Need freedom to declare in multiple codeblocks
:- discontiguous global_impl_get/3,global_impl_set/3,global_impl_keys/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dynamic predicate example 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic(global_impl2/2).
global_impl2(_,"a",1).
global_impl2(_,"b",2).

global_impl_get(NameSpace,Key,Value):-global_impl2(NameSpace,Key,Value).
global_impl_set(NameSpace,Key,Value):-retractall(global_impl2(NameSpace,Key,_)),assert(global_impl2(NameSpace,Key,Value)).
global_impl_keys(NameSpace,Key):-global_impl2(NameSpace,Key,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set up sitting on ground based botvar (Side effect based example)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global_impl_get(NameSpace,Key,Value):-same_botkey(NameSpace,Key,'isSittingGround'),!,
   botget(['Self','Movement','SitOnGround'],Result), (cli_is_true(Result)-> Value="Yes" ; Value="No").

global_impl_set(NameSpace,Key,Value):-same_botkey(NameSpace,Key,'isSittingGround'),!,
   (Value="Yes" -> botcall(['Self','SitOnGround'],_) ; botcall(['Self','StandUp'],_)).

global_impl_keys(_NameSpace,'isSittingGround').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set up a isNight example (readonly based example)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global_impl_get(NameSpace,Key,Value):-same_botkey(NameSpace,Key,'isNight'),!, ( isNight -> Value="Yes" ; Value="No").

global_impl_set(NameSpace,Key,Value):-same_botkey(NameSpace,Key,'isNight'),!, 'format'(user_error,'Someone request isNight=~w~n',Value).

global_impl_keys(_NameSpace,'isNight').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% register our examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-global_addvars("Otopopo Dougstribe", global_impl_get, global_impl_set, global_impl_keys).


%%:-ebt.

end_of_file.


/*
%% decleare the botvar predicate
:-dynamic(oto_impl/3).
%% create a arity 2 version to gather the keys
oto_impl(_,N):-oto_impl(_,N,_).
%% declare some values in arity 2 predicate
oto_impl(_,"favfood",corn).
%% register the arity 2 version
:-bot_addvars_dynpred(oto_impl).
*/

:-set_num_bots(1).
:-set_tribe('Hillperson').

:-set_moveproc(astargoto).

%%:-set_bot_writeln_delegate(cli_fmt).
:-set_bot_writeln_delegate(null_callback).
:-use_module(library('dialect/ifprolog')).

get_test_waypoints(_,_):-fail.

:-logon_bots.
