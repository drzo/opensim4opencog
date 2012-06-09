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
:-app_init(botdo(showgui)).
:-app_init(botdo('setmaster Douglas Miles')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: Dynamic predicates 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic(hook_botvar2/2).
hook_botvar2(_,"a",1).
hook_botvar2(_,"b",2).

bv:hook_botvar_get(_BotID,NS,Key,Value):-hook_botvar2(NS,Key,Value).
bv:hook_botvar_set(_BotID,NS,Key,Value):-retractall(hook_botvar2(NS,Key,_)),assert(hook_botvar2(NS,Key,Value)).
bv:hook_botvar_key(_BotID,NS,Key):-hook_botvar2(NS,Key,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: Set up sitting on ground based botvar (Side effect based example)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bv:hook_botvar_get(BotID,bot,'isSittingGround',Value):- wbotget(BotID,['Self','Movement','SitOnGround'],Result),!,
    (cli_is_true(Result)-> Value="Yes" ; Value="No").

bv:hook_botvar_set(BotID,bot,'isSittingGround',Value):- Value="Yes" -> wbotcall(BotID,['Self','SitOnGround'],_) ; wbotcall(BotID,['Self','Stand'],_).

bv:hook_botvar_key(_,bot,'isSittingGround').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: set up a isNight (readonly based example)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic isNight/0.

bv:hook_botvar_get(_,bot,'isNight',Value):- isNight -> Value="Yes" ; Value="No".

bv:hook_botvar_set(_,bot,'isNight',Value):- 'format'(user_error,'Someone request isNight=~w~n',Value).

bv:hook_botvar_key(_,bot,'isNight').


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
