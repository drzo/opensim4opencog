:- module(aimlbot, []).

:- use_module(hillpeople(hillpeople)).
:- use_module(library(swicli)).
:- use_module(cogbot(cogrobot)).

:-set_prolog_flag(double_quotes,string).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: Set up sitting on ground based botvar (Side effect based example)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bv:hook_botvar_get(BotID,bot,'isSittingGround',Value):- 
     wbotget(BotID,['Self','Movement','SitOnGround'],Result),!,
        (cli_is_true(Result)-> Value="Yes" ; Value="No").

bv:hook_botvar_set(BotID,bot,'isSittingGround',Value):- 
   Value="Yes" -> wbotcall(BotID,['Self','SitOnGround'],_) ; wbotcall(BotID,['Self','Stand'],_).

bv:hook_botvar_key(_,bot,'isSittingGround').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% On Bot Created
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bv:hook_bot_created(BotID):- '@'( wbotdo(BotID,'load AIMLBotModule noserv') , cogrobot).
bv:hook_bot_created(BotID):-'format'(user_error,'Bot created! ~w ~n ',[BotID]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% On Bot Loggedin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bv:hook_bot_loggedin(BotID):-system:'format'(user_error,'Bot Logged In! ~w ~n ',[a(BotID)]).
bv:hook_bot_loggedin(BotID):- '@'( cogrobot:wbotdo(BotID,'aiml @load examples/hillpeople/aiml/HillPeople.aiml') , cogrobot ).
bv:hook_bot_loggedin(BotID):- '@'( cogrobot:wbotdo(BotID,'say "prolog says i am on"'), cogrobot).


%%%bv:hook_bot_event(BotID,B,C):-system:'format'(user_error,'Bot event! ~w ~n ',[a(BotID,B,C)]).

