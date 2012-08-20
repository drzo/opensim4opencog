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


:-use_module('examples/hillpeople/hillpeople.pl').

%:-use_module('../test/movesupport'),use_module('../test/acctsupport'),use_module('../test/testpathfind'),use_module('../test/testsupport').


% name_to_location_ref(hut1,X).


:-use_module(cogbot(cogrobot)).

pbd:-cli_test_pbd(wbotvar_impl,O),writeq(O),nl.
pbd1:-cli_test_pbct(wbotvar_impl,O),writeq(O),nl.
pbd2:-cli_new_prolog_collection(wbotvar_impl,object,Obj0),cli_call(Obj0,'Copy',Obj),!,forall(cli_col(Obj,E),writeq(E)).

swt0:-cli_new('System.Collections.Generic.List'(string),[],[],O),cli_call(O,add("sdfsdf"),_),cli_lib_call('ToString'(O),WS),writeq(WS),nl.
%%swt1:-start_wearing(_,Items),!,bot_replaceoutfit(["tribal"],Items).

l1:-logon_bot('ExampleBot','Resident','pass123', "https://login.agni.lindenlab.com/cgi-bin/login.cgi","last",_).
l2:-logon_bot('Nephrael','Rae','abc123', "https://login.agni.lindenlab.com/cgi-bin/login.cgi","last",_).

:-cli_load_assembly('System.dll').
:-cli_load_assembly('System.Core').
:-cli_load_assembly('IKVM.OpenJDK.Core.dll').


:-set_num_bots(2).
:-set_tribe('Dougstribe').
%%:-logon_bots.
%%:-logon_bots,ebt.
%%:-current_bot(Obj),cli_add_event_handler(Obj,'EachSimEvent',c(A,(attach_console,'format'(user_error,'EV = ~q.~n',[A]))),_Out).

/*
C# Way

        public UUID FindUUIDForName(string nameString)
        {

            ManualResetEvent NameSearchEvent = new ManualResetEvent(false);
            UUID queryID = UUID.Random();
            UUID found = UUID.Zero;
            EventHandler<AvatarPickerReplyEventArgs> callback =
                new EventHandler<AvatarPickerReplyEventArgs>((s, e) =>
                {
                    if (queryID == e.QueryID)
                    {

                        foreach (KeyValuePair<UUID, string> kvp in e.Avatars)
                        {
                            if (kvp.Value == nameString) {
                               found = kvp.Key;
                               break;
                            }
                        }
                        NameSearchEvent.Set();
                    }
                });
            try
            {
                client.Avatars.AvatarPickerReply += callback;
                // Send the Query
                client.Avatars.RequestAvatarNameSearch(nameString, queryID);

                NameSearchEvent.WaitOne(10000, false);
            }
            finally
            {
                client.Avatars.AvatarPickerReply -= callback;
            }
        }

*/
/*
Prolog Way

 ?- findUUIDForName("Douglas Miles",X).
 X = uuid("8f6ce54e-95f5-46d0-b090-c5361c821232").

*/
findUUIDForName0(NameString,Found):-
   botget('Avatars',AvMan),          % get the AvatarManager object
   cli_new_event_waiter(AvMan,'AvatarPickerReply',WaitOn),  % create a new event handler object (and register it)
   cli_call('UUID','Random',QID),      % make a random UUID
   cli_call(AvMan,'RequestAvatarNameSearch'(NameString,QID),_),   % make the request to the server
   cli_block_until_event(WaitOn, 10000, closure(Evt,cli_get(Evt,'QueryID',QID)), _),  %  wait for up to 10 seconds for the right the QueryID
   cli_get(Evt,'Avatars',U2SDict),  % get the UUID2Name dictionary
   cli_map(U2SDict,Found,NameString),   % Search that dictionary for the UUID by matching the String
   cli_call(WaitOn,'Dispose',_).  % unregister the handler and dispose


end_of_file.

:-app_init(botdo(showgui)).
:-app_init(botdo('setmaster Douglas Miles')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: set up a isNight (readonly based example)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic isNight/0.

bv:hook_botvar_get(_,bot,'isNight',Value):- isNight -> Value="Yes" ; Value="No".

bv:hook_botvar_set(_,bot,'isNight',Value):- 'format'(user_error,'Someone request isNight=~w~n',Value).

bv:hook_botvar_key(_,bot,'isNight').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: Set up sitting on ground based botvar (Side effect based example)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bv:hook_botvar_get(BotID,bot,'isSittingGround',Value):- 
     wbotget(BotID,['Self','Movement','SitOnGround'],Result),!,
        (cli_is_true(Result)-> Value="Yes" ; Value="No").

bv:hook_botvar_set(BotID,bot,'isSittingGround',Value):- 
   Value="Yes" -> wbotcall(BotID,['Self','SitOnGround'],_) ; wbotcall(BotID,['Self','Stand'],_).

bv:hook_botvar_key(_,bot,'isSittingGround').

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: Dynamic predicates 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic(dyn_store_botvar/3).
dyn_store_botvar(_,"a",1).
dyn_store_botvar(_,"b",2).

bv:hook_botvar_get(_BotID,NS,Key,Value):-dyn_store_botvar(NS,Key,Value).
bv:hook_botvar_set(_BotID,NS,Key,Value):-
    retractall(dyn_store_botvar(NS,Key,_)),assert(dyn_store_botvar(NS,Key,Value)).
bv:hook_botvar_key(_BotID,NS,Key):-dyn_store_botvar(NS,Key,_).
*/
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

%%:-logon_bots.
