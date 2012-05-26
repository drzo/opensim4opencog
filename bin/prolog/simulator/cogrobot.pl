%------------------------------------------------------------------------------
%
%  cogrobot.pl
%
%     Module for use of _cogbot_ in SecondLife!!!
%
% _cogbot_ is usually in this mode
% set_prolog_flag(double_quotes,string).
%
%------------------------------------------------------------------------------
:-module(cogrobot,
  [
   run_sl/0,
   gridclient_ref/1,
   world_ref/1, world_get/2, client_manager_ref/1,
   current_bot/1, botget/2,
   botcall/1, botcall/2,

   botdo/1,wb_botdo/2,wabdo/1,

   botcmd/1, botcmd/2, botcmd/3,   
   wb_botcmd/2, wb_botcmd/3, wb_botcmd/4,

   grid_object/1, world_avatar/1, world_object/1,
   grid_asset/1, grid_account/1,
   simAvDistance/3, 
   name_to_location_ref/2,
   vectorAdd/3,
   distance_to/2,
   position_to_v3d/2,
   position_to_v3/2,
   %%on_sim_event/3,  %% uses user:* wrapper
   sim_event_db/3,
   obj2Npl/2,
   npl2Obj/2,
   chat/1,
   chat/2,
   chat/3,
   %%cli_fmt/3,
   create_write_hook/2,
   create_write_hook/1,
   uuid_to_cli_image/2,
   uuid_to_image_parts/2,
   request_texture/1,
   object_color/2,
   set_current_bot/1,
   unset_current_bot/1,
   %%robot_to_str/2,   %% uses user:* wrapper
   cmdargs_to_atomstr/2,
   set_bot_writeln_delegate/1,
   bot_writeln_delegate/1,
   prolog_in_thread/3,
   current_botname/1,
   logon_bot/6,
   create_bot/6,
   wb_botinventory/3,
   inventory_node_name/2
   ]).

:-set_prolog_flag(double_quotes,string).

app_restore(Call):-term_to_atom(Call,Atom),atom_concat(Atom,'_done',Did),dynamic(Did),app_init_call(app_restore(Did,Call)).
app_init_call(Call):-at_initialization(Call),Call.
app_restore(Did,_Call):-Did,!.
app_restore(Did,Call):-assert(Did),!,Call.

btrace:-attach_console,trace.

%%:- absolute_file_name('.',X),asserta(prev_dir6(X)),listing(prev_dir6).

%%:-source_location(File,_Line),file_directory_name(File, Directory),cd(Directory).


assert_once(Gaf):-catch(call(Gaf),_,fail),!.
assert_once(Gaf):-assert(Gaf).

:- assert_once(user:file_search_path(foreign, '.')).
:- assert_once(user:file_search_path(jpl_examples, 'examples/prolog')).
:- assert_once(user:file_search_path(jar, '.')).
:- assert_once(user:file_search_path(library, '.')).
:- assert_once(user:file_search_path(library, '..')).
:- assert_once(user:file_search_path(library, '../..')).
:- assert_once(user:file_search_path(library, '../../test')).
:- assert_once(user:file_search_path(test, '../test')).

%%:- use_module(library(testsupport)).
:-use_module(library(swicli)).


%:-app_restore(cli_load_assembly('AForge.Imaging.dll')).
%:-app_restore(cli_load_assembly('AForge.Imaging.Formats.dll')).
%------------------------------------------------------------------------------

%% load needed modules

:-use_module(library(swicli)).
%%:-use_module(library(jpl)).

%------------------------------------------------------------------------------

%% load the cogbot assembly
:-dynamic(loaded_cogbot_assembly/0).
load_cogbot_assembly:-loaded_cogbot_assembly,!.
load_cogbot_assembly:-assert(loaded_cogbot_assembly),current_prolog_flag(address_bits,32) -> cli_load_assembly('Cogbot32.exe') ; cli_load_assembly('Cogbot.exe').
:-app_restore(load_cogbot_assembly).

%% cache the type names
% prevents us having to use long names for things like SimAvatar
%
cache_cogbot_types:-
  cli_members('cogbot.TheOpenSims.SimAvatar',_),
  cli_members('cogbot.Listeners.WorldObjects',_),
  cli_members('OpenMetaverse.Primitive',_).

:-app_restore(cache_cogbot_types).
%------------------------------------------------------------------------------
% some type layout conversions (to make cleaner code)
%
%  Layouts are records - it's a field layout
%  cli_to_from_layout is a way to register an automagic conversion type
%  cli_add_layout adds a conversion between C# type and Prolog type
%------------------------------------------------------------------------------

add_layouts:-
  cli_add_layout('Vector3',v3(x,y,z)),
  cli_add_layout('Vector3d',v3d('X','Y','Z')),
  cli_add_layout('Vector4',v4('X','Y','Z','W')),
  cli_add_layout('Quaternion',quat('X','Y','Z','W')),
 %%  cli_add_layout('UUID',uuid('_guid')),
 cli_to_from_layout('UUID',uuid('ToString'),'UUIDFromString'),
 %%  cli_add_layout('Guid',guid(string)),
  !.

:-app_restore(add_layouts).

%------------------------------------------------------------------------------
% much code uses current_bot(Me) like botcmd and say/n  
%
%  and is a thread_local predicate if no bot has been registered by thread its the last created bot
%  
% usage:
%  set_current_bot(Me), .....  unset_current_bot(Me)
%
% bug prone antipattern, but it's supported:
%  current_bot(OldBot), set_current_bot(Me) .....   set_current_bot(OldBot)
% though current_bot(OldBot) may throw if not bot is set
% ------------------------------------------------------------------------------
%
:-dynamic current_bot_db/2.
current_bot(BotID):-thread_self(TID),current_bot_db(TID,BotID),!.
current_bot(BotID):-client_manager_ref(Man),cli_get(Man,'LastBotClient',BotID).

set_current_bot(BotID):-thread_self(TID),retractall(current_bot_db(TID,_)),asserta(current_bot_db(TID,BotID)).

unset_current_bot(BotID):-thread_self(TID),current_bot_db(TID,OLD), 
    (OLD=BotID -> retract(current_bot_db(TID,OLD)) ; cogbot_throw(unset_current_bot(tid(TID),used(BotID),expected(OLD)))).

current_botname(Name) :- botget(name,X),string_to_atom(X,Name).
%------------------------------------------------------------------------------
% throws cogbot based exceptions
% PRIVATE
% ------------------------------------------------------------------------------
cogbot_throw(Error):-throw(cogbot_user_error(Error)).


%------------------------------------------------------------------------------
% syncronously log a bot onto simulator and set the current_bot/1
% PUBLIC    
% TODO - should we remove the return result since we set current bot?
% ------------------------------------------------------------------------------
logon_bot(First, Last, Password, Loginuri, Location, BotID):-
        create_bot(First, Last, Password, Loginuri, Location, BotID),
        set_current_bot(BotID),
	cli_call(BotID,'LoginBlocked',_).

%------------------------------------------------------------------------------
% create a botclient (will call startups (like botconfig.xml) but no call to implicit login)
% PUBLIC
% ------------------------------------------------------------------------------
create_bot(First, Last, Password, Loginuri, Location, BotID):-        
	client_manager_ref(CM),
	cli_call(CM,'CreateBotClient'(First, Last, Password, Loginuri, Location), BotID),
        asserta(bot_client_db(First, Last, Password, Loginuri, Location, BotID)).


%------------------------------------------------------------------------------
% System shutdowns
% ------------------------------------------------------------------------------
:-dynamic(bot_client_db/6).

ignore_caught(Call):-ignore(catch(Call,_,true)).


kill_pl_threads:-thread_property(ID,status(running)),ID\=main,thread_signal(ID,thread_exit(true)),fail.
kill_pl_threads:-cli_halt(0).

app_quit:-write('logoutbots\n'),flush_output,client_manager_ref(CM),cli_call(CM,'Quit',_),kill_pl_threads.
%%logoutBots:-bot_client_db(_First, _Last, _Password, _Loginuri, _Location, BotID),ignore_caught(cli_call(BotID,'Dispose',_)),fail.
%%logoutBots:-ignore_caught((client_manager_ref(CM),cli_call(CM,'ShutDown',_))),cli_call('System.Environment','Exit'(0),_).

:-at_halt(app_quit).


%------------------------------------------------------------------------------
% Refernce to the scene is world_ref
%------------------------------------------------------------------------------
world_ref(Sys):-cli_get('cogbot.Listeners.WorldObjects','GridMaster',Sys).

% gets some property of the GridMaster
%  world_get(+Field, -Value)
%
world_get(Field,Value):-world_ref(Sys),cli_get(Sys,Field,Value).

%% get each SimObject
% this is every primitive, linked or not, as a complex term
% it's a partially marshalled object from the simulator
% cli_col iterates thru elements
grid_object(Ele):-world_get('SimObjects',Objs),cli_col(Objs,Ele),not(cli_is_type(Ele,'SimAvatar')).

%------------------------------------------------------------------------------
% ways of iterating in world objects
%------------------------------------------------------------------------------
world_object(Ele):-grid_object(Ele),cli_get(Ele,isattachment,@(false)).

world_root_object(Ele):-world_get('SimRootObjects',Objs),cli_col(Objs,Ele).

%% get each SimAvatar
%
% this is the set of av's that are known to the simulator, they are only
% actually present if they have a prim
%
% grid_object and world_avatar handle the complexity of sim crossings
%
world_avatar(Ele):-grid_account(Ele),cli_get(Ele,hasprim,@(true)).

%
% a grid_account/1 is like world_avatar (they are avatars known about in system..
%    including friends not logged in)
%
grid_account(Ele):-world_get('SimAvatars',Objs),cli_col(Objs,Ele).

%
% a grid_region returns all regions known to cogbot
%
grid_region(Ele):-cli_get('cogbot.TheOpenSims.SimRegion','CurrentRegions',Objs),cli_col(Objs,Ele).

grid_parcels(Ele):-grid_region(Sim),cli_get(Sim,parcels,Objs),cli_col(Objs,Ele).

%% get the client_manager_ref Instance
%%
%  A class that holds all the static singletons
%  A Client is a logged on acct in this context
%
%  Clientmanager binds radegast to the Client
%  botconfig is run from Clientmanager
client_manager_ref(SingleInstance):-cli_get('cogbot.ClientManager','SingleInstance',SingleInstance).

% given an object and a property returns value for the avatar
%
% walks down property tree
% botget([name,length,X)
% botget([name,length],X)
% botget([position,z],X)
%
% a.b.c.d
% botget([a.b.c.d],X).
% botget([a,b,c,d],X).
% botget([a,b,c,d],X). = mybot.a.b.c.d
% prolog botget([a,b,c,d],X). = c# object X = myBotClient.a.b.c.d
% botget(['Inventory','Store',rootfolder,name],Y).
% Y = "My Inventory".
%
% botget(['Inventory','Store',rootnode,nodes,values],Y),
%	findall(S,(cli_col(Y,Z),cli_to_str(Z,S)),L),writeq(L).
%	["Scripts","Photo Album","*MD* Brown Leather Hat w/Bling",
%	"Body Parts","Notecards","Objects","Clothing","Landmarks","Textures",
%     "Gestures","boxed fem_talk","Calling Cards","Animations","Sounds",
%	"Trash","Lost And Found"]
%       Y = @'C#720558400'
%
%       finds all grandchildren
%       botget(['Inventory','Store',rootnode,nodes,values],Y),
%	     findall(S,(cli_col(Y,Z),cli_get(Z,'children',GC),
%	     cli_collecton(GC,'children',GCReal),cli_to_str(GCReal,S)),L),writeq(L).
%
botget(Property,Value):-current_bot(BotID),wb_botget(BotID,Property,Value).

wb_botget(BotID,Property,Value):-cli_get(BotID,Property,Value),!.

% wb_botinventory(+BotID,?Path,?NodeDataRef).
% if just checking for items existence you  _  the third arg
% (the third arg is the Node contents)
%
% ?- current_bot(BotID),wb_botinventory(BotID,[A,'Clothing'],X).
% BotID = @'C#508280816',
% A = "My Inventory",
% X = @'C#598007568' ;
% BotID = @'C#508280816',
% A = "Library",
% X = @'C#598013968' ;
% false.
%
% ?- current_bot(BotID),wb_botinventory(BotID,[A,'Clothing',What],X).
% BotID = @'C#508280816',
% A = "Library",
% What = "Female Shape & Outfit",
% X = @'C#598060416' ;
% BotID = @'C#508280816',
% A = "Library",
% What = "Gamer Male",
% X = @'C#598062912' 

wb_botinventory(BotID,[StartName,TopName|Path],Node):-
   lists:member(Start,['RootNode','LibraryRootNode']),   
   wb_botget(BotID,['Inventory','Store',Start],StartNode),
   cli_get(StartNode,[nodes,values],StartCol),
   cli_get(StartNode,[data,name],StartName),
   cli_col(StartCol,Top),cli_get(Top,data,TopData),
   cli_get(TopData,name,TopName),
   inventory_children(Top,TopData,Path,Node).

inventory_children(_Top,TopData,[],NodeData):-cli_unify(NodeData,TopData).
inventory_children(Mid,_MidData,[TopName|Path],NodeData):-
   cli_get(Mid,[nodes,values],StartCol),
   cli_col(StartCol,Top),cli_get(Top,data,TopData),
   cli_get(TopData,name,TopName),
   inventory_children(Top,TopData,Path,NodeData).
      
inventory_node_name(Node,Name):-cli_get(Node,[data,name],Name),!.
inventory_node_name(Node,Name):-cli_get(Node,[data,name],Name),!.
inventory_node_name(_Node,'unk').

             


% a way to call a method on c#
% cli_call('System',printf(32),Y).
botcall(Call):-current_bot(BotID),wb_botcall(BotID,Call).
botcall(Call,Res):-current_bot(BotID),wb_botcall(BotID,Call,Res).

wb_botcall(BotID,Call):-wb_botcall(BotID,Call,Res),cli_writeln(Res).
wb_botcall(BotID,[P|N],Value):-!,cli_get(BotID,P,Mid),cli_get(Mid,N,Value).
wb_botcall(BotID,Property,Value):-cli_call(BotID,Property,Value).


% wrappered execute command in a convenience pred
% botCmd(say("hi"))
%
botdo(In):-current_bot(BotID),wb_botdo(BotID,In),!.
wb_botdo(BotID,In):-wb_botcmd(BotID,In,cli_fmt(botcmd),_).

wabdo(In):-current_bot_db(_,BotID),wb_botdo(BotID,In),fail.
wabdo(_).

% wrappered execute command in a convenience pred
% botcmd(say("hi"))
%
botcmd(In):-current_bot(BotID),wb_botcmd(BotID,In),!.
botcmd(In,Out):-current_bot(BotID),wb_botcmd(BotID,In,Out),!.
botcmd(Str,WriteDelegate,Out):-current_bot(BotID),wb_botcmd(BotID,Str,WriteDelegate,Out).

wb_botcmd(BotID,StrIn):-wb_botcmd(BotID,StrIn,Out),cli_get(Out,success,@(true)).
wb_botcmd(BotID,StrIn,Out):-cmdargs_to_atomstr(StrIn,Str),wb_botcmd(BotID,Str,pluggable_callback(botcmd),Out).
wb_botcmd(BotID,StrIn,WriteDelegate,Out):-cmdargs_to_atomstr(StrIn,Str),cli_call(BotID,executeCommand(Str,BotID,WriteDelegate),Out).


% wrappered execute command in a convenience pred
% cmdargs_to_atomstr(say("hi"),Out)
%
cmdargs_to_atomstr([C|Cmd],Out):-toStringableArgs(Cmd,SCmd),!,concat_atom([C|SCmd],' ',Str),cmdargs_to_atomstr(Str,Out).
cmdargs_to_atomstr(C,Out):-compound(C),C=..[F,A|B],is_movement_proc(F),\+ is_vector(A),\+ cli_is_type(A,'SimPosition'),!,
    name_to_location_ref(A,AA),CC=..[F,AA|B],cmdargs_to_atomstr(CC,Out).
cmdargs_to_atomstr(C,Out):-compound(C),!,C=..[F|A],listifyFlat([F|A],FL),cmdargs_to_atomstr(FL,Out).
cmdargs_to_atomstr(Str,Str):-!. %%toStringableArg(StrIn,Str).

is_movement_proc(astargoto).
is_movement_proc(moveto).

is_vector(V3):-notrace((compound(V3),functor(V3,F,3),(F==v3;F==v3d))).

toStringableArgs(Var,Var):-var(Var),!.
toStringableArgs([C|Cmd],[A|Amd]):-toStringableArg(C,A),toStringableArgs(Cmd,Amd),!.
toStringableArgs(CCmd,CCmd).

toStringableArg(Var,Var):-var(Var),!,throw(toStringableArgVar(Var)).
toStringableArg(v3d(X,Y,Z),A):-concat_atom([X,Y,Z],'/',A).
toStringableArg(uuid(ID),ID):-!.
toStringableArg(v3(X,Y,Z),A):-concat_atom([X,Y,Z],'/',A).
toStringableArg(S,Out):-string(S),!,string_to_atom(S,A),toStringableArg(A,Out).
toStringableArg(A,A):-atom(A),atom_concat('"',_,A),!.
toStringableArg(A,Out):-atom(A),!,concat_atom(['"',A,'"'],'',Out).
toStringableArg('@'(OBJ),Out):-cli_is_type('@'(OBJ),'SimObject'),!,cli_get('@'(OBJ),id,uuid(Out)).
toStringableArg(Var,Var).

% helper pred for botcmd
listifyFlat([],[]):-!.
listifyFlat([H|T],HT):-!,listifyFlat(H,HL),listifyFlat(T,TL),!,append(HL,TL,HT).
listifyFlat(C,FA):-functor(C,F,1),!,C=..[F,A],!,listifyFlat(A,FA).
listifyFlat(v3d(X,Y,Z),[v3d(X,Y,Z)]).
listifyFlat(v3(X,Y,Z),[v3(X,Y,Z)]).
listifyFlat(C,FA):-compound(C),!,C=..[F|A],!,listifyFlat([F|A],FA).
listifyFlat(C,[C]).

%% get the GridClient Instance
%  libOMV's version of gridclient_ref, in case you want the direct one
gridclient_ref(Obj):-current_bot(BC),cli_get(BC,'gridClient',Obj).


%------------------------------------------------------------------------------
% create a writeline delegate
%------------------------------------------------------------------------------
create_write_hook(WID):-create_write_hook(pluggable_callback(cogrobot),WID).
create_write_hook(WriteDelegate,WID):-cli_new_delegate('MushDLR223.ScriptEngines.OutputDelegate',WriteDelegate,WID).

null_callback(_,_,_).

:-dynamic(bot_writeln_delegate/1).

bot_writeln_delegate(null_callback).

user:pluggable_callback(A,B,C):-cogrobot:pluggable_callback(A,B,C).
pluggable_callback(A,B,C):-bot_writeln_delegate(Pred),call(Pred,A,B,C).


%------------------------------------------------------------------------------
% set a default a writeline delegate
%------------------------------------------------------------------------------
set_bot_writeln_delegate(Pred/3):-!,set_bot_writeln_delegate(Pred).
set_bot_writeln_delegate(Pred):-retractall(bot_writeln_delegate(_)),assert(bot_writeln_delegate(Pred)).


%------------------------------------------------------------------------------
% event handler functions
%------------------------------------------------------------------------------
robot_to_str(C,C):-var(C).
robot_to_str([],[]).
robot_to_str([A|B],[AA|BB]):-robot_to_str(A,AA),robot_to_str(B,BB).
robot_to_str(Obj,array(ArrayS)):-Obj='@'(_O), cli_is_type(Obj,'System.Array'),cli_array_to_termlist(Obj,Array),!,robot_to_str(Array,ArrayS).
robot_to_str(Obj,list(ArrayS)):-Obj='@'(_O), cli_is_type(Obj,'System.Collections.Generic.IList'('MushDLR223.ScriptEngines.NamedParam')),cli_call(Obj,'ToArray',[],Array),robot_to_str(Array,ArrayS).
robot_to_str(Obj,enumr(ArrayS)):-Obj='@'(_O), cli_is_type(Obj,'System.Collections.IEnumerable'),cli_array_to_termlist(Obj,Array),robot_to_str(Array,ArrayS).
robot_to_str(C,AS):-compound(C),C=..[F|Args],not(member(F,['@'])),robot_to_str(Args,ArgS),AS=..[F|ArgS].
robot_to_str(C,AS):-cli_to_str(C,AS).

user:robot_to_str(A,B):-cogrobot:robot_to_str(A,B).


nop(_).


%% print some events
on_sim_event(_A,_B,_C):-!. % comment out this first line to print them
:-dynamic(sim_event_db/3).
on_sim_event(_A,B,C):-contains_var("On-Log-Message",a(B,C)),!.
on_sim_event(_A,B,C):-contains_var('DATA_UPDATE',a(B,C)),!.
on_sim_event(A,B,C):-!,nop(assertz(sim_event_db(A,B,C))),!,robot_to_str(C,AS),!,writeq(on_sim_event(AS)),nl.

user:on_sim_event(A,B,C):-cogrobot:on_sim_event(A,B,C).

%% trim_sim_events(NumToLeave):- predicate_property(sim_event_db(_,_,_),number_of_clauses(N)),Remove is N-Num, (Remove<=0->true;( ... )).
trim_sim_events(Num):- predicate_property(sim_event_db(_,_,_),number_of_clauses(N)),Num>=N,!.
trim_sim_events(Num):- retract(sim_event_db(_,_,_)),predicate_property(sim_event_db(_,_,_),number_of_clauses(N)),Num >= N,!.
trim_sim_events(_Num).

prolog_in_thread(Named,_,_):-thread_property(N,_),N==Named,!.
prolog_in_thread(Named,Goal,Options):-thread_create(Goal,_,[alias(Named)|Options]).

%% Every minute trim EventLog to 1000 entries
keep_1000_events:-repeat,sleep(60),trim_sim_events(1000),fail.
:-app_restore(prolog_in_thread(keep_1000_events,keep_1000_events,[detached(true)])).

%%:-module_transparent(first_bot_client_hook/2).

%% on first bot Client created register the global event handler
user:first_bot_client_hook(A,B):- %%%attach_console,trace,
 current_bot(Obj),
  % uncomment the next line if you want all commands to run thru the universal event handler
   cli_add_event_handler(Obj,'EachSimEvent',on_sim_event(_,_,_)),
   cli_to_str(first_bot_client_hook(A-B-Obj),Objs),writeq(Objs),nl.

%% register first_bot_client_hook
register_on_first_bot_client:- cli_add_event_handler('cogbot.ClientManager','BotClientCreated',first_bot_client_hook(_,_)).
:-app_restore(register_on_first_bot_client).

%------------------------------------------------------------------------------
% start Radegast!
%------------------------------------------------------------------------------
:-dynamic(ran_sl).

run_sl:-ran_sl,!.
% this is so you can reconsult this file without restarting radegast
run_sl:-asserta(ran_sl),!,
   cli_set('MushDLR223.Utilities.DLRConsole','NoConsoleVisible','@'(true)),
   cli_set('ABuildStartup.Program','UseApplicationExit','@'(false)),
   cli_set('cogbot.ClientManager','noGUI','@'(true)),
   cli_call('ABuildStartup.Program','Main',[],_).

% assert_once is assert a new grounded atomic fact only if the predicate
% was previously undefined

%:-retractall(cli_subproperty(_,_)).
:-assert_once(cli_subproperty('cogbot.TheOpenSims.SimAvatar','ProfileProperties')).
:-assert_once(cli_subproperty('cogbot.TheOpenSims.SimAvatar','AvatarInterests')).
:-assert_once(cli_subproperty('cogbot.TheOpenSims.SimAvatar','FriendshipInfo')).
:-assert_once(cli_subproperty('cogbot.TheOpenSims.SimObject','Prim')).
:-assert_once(cli_subproperty('cogbot.TheOpenSims.SimObject','Properties')).


obj2Npl(O,npl(66,O)).
npl2Obj(npl(66,O),O).

registerNamedParamRecomposer:-!.
registerNamedParamRecomposer:-cli_to_from_recomposer('System.Collections.Generic.IList'('cogbot.NamedParam'),'npl'(_,_),obj2Npl,npl2Obj).

:-app_restore(registerNamedParamRecomposer).

%------------------------------------------------------------------------------
% CLR Introspection of event handlers
%------------------------------------------------------------------------------
grid_asset(Asset):-  cli_get('cogbot.TheOpenSims.SimAssetStore','SimAssets',Assets),cli_col(Assets,Asset).


gridCliientEvents(E):-cli_memb('OpenMetaverse.GridClient',f,M),arg(3,M,Type),cli_memb(Type,e,E).


listMembs:-cli_new('System.Collections.Generic.List'(string),[int],[10],O),cli_members(O,M),member(E,M),writeq(E),nl,fail.
listMembs:-gridCliientEvents(E),writeq(E),nl,fail.
listMembs. % so pred doesnt fail

%%:-listMembs.

% coerces anything to avatar object
to_avatar(Name,Name):-cli_is_object(Name),cli_is_type(Name,'SimAvatar'),!.
to_avatar(Name,Object):-cli_is_object(Name),cli_to_str(Name,String),!,to_avatar(String,Object).
to_avatar(Name,Object):-cli_call('cogbot.Listeners.WorldObjects','GetSimAvatarFromNameIfKnown'(string),[Name],Object).

%% name_to_location_ref(start_hill_walk,O),object_color(O,C),cli_writeln(C).
%% cli_call(static('cogbot.TheOpenSims.SimImageUtils'),'ToNamedColors'('OpenMetaverse.Color4'),[struct('Color4',1,0,1,0)],Named),cli_col(Named,NamedE),cli_writeln(NamedE).
object_color(A,NamedE):-grid_object(A),cli_call(static('cogbot.TheOpenSims.SimImageUtils'),'ToNamedColors'('cogbot.TheOpenSims.SimObject'),[A],Named),cli_col(Named,NamedE).
object_color(A,NamedE):-fail,grid_object(A),cli_get(A,textures,B),cli_get(B,faceTextures,C),cli_col(C,E),E\=='@'(null),cli_get(E,rgba,CC),
  cli_call(static('cogbot.TheOpenSims.SimImageUtils'),'ToNamedColors'('OpenMetaverse.Color4'),[CC],Named),cli_col(Named,NamedE).

/*

38 ?- grid_asset(A),cli_get(A,assetType, enum('AssetType', 'Texture')),cli_get(A,id,UUID),cli_get(A,assetData,Data),Data\=='@'(null), cli_call('OpenMetaverse.Imaging.OpenJPEG','DecodeToImage'(Data,O1,O2),_),cli_get_type(O2,T),cli_writeln(T).
"System.D_rawing.Bitmap"
A = @'C#664147632',
UUID = uuid("38b86f85-2575-52a9-a531-23108d8da837"),
Data = @'C#664150288',
O1 = @'C#664150368',
O2 = @'C#664150128',
T = @'C#664150520' .

*/

%%b2img('@'(null),'@'(null)):-!.
b2img(Data,Image):-Data\=='@'(null),cli_call('OpenMetaverse.Imaging.OpenJPEG','DecodeToImage'(Data,_O1,Image),_).

uuid_to_cli_image(UUID,Image):-nonvar(UUID),!,cli_call('cogbot.Listeners.WorldObjects',['GridMaster','TextureBytesForUUID'(UUID)],Bytes),b2img(Bytes,Image).
uuid_to_cli_image(UUID,Image):-var(UUID),!,grid_asset(A),cli_get(A,assetType, enum('AssetType', 'Texture')),cli_get(A,id,UUID),cli_get(A,assetData,Data),b2img(Data,Image).
uuid_to_image_parts(UUID,Part):-grid_asset(A),cli_get(A,assetType, enum('AssetType', 'Texture')),cli_get(A,id,UUID),cli_get(A,imageStats,Parts),cli_array_to_termlist(Parts,List),List=[_|_],member(Part,List).


request_texture(UUID):-world_ref(Sys),cli_call(Sys,'StartTextureDownload'(UUID),_O).

cache_objects(_Object,DB,_PROC):-call(DB),!.
cache_objects(Object,DB,PROC):-once(PROC),cli_is_object(Object)->asserta(DB);true.

:-dynamic name_to_location_ref_cache/2.

add_cache_pred(Spec):-assert(cache_pred_db(Spec)).
expire_caches:-cache_pred_db(Spec),retractall(Spec),fail.
expire_caches.
:-add_cache_pred(name_to_location_ref_cache(_,_)).

expire_caches_120:-repeat,sleep(120),expire_caches,fail.
:-app_restore(prolog_in_thread(expire_caches_120,expire_caches_120,[detached(true)])).

name_to_location_ref(Object,Object):-cli_is_type(Object,'SimPosition'),!.
name_to_location_ref(Name,Object):-
   cache_objects(Object,name_to_location_ref_cache(Name,Object), cli_call('cogbot.Listeners.WorldObjects','GetSimPositionByName'(string),[Name],Object)).

sayTo(Speaker,ToWho,What):-to_avatar(ToWho,Listener),cli_call(Speaker,talkto('SimAvatar',string),[Listener,What],_O).

% gives you a list of all the properties on all grid objects
grid_object(X,OE):-grid_object(X),cli_get(X,infoMap,Y),cli_col(Y,PE),cli_unify(OE,PE).

:-set_prolog_flag(double_quotes,string).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
simDistance(V1,V2,D):-cli_call(V1,distance(V1,V2),D).

simAvDistance(A,C,E):-var(A),nonvar(C),!,simAvDistance(C,A,E).
simAvDistance(A,C,E):-world_avatar(A),cli_get(A,globalposition,B),world_avatar(C),A\=C,cli_get(C,globalposition,D),simDistance(B,D,E).
simObjDistance(A,C,E):-var(A),nonvar(C),!,simObjDistance(C,A,E).

%------------------------------------------------------------------------------
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% Position/Vectort calls
%
%------------------------------------------------------------------------------
vectorAdd(A1,A2,R):-cli_call(A1,add(A1,A2),R).

% already global vect!
position_to_v3d(Vect,Vect):-functor(Vect,v3d,3),!.
position_to_v3d(v3(A,B,C),Vect):-botget(['Network','CurrentSim','Handle'],S),cli_call('SimRegion','HandleLocalToGlobal'(S,v3(A,B,C)),Vect),!.
%% ?- position_to_v3d('annies haven/129.044327/128.206070/81.519630',D).
position_to_v3d(A,Vect):-atom(A),concat_atom([R,X,Y,Z|_],'/',A),!,gridclient_ref(BC),cli_call('SimRegion','GetRegionByName'(R,BC),Reg),cli_call(Reg,'LocalToGlobal'(v3(X,Y,Z)),Vect).
%% ?- position_to_v3d('129.044327/128.206070/81.519630',D).
position_to_v3d(A,Vect):-atom(A),concat_atom([X,Y,Z],'/',A),!,position_to_v3d(v3(X,Y,Z),Vect).
%% ?- position_to_v3d('CyberPunk Buddha - L',D).
position_to_v3d(A,Vect):-atom(A),!,name_to_location_ref(A,Obj),cli_get(Obj,globalposition,Vect),!.
position_to_v3d(Obj,Vect):-cli_get(Obj,globalposition,Vect),!.

position_to_v3(Obj,LV):-position_to_v3d(Obj,Vect),cli_call('SimRegion','GlobalToLocalStatic'(Vect),LV).

%% 
distance_to(A,R):-position_to_v3d(A,A2),!,botget(['Self','GlobalPosition'],A1),cli_get(A1,'Z',Z),cli_set(A2,'Z',Z), cli_call(A2,distance(A1,A2),R).

% ?- moveTo('CyberPunk Buddha - L',4,FD).

moveTo(Dest,Time,FDist):-botcmd(moveto(Dest)),botcmd(waitpos(Time,Dest)),botcmd(stopMoving),distance_to(Dest,FDist).

chat(Msg):-chat(Msg,0).
chat(Msg,Ch):-chat(Msg,Ch,'Normal').
chat(Msg,Ch,Type):-current_bot(X),cli_call(X,talk(Msg,Ch,Type),_).

%%:-prev_dir6(X),cd(X).


%------------------------------------------------------------------------------
% listing functions
%------------------------------------------------------------------------------

listS(P):-call(P,A),cli_to_str(A,S),writeq(S),fail.
listS(P):-writeq('done'(P)),nl.

listAvatars:-listS(world_avatar).
listPrims:-listS(grid_object).


