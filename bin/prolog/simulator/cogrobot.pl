%------------------------------------------------------------------------------
%
%  cogrobot.pl
%
%     Module for use of Cogbot in SecondLife!!!
%
% Cogbot is usually in this mode
% set_prolog_flag(double_quotes,string).
%
%------------------------------------------------------------------------------
:-module(cogrobot,
  [
   runSL/0,listMembs/0,
   worldSystem/1, worldSystem/2,
   botClient/1, botClient/2,
   botClientCall/1, botClientCall/2,
   botClientCmd/1, botClientCmd/2, botClientCmd/3,
   simObject/1, simAvatar/1, simAvDistance/3, simAsset/1, simAccount/1,
   gridCliient/1,
   resolveObjectByName/2,
   vectorAdd/3,
   distanceTo/2,
   toGlobalVect/2,
   toLocalVect/2,
   onSimEvent/3,wasSimEvent/3,
   obj2Npl/2,
   npl2Obj/2,
   chat/1,
   chat/2,
   chat/3,
   %%cli_fmt/3,
   createWritelnDelegate/2,
   createWritelnDelegate/1,
   textureIDToImage/2,
   textureIDToImageParts/2,
   requestTexture/1,
   simObjectColor/2,
   current_bot/1,
   set_current_bot/1,
   unset_current_bot/1
   ]).

:-set_prolog_flag(double_quotes,string).

atInit(Call):-term_to_atom(Call,Atom),atom_concat(Atom,'_done',Did),dynamic(Did),atInitCall(atInit(Did,Call)).
atInitCall(Call):-at_initialization(Call),Call.
atInit(Did,_Call):-Did,!.
atInit(Did,Call):-assert(Did),!,Call.


%%:- absolute_file_name('.',X),asserta(prev_dir6(X)),listing(prev_dir6).

%%:-source_location(File,_Line),file_directory_name(File, Directory),cd(Directory).


assertIfNew(Gaf):-catch(call(Gaf),_,fail),!.
assertIfNew(Gaf):-assert(Gaf).

:- assertIfNew(user:file_search_path(foreign, '.')).
:- assertIfNew(user:file_search_path(jpl_examples, 'examples/prolog')).
:- assertIfNew(user:file_search_path(jar, '.')).
:- assertIfNew(user:file_search_path(library, '.')).
:- assertIfNew(user:file_search_path(library, '..')).
:- assertIfNew(user:file_search_path(library, '../..')).
:- assertIfNew(user:file_search_path(library, '../../test')).
:- assertIfNew(user:file_search_path(test, '../test')).

%%:- use_module(library(testsupport)).
:-use_module(library(swicli)).


%:-atInit(cli_load_assembly('AForge.Imaging.dll')).
%:-atInit(cli_load_assembly('AForge.Imaging.Formats.dll')).
%------------------------------------------------------------------------------

%% load needed modules

:-use_module(library(swicli)).
%%:-use_module(library(jpl)).

%------------------------------------------------------------------------------

%% load the cogbot assembly
:-dynamic(loadedCogbotAssembly/0).
loadCogbotAssembly:-loadedCogbotAssembly,!.
loadCogbotAssembly:-assert(loadedCogbotAssembly),current_prolog_flag(address_bits,32) -> cli_load_assembly('Cogbot32.exe') ; cli_load_assembly('Cogbot.exe').
:-atInit(loadCogbotAssembly).

%% cache the type names
% prevents us having to use long names for things like SimAvatar
%
cacheShortNames:-
  cli_members('cogbot.TheOpenSims.SimAvatar',_),
  cli_members('cogbot.Listeners.WorldObjects',_),
  cli_members('OpenMetaverse.Primitive',_).

:-atInit(cacheShortNames).
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

:-atInit(add_layouts).

%------------------------------------------------------------------------------
% much code uses botClient(Me) like botClientCmd and say/n  
%
% so current_bot(Me) is checked by botClient(Me) and is a thread_local predicate
%  
% usage:
%  set_current_bot(Me), .....  unset_current_bot(Me)
%
% bug prone antipattern, but it's supported:
%  current_bot(OldBot), set_current_bot(Me) .....   set_current_bot(OldBot)
% though current_bot(OldBot) may throw if not bot is set
% ------------------------------------------------------------------------------
%
cogbot_throw(Error):-throw(cogbot_user_error(Error)).

:-dynamic current_bot_db/2.
current_bot(BotID):-thread_self(TID),current_bot_db(TID,BotID),!.
current_bot(BotID):-clientManager(Man),cli_get(Man,'LastBotClient',BotID).
set_current_bot(BotID):-thread_self(TID),retractall(current_bot_db(TID,_)),asserta(current_bot_db(TID,BotID)).
unset_current_bot(BotID):-thread_self(TID),current_bot_db(TID,OLD), 
    (OLD=BotID -> retract(current_bot_db(TID,OLD)) ; cogbot_throw(unset_current_bot(tid(TID),used(BotID),expected(OLD)))).


%------------------------------------------------------------------------------
% object getter functions
%
% cli_get is a field accessor that says use the property, if you can't
% find that use the _raw field and return.
%
% So this is 'get the GridMaster property from the static
% cogbot.Listeners.WorldObjects and return in Sys'
%
% ------------------------------------------------------------------------------
%
%

worldSystem(Sys):-cli_get('cogbot.Listeners.WorldObjects','GridMaster',Sys).

% gets some property of the GridMaster
%  worldSystem(+Field, -Value)
%
worldSystem(Field,Value):-worldSystem(Sys),cli_get(Sys,Field,Value).

%% get each SimObject
% this is every primitive, linked or not, as a complex term
% it's a partially marshalled object from the simulator
% cli_col iterates thru elements
simObject(Ele):-worldSystem('SimObjects',Objs),cli_col(Objs,Ele).

simRootObject(Ele):-worldSystem('SimRootObjects',Objs),cli_col(Objs,Ele).

% the above is simpler form of:   simObject(Ele):-worldSystem(Sys),cli_get(Sys,'SimObjects',Obj),cli_col(Obj,Ele).

%% get each SimAvatar
%
% this is the set of av's that are known to the simulator, they are only
% actually present if they have a prim
%
% simObject and simAvatar handle the complexity of sim crossings
%
simAvatar(Ele):-simAccount(Ele),cli_get(Ele,hasprim,@(true)).

%
% a simAccount/1 is like simAvatar (they are avatars known about in system..
%    including friends not logged in)
%
simAccount(Ele):-worldSystem('SimAvatars',Objs),cli_col(Objs,Ele).

%
% a simRegion predicate with simParcels (cli_ not done yet)
%
simRegion(Ele):-cli_get('cogbot.TheOpenSims.SimRegion','CurrentRegions',Objs),cli_col(Objs,Ele).

simParcel(Ele):-simRegion(Sim),cli_get(Sim,parcels,Objs),cli_col(Objs,Ele).

%% get the clientManager Instance
%%
%  A class that holds all the static singletons
%  A Client is a logged on acct in this context
%
%  Clientmanager binds radegast to the Client
%  botconfig is run from Clientmanager
clientManager(SingleInstance):-cli_get('cogbot.ClientManager','SingleInstance',SingleInstance).

%% get the botClient Instance
% this only unifies once, someday there will be a BotClients
botClient(Obj):-catch(current_bot(Obj),_,fail),!.
botClient(Obj):-clientManager(Man),cli_get(Man,'LastBotClient',Obj).

% given an object and a property returns value for the avatar
%
% walks down property tree
% botClient([name,length,X)
% botClient([name,length],X)
% botClient([position,z],X)
%
% a.b.c.d
% botClient([a.b.c.d],X).
% botClient([a,b,c,d],X).
% botClient([a,b,c,d],X). = mybot.a.b.c.d
% prolog botClient([a,b,c,d],X). = c# object X = myBotClient.a.b.c.d
% botClient(['Inventory','Store',rootfolder,name],Y).
% Y = "My Inventory".
%
% botClient(['Inventory','Store',rootnode,nodes,values],Y),
%	findall(S,(cli_col(Y,Z),cli_to_str(Z,S)),L),writeq(L).
%	["Scripts","Photo Album","*MD* Brown Leather Hat w/Bling",
%	"Body Parts","Notecards","Objects","Clothing","Landmarks","Textures",
%     "Gestures","boxed fem_talk","Calling Cards","Animations","Sounds",
%	"Trash","Lost And Found"]
%       Y = @'C#720558400'
%
%       finds all grandchildren
%       botClient(['Inventory','Store',rootnode,nodes,values],Y),
%	     findall(S,(cli_col(Y,Z),cli_get(Z,'children',GC),
%	     cli_collecton(GC,'children',GCReal),cli_to_str(GCReal,S)),L),writeq(L).
%

botClient([P|N],Value):-!,botClient(Obj),cli_get(Obj,[P|N],Value).
botClient(Property,Value):-botClient(Obj),cli_get(Obj,Property,Value),!.

% a way to call a method on c#
% cli_call('System',printf(32),Y).
botClientCall(Call):-botClient(BotID),at_botClientCall(BotID,Call).
botClientCall(Call,Res):-botClient(BotID),at_botClientCall(BotID,Call,Res).

at_botClientCall(BotID,Call):-at_botClientCall(BotID,Call,Res),cli_writeln(Res).
at_botClientCall(BotID,[P|N],Value):-!,cli_get(BotID,P,Mid),cli_get(Mid,N,Value).
at_botClientCall(BotID,Property,Value):-cli_call(BotID,Property,Value).


% wrappered execute command in a convenience pred
% botClientCmd(say("hi"))
%
botClientCmd(In):-botClient(BotID),at_botClientCmd(BotID,In),!.
botClientCmd(In,Out):-botClient(BotID),at_botClientCmd(BotID,In,Out),!.
botClientCmd(Str,WriteDelegate,Out):-botClient(BotID),at_botClientCmd(BotID,Str,WriteDelegate,Out).

at_botClientCmd(BotID,In):-at_botClientCmd(BotID,In,Out),cli_writeln(Out),!.
at_botClientCmd(BotID,StrIn,Out):-args_to_string(StrIn,Str),at_botClientCmd(BotID,Str,cli_fmt(botClientCmd),Out).
at_botClientCmd(BotID,StrIn,WriteDelegate,Out):-args_to_string(StrIn,Str),cli_call(BotID,executeCommand(Str,BotID,WriteDelegate),Out).

args_to_string(StrIn,StrIn):- (atom(StrIn);string(StrIn)),!.
args_to_string([C|Cmd],Out):-toStringableArgs([C|Cmd],CCmd),!,concat_atom(CCmd,' ',Str),args_to_string(Str,Out).
args_to_string(C,Out):-compound(C),!,C=..[F|A],listifyFlat([F|A],FL),args_to_string(FL,Out).
args_to_string(StrIn,StrIn).

toStringableArgs(Var,Var):-var(Var),!.
toStringableArgs([C|Cmd],[A|Amd]):-toStringableArg(C,A),toStringableArgs(Cmd,Amd).
toStringableArgs(CCmd,CCmd).

toStringableArg(Var,Var):-var(Var),!,throw(toStringableArgVar(Var)).
toStringableArg(v3d(X,Y,Z),A):-concat_atom([X,Y,Z],'/',A).
toStringableArg(Var,Var).

% helper pred for botClientCmd
listifyFlat([],[]):-!.
listifyFlat([H|T],HT):-!,listifyFlat(H,HL),listifyFlat(T,TL),!,append(HL,TL,HT).
listifyFlat(C,FA):-functor(C,F,1),!,C=..[F,A],!,listifyFlat(A,FA).
listifyFlat(v3d(X,Y,Z),[v3d(X,Y,Z)]).
listifyFlat(v3(X,Y,Z),[v3(X,Y,Z)]).
listifyFlat(C,FA):-compound(C),!,C=..[F|A],!,listifyFlat([F|A],FA).
listifyFlat(C,[C]).

%% get the gridCliient Instance
%  libOMV's version of gridCliient, in case you want the direct one
gridCliient(Obj):-botClient(BC),cli_get(BC,'gridCliient',Obj).


%------------------------------------------------------------------------------
% create a writeline delegate
%------------------------------------------------------------------------------
createWritelnDelegate(WID):-createWritelnDelegate(cli_fmt(cogrobot),WID).
createWritelnDelegate(WriteDelegate,WID):-cli_new_delegate('MushDLR223.ScriptEngines.OutputDelegate',WriteDelegate,WID).


%------------------------------------------------------------------------------
% listing functions
%------------------------------------------------------------------------------

listS(P):-call(P,A),cli_to_str(A,S),writeq(S),fail.
listS(P):-writeq('done'(P)),nl.

listAvatars:-listS(simAvatar).
listPrims:-listS(simObject).


%------------------------------------------------------------------------------
% event handler functions
%------------------------------------------------------------------------------
robotToString(C,C):-var(C).
robotToString([],[]).
robotToString([A|B],[AA|BB]):-robotToString(A,AA),robotToString(B,BB).
robotToString(Obj,array(ArrayS)):-Obj='@'(_O), cli_is_type(Obj,'System.Array'),cli_array_to_termlist(Obj,Array),!,robotToString(Array,ArrayS).
robotToString(Obj,list(ArrayS)):-Obj='@'(_O), cli_is_type(Obj,'System.Collections.Generic.IList'('cogbot.NamedParam')),cli_call(Obj,'ToArray',[],Array),robotToString(Array,ArrayS).
robotToString(Obj,enumr(ArrayS)):-Obj='@'(_O), cli_is_type(Obj,'System.Collections.IEnumerable'),cli_array_to_termlist(Obj,Array),robotToString(Array,ArrayS).
robotToString(C,AS):-compound(C),C=..[F|Args],not(member(F,['@'])),robotToString(Args,ArgS),AS=..[F|ArgS].
robotToString(C,AS):-cli_to_str(C,AS).

nop(_).


%% print some events
%%%onSimEvent(_A,_B,_C):-!. % comment out this first line to print them
:-dynamic(wasSimEvent/3).
onSimEvent(_A,B,C):-contains_var("On-Log-Message",a(B,C)),!.
onSimEvent(_A,B,C):-contains_var('DATA_UPDATE',a(B,C)),!.
onSimEvent(A,B,C):-!,nop(assertz(wasSimEvent(A,B,C))),!,robotToString(C,AS),!,writeq(onSimEvent(AS)),nl.


%% clearSimEvent(NumToLeave):- predicate_property(wasSimEvent(_,_,_),number_of_clauses(N)),Remove is N-Num, (Remove<=0->true;( ... )).
clearSimEvent(Num):- predicate_property(wasSimEvent(_,_,_),number_of_clauses(N)),Num>=N,!.
clearSimEvent(Num):- retract(wasSimEvent(_,_,_)),predicate_property(wasSimEvent(_,_,_),number_of_clauses(N)),Num >= N,!.
clearSimEvent(_Num).

%% Every minute trim EventLog to 1000 entries
clearEvents:-repeat,sleep(60),clearSimEvent(1000),fail.
:-atInit(thread_create(clearEvents,_,[])).

%%:-module_transparent(onFirstBotClient/2).

%% on first bot Client created register the global event handler
user:onFirstBotClient(A,B):- %%%attach_console,trace,
 botClient(Obj),
  % uncomment the next line if you want all commands to run thru the universal event handler
   %%cli_add_event_handler(Obj,'EachSimEvent',onSimEvent(_,_,_)),
   cli_to_str(onFirstBotClient(A-B-Obj),Objs),writeq(Objs),nl.

%% register onFirstBotClient
registerOnFirstBotClient:- cli_add_event_handler('cogbot.ClientManager','BotClientCreated',onFirstBotClient(_,_)).
%%:-atInit(registerOnFirstBotClient).

%------------------------------------------------------------------------------
% start Radegast!
%------------------------------------------------------------------------------
:-dynamic(ranSL).

runSL:-ranSL,!.
% this is so you can reconsult this file without restarting radegast
runSL:-asserta(ranSL),!,
   cli_set('MushDLR223.Utilities.DLRConsole','NoConsoleVisible','@'(true)),
   cli_set('ABuildStartup.Program','UseApplicationExit','@'(false)),
   cli_set('cogbot.ClientManager','noGUI','@'(true)),
   cli_call('ABuildStartup.Program','Main',[],_).

% assertIfNew is assert a new grounded atomic fact only if the predicate
% was previously undefined

%:-retractall(cli_SubProperty(_,_)).
:-assertIfNew(cli_SubProperty('cogbot.TheOpenSims.SimAvatar','ProfileProperties')).
:-assertIfNew(cli_SubProperty('cogbot.TheOpenSims.SimAvatar','AvatarInterests')).
:-assertIfNew(cli_SubProperty('cogbot.TheOpenSims.SimAvatar','FriendshipInfo')).
:-assertIfNew(cli_SubProperty('cogbot.TheOpenSims.SimObject','Prim')).
:-assertIfNew(cli_SubProperty('cogbot.TheOpenSims.SimObject','Properties')).


obj2Npl(O,npl(66,O)).
npl2Obj(npl(66,O),O).

registerNamedParamRecomposer:-!.
registerNamedParamRecomposer:-cli_to_from_recomposer('System.Collections.Generic.IList'('cogbot.NamedParam'),'npl'(_,_),obj2Npl,npl2Obj).

:-atInit(registerNamedParamRecomposer).

%------------------------------------------------------------------------------
% CLR Introspection of event handlers
%------------------------------------------------------------------------------
simAsset(Asset):-  cli_get('cogbot.TheOpenSims.SimAssetStore','SimAssets',Assets),cli_col(Assets,Asset).


gridCliientEvents(E):-cli_memb('OpenMetaverse.GridClient',f,M),arg(3,M,Type),cli_memb(Type,e,E).


listMembs:-cli_new('System.Collections.Generic.List'(string),[int],[10],O),cli_members(O,M),member(E,M),writeq(E),nl,fail.
listMembs:-gridCliientEvents(E),writeq(E),nl,fail.
listMembs. % so pred doesnt fail

%%:-listMembs.

% coerces anything to avatar object
resolveAvatar(Name,Name):-cli_is_object(Name),cli_is_type(Name,'SimAvatar'),!.
resolveAvatar(Name,Object):-cli_is_object(Name),cli_to_str(Name,String),!,resolveAvatar(String,Object).
resolveAvatar(Name,Object):-cli_call('cogbot.Listeners.WorldObjects','GetSimAvatarFromNameIfKnown'(string),[Name],Object).

%% resolveObjectByName(start_hill_walk,O),simObjectColor(O,C),cli_writeln(C).
%% cli_call(static('cogbot.TheOpenSims.SimImageUtils'),'ToNamedColors'('OpenMetaverse.Color4'),[struct('Color4',1,0,1,0)],Named),cli_col(Named,NamedE),cli_writeln(NamedE).
simObjectColor(A,NamedE):-simObject(A),cli_call(static('cogbot.TheOpenSims.SimImageUtils'),'ToNamedColors'('cogbot.TheOpenSims.SimObject'),[A],Named),cli_col(Named,NamedE).
simObjectColor(A,NamedE):-fail,simObject(A),cli_get(A,textures,B),cli_get(B,faceTextures,C),cli_col(C,E),E\=='@'(null),cli_get(E,rgba,CC),
  cli_call(static('cogbot.TheOpenSims.SimImageUtils'),'ToNamedColors'('OpenMetaverse.Color4'),[CC],Named),cli_col(Named,NamedE).

/*

38 ?- simAsset(A),cli_get(A,assetType, enum('AssetType', 'Texture')),cli_get(A,id,UUID),cli_get(A,assetData,Data),Data\=='@'(null), cli_call('OpenMetaverse.Imaging.OpenJPEG','DecodeToImage'(Data,O1,O2),_),cli_get_type(O2,T),cli_writeln(T).
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

textureIDToImage(UUID,Image):-nonvar(UUID),!,cli_call('cogbot.Listeners.WorldObjects',['GridMaster','TextureBytesForUUID'(UUID)],Bytes),b2img(Bytes,Image).
textureIDToImage(UUID,Image):-var(UUID),!,simAsset(A),cli_get(A,assetType, enum('AssetType', 'Texture')),cli_get(A,id,UUID),cli_get(A,assetData,Data),b2img(Data,Image).
textureIDToImageParts(UUID,Part):-simAsset(A),cli_get(A,assetType, enum('AssetType', 'Texture')),cli_get(A,id,UUID),cli_get(A,imageStats,Parts),cli_array_to_termlist(Parts,List),List=[_|_],member(Part,List).


requestTexture(UUID):-worldSystem(Sys),cli_call(Sys,'StartTextureDownload'(UUID),_O).

resolveObjectByName(Name,Object):-cli_call('cogbot.Listeners.WorldObjects','GetSimPositionByName'(string),[Name],Object).

sayTo(Speaker,ToWho,What):-resolveAvatar(ToWho,Listener),cli_call(Speaker,talkto('SimAvatar',string),[Listener,What],_O).

% gives you a list of all the properties
simObject(X,OE):-simObject(X),cli_get(X,infoMap,Y),cli_col(Y,PE),cli_unify(OE,PE).

:-set_prolog_flag(double_quotes,string).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
simDistance(V1,V2,D):-cli_call(V1,distance(V1,V2),D).

simAvDistance(A,C,E):-var(A),nonvar(C),!,simAvDistance(C,A,E).
simAvDistance(A,C,E):-simAvatar(A),cli_get(A,globalposition,B),simAvatar(C),A\=C,cli_get(C,globalposition,D),simDistance(B,D,E).
simObjDistance(A,C,E):-var(A),nonvar(C),!,simObjDistance(C,A,E).
simObjDistance(A,C,E):-simObject(A),cli_get(A,globalposition,B),simObject(C),A\=C,cli_get(C,globalposition,D),simDistance(B,D,E).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% Position/Vectort calls
%
%------------------------------------------------------------------------------
vectorAdd(A1,A2,R):-cli_call(A1,add(A1,A2),R).

% already global vect!
toGlobalVect(Vect,Vect):-functor(Vect,v3d,3),!.
toGlobalVect(v3(A,B,C),Vect):-botClient(['Network','CurrentSim','Handle'],S),cli_call('SimRegion','HandleLocalToGlobal'(S,v3(A,B,C)),Vect),!.
%% ?- toGlobalVect('annies haven/129.044327/128.206070/81.519630',D).
toGlobalVect(A,Vect):-atom(A),concat_atom([R,X,Y,Z|_],'/',A),!,gridCliient(BC),cli_call('SimRegion','GetRegionByName'(R,BC),Reg),cli_call(Reg,'LocalToGlobal'(v3(X,Y,Z)),Vect).
%% ?- toGlobalVect('129.044327/128.206070/81.519630',D).
toGlobalVect(A,Vect):-atom(A),concat_atom([X,Y,Z],'/',A),!,toGlobalVect(v3(X,Y,Z),Vect).
%% ?- toGlobalVect('CyberPunk Buddha - L',D).
toGlobalVect(A,Vect):-atom(A),!,resolveObjectByName(A,Obj),cli_get(Obj,globalposition,Vect),!.
toGlobalVect(Obj,Vect):-cli_get(Obj,globalposition,Vect),!.

toLocalVect(Obj,LV):-toGlobalVect(Obj,Vect),cli_call('SimRegion','GlobalToLocalStatic'(Vect),LV).

%% 
distanceTo(A,R):-toGlobalVect(A,A2),!,botClient(['Self','GlobalPosition'],A1),cli_call(A2,distance(A1,A2),R).

% ?- moveTo('CyberPunk Buddha - L',4,FD).

moveTo(Dest,Time,FDist):-botClientCmd(moveto(Dest)),botClientCmd(waitpos(Time,Dest)),botClientCmd(stopMoving),distanceTo(Dest,FDist).

chat(Msg):-chat(Msg,0).
chat(Msg,Ch):-chat(Msg,Ch,'Normal').
chat(Msg,Ch,Type):-botClient(X),cli_call(X,talk(Msg,Ch,Type),_).

%%:-prev_dir6(X),cd(X).
