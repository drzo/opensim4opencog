
:-set_prolog_flag(double_quotes,string).

:- exists_file('mudmaker.pl') -> cd('../..') ; true.

:-['./examples/hillpeople/hillpeople.pl'].
%%:-['./examples/mudmaker/startrek/mudreader.pl'].
%%:-['./examples/mudmaker/startrek/runtest.pl'].

:-use_module(hillpeople(hillpeople)).
:-use_module(cogbot(cogrobot)).

:-set_bot_writeln_delegate(cli_fmt).

lob:-hillpeople:logon_bots.
ebt:-hillpeople:ebt.

% all say their own names
say:-botID(Name, BotID),wb_botcmd(BotID,say(Name)),fail.
say.

:-use_module(library('dialect/ifprolog')).

iki:-logon_bot('ExampleBot','Resident','pass123', "https://login.agni.lindenlab.com/cgi-bin/login.cgi","last",_).

end_of_file.


grid_object(X),cli_to_str(X,Y),atom_split(Y,"Name",[Z,ZZ|ZZZ]).
X = @('C#37218424'),
Y = "NameText | Desciption Text Box 7fac5795-9852-4653-ba43-c3d1956737f8 | floaty text (localID 2085352340)(ch0)(PrimFlagsTrue Scripted)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](IsTouchDefined)(IsSitDefined)(SimVerb \"Be Touched\")(!IsPassable)",

?- botget([masterkey],ID),botcmd(im(ID,"hello master!")).

