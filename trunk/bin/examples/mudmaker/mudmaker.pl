
:-set_prolog_flag(double_quotes,string).

:- exists_file('mudmaker.pl') -> cd('../..') ; true.

:-['./examples/hillpeople/hillpeople.pl'].
:-['./examples/mudmaker/startrek/mudreader.pl'].
:-['./examples/mudmaker/startrek/runtest.pl'].

:-use_module(hillpeople(hillpeople)).
:-use_module(cogbot(cogrobot)).

end_of_file.

?- botClient([masterkey],ID),botClientCmd(im(ID,"hello master!")).

