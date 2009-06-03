% ex: set ts=2 noet:
% $Id$

-module(char).
-author("pizza@parseerror.com").
-export([
	ispunct/1,
	isalnum/1,
	isdigit/1,
	isctrl/1,
	isspace/1,
	isprint/1
	]).

ispunct(Char) ->
	((Char >= $!) and (Char =< $/)) or
	((Char >= $:) and (Char =< $@)) or
	((Char >= $[) and (Char =< $`)) or
	((Char >= ${) and (Char =< $~)).

isalnum(Char) ->
	((Char >= $a) and (Char =< $z)) or
	((Char >= $A) and (Char =< $Z)) or
	isdigit(Char).

isdigit(Char) ->
	((Char >= $0) and (Char =< $9)).

isspace(Char) ->
	(Char == 32) or % space
	(Char == 10) or % \r
	(Char == 13) or % \n
	(Char ==  9).   % tab

isctrl(Char) ->
	((Char =< 31) or (Char >= 128)).

isprint(Char) ->
  isalnum(Char) or
	isspace(Char) or
	ispunct(Char) or
	(Char >= 128). % try to support Unicode

