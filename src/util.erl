% ex: set ts=2 noet:
% $Id$
% stuff i wish existed in the erlang standard libraries

-module(util).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		relpath/2,
		hd/2,
		min/2,
		join/2, j/1,
		rtrim/2, rtrim/1,
		ltrim/3, ltrim/2, ltrim/1,
		trim/2, trim/1,
		split/3,
		strsplit/2,
		tokens/3,
		show/1,
		find_or/3,
		nth/3,
		unescape/1,
		lines/1,
		utime_diffsec/2,
		readlines/1,
		ensure_dir/1,
		hex/1,
		hex/2,
		islower/1,
		isupper/1,
		isdigit/1,
		isalpha/1,
		isalnum/1,
		shell_escape/1,
		timestamp/0
	]).
-import(test).

test() ->
	test:unit(util,
		[
			{ join, 		test_join()				},
			{ rtrim, 		test_rtrim()			},
			{ ltrim, 		test_ltrim()			},
			{ show, 		test_show()				},
			{ unescape, test_unescape()		},
			{ nth, 			test_nth()				},
			{ split, 		test_split()			},
			{ tokens,		test_tokens()			},
			{	strsplit,	test_strsplit()		},
			{	hex,			test_hex()				},
			{	hex,			test_hex2()				},
			{	shell_escape, test_shell_escape()		}
		]).

hd([], Else) -> Else;
hd([H|_], _) -> H.

% construct an absolute path from a relative one
% used like so: realpath("foo.erl", "rel/a/tive")
relpath(File, Path) ->
	case code:where_is_file(File) of
		non_existing -> Path;
		Where ->
			string:substr(Where, 1, string:rstr(Where, "/")) ++ Path
	end.

lines(Str) ->
	string:tokens(Str, "\r\n").

min(A, B) ->
	if
		A < B -> A;
		true -> B
		end.

join(_, []) ->
	[];
join(_, [Head]) when is_integer(Head)->
	[ Head ];
join(_, [Head]) ->
	Head;
join(Glue, [Head|Tail]) when is_integer(Head) ->
	integer_to_list(Head) ++ Glue ++ join(Glue, Tail);
join(Glue, [Head|Tail]) when is_tuple(Head) ->
	tuple_to_list(Head) ++ Glue ++ join(Glue, Tail);
join(Glue, [Head|Tail]) ->
	Head ++ Glue ++ join(Glue, Tail).

test_join() ->
	[
		{ [ "", "" ], "" },
		{ [ ":", "" ], "" },
		{ [ "", ["a"]], "a" },
		{ [ "", ["a","b"] ], "ab" },
		{ [ " ", ["a","b"] ], "a b" }
		%{ [ " ", [$1,$2] ], "1 2" } % strange results i don't understand
	].

% wrapper for most common join
j(X) -> join(" ", X).

rtrim("", _) -> "";
rtrim(Word, Trim) when is_list(Word) ->
	Char = lists:last(Word),
	case Trim == Char of
		true -> rtrim(lists:sublist(Word, length(Word)-1), Trim);
		false -> Word
		end.

rtrim(Str) ->
	rtrim(Str, 32).

test_rtrim() ->
	[
		{ [ "" ], "" },
		{ [ " " ], "" },
		{ [ "a" ], "a" },
		{ [ "aa" ], "aa" },
		{ [ "a " ], "a" },
		{ [ " a " ], " a" }
	].

ltrim(Str) ->
	ltrim(Str, 32).

ltrim([], _) -> [];
ltrim([H|T], Chr) when H == Chr ->
	ltrim(T, Chr);
ltrim(Str, _) ->
	Str.

ltrim([], _, _) -> [];
ltrim(Str, _, 0) -> Str;
ltrim([H|T], Chr, Cnt) when H == Chr ->
	ltrim(T, Chr, Cnt-1);
ltrim(Str, _, _) ->
	Str.

test_ltrim() ->
	[
		{ [ "" ], "" },
		{ [ " " ], "" },
		{ [ "  " ], "" },
		{ [ "a" ], "a" },
		{ [ "aa" ], "aa" },
		{ [ " aa" ], "aa" },
		{ [ "  aa" ], "aa" },
		{ [ " a a" ], "a a" },
		{ [ "" ], "" }
	].

trim(Str) ->
	ltrim(rtrim(Str)).

trim([], _) -> [];
trim(Str, Chr) ->
	ltrim(rtrim(Str, Chr), Chr).

split(Str, Chr, Limit) ->
	split_(Str, Chr, Limit, [], []).

split_([], _, _, List, []) ->
	List;
split_([], _, _, List, Unsplit) ->
	List ++ [ Unsplit ];
split_(Rest, _, 0, List, Unsplit) ->
	List ++ [ Unsplit ++ Rest ];
split_(Rest, _, 1, List, Unsplit) ->
	List ++ [ Unsplit ++ Rest ];
split_([Chr|Rest], Chr, Limit, List, Unsplit) ->
	split_(Rest, Chr, Limit-1, List ++ [Unsplit], []);
split_([NoMatch|Rest], Chr, Limit, List, Unsplit) ->
	split_(Rest, Chr, Limit, List, Unsplit ++ [ NoMatch ]).

test_split() ->
	[
		{ [ "", $:, 0 ], [] },
		{ [ "", $:, 1 ], [] },
		{ [ "a", $:, 0 ], ["a"] },
		{ [ "a", $:, 1 ], ["a"] },
		{ [ "a:", $:, 1 ], ["a:"] },
		{ [ "a:b", $:, 1 ], ["a:b"] },
		{ [ "a:b", $:, 2 ], ["a","b"] },
		{ [ "a:b:c", $:, 2 ], ["a","b:c"] },
		{ [ "a:b:c", $:, 3 ], ["a","b","c"] },
		{ [ "a,b,c", $:, 3 ], ["a,b,c"] }
	].


% split Str by any of the characters appearing in Split, producing a
% list of results at most Limit items in length
tokens(Str, Split, Limit) ->
	tokens_(Str, Split, Split, Limit, [], []).

tokens_([], _, _, _, List, []) ->
	% empty string
	List;
tokens_([], _, _, _, List, Unsplit) ->
	% end of non-empty string
	List ++ [ Unsplit ];
tokens_(Rest, _, _, 0, List, Unsplit) ->
	% limit reached
	List ++ [ Unsplit ++ Rest ];
tokens_(Rest, _, _, 1, List, Unsplit) ->
	% limit reached
	List ++ [ Unsplit ++ Rest ];
tokens_([StrH|StrT], Split, [], Limit, List, Unsplit) ->
	% end of split, no match; char is unsplit, start over with split on rest of string
	tokens_(StrT, Split, Split, Limit, List, Unsplit ++ [StrH]);
tokens_([StrH|StrT], Split, [SplitH|_], Limit, List, []) when StrH =:= SplitH ->
	% split match with empty unsplit, ditch unsplit, start over on rest of string
	tokens_(StrT, Split, Split, Limit, List, []);
tokens_([StrH|StrT], Split, [SplitH|_], Limit, List, Unsplit) when StrH =:= SplitH ->
	% split match, save unsplit, start over on rest of string
	tokens_(StrT, Split, Split, Limit-1, List ++ [Unsplit], []);
tokens_([StrH|_]=Str, Split, [SplitH|SplitT], Limit, List, Unsplit) when StrH /= SplitH ->
	% no split match, try more split chars on same string
	tokens_(Str, Split, SplitT, Limit, List, Unsplit).

test_tokens() ->
	[
		{ [ "", "", 0 ], [] },
		{ [ "", "", 1 ], [] },
		{ [ "", ":", 0 ], [] },
		{ [ "", ":", 1 ], [] },
		{ [ "a", ":", 0 ], ["a"] },
		{ [ "a", ":", 1 ], ["a"] },
		{ [ "a:", ":", 1 ], ["a:"] },
		{ [ "a:b", ":", 1 ], ["a:b"] },
		{ [ "a:b", ":", 2 ], ["a","b"] },
		{ [ "a:b:c", ":", 2 ], ["a","b:c"] },
		{ [ "a:b:c", ":", 3 ], ["a","b","c"] },
		{ [ "a,b,c", ":", 3 ], ["a,b,c"] },
		{ [ "a,b:c", ":,", 0 ], ["a,b:c"] },
		{ [ "a,b:c", ":,", 1 ], ["a,b:c"] },
		{ [ "a,b:c", ":,", 2 ], ["a","b:c"] },
		{ [ "a,b:c", ":,", 3 ], ["a","b","c"] },
		{ [ "a,b:c", ":,", 4 ], ["a","b","c"] },
		{ [ ":a,b:c", ":,", 4 ], ["a","b","c"] },
		{ [ ":pizza_!~pizza_@a.b.c.d PRIVMSG #foo :mod_pizza_: whoops is a:, b", ": ", 4 ],
			["pizza_!~pizza_@a.b.c.d", "PRIVMSG", "#foo", ":mod_pizza_: whoops is a:, b"] }
	].

% show X in the most human-friendly way
show(X) ->
	lists:flatten(io_lib:format("~p", [X])).

test_show() ->
	[
		{ [ "" ], "[]" },
		{ [ nil ], "nil" },
		{ [ [] ], "[]" },
		{ [ 0 ], "0" },
		{ [ "a" ], "\"a\"" },
		{ [ [1,2,3] ], "[1,2,3]" },
		{ [ "abc" ], "\"abc\"" }
	].

unescape([]) ->
	[];
unescape([_|_]=Str) ->
	unescape_(Str, []).

unescape_([], Done) ->
	Done;
unescape_([H], Done) ->
	Done ++ [H];
unescape_([H,T|Rest], Done) ->
	if
		(92 == H) -> % \
			unescape_(Rest, Done ++ [T]);
		true ->
			unescape_([T] ++ Rest, Done ++ [H])
		end.

test_unescape() ->
	[
		{ ["\\\"hello\\\"" ], "\"hello\""},
		{ [ "\\\"a\\\n\\\b\\\"" ], "\"a\n\b\""}
	].

% return a dict entry if found or something else instead
find_or(Key, Dict, Or) ->
	case dict:find(Key, Dict) of
		error -> Or;
		{ok, Val} -> Val
	end.

nth(0, _, Else) ->
	Else;
nth(N, List, Else) when N > 0 ->
	if
		length(List) < N -> Else;
		true -> lists:nth(N, List)
	end.

test_nth() ->
	[
		{ [ 0, [], nil ], 			nil },
		{ [ 1, [], nil ], 			nil },
		{ [ 100, [], nil ], 		nil },
		{ [ 0, [4,5,6], nil ], 	nil },
		{ [ 1, [4,5,6], nil ], 	4 },
		{ [ 2, [4,5,6], nil ], 	5 },
		{ [ 3, [4,5,6], nil ], 	6 },
		{ [ 4, [4,5,6], nil ], 	nil }
	].

% calculate the difference in seconds between two
% erlang:universaltime() calls
% NOTE: there are tons of special cases where this
% will produce the wrong answer, oh well, close enough.
utime_diffsec({{Y1,M1,D1},{H1,I1,S1}}=_Then,
							{{Y2,M2,D2},{H2,I2,S2}}=_Now) ->
	% {{2009,5,25},{4,58,17}}
	((S2 - S1) +
	((I2 - I1) * 60) +
	((H2 - H1) * 60 * 60) +
	((D2 - D1) * 60 * 60 * 24) +
	((M2 - M1) * 60 * 60 * 24 * 31) +
	((Y2 - Y1) * 60 * 60 * 24 * 365)).

readlines(FileName) ->
	case file:open(FileName, [read]) of
		{ok, Dev} -> get_all_lines(Dev, []);
		{error, _Why} -> []
	end.

get_all_lines(Device, Accum) ->
	case io:get_line(Device, "") of
		eof  -> file:close(Device), Accum;
		Line -> get_all_lines(Device, Accum ++ [Line])
	end.

ensure_dir(Dir) ->
	case file:make_dir(Dir) of
		ok -> true;
		{error, eexist} -> true;
		Whoops -> Whoops
	end.

% split a string by another string
strsplit([], _)									-> [[]];
strsplit(A, A)									-> [[]];
strsplit(Str, Spl)							-> strsplit(Str, Spl, [], []).
strsplit([], _, L, [])					-> L; % empty string
strsplit([], _, L, U)						-> L ++ [U]; % end of non-empty string
strsplit([H|T], [], L, _) 			-> strsplit(T, [], L ++ [[H]], []);
strsplit([H|T]=Str, Spl, L, U)	->
	case lists:prefix(Spl, Str) of
		false -> strsplit(T, Spl, L, U ++ [H]);
		true -> strsplit(lists:nthtail(length(Spl), Str), Spl, L ++ [U], [])
	end.

test_strsplit() ->
	[
		{ [ "", "" ], [ "" ] },
		{ [ "", "x" ], [ "" ] },
		{ [ "a", "a" ], [ "" ] },
		{ [ "ab", "" ], [ "a","b" ] },
		{ [ "ab", "ab" ], [ "" ] },
		{ [ "ab", "abc" ], [ "ab" ] },
		{ [ "ab", "a" ], [ "","b" ] }, % hmm
		{ [ "a-b", "-" ], [ "a","b" ] },
		{ [ "a--b", "--" ], [ "a","b" ] },
		{ [ "a---b", "---" ], [ "a","b" ] },
		{ [ "a-b-c", "-" ], [ "a","b","c" ] },
		{ [ "", " | " ], [ "" ] },
		{ [ " ", " | " ], [ " " ] },
		{ [ " |", " | " ], [ " |" ] },
		{ [ " | ", " | " ], [ "" ] },
		{ [ "a | ", " | " ], [ "a" ] },
		{ [ "a | b", " | " ], [ "a", "b" ] },
		{ [ "a | b ", " | " ], [ "a", "b " ] },
		{ [ "a | b |", " | " ], [ "a", "b |" ] },
		{ [ "a | b | ", " | " ], [ "a", "b" ] },
		{ [ "|x|", " | " ], [ "|x|" ] },
		{ [ " |x|", " | " ], [ " |x|" ] },
		{ [ "  |x|", " | " ], [ "  |x|" ] },
		{ [ "  |x| ", " | " ], [ "  |x| " ] },
		{ [ "  |x|  ", " | " ], [ "  |x|  " ] },
		{ [ "", "" ], [ "" ] }
	].

% produce hex representation of digit
hex(0) -> "0";
hex(N) -> hex_(N, []).
hex_(0, Acc) -> Acc;
hex_(N, Acc) ->
	Hex = lists:nth((N band 15) + 1, "0123456789abcdef"),
	hex_(N bsr 4, [Hex] ++ Acc).

test_hex() ->
	[
		{ [   0 ], "0"  },
		{ [   1 ], "1"   },
		{ [   9 ], "9"   },
		{ [  10 ], "A"   },
		{ [  11 ], "B"   },
		{ [  12 ], "C"   },
		{ [  13 ], "D"   },
		{ [  14 ], "E"   },
		{ [  15 ], "F"   },
		{ [  16 ], "10"  },
		{ [ 255 ], "FF"  },
		{ [ 256 ], "100" }
	].

% produce dec->hex to a certain size
hex(_, 0) -> "";
hex(N, Places) when Places > 0 -> hex_(N, Places, []).
hex_(_, 0, Acc) -> Acc;
hex_(N, Places, Acc) ->
	Hex = lists:nth((N band 15)+1, "0123456789abcdef"),
	hex_(N bsr 4, Places-1, [Hex] ++ Acc).

test_hex2() ->
	[
		{ [   0, 0 ], ""    },
		{ [   1, 0 ], ""    },
		{ [   0, 1 ], "0"   },
		{ [   0, 2 ], "00"  },
		{ [   0, 3 ], "000" },
		{ [   1, 1 ], "1"   },
		{ [   9, 1 ], "9"   },
		{ [   9, 2 ], "09"  },
		{ [  10, 1 ], "A"   },
		{ [  10, 2 ], "0A"  },
		{ [  11, 1 ], "B"   },
		{ [  12, 1 ], "C"   },
		{ [  13, 1 ], "D"   },
		{ [  14, 1 ], "E"   },
		{ [  15, 1 ], "F"   },
		{ [  16, 1 ], "0"   },
		{ [  16, 2 ], "10"  },
		{ [ 255, 2 ], "FF"  },
		{ [ 256, 3 ], "100" }
	].

islower(C) when C >= $a and (C =< $z) -> true;
islower(_) -> false.

isupper(C) when C >= $A and (C =< $Z) -> true;
isupper(_) -> false.

isdigit(C) when C >= $0 and (C =< $9) -> true;
isdigit(_) -> false.

isalpha(C) -> islower(C) or isupper(C).

isalnum(C) -> isalpha(C) or isdigit(C).

% prepare code for being run in a shell
% escape: ", $
shell_escape(Str) ->
  %{ok, Match} = re:compile("[\"$`]"),
  %re:replace(Str, Match,"\\&", [{return, list}]).
	% bloody hell, the re module doesn't seem to be working
  esc_(Str, []).

esc_([], Esc) ->
  Esc;
esc_("`" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "\\\`");
esc_("\"" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "\\\"");
esc_("$" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "\\$");
esc_([C|Unesc], Esc) ->
  esc_(Unesc, Esc ++ [C]).

test_shell_escape() ->
	[
  	{ [ "" ], "" },
  	{ [ " " ], " " },
  	{ [ "\"" ], "\\\"" },
  	{ [ "`ls`" ], "\\\`ls\\\`" },
  	{ [ "$SAFE=4" ], "\\\$SAFE=4" }
	].


timestamp() ->
	{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
	lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
		[Year,Month,Day,Hour,Min,Sec])).

