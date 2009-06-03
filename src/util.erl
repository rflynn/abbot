% ex: set ts=2 noet:
% $Id$
% stuff i wish existed in the erlang standard libraries

-module(util).
-author("pizza@parseerror.com").
-export(
	[
		relpath/2,
		min/2,
		join/2, j/1,
		rtrim/2, rtrim/1,
		ltrim/3, ltrim/2, ltrim/1,
		trim/2, trim/1,
		split/3,
		tokens/3,
		show/1,
		nth/3,
		unescape/1,
		lines/1,
		utime_diffsec/2,
		readlines/1,
		ensure_dir/1,
		test/0
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
			{ tokens,		test_tokens()			}
		]).

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

