% ex: set ts=2 noet:
% $Id$
% stuff i wish existed in the erlang standard libraries

-module(util).
-author("pizza@parseerror.com").
-export(
	[
		min/2,
		join/2, j/1,
		rtrim/2, rtrim/1,
		ltrim/2, ltrim/1,
		lines/1,
		split/3,
		show/1,
		nth/3,
		unescape/1,
		test/0
	]).
-import(test).

test() ->
	test:unit(util,
		[
			{ j, test_j() },
			{ rtrim, test_rtrim() },
			{ ltrim, test_ltrim() },
			{ show, test_show() },
			{ unescape, test_unescape() }
		]).

min(A, B) ->
	if
		A < B -> A;
		true -> B
		end.

join(_, [Head]) when is_integer(Head)->
	[ Head ];
join(_, [Head]) ->
	Head;
join(Glue, [Head|Tail]) when is_integer(Head) ->
	integer_to_list(Head) ++ Glue ++ join(Glue, Tail);
join(Glue, [Head|Tail]) when is_tuple(Head) ->
	tuple_to_list(Head) ++ Glue ++ join(Glue, Tail);
join(Glue, [Head|Tail]) ->
	Head ++ Glue ++ join(Glue, Tail);
join(_, []) ->
	"".

% wrapper for most common join
j(X) -> join(" ", X).

test_j() ->
	[
		{ [], [] },
		{ [""], "" },
		{ ["a","b"], "a b" }
		%{ [$1,$2], "1 2" }
	].

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
		{ "", "" },
		{ " ", "" },
		{ "a", "a" },
		{ "aa", "aa" },
		{ "a ", "a" },
		{ " a ", " a" }
	].

ltrim(Str) ->
	ltrim(Str, 32).

ltrim([], _) -> [];
ltrim([H|T], Chr) when H == Chr ->
	ltrim(T, Chr);
ltrim(Str, _) ->
	Str.

test_ltrim() ->
	[
		{ "", "" },
		{ " ", "" },
		{ "  ", "" },
		{ "a", "a" },
		{ "aa", "aa" },
		{ " aa", "aa" },
		{ "  aa", "aa" },
		{ " a a", "a a" },
		{ "", "" }
	].

split(Str, Chr, Limit) ->
	split_(Str, Chr, Limit, []).

split_(Str, _, Limit, List) when Limit == 0 ->
	List ++ [Str];
split_(Str, Chr, Limit, List) when Limit > 0 ->
	Index = string:chr(Str, Chr),
	case Index of
		0 -> split_(Str, Chr, 0, List);
		_ ->
			Left = string:substr(Str, 1, Index-1),
			Right = string:substr(Str, Index+1),
			split_(Right, Chr, Limit-1, List ++ [ Left ])
			end.

lines(Str) ->
	string:tokens(Str, "\n").

% show X in the most human-friendly way
show(X) ->
	lists:flatten(io_lib:format("~p", [X])).

test_show() ->
	[
		{ "", "[]" },
		{ nil, "nil" },
		{ [], "[]" },
		{ 0, "0" },
		{ "a", "\"a\"" },
		{ [1,2,3], "[1,2,3]" },
		{ "abc", "\"abc\"" }
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
		{ "\\\"hello\\\"", "\"hello\""},
		{ "\\\"a\\\n\\\b\\\"", "\"a\n\b\""}
	].

nth(N, List, Else) when N >= 0 ->
	if
		length(List) < N -> Else;
		true -> lists:nth(N, List)
	end.

test_nth() ->
	[
		{ true, [ 0, [], nil ], nil },
		{ true, [ 1, [], nil ], nil },
		{ true, [ 100, [], nil ], nil },
		{ true, [ 0, [4,5,6], nil ], nil },
		{ true, [ 1, [4,5,6], nil ], 4 },
		{ true, [ 2, [4,5,6], nil ], 5 },
		{ true, [ 3, [4,5,6], nil ], 6 },
		{ true, [ 4, [4,5,6], nil ], nil }
	].

