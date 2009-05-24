% ex: set ts=2 noet:
% $Id$

-module(util).
-author("pizza@parseerror.com").
-export(
	[
		join/2,
		j/1,
		rtrim/2,
		test/0
	]).
-import(ctype).
-import(test).

% for some stupid reason the "lists" module doesn't have a join()...
join(_, [Head]) ->
	Head;
join(Glue, [Head|Tail]) ->
	Head ++ Glue ++ join(Glue, Tail);
join(_, []) ->
	"".

test() ->
	test:unit(util,
		[
			{ j, test_j() }
		]).

% wrapper for most common join
j(X) -> join(" ", X).

test_j() ->
	[
		{ [], [] },
		{ [""], "" },
		{ ["a","b"], "a b" }
	].

rtrim("", _) -> "";
rtrim(Word, Trim) when is_list(Word) ->
	Char = lists:last(Word),
	case Trim == Char of
		true -> rtrim(lists:sublist(Word, length(Word)-1), Trim);
		false -> Word
		end.

test_rtrim() ->
	[
		{ [ "", $? ], "" },
		{ [ "?", $? ], "" },
		{ [ "a", $? ], "a" },
		{ [ "aa", $? ], "aa" },
		{ [ "a?", $? ], "a" },
		{ [ "?a?", $? ], "?a" }
	].

