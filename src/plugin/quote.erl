% ex: set ts=2 noet:
%  

-module(quote).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).

test() ->
	true.

loop() ->
	receive
		{ act, Pid, Irc, _Msg, Dst, Nick, ["quote", Who | Search]} ->
			quote(Pid, Irc, _Msg, Dst, Nick, ["quote", Who | Search]),
			loop();
		{ act, _, _, _, _, _, _ } ->
			loop();
		{ help, Pid, Dst, Nick } ->
			help(Pid, Dst, Nick),
			loop()
	end.

quote(Pid, Irc, _Msg, Dst, Nick, ["quote", Who | Search]) ->
	Search2 = util:j(Search),
	Someone = Who,
	io:format("quote Someone=~p Search2=~p~n", [Someone, Search2]),
	PathSafe = % verify path contains no funny business
		lists:all(
			fun(C)->
				char:isalnum(C) or char:isspace(C)
			end, Someone),
	if
		not PathSafe -> Irc;
		PathSafe ->
			Path = "quote/" ++ Someone,
			Quotes = lists:filter(
				fun(Q) ->
					("" == Search2) or (string:str(Q, Search2) /= 0)
				end,
				util:readlines(Path)),
			if
				0 == length(Quotes) ->
					Pid ! {q, irc:resp(Dst, Nick, "Quote not found.")};
				true ->
					{S1, S2, S3} = now(),
					random:seed(S1, S2, S3),
					Quote = util:rtrim( % trim newline
						lists:nth(random:uniform(length(Quotes)), Quotes), 10),
					Pid ! {q, irc:resp(Dst, Nick, Quote)}
			end
	end.

help(Pid, Dst, Nick) ->
	Who = % list quotefiles
		case file:list_dir("quote") of
			{ok, List} ->
				lists:filter( % disregard dot-files, sort
					fun(C) ->
						(length(C) > 0) and
						(lists:nth(1, C) /= $.)
					end,
					lists:sort(List));
			{error, _} -> []
		end,
	Pid ! { q,
		[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
			[
				"[\"quote\", Who] -> random quote from Who",
				"Who = [\"" ++ util:join("\",\"", Who) ++ "\"]"
			]
		] }.

